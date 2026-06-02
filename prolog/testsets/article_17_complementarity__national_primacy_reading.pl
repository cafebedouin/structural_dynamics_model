% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__national_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__national_primacy_reading, []).

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
 *   constraint_id: article_17_complementarity__national_primacy_reading
 *   human_readable: Article 17 Complementarity Principle (National Primacy Reading)
 *   domain: international_law/state_sovereignty/criminal_justice
 *
 * SUMMARY:
 *   Article 17 of the Rome Statute establishes the complementarity principle:
 *   the ICC is a court of last resort, admissible only when national courts
 *   are 'unwilling' or 'unable' to investigate or prosecute. The national
 *   primacy reading interprets this as a strong presumption in favor of
 *   domestic proceedings — national courts are presumed adequate unless the
 *   ICC can affirmatively prove they are a sham. This reading prioritizes
 *   state sovereignty, defers to national judicial authority, and places a
 *   high burden on the ICC Prosecutor to demonstrate inadmissibility. It
 *   reflects a legal doctrine that experienced substantial entrenchment
 *   through the first two decades of ICC operation, during which the
 *   Prosecutor struggled to meet the Article 17(2) standard and victims in
 *   weak-but-technically-functioning judiciaries (particularly in African
 *   states) found themselves excluded from ICC reach while their domestic
 *   courts delivered inadequate justice. The constraint is tangled: it
 *   coordinates state participation in the international criminal justice
 *   system (states cooperate knowing their domestic proceedings are presumed
 *   adequate) while simultaneously extracting from victims whose states
 *   maintain façades of functionality without delivering justice. Theater
 *   ratio (0.64) reflects the performative nature of 'adequacy' assessment —
 *   the OTP must prove unwillingness or inability through proxies
 *   (independence of judges, political motivation of investigators) that
 *   cannot be directly observed from outside the state system.
 *
 * KEY AGENTS:
 *   - National Judiciaries: Primary beneficiary (institutional/arbitrage) — experience the constraint as coordination; presumption of adequacy protects their authority and decision-making autonomy
 *   - Sovereignty-Maximizing States: Primary beneficiary (institutional/arbitrage) — preserve room to manage their own accountability without ICC interference; insulation from international review
 *   - Victims in Weak-but-Functional Jurisdictions: Primary victim (powerless/trapped) — fall outside ICC reach because their state courts meet the (low) adequacy threshold; face combined harm of inadequate domestic justice + foreclosed international remedy
 *   - International Justice Advocates and NGOs: Secondary victim (organized/constrained) — constrained by high burden to prove sham status; must litigate inadequacy rather than pursuing substantive justice claims
 *   - ICC Office of the Prosecutor: Institutional actor (institutional/constrained) — operates under structural constraint of high inadmissibility burden; discretion severely limited by Article 17 interpretation
 *   - Article 17 Legal Apparatus: Institutional inertia (institutional/constrained) — the formal doctrine persists through reliance despite declining functional verification of actual court adequacy
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing state sovereignty as foundational law rather than examining it as contingent institutional prioritization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.52).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.58).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "Article 17 Complementarity Principle (National Primacy Reading)").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/state_sovereignty/criminal_justice").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, '21347715-85d8-4920-8041-3adc654d0622').
narrative_ontology:cs_kernel_codification('21347715-85d8-4920-8041-3adc654d0622', formalized).
narrative_ontology:cs_authority_grounding('21347715-85d8-4920-8041-3adc654d0622', lineage).
narrative_ontology:cs_interpretation_layer_present('21347715-85d8-4920-8041-3adc654d0622').
narrative_ontology:cs_reading_relation('21347715-85d8-4920-8041-3adc654d0622', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('21347715-85d8-4920-8041-3adc654d0622', foundational, state_sovereignty_primacy_in_criminal_jurisdiction).
narrative_ontology:cs_axiom_status(state_sovereignty_primacy_in_criminal_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('21347715-85d8-4920-8041-3adc654d0622', state_sovereignty_primacy_in_criminal_jurisdiction, conventional).
narrative_ontology:cs_axiom('21347715-85d8-4920-8041-3adc654d0622', foundational, presumption_of_domestic_adequacy_absent_proof_of_sham).
narrative_ontology:cs_axiom_status(presumption_of_domestic_adequacy_absent_proof_of_sham, holdable).
narrative_ontology:cs_axiom_grounding('21347715-85d8-4920-8041-3adc654d0622', presumption_of_domestic_adequacy_absent_proof_of_sham, deontological).
narrative_ontology:cs_reference_frame('21347715-85d8-4920-8041-3adc654d0622', state_sovereignty_primacy).
narrative_ontology:cs_drift_state('21347715-85d8-4920-8041-3adc654d0622', contemporary_justice_accountability_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('21347715-85d8-4920-8041-3adc654d0622', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, state_judiciaries).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_in_weak_but_functional_jurisdictions).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, international_justice_system_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VICTIMS IN WEAK JURISDICTIONS (SNARE) — Trapped by the high inadmissibility threshold. A state with demonstrably corrupt but technically functioning courts is insulated from ICC jurisdiction under this reading. Victims face the combined extraction: inadequate domestic justice + foreclosed international remedy. Maximum suppression and no exit.
constraint_indexing:constraint_classification(article_17_complementarity__national_primacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERNATIONAL JUSTICE ADVOCATES (TANGLED ROPE) — Constrained by the high burden imposed on ICC prosecution to prove inadequacy. The reading's beneficiary-protective presumption creates genuine coordination (states can cooperate in good faith, knowing their domestic proceedings are presumed adequate) alongside extraction (the ICC's remedial capacity is structurally constrained, forcing advocates to litigate 'sham' status rather than substantive justice). Mixed coordination and asymmetric constraint.
constraint_indexing:constraint_classification(article_17_complementarity__national_primacy_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: NATIONAL JUDICIARIES (ROPE) — Primary beneficiary of this reading. The presumption of adequacy creates coordination function (ICC defers, states retain decision authority over their own accountability mechanisms) with minimal coercive overhead. States experience this as pure coordination: international recognition of domestic judicial authority. Net beneficiary position.
constraint_indexing:constraint_classification(article_17_complementarity__national_primacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRANSITIONAL JUSTICE MECHANISMS (SCAFFOLD) — See the complementarity principle as a temporary framework enabling state-led transitional justice during post-conflict reconstruction. This reading supports hybrid courts and Truth and Reconciliation Commissions as interim solutions with sunset logic: they prove domestic capacity and demonstrate adequate proceedings, potentially leading to eventual ICC withdrawal. Low theater, genuine coordination function with explicit or implicit time limits.
constraint_indexing:constraint_classification(article_17_complementarity__national_primacy_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: THE ARTICLE 17 APPARATUS (PITON) — The formal criteria for assessing 'unwillingness' and 'inability' (Art. 17(2)) are substantially performative. Courts can appear functional without delivering justice; state consent can masquerade as institutional integrity. The apparatus continues to operate as the primary gateway to ICC jurisdiction, but the theater ratio indicates declining functional verification — the legal test certifies what it cannot actually observe (internal state motivation, genuine independence of judges, actual impartiality of proceedings). Inertial maintenance through institutional dependence.
constraint_indexing:constraint_classification(article_17_complementarity__national_primacy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SOVEREIGNTY AS NATURAL LAW (MOUNTAIN) — From a civilizational perspective, this reading naturalizes state sovereignty as an immutable principle: national courts have jurisdictional primacy as a fundamental structural feature of international law, not a contingent allocation choice. Complementarity is presented as inherent to the state system itself. However, structural data reveals this as a false summit: the beneficiary set (state judiciaries, sovereignty-maximizing states) is identifiable, and the framework actively privileges their interests — a constructed prioritization, not a law of nature.
constraint_indexing:constraint_classification(article_17_complementarity__national_primacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__national_primacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article_17_complementarity__national_primacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_17_complementarity__national_primacy_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, TR),
    TR >= 0.70.

:- end_tests(article_17_complementarity__national_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The national primacy reading creates asymmetric extraction favoring states and their judiciaries while burdening victims and international justice mechanisms. The extraction is not extreme (0.70+) because the doctrine includes genuine coordination: states can participate in the international system with reasonable certainty that domestic proceedings are presumed adequate, enabling cooperative governance. But the extraction is substantial because victims are systematically excluded from ICC reach based on a low threshold of domestic 'adequacy' that cannot be meaningfully verified from outside state boundaries. Suppression (0.58): Moderate-high. Significant barriers exist to proving a state's proceedings are a sham: (1) epistemological barrier — sham status requires proving internal state motivation, judicial bias, prosecutorial independence from outside the jurisdiction; (2) procedural barrier — high burden of proof; (3) political barrier — states retain veto power over cooperation and investigation access. Suppression has increased over the interval as the OTP doctrine hardened and admissibility challenges accumulated. Theater ratio (0.64): Moderate-high. The Article 17 apparatus relies on proxy indicators (judicial independence, prosecutorial impartiality) to assess court adequacy, but these proxies cannot capture actual delivery of justice or genuine impartiality. Courts that maintain procedural independence while being politically controlled, or judges who are independent but lack resources to conduct meaningful investigations, appear 'adequate' under the legal test while failing victims in practice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. State judiciaries see pure coordination (Rope) — they experience the presumption as recognizing their legitimate authority. Victims in weak jurisdictions see pure extraction (Snare) — they face combined harms with no remedy. International justice advocates see mixed coordination-extraction (Tangled Rope) — the system enables state cooperation while constraining ICC reach. Transitional justice mechanisms see a temporary scaffold with sunset logic — they operate under the presumption, proving capacity and enabling eventual ICC withdrawal. The Article 17 apparatus itself is piton — the legal test persists through institutional dependence despite declining functional verification. The analytical observer risks seeing state sovereignty as natural law (Mountain) when the structural data reveals beneficiary preference and constructed prioritization. The gap between victim experience (Snare) and state experience (Rope) from identical structural data reveals that the constraint's type depends entirely on agent position relative to the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness derives from structural position. National judiciaries occupy beneficiary + arbitrage position: they experience low or negative effective extraction (χ approximates negative value) because they gain decision authority and international deference without bearing costs. Victims in weak jurisdictions occupy victim + trapped position: they experience maximum effective extraction (χ approximates high value) because they bear costs (inadequate domestic justice + foreclosed international remedy) with no exit. International justice advocates occupy organized position with constrained exit: they experience moderate extraction (χ ≈ 0.6-0.7 range) because they have organizational capacity but face structural barriers to ICC access. The high inadmissibility threshold maintained by the reading directly translates to high d (directionality toward victim position) for these groups, producing elevated χ values. The beneficiary set's structural interests (state sovereignty, domestic judicial authority) are clearly identifiable and actively protected by the doctrine, undermining the mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading achieves mandatrophy closure by acknowledging that all six types are legitimate perspectival readings, but the national primacy reading constrains which perspectives can emerge. The reading's high inadmissibility threshold prevents snare-classified victims from accessing international remedy, effectively suppressing the snare perspective from policy relevance. This is not a logical contradiction but a structural feature of the reading: complementarity doctrine *is* designed to protect state sovereignty and limit ICC reach, so the suppression of snare-classified victims is the intended effect. The mandatrophy resolves by recognizing that the reading's legitimacy depends on accepting a premise (state sovereignty primacy) that beneficiary analysis reveals as contingent rather than foundational. If that premise shifts (international oversight reading becomes dominant), the classification distribution changes: snare perspective becomes policy-relevant, tangled rope perspective dominates, rope and scaffold perspectives recede. The reading resolves mandatrophy not by proving one type is 'correct,' but by showing that type distribution is reading-dependent — different choices about foundation (sovereignty vs. victim protection) produce different perspectival configurations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sham_versus_functional_inadequacy_threshold,
    'What threshold of judicial dysfunction justifies finding a state''s proceedings are a ''sham'' under Article 17(2)(a), rather than merely weak or corrupted?',
    'Systematic analysis of ICC OTP Office of the Prosecutor) admissibility decisions; statistical correlation between factors assessed in unwillingness determination (political motive, bias of investigator, impartiality of prosecutor, independence of judge) and actual case outcomes; post-judgment analysis of convictions to assess whether convictions were politically motivated or judicially independent',
    'If threshold is low: ''sham'' status becomes discoverable for many weak states; ICC reach expands; more victims access international remedy. If threshold is high: only complete institutional collapse qualifies; most weak judiciaries remain insulated; national primacy strongly protected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sham_versus_functional_inadequacy_threshold, empirical, 'Admissibility threshold distinguishing sham from inadequate proceedings').

omega_variable(
    state_cooperation_substitution_adequacy,
    'Does genuine state cooperation with ICC investigation constitute an adequate domestic proceeding under Article 17, even when the state''s own courts are non-functional?',
    'Case-by-case analysis of hybrid prosecutions and state-ICC referral patterns; examination of complementarity judgments treating state cooperation as substitute for state proceedings; interviews with OTP prosecutors on evidentiary weight given to cooperation',
    'If cooperation suffices: national primacy is satisfied through deference to state choice rather than state capacity; complementarity becomes politically flexible. If capacity required: weak states with only cooperation cannot satisfy Article 17; victims in non-functional jurisdictions need direct ICC access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_cooperation_substitution_adequacy, empirical, 'Whether state cooperation substitutes for state court adequacy in complementarity test').

omega_variable(
    national_primacy_reading_versus_international_oversight_reading_logical_structure,
    'Do the national primacy and international oversight readings of Article 17 represent logically coexisting interpretations held by different parties in an ongoing dispute, or does one foreclose the other at the level of treaty interpretation doctrine?',
    'Jurisprudential analysis of ICJ and ICC case law; examination of whether states and the international community have articulated a single unified principle or competing frameworks; tracking of treaty amendment proposals and opt-out declarations indicating parties'' preferred reading',
    'If coexisting: both readings remain live options for different actors; complementarity is a genuinely contested principle. If one forecloses the other: Article 17 has a determinate meaning; the alternative reading is epistemically defeated but may persist as a strategic position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(national_primacy_reading_versus_international_oversight_reading_logical_structure, conceptual, 'Logical relationship between national primacy and international oversight readings of Article 17').

omega_variable(
    false_summit_natural_law_claim_on_sovereignty,
    'Is state sovereignty over domestic criminal proceedings a natural law of international relations, or is it a constructed institutional prioritization that benefits identifiable actors (national judiciaries, states with weak accountability systems)?',
    'Historical analysis of how complementarity doctrine has evolved; examination of whether other ICC principles (universality, justice for victims) would flow naturally from different premises; analysis of beneficiary set and their advocacy for high inadmissibility threshold',
    'If natural law: complementarity is unchallengeable principle; mountain classification is legitimate. If constructed: beneficiary presence triggers false summit detector; reclassification to higher-extraction type; mandatrophy analysis required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim_on_sovereignty, conceptual, 'Whether state sovereignty precedence is natural law or false summit naturalizing institutional allocation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(compl_natl_tr_t0, article_17_complementarity__national_primacy_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(compl_natl_tr_t5, article_17_complementarity__national_primacy_reading, theater_ratio, 5, 0.6).
narrative_ontology:measurement(compl_natl_tr_t10, article_17_complementarity__national_primacy_reading, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(compl_natl_be_t0, article_17_complementarity__national_primacy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(compl_natl_be_t5, article_17_complementarity__national_primacy_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(compl_natl_be_t10, article_17_complementarity__national_primacy_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(compl_natl_su_t0, article_17_complementarity__national_primacy_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(compl_natl_su_t5, article_17_complementarity__national_primacy_reading, suppression_requirement, 5, 0.56).
narrative_ontology:measurement(compl_natl_su_t10, article_17_complementarity__national_primacy_reading, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, article_17_complementarity__international_oversight_reading).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, icc_admissibility_threshold_burden_allocation).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, state_cooperation_versus_state_capacity_substitution).

% DUAL FORMULATION NOTE:
% Article 17 complementarity is a kernel with two distinct readings producing different constraint classifications. The national primacy reading (this file) emphasizes state sovereignty and high inadmissibility threshold, producing tangled rope classification with beneficiaries (states) and victims (justice-seeking populations in weak jurisdictions). The international oversight reading produces different constraint family with lower threshold and different beneficiary/victim alignment. Both readings instantiate complementarity doctrine but with structurally distinct extraction profiles. Network links indicate that this reading influences downstream constraints on admissibility burden allocation and state capacity substitution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_17_complementarity__national_primacy_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
