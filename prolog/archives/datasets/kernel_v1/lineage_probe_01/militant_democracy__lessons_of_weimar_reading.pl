% ============================================================================
% CONSTRAINT STORY: militant_democracy__lessons_of_weimar_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_militant_democracy_lessons_of_weimar_reading, []).

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
 *   constraint_id: militant_democracy__lessons_of_weimar_reading
 *   human_readable: Militant Democracy: Lessons of Weimar Reading
 *   domain: constitutional_law/doctrinal
 *
 * SUMMARY:
 *   Militant democracy, as instantiated in the lessons-of-Weimar reading, is
 *   the Federal Republic's constitutional doctrine that rejects value-neutral
 *   proceduralism in favor of active self-defense against anti-constitutional
 *   forces. The reading grounds this doctrine in a specific historical causal
 *   claim: Weimar fell because it remained neutral about forces that sought
 *   to destroy it, permitting the Nazis to use democratic freedoms to seize
 *   power. Bonn resolved that a second republic would never commit that error
 *   — the constitution itself would be actively defended, not passively
 *   presumed. This reading differs from its siblings (basic-rights-forfeiture
 *   and party-ban instrument) in locating the justification in historical
 *   lesson rather than in doctrinal mechanics. The constraint exhibits the
 *   classic structure of justified suppression: a genuine coordination
 *   problem (preserving constitutional order) married to asymmetric
 *   extraction (suppression applied to the radical opposition, not to the
 *   majority). The extractiveness of 0.52 and suppression of 0.68 reflect a
 *   tangled_rope: real coordination function, but significant coercive
 *   overhead and asymmetric impact. The theater ratio of 0.35 (relatively
 *   low) indicates that the doctrine's justification rests substantially on
 *   material threat assessment rather than performative ritual — though as
 *   the threat recedes over time (Cold War end, successful 70-year
 *   stability), the theater rises slightly, suggesting early piton dynamics
 *   where historical narrative must increasingly sustain the doctrine.
 *
 * KEY AGENTS:
 *   - Second Republic (Bonn/Federal Republic): Primary beneficiary (institutional/arbitrage) — militant democracy doctrine ensures institutional survival; can modulate enforcement based on threat assessment
 *   - Value-Neutral Proceduralism: Primary victim (powerless/trapped) — abstract liberal principle that cannot defend itself; suppressed by doctrine that rejects neutrality
 *   - Radical Opposition / Antidemocratic Forces: Secondary victim (powerless/trapped) — suppressed through party bans and speech restrictions justified by Weimar memory
 *   - Constitutional Court (Karlsruhe): Institutional beneficiary (institutional/constrained) — gains authority to assess and suppress threats; also carries burden of rule-of-law legitimation
 *   - Liberal Democratic Majority Coalition: Secondary beneficiary (powerful/mobile) — protected by suppression of radical minority; has agency to revise doctrine if consensus shifts
 *   - Weimar Historical Narrative: Epistemic actor (analytical/analytical) — the causal story 'Weimar fell because it was neutral' grounds the entire doctrine; contested by historians and political scientists
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(militant_democracy__lessons_of_weimar_reading, 0.52).
domain_priors:suppression_score(militant_democracy__lessons_of_weimar_reading, 0.68).
domain_priors:theater_ratio(militant_democracy__lessons_of_weimar_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(militant_democracy__lessons_of_weimar_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(militant_democracy__lessons_of_weimar_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(militant_democracy__lessons_of_weimar_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(militant_democracy__lessons_of_weimar_reading, tangled_rope).
narrative_ontology:human_readable(militant_democracy__lessons_of_weimar_reading, "Militant Democracy: Lessons of Weimar Reading").
narrative_ontology:topic_domain(militant_democracy__lessons_of_weimar_reading, "constitutional_law/doctrinal").

domain_priors:requires_active_enforcement(militant_democracy__lessons_of_weimar_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(militant_democracy__lessons_of_weimar_reading, '812567f1-a702-401c-bc41-bfeede7328ff').
narrative_ontology:cs_kernel_codification('812567f1-a702-401c-bc41-bfeede7328ff', fixed_text).
narrative_ontology:cs_authority_grounding('812567f1-a702-401c-bc41-bfeede7328ff', lineage).
narrative_ontology:cs_interpretation_layer_present('812567f1-a702-401c-bc41-bfeede7328ff').
narrative_ontology:cs_reading_relation('812567f1-a702-401c-bc41-bfeede7328ff', militant_democracy__basic_rights_forfeiture_reading, coexists_with).
narrative_ontology:cs_reading_relation('812567f1-a702-401c-bc41-bfeede7328ff', militant_democracy__party_ban_instrument_reading, coexists_with).
narrative_ontology:cs_axiom('812567f1-a702-401c-bc41-bfeede7328ff', foundational, weimar_collapse_causation_thesis).
narrative_ontology:cs_axiom_status(weimar_collapse_causation_thesis, holdable).
narrative_ontology:cs_axiom_grounding('812567f1-a702-401c-bc41-bfeede7328ff', weimar_collapse_causation_thesis, empirically_contingent).
narrative_ontology:cs_axiom('812567f1-a702-401c-bc41-bfeede7328ff', foundational, constitutional_self_preservation_necessity).
narrative_ontology:cs_axiom_status(constitutional_self_preservation_necessity, holdable).
narrative_ontology:cs_axiom_grounding('812567f1-a702-401c-bc41-bfeede7328ff', constitutional_self_preservation_necessity, deontological).
narrative_ontology:cs_reference_frame('812567f1-a702-401c-bc41-bfeede7328ff', neutral_republic_vulnerability).
narrative_ontology:cs_drift_state('812567f1-a702-401c-bc41-bfeede7328ff', contemporary_post_cold_war, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('812567f1-a702-401c-bc41-bfeede7328ff', '').
narrative_ontology:cs_kernel_id(militant_democracy__lessons_of_weimar_reading, militant_democracy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(militant_democracy__lessons_of_weimar_reading, second_republic_institutional_survival).
narrative_ontology:constraint_victim(militant_democracy__lessons_of_weimar_reading, value_neutral_proceduralism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUPPRESSED RADICAL OPPOSITION (SNARE) — Faces maximum suppression justified by invocation of Weimar collapse. Cannot exit the constraint (trapped within the national legal order); any use of democratic freedoms to advocate systemic change is vulnerable to militant democracy enforcement. The constraint extracts through preventive restriction: the opposition is trapped between enforced silence and self-incrimination. No coordination benefit — the suppression is asymmetric and total.
constraint_indexing:constraint_classification(militant_democracy__lessons_of_weimar_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTITUTIONAL COURT & GUARDIANS (TANGLED ROPE) — Faces a genuine coordination problem: how to preserve constitutional order against forces that would use constitutional rights to destroy constitutionalism. Also benefits from the enforcement prerogative (institutional power to adjudicate threat). The constraint is hybrid: real coordination function (defending the constitutional framework) combined with asymmetric extraction (concentrating judgment authority in the court). Suppression ≥ 0.60 reflects active enforcement infrastructure; extractiveness bounded by rule-of-law limits on force, but not zero.
constraint_indexing:constraint_classification(militant_democracy__lessons_of_weimar_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE FEDERAL REPUBLIC AS SURVIVOR (ROPE) — Net beneficiary of the constraint (institutional survival is the explicit benefit). Experiences militant democracy as pure coordination: the doctrine solves the collective action problem of preventing democratic collapse. The republic can arbitrage between strict enforcement and liberal tolerance depending on threat level. High agency, clear exit options (relax enforcement when threats recede). No asymmetric extraction — the benefit is distributed as institutional survival, not concentrated on a beneficiary class.
constraint_indexing:constraint_classification(militant_democracy__lessons_of_weimar_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LIBERAL DEMOCRATIC MAJORITY COALITION (TANGLED ROPE) — Benefits from constitutional stability provided by militant democracy while bearing suppression costs selectively (radical opposition), not universally. Has mobile exit options (can liberalize enforcement if majority consensus shifts). Coordination function is genuine (collective defense of democratic order) but extraction is asymmetric (restrictions applied to minority, not majority). Experiences the constraint as legitimate but coercive — moderate effective extraction because the majority has agency and can revise the doctrine if consensus collapses.
constraint_indexing:constraint_classification(militant_democracy__lessons_of_weimar_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: WEIMAR HISTORICAL MYTHOLOGY (PITON) — The doctrine rests on a contested historical narrative: 'Weimar fell because it was too neutral.' The narrative functions performatively — it legitimates suppression through historiography rather than through active verification of contemporary threat. Theater ratio 0.35 reflects that the core justification is narrative-dependent; the actual threat assessment is often weaker than the evoked historical memory. The piton derives from the doctrine's dependence on a maintained historical reading that may not survive empirical scrutiny.
constraint_indexing:constraint_classification(militant_democracy__lessons_of_weimar_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some self-defense mechanism is inherent to any constitutional order: no system can remain neutral about forces that seek to destroy it. This perspective sees militant democracy as an immutable property of constitutional life itself — a natural law of political systems. However, this naturalizes what is a contingent institutional choice. The engine will compute this as a false summit because the doctrine has identifiable beneficiaries (the second republic's institutional survival, the majority coalition) and clear suppressive mechanisms that could be designed otherwise.
constraint_indexing:constraint_classification(militant_democracy__lessons_of_weimar_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(militant_democracy__lessons_of_weimar_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(militant_democracy__lessons_of_weimar_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(militant_democracy__lessons_of_weimar_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(militant_democracy__lessons_of_weimar_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(militant_democracy__lessons_of_weimar_reading, TR),
    TR >= 0.70.

:- end_tests(militant_democracy__lessons_of_weimar_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The doctrine concentrates institutional power in the Constitutional Court to assess and suppress threats. The beneficiary (second republic survival) is genuine, but the suppression mechanism is asymmetric — applied only to those deemed threats, not universally. The extractiveness is not maximal (not snare-level, ε ≥ 0.66) because: (1) the coordination function is real (preserving constitutional order is not a cover story), (2) the suppression is rule-of-law bounded (not arbitrary), (3) the beneficiary class (all citizens of a stable republic) is broad. But extractiveness is above rope level (ε ≤ 0.45) because the asymmetry is substantial and the mechanism is actively enforced. Suppression (0.68): High. The doctrine creates significant barriers to exit for those classified as anti-constitutional: they cannot legally advocate systemic change without triggering suppression mechanisms. No right to be hostile to the constitutional order; hostile actors face speech restrictions, party bans, and rights forfeiture. Suppression is structural, not arbitrary, but it is substantial. Theater ratio (0.35): Low-moderate. The doctrine's core justification is the historical causal claim about Weimar, which is empirically contestable. The doctrinal mechanics (Article 21 party bans, Article 18 rights forfeiture) are functionally operative, not purely performative. However, as the historical threat recedes (Cold War end, 40+ years of stability), the narrative must work harder to sustain the suppression — suggesting early piton dynamics.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates maximal perspectival disagreement. The suppressed radical opposition sees pure snare: trapped, with no exit from suppression justified by historical narrative. The Constitutional Court sees justified tangled_rope: genuine coordination problem (preserving order) plus institutional authority (beneficiary). The Federal Republic sees rope: pure coordination benefit, with suppression as necessary cost. The liberal majority sees tangled_rope: benefits from stability while minority bears suppression costs. The historical narrative sees piton: the doctrine's function has attenuated as immediate threat receded, yet suppression machinery persists through inertia. The analytical observer risks seeing mountain: militant democracy as inherent to constitutional self-preservation. But the structural data (clear beneficiaries, asymmetric suppression, institutional concentration of authority) reveals this mountain as false — the doctrine is a contingent choice, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   The core directionality vector runs from the radical opposition (d ≈ 0.95, maximum target status) to the constitutional court and republic (d ≈ 0.05, beneficiary status with arbitrage options). The liberal majority occupies the middle (d ≈ 0.40, selective beneficiary status for majority members, suppression costs for others). The first-order power analysis: powerless opposition faces trapped exit → very high d → very high χ. Institutional beneficiary faces arbitrage exit → very low d → negative or near-zero χ. But the sigmoid f(d) function and scope σ(S) apply at the national level, creating context-dependent effective extraction. The key structural insight is that this is not neutral proceduralism — the doctrine explicitly rejects neutrality in favor of defending a specific order. The beneficiary is not a person but an institutional arrangement (the constitutional order itself). The victim is not a person but a principle (value-neutral proceduralism) and those classified as threats to the order. This produces a peculiar directionality signature where the beneficiary is collective/abstract and the victims are concrete/specific.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that militant democracy contains genuine coordination function (preserving constitutional order) combined with asymmetric extraction (suppression of radical opposition). This is textbook tangled_rope: 0.40 ≤ χ ≤ 0.90, base extraction ε ≥ 0.30, suppression ≥ 0.40, with BOTH coordination function AND asymmetric extraction present. The snare perspective (radical opposition) sees only extraction; the rope perspective (republic) sees only coordination. The truth is both. The mandate is not to choose between them but to recognize that the constraint simultaneously coordinates and extracts, and to measure how much of each. The doctrine's ethics depend on whether the historical causal claim (Weimar fell because of neutrality) is true and whether the suppression level is proportionate to the actual threat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    weimar_causality_thesis,
    'Did Weimar''s collapse actually result from institutional neutrality toward antidemocratic forces, or from material conditions, economic crisis, and institutional weakness independent of procedural design?',
    'Comparative institutional analysis: other democracies with similar neutrality doctrines and their trajectories; counterfactual modeling of Weimar with militant democracy safeguards; historical scholarship on causality of regime collapse',
    'If the thesis is false: the entire doctrinal justification rests on a misreading of history. Militant democracy would reclassify from justified suppression to pre-emptive extraction. If true: the doctrine is structurally necessary self-defense, not extraction. ε would remain ~0.52 but classification would shift toward tangled_rope with clearer coordination justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weimar_causality_thesis, empirical, 'Whether Weimar''s neutrality toward antidemocratic forces caused its collapse').

omega_variable(
    contemporary_threat_calibration,
    'Does the contemporary threat environment in the Federal Republic justify the suppression level encoded in militant democracy doctrine, or does the doctrine persist as historical inertia protecting against a receded threat?',
    'Longitudinal analysis of actual party bans and Article 18 forfeitures: frequency, legal success rate, threat assessment accuracy; comparison of contemporary security threat levels to historical Weimar period; survey of constitutional scholars on whether doctrine remains proportionate',
    'If doctrine is over-calibrated: militant democracy functions as piton (inertial maintenance of suppression) rather than tangled_rope (genuine coordination). Extractiveness would remain ~0.52 but the coordination justification weakens. If doctrine is appropriately calibrated: tangled_rope classification confirmed with strong beneficiary-victim asymmetry justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contemporary_threat_calibration, empirical, 'Whether suppression levels match contemporary threat assessment').

omega_variable(
    reading_contest_foreclosure,
    'Does the lessons-of-Weimar reading logically foreclose the basic-rights-forfeiture reading, or do the two coexist as competing legitimate readings of the same constitutional kernel?',
    'Jurisprudential analysis of Karlsruhe decisions: are Article 18 and Article 21 interpreted as species of a single martial principle (lessons reading) or as distinct doctrinal tools with separate justifications (coexistence)? Analysis of whether courts invoke Weimar causality equally for both mechanisms.',
    'If forecloses: the readings are mutually exclusive; only one can be the operative doctrine. If coexist: both readings are live, producing divergent suppression mechanisms with different targets and justifications. Affects classification of the sibling constraints and network relationships.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Structural relationship between lessons-of-Weimar and basic-rights-forfeiture readings').

omega_variable(
    rule_of_law_boundary_paradox,
    'At what level of suppression does militant democracy violate the rule of law principle it claims to protect? Is there a determinate threshold where legitimate self-defense becomes authoritarian pre-emption?',
    'Comparative analysis of democracies with and without militant democracy provisions; longitudinal measurement of suppression creep; legal theory on limits of constitutional self-defense; empirical tracking of whether doctrine scope expands over time',
    'If paradox is resolvable: suppression level remains bounded and the tangled_rope classification holds. If paradox is irresolvable: the doctrine is self-undermining — it claims to protect democratic order by restricting democracy. Extractiveness would rise toward snare territory (ε > 0.65) and classification would shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rule_of_law_boundary_paradox, conceptual, 'Whether militant democracy''s self-defense can be bounded by rule-of-law principles').

omega_variable(
    epistemic_authority_concentration,
    'Should threat assessment for militant democracy enforcement be concentrated in the Constitutional Court (current German model) or distributed across multiple democratic institutions?',
    'Comparative study of party ban decisions across democracies; analysis of Karlsruhe''s success rate in threat prediction; empirical comparison of concentrated vs distributed enforcement outcomes; survey of democratic theory on who should judge threats to democracy',
    'If concentrated authority is justified: the institutional power asymmetry is a necessary feature, not an extractive bug. If distributed authority would be better: the concentration represents unnecessary suppression. Affects whether the beneficiary is ''the court''s authority'' (extraction) or ''the republic''s survival'' (coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_authority_concentration, preference, 'Whether threat assessment should be concentrated in one court or distributed').

omega_variable(
    natural_law_vs_constructed_choice,
    'Is militant democracy an inherent natural law of constitutional self-preservation, or a contingent doctrinal choice that other constitutional orders have rejected?',
    'Comparative constitutional design: does U.S. Constitution employ militant democracy? Does it need to? Does UK constitutional tradition include such safeguards? Analysis of why some democracies adopt the doctrine and others do not; whether democracies without militant democracy are more fragile',
    'If natural law: mountain classification for the analytical perspective is correct. If contingent choice: the mountain is a false summit, masking a tangled_rope that beneficiaries legitimize through naturalization. Affects credibility of all six perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_choice, empirical, 'Whether militant democracy is natural law or contingent doctrinal choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(militant_democracy__lessons_of_weimar_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(md_weimar_tr_t0, militant_democracy__lessons_of_weimar_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(md_weimar_tr_t15, militant_democracy__lessons_of_weimar_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(md_weimar_tr_t40, militant_democracy__lessons_of_weimar_reading, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(md_weimar_be_t0, militant_democracy__lessons_of_weimar_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(md_weimar_be_t15, militant_democracy__lessons_of_weimar_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(md_weimar_be_t40, militant_democracy__lessons_of_weimar_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(md_weimar_su_t0, militant_democracy__lessons_of_weimar_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(md_weimar_su_t15, militant_democracy__lessons_of_weimar_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(md_weimar_su_t40, militant_democracy__lessons_of_weimar_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(militant_democracy__lessons_of_weimar_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(militant_democracy__lessons_of_weimar_reading, militant_democracy__basic_rights_forfeiture_reading).
narrative_ontology:affects_constraint(militant_democracy__lessons_of_weimar_reading, militant_democracy__party_ban_instrument_reading).

% DUAL FORMULATION NOTE:
% Militant democracy is a contested kernel with three distinct readings. This file models the lessons-of-Weimar reading (suppression justified by remembered collapse). The sibling readings (basic_rights_forfeiture and party_ban_instrument) model the same suppression mechanisms justified through different doctrinal principles. All three readings coexist in German constitutional practice and jurisprudence. Each reading has its own ε, beneficiary/victim structure, and perspective set. The network links them as readings of the same kernel, not as hierarchical dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(militant_democracy__lessons_of_weimar_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
