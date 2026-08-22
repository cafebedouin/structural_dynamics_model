% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__hyper_presidential_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__hyper_presidential_reading, []).

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
 *   constraint_id: fifth_republic_constitution__hyper_presidential_reading
 *   human_readable: Fifth Republic Constitution — Hyper-Presidential Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates the hyper-presidential reading of the Fifth
 *   Republic constitutional kernel: the presidency as the direct embodiment
 *   of national sovereign will, elected outside and above the parliamentary
 *   system, wielding Article 49.3 (forced adoption absent successful censure)
 *   and Article 16 (emergency powers) as ordinary tools of governance rather
 *   than exceptional safety valves. Under this reading the National Assembly
 *   is not a co-equal legislative partner but a body whose formal consent has
 *   been rendered largely a formality that the president's government can
 *   route around whenever floor arithmetic is unfavorable. This is one
 *   reading among three of the same constitutional kernel — the
 *   cohabitation_equilibrium_reading treats the same text as mandating
 *   negotiated dual-executive authority allocation, and the
 *   parliamentary_constraint_reading treats the president as bound to
 *   legislative authorization for implementation. Each reading is a
 *   structurally distinct constraint with its own ε; this file authors only
 *   the hyper-presidential one.
 *
 * KEY AGENTS:
 *   - incumbent_president: agenda_setter/beneficiary (institutional/arbitrage) — directs government use of forcing mechanisms
 *   - presidency_as_institution: beneficiary (institutional/analytical) — accrues expanding precedent across presidencies
 *   - national_assembly: payer (organized/constrained) — bears bypassed deliberation
 *   - opposition_parliamentary_blocs: payer (organized/constrained) — absorbs overridden preferences
 *   - electorate_seeking_legislative_recourse: payer (powerless/trapped) — no reversal path
 *   - constitutional_council: observer (institutional/analytical) — validates the mechanism procedurally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, 0.72).
domain_priors:suppression_score(fifth_republic_constitution__hyper_presidential_reading, 0.61).
domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Fifth Republic Constitution — Hyper-Presidential Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "constitutional/political").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, '1b232263-b571-4a14-8579-054acf875836').
narrative_ontology:cs_kernel_codification('1b232263-b571-4a14-8579-054acf875836', formalized).
narrative_ontology:cs_authority_grounding('1b232263-b571-4a14-8579-054acf875836', lineage).
narrative_ontology:cs_interpretation_layer_present('1b232263-b571-4a14-8579-054acf875836').
narrative_ontology:cs_reading_relation('1b232263-b571-4a14-8579-054acf875836', fifth_republic_constitution__parliamentary_constraint_reading, forecloses).
narrative_ontology:cs_reading_relation('1b232263-b571-4a14-8579-054acf875836', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('1b232263-b571-4a14-8579-054acf875836', foundational, president_embodies_national_will_directly).
narrative_ontology:cs_axiom_status(president_embodies_national_will_directly, holdable).
narrative_ontology:cs_axiom_grounding('1b232263-b571-4a14-8579-054acf875836', president_embodies_national_will_directly, conventional).
narrative_ontology:cs_axiom('1b232263-b571-4a14-8579-054acf875836', secondary, legislative_consent_is_ordinarily_dispensable).
narrative_ontology:cs_axiom_status(legislative_consent_is_ordinarily_dispensable, holdable).
narrative_ontology:cs_axiom_grounding('1b232263-b571-4a14-8579-054acf875836', legislative_consent_is_ordinarily_dispensable, instrumental).
narrative_ontology:cs_reference_frame('1b232263-b571-4a14-8579-054acf875836', gaullist_direct_mandate_sovereignty).
narrative_ontology:cs_drift_state('1b232263-b571-4a14-8579-054acf875836', post_quinquennat_synchronization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1b232263-b571-4a14-8579-054acf875836', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, executive_ministerial_apparatus).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, national_assembly).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, opposition_parliamentary_blocs).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, electorate_seeking_legislative_recourse).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, direct_popular_mandate_doctrine).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, national_unity_embodiment_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds direct popular mandate via universal suffrage election, appoints the prime minister, can invoke Article 16 emergency powers, dissolve the Assembly, and direct the government to force legislation through via Article 49.3 without a vote. Answers to no parliamentary confidence requirement between elections and treats legislative resistance as an obstacle to the popular will he was elected to execute.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, beneficiary).

% The office itself accrues expanding practical authority across successive presidencies as precedent for aggressive use of 49.3 and Article 16 accumulates; each unchallenged invocation widens the operating envelope for the next occupant regardless of personal restraint.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution, beneficiary,
    institutional, generational, analytical, national).

% Elected body whose deliberative and amendment functions are structurally bypassed when the government invokes Article 49.3 — a bill is deemed adopted without a vote unless a no-confidence motion succeeds, which risks dissolving the very body attempting to resist. Debate becomes theater once the government signals it will use the mechanism; the Assembly's exit is a no-confidence vote that can cost members their seats.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, national_assembly, payer,
    organized, biographical, constrained, national).

% Represent electoral constituencies whose legislative preferences are overridden when 49.3 forces adoption; their only formal recourse is a censure motion requiring an absolute majority they typically lack, especially in a presidential-majority configuration. They bear the political cost of appearing obstructionist for challenging a mechanism the constitution itself authorizes.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, opposition_parliamentary_blocs, payer,
    organized, biographical, constrained, national).

% Citizens who elected assembly members expecting deliberative representation find contested legislation enacted without a floor vote. Their exit is limited to protest, the next election cycle, or referenda the president alone can call — none of which reverses an already-enacted 49.3 measure.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, electorate_seeking_legislative_recourse, payer,
    powerless, biographical, trapped, national).

% Ministers serve at presidential direction and gain streamlined policy implementation when 49.3 removes the need to build a floor majority for each measure, reducing negotiation costs and accelerating the government's legislative program.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, executive_ministerial_apparatus, beneficiary,
    institutional, biographical, arbitrage, national).

% Reviews the constitutionality of specific bills and procedural challenges but has historically declined to rule on the political propriety or frequency of 49.3 invocation itself, treating it as a validly available constitutional tool rather than a structural imbalance to be checked.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__hyper_presidential_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for a government to pass its legislative program without perpetual floor negotiation, avoiding paralysis when no stable majority coalition exists — solving a genuine governability problem in a multiparty system.
% TRANSFER_FUNCTION: Moves effective lawmaking authority from the elected deliberative chamber to the president and the government he appoints and directs, converting parliamentary consent from an active vote into a passive default absent a costly no-confidence gamble.
% ABSENT_VOICES: Backbench deputies across party lines who would prefer genuine floor debate and amendment rights are structurally sidelined once 49.3 is invoked; minor parties without the numbers to mount credible censure motions have no practical voice in the outcome despite representing real constituencies.
% DISAPPEARANCE_RATIONALE: If the hyper-presidential reading's operative mechanisms (49.3 forcing, Article 16 emergency assumption, dissolution threat) were removed, governments would need to build genuine floor majorities for every measure, legislative bargaining power would shift decisively back to the Assembly, and coalition politics would become the dominant mode of governance rather than presidential direction.
% FOUNDING_PROBLEM: The Fourth Republic collapsed under chronic cabinet instability and parliamentary paralysis; the 1958 constitution was built to give the executive tools to govern decisively despite a fragmented, undisciplined parliament, ending the revolving-door governments of the previous regime.
% FOUNDING_PROBLEM_CORROBORATION: Presidents and their governments attest the instability risk remains live, citing coalition fragility whenever it recurs. Constitutional scholars, several former prime ministers under cohabitation, and comparative political scientists outside the executive's orbit attest that the instability problem has been substantially solved by modern party discipline and electoral calendar alignment, and that continued aggressive 49.3/Article 16 use now functions primarily as a majority-management tool rather than a stability safeguard.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__hyper_presidential_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__hyper_presidential_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__hyper_presidential_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fifth_republic_constitution__hyper_presidential_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__hyper_presidential_reading, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the measured interval (0.45 to 0.72) tracking the documented escalation in 49.3 invocation frequency and the normalization of using it for ordinary budget and policy bills rather than exceptional crises. Suppression (0.61) reflects the structural coercion built into the censure-motion mechanism: challenging a forced bill requires deputies to risk dissolution and their own seats, which chills resistance independent of any single president's temperament. Theater ratio (0.38) captures that floor debate increasingly proceeds as performance once a 49.3 invocation is signaled, since the outcome is already structurally determined.
 *
 * PERSPECTIVAL GAP:
 *   From the presidential seat, this is coordinated national governance solving a structural stability problem the Fourth Republic could not solve. From the Assembly and opposition seats, the identical textual provisions operate as an enforced transfer mechanism that converts their constitutional role from active legislator to passive rubber stamp. The engine computes these as different seat-level classifications from the same structural data; the divergence is not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The incumbent president and the presidency as an institution sit at the low-d beneficiary end: the constitutional text and accumulated precedent subsidize executive action at the direct expense of deliberative process. The National Assembly, opposition blocs, and the electorate sit at the high-d target end — they bear a transfer of effective lawmaking authority they cannot recover without an electorally costly and often unsuccessful no-confidence gamble. The ministerial apparatus co-benefits as an extension of presidential direction rather than as an independent seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Fourth Republic cabinet instability — was real, and the coordination function (enabling a government to actually govern without perpetual floor renegotiation) is genuine, which is why this reading classifies as tangled_rope rather than pure snare: there is an authentic coordination function underneath the extraction. But the founding_problem_status is contested precisely because modern party discipline and five-year electoral synchronization have substantially reduced the instability the mechanism was built to prevent, while invocation frequency has continued rising. This is the classic mandatrophy signature: a mechanism whose justifying crisis has receded while its operative footprint expands, and the tangled_rope classification prevents both extremes — mislabeling it pure snare (which would ignore the real 1958 governability problem it solved) and mislabeling it pure rope (which would ignore that it now transfers authority asymmetrically to a beneficiary that faces no matching accountability increment).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hyper_presidential_reading_identity,
    'Is the hyper-presidential reading of the Fifth Republic kernel the textually correct reading, or a drift interpretation that has hardened into practice through unchallenged precedent?',
    'Comparative analysis of constitutional drafting debates (the 1958 travaux préparatoires), Constitutional Council jurisprudence on 49.3/16 limits, and comparison with periods of cohabitation where the same text produced markedly different practical authority allocation.',
    'If the reading is textually intended, the extraction is a designed feature of the founding bargain; if it is drift from a more balanced original design, the extraction represents accumulated interpretive capture that the parliamentary_constraint_reading would characterize as an unauthorized expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hyper_presidential_reading_identity, conceptual, 'Whether hyper-presidentialism is the kernel''s intended reading or a drifted one.').

omega_variable(
    cohabitation_disproof_condition,
    'Do historical cohabitation periods (1986-88, 1993-95, 1997-2002) disprove the hyper-presidential reading by demonstrating the same text supports a genuinely constrained presidency when the Assembly majority opposes the president?',
    'Structural comparison of presidential 49.3/Article 16 usage rates and effective authority during cohabitation versus unified-majority periods.',
    'If cohabitation periods show the presidency genuinely constrained by an opposing Assembly majority, this suggests the hyper-presidential reading is not an intrinsic property of the text but a contingent function of electoral alignment — meaning this constraint''s high ε is conditional, not structural, and the cohabitation_equilibrium_reading may be the more accurate default account of the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cohabitation_disproof_condition, empirical, 'Whether cohabitation history falsifies the structural inevitability of hyper-presidentialism.').

omega_variable(
    founding_crisis_recurrence_risk,
    'Would dismantling the 49.3/Article 16 forcing apparatus reintroduce genuine Fourth-Republic-style governability collapse, or has electoral calendar synchronization (five-year concurrent terms since 2000) permanently resolved that risk independent of these mechanisms?',
    'Track record of governments under aligned electoral calendars that voluntarily restrained 49.3 use, and comparative study of peer parliamentary democracies without equivalent forcing mechanisms.',
    'If the calendar reform alone suffices for stability, continued reliance on 49.3/Article 16 is closer to pure institutional extraction than genuine coordination necessity, strengthening the mandatrophy finding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_crisis_recurrence_risk, empirical, 'Whether the founding governability problem persists independent of these specific mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__hyper_presidential_reading, 1958, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1958, 0.12).
narrative_ontology:measurement(fift_tr_t1971, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1971, 0.18).
narrative_ontology:measurement(fift_tr_t1984, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1984, 0.22).
narrative_ontology:measurement(fift_tr_t1997, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1997, 0.27).
narrative_ontology:measurement(fift_tr_t2010, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2010, 0.32).
narrative_ontology:measurement(fift_tr_t2024, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1958, 0.45).
narrative_ontology:measurement(fift_be_t1971, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1971, 0.52).
narrative_ontology:measurement(fift_be_t1984, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1984, 0.58).
narrative_ontology:measurement(fift_be_t1997, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1997, 0.61).
narrative_ontology:measurement(fift_be_t2010, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement(fift_be_t2024, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1958, 0.35).
narrative_ontology:measurement(fift_su_t1971, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1971, 0.42).
narrative_ontology:measurement(fift_su_t1984, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1984, 0.47).
narrative_ontology:measurement(fift_su_t1997, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1997, 0.51).
narrative_ontology:measurement(fift_su_t2010, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2010, 0.56).
narrative_ontology:measurement(fift_su_t2024, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2024, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__hyper_presidential_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, cohabitation_equilibrium_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, parliamentary_constraint_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'the Fifth Republic executive-legislative balance' under the ε-invariance principle. Measuring the same constitutional text through the lens of unified-majority presidential practice yields high ε (this file); measuring it through cohabitation-era negotiated practice yields markedly lower ε and a different beneficiary/victim structure (cohabitation_equilibrium_reading); measuring it through a strict-authorization textualist lens yields a rope-leaning classification (parliamentary_constraint_reading). All three share the same constitutional kernel_id and are linked here rather than averaged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
