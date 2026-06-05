% ============================================================================
% CONSTRAINT STORY: eighth_amendment__fixed_original_meaning_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eighth_amendment__fixed_original_meaning_reading, []).

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
 *   constraint_id: eighth_amendment__fixed_original_meaning_reading
 *   human_readable: Eighth Amendment: Fixed Original Meaning (Founding-Era Standards Reading)
 *   domain: constitutional/legal_doctrine
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the Eighth Amendment's cruel
 *   and unusual punishment clause: the fixed original meaning reading, which
 *   holds that 'cruel and unusual' carries the meaning it had at the time of
 *   ratification (1791) and does not evolve with contemporary standards of
 *   decency. Under this reading, a punishment is forbidden only if it was
 *   cruel by founding-era standards OR if it adds significant terror to
 *   traditional methods known in 1791. The structural consequence is that the
 *   constraint freezes the baseline of permissible punishment at the founding
 *   era, preserving historical punishment regimes from challenge via
 *   evolving-decency arguments. The constraint exhibits all six DR types from
 *   different perspectives: to incarcerated populations trapped under fixed
 *   standards it is pure extraction (snare); to originalist judges it is
 *   coordination (rope); to states it is mixed coordination and extraction
 *   (tangled rope); to the enforcement mechanism it is performative ritual
 *   (piton); to advocates of evolving standards it is foreclosed possibility
 *   (tangled rope); to the civilizational analytical observer it risks
 *   appearing as an immutable natural law (mountain), but the structural data
 *   reveals a false summit — the constraint benefits identifiable
 *   institutions and suppression is high and active.
 *
 * KEY AGENTS:
 *   - Incarcerated Populations: Primary victim (powerless/trapped) — no exit from punishment regimes frozen at 1791 baseline; cannot appeal to evolving decency standards
 *   - Originalist Judiciary: Primary beneficiary (institutional/arbitrage) — captures authority to determine constitutional meaning through originalist methodology; experiences constraint as rule-clarity coordination
 *   - Evolving-Standards Legal Advocates: Secondary victim (moderate/constrained) — civil rights attorneys and doctrinal reformers face constrained options; some pathways (evolving standards doctrine) are foreclosed by this reading
 *   - State Correctional Apparatus: Secondary beneficiary (powerful/constrained) — benefits from protection of traditional punishment regimes; constrained by requirement to justify practices via 1791-comparative analysis
 *   - Judicial System: Institutional actor (institutional/arbitrage) — maintains the historical-comparison ritual; sees own enforcement mechanism as degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing an interpretive choice (originalism) as a law-like feature of constitutional meaning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eighth_amendment__fixed_original_meaning_reading, 0.58).
domain_priors:suppression_score(eighth_amendment__fixed_original_meaning_reading, 0.72).
domain_priors:theater_ratio(eighth_amendment__fixed_original_meaning_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eighth_amendment__fixed_original_meaning_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(eighth_amendment__fixed_original_meaning_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(eighth_amendment__fixed_original_meaning_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eighth_amendment__fixed_original_meaning_reading, snare).
narrative_ontology:human_readable(eighth_amendment__fixed_original_meaning_reading, "Eighth Amendment: Fixed Original Meaning (Founding-Era Standards Reading)").
narrative_ontology:topic_domain(eighth_amendment__fixed_original_meaning_reading, "constitutional/legal_doctrine").

domain_priors:requires_active_enforcement(eighth_amendment__fixed_original_meaning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eighth_amendment__fixed_original_meaning_reading, '9ae209fe-f54d-4492-8c2b-eac81eb2e642').
narrative_ontology:cs_kernel_codification('9ae209fe-f54d-4492-8c2b-eac81eb2e642', fixed_text).
narrative_ontology:cs_authority_grounding('9ae209fe-f54d-4492-8c2b-eac81eb2e642', lineage).
narrative_ontology:cs_interpretation_layer_present('9ae209fe-f54d-4492-8c2b-eac81eb2e642').
narrative_ontology:cs_reading_relation('9ae209fe-f54d-4492-8c2b-eac81eb2e642', eighth_amendment__evolving_standards_reading, coexists_with).
narrative_ontology:cs_axiom('9ae209fe-f54d-4492-8c2b-eac81eb2e642', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('9ae209fe-f54d-4492-8c2b-eac81eb2e642', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('9ae209fe-f54d-4492-8c2b-eac81eb2e642', foundational, founding_era_baseline_dispositive).
narrative_ontology:cs_axiom_status(founding_era_baseline_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('9ae209fe-f54d-4492-8c2b-eac81eb2e642', founding_era_baseline_dispositive, deontological).
narrative_ontology:cs_reference_frame('9ae209fe-f54d-4492-8c2b-eac81eb2e642', founding_era_punishment_baseline).
narrative_ontology:cs_drift_state('9ae209fe-f54d-4492-8c2b-eac81eb2e642', contemporary_decency_standards_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9ae209fe-f54d-4492-8c2b-eac81eb2e642', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(eighth_amendment__fixed_original_meaning_reading, eighth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eighth_amendment__fixed_original_meaning_reading, traditional_punishment_regimes).
narrative_ontology:constraint_beneficiary(eighth_amendment__fixed_original_meaning_reading, originalist_judiciary).
narrative_ontology:constraint_victim(eighth_amendment__fixed_original_meaning_reading, incarcerated_populations).
narrative_ontology:constraint_victim(eighth_amendment__fixed_original_meaning_reading, evolving_standards_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Prisoners and detainees have no exit from punishment regimes frozen at 1791 baseline. Cannot appeal to evolving standards of decency. Trapped by the interpretive framework itself — the constraint forbids only what the founding generation forbade. Maximum extraction: the reading preserves historical punishment practices that contemporary standards would reject.
constraint_indexing:constraint_classification(eighth_amendment__fixed_original_meaning_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Civil rights attorneys and doctrinal reformers face constrained options: must work within the originalist framework or challenge it entirely. Some benefit from clarified constitutional rules; significant extraction through foreclosed reform pathways. Moderate power, real constraints, but structural openings for legal strategy.
constraint_indexing:constraint_classification(eighth_amendment__fixed_original_meaning_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Primary beneficiary. Originalist judges experience the constraint as pure coordination: establishing clear, historically-grounded rules with no ambiguity about contemporary policy preferences. Arbitrage option available — judges can exit to competing interpretive methodologies (living constitutionalism) but choose the originalist path. Net beneficiary of the rule.
constraint_indexing:constraint_classification(eighth_amendment__fixed_original_meaning_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% States benefit from the fixed baseline — punishment regimes justified under 1791 standards are protected from challenge. But also constrained by the specific rule requiring 1791-comparative analysis. Generational perspective shows how doctrine accumulates: each narrow application freezes more practices. Powerful institution, constrained methodology, mixed coordination and extraction.
constraint_indexing:constraint_classification(eighth_amendment__fixed_original_meaning_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The doctrine's enforcement mechanism is largely performative: courts conduct historical analysis of founding-era punishment practices, but the analysis often turns on fragmentary evidence, contestable history, and interpretive choices. The ritual of historical comparison persists through institutional inertia despite the fact that 18th-century punishment texts cannot speak clearly to 21st-century technologies (solitary confinement duration, chemical sedation, etc.). Theater ratio reflects the gap between the precision promised by 'what it meant in 1791' and the historical indeterminacy of that claim.
constraint_indexing:constraint_classification(eighth_amendment__fixed_original_meaning_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a civilizational perspective, the mountain view claims: the meaning of a constitutional text is fixed at ratification; evolving interpretation violates the rule of law itself and is a form of judicial legislation. This reading sees originalism as an immutable logical requirement of constitutional authority, not a contested interpretive choice. However, the structural data shows this is a false summit: the constraint benefits identifiable institutions (originalist judiciary, states avoiding decency standards), suppression is high, and enforcement is active and contested.
constraint_indexing:constraint_classification(eighth_amendment__fixed_original_meaning_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eighth_amendment__fixed_original_meaning_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eighth_amendment__fixed_original_meaning_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eighth_amendment__fixed_original_meaning_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eighth_amendment__fixed_original_meaning_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eighth_amendment__fixed_original_meaning_reading, TR),
    TR >= 0.70.

:- end_tests(eighth_amendment__fixed_original_meaning_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reading freezes the baseline of permissible punishment at the founding era, preserving historical practices that contemporary decency standards would reject. The beneficiaries (originalist judiciary, traditional punishment regimes) capture advantage through this freeze. The extractiveness increased from ~0.42 in 1985 (when evolving-standards doctrine was ascendant) to 0.58 by 2000-2025 (as originalism became the dominant judicial methodology). The extraction is not maximal because the rule itself is semantically determinate (historical comparison has constraints), and some constraints do get forbidden even under the original-meaning reading (practices that would have been seen as adding terror to traditional methods in 1791). Suppression (0.72): High. The constraint suppresses the alternative pathway (evolving standards) through doctrinal closure. Incarcerated persons cannot appeal to contemporary decency standards; advocates cannot claim the text authorizes such appeals. The suppression is active and enforced — originalist courts systematically reject evolving-standards arguments. Suppression increased from 0.55 in 1985 to 0.72 by 2010 as originalism became institutionally dominant. Theater ratio (0.48): Moderate. The historical-comparison methodology is partially performative — courts conduct ostensibly historical analysis of founding-era punishment practice, but often work backward from desired outcomes, selectively cite historical sources, and struggle to apply 1791 standards to technologies (chemical restraint, supermax solitary) that did not exist. However, the ritual is not purely theater — genuine historical research occurs, and sometimes historical evidence produces determinate constraints. The theater ratio increased slightly over time as courts grew more sophisticated in applying historical comparison to novel cases, but the underlying indeterminacy of historical sources remains.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is the entire justification of the constraint as a case study. From the incarcerated person's position, this is a snare — no exit, high extraction, high suppression. From the originalist judge's position, it is coordination — clear rule, no discretion, respected interpretive methodology. From the evolving-standards advocate's position, it is foreclosed possibility — snare-like but with institutional rather than individual trapping. From the state's position, it is tangled rope — coordination of punishment practices plus extraction of authority from those subject to punishment. From the analytical position, the mountain view (natural law of meaning fixation) collides with the structural data showing high extraction and active suppression, producing a false-summit candidate. The constraint's classification type depends entirely on whose structural position you occupy.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) reflects the agent's structural position relative to the constraint. Incarcerated populations are trapped victims (d ≈ 0.95) — the constraint extracts maximum cost from them with no exit option. Originalist judges are net beneficiaries with arbitrage options (d ≈ 0.15) — they benefit from the clarity and authority of originalism but could exit to other methodologies; the low d produces negative effective extraction (they see the constraint as beneficial coordination). Evolving-standards advocates are moderately victimized with constrained options (d ≈ 0.70) — the constraint forecloses their preferred doctrinal pathway but doesn't trap them entirely; they can work within originalist frameworks or challenge the methodology itself. States are mixed beneficiary-constrained (d ≈ 0.45) — they benefit from the protection of traditional regimes but are constrained by the requirement to justify practices via historical comparison. The constraint's effective extractiveness (χ) scales by agent position: experienced extraction is high for trapped victims, low for beneficiaries with arbitrage, moderate for constrained moderates. The directional flow: extraction runs from incarcerated populations and evolving-standards claimants toward originalist judiciary and traditional punishment regimes.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by exposing the kernel reading contest itself. The mandatrophy question is: 'Is this a coordination mechanism (rope) or an extraction mechanism (snare)?' The answer differs by perspective. For originalist judges, it is coordination — the rule clarifies constitutional meaning and removes discretion. For incarcerated populations, it is extraction — the rule forecloses appeals to contemporary decency and preserves historical punishment. The constraint is BOTH simultaneously, depending on whose extraction is being measured. The constraint does not fail the mandatrophy test because it genuinely exhibits both coordination and asymmetric extraction. The coordination (for originalist judges) is a real benefit: clear interpretive rules, reduced judicial discretion. The extraction (for incarcerated populations) is real: foreclosed doctrinal pathways, frozen baseline, inability to appeal to evolving standards. The kernel reading contest IS the mandatrophy's resolution: original-meaning reading classifies as snare from the incarcerated perspective, rope from the originalist perspective, and tangled rope from moderate/state perspectives. The same structural phenomenon exhibits different classification types because the kernel reading has fundamentally different consequences for different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_era_historical_determinacy,
    'What did the founding generation actually mean by ''cruel and unusual''? Is this meaning historically determinate or contested?',
    'Scholarly historical analysis of founding-era legal documents, legislative debates, execution and punishment practices; cross-source evidence of consensus vs. ambiguity among framers',
    'If determinate: originalist methodology has genuine epistemic grounding — fixed baseline is knowable. If contested: the ''fixed'' meaning is actually a chosen interpretation among multiple historical readings. If indeterminate: the constraint becomes a game of selecting which historical sources count, privileging some framers'' views over others.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_era_historical_determinacy, empirical, 'Whether founding-era meaning of ''cruel and unusual'' is historically determinate').

omega_variable(
    id_1791_baseline_applicability_to_modern_punishments,
    'Can 1791-era standards meaningfully apply to punishments (solitary confinement for years, chemical restraint, indefinite detention) that did not exist in 1791?',
    'Doctrinal case analysis: track how courts apply ''founding-era standards'' to novel punishment technologies; measure consistency of historical analogy reasoning; examine whether the comparison produces determinate answers or requires modern policy judgments disguised as historical analysis',
    'If applicable: the constraint preserves a coherent legal rule. If inapplicable: courts must smuggle modern standards into the ''historical comparison'' framework, making the constraint functionally indistinguishable from evolving standards while maintaining originalist rhetoric. This is the mandatrophy vector.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(id_1791_baseline_applicability_to_modern_punishments, empirical, 'Applicability of 1791 standards to modern punishment technologies').

omega_variable(
    kernel_reading_foreclosure,
    'Does this reading''s core premise logically foreclose the evolving-standards reading within a single constitutional framework, or can both readings coexist as competing interpretive traditions?',
    'Constitutional theory analysis: examine whether original-meaning methodology entails that evolving standards are categorically illegitimate, or whether multiple interpretive theories can coherently operate within the same constitutional system through institutional pluralism or acknowledgement of competing epistemic bases',
    'If forecloses: the two readings are logically incompatible; a court cannot hold both simultaneously. The constraint-system contest is zero-sum. If coexists: both readings persist across different institutional contexts or judicial camps; the broader constitutional system accommodates the contest. Classification of reading_relations changes accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether original-meaning reading logically forecloses evolving-standards reading').

omega_variable(
    naturalization_of_interpretive_choice,
    'Is originalism itself a neutral method for reading the Constitution, or is it an interpretive choice that advantages certain political outcomes?',
    'Meta-jurisprudential analysis: track how originalism generates systematically different outcomes from living constitutionalism across a corpus of cases (criminal procedure, due process, equal protection); measure whether originalism correlates with conservative policy outcomes; examine whether originalists acknowledge or deny this correlation',
    'If neutral method: the mountain perspective is justified — original meaning is a law-like feature of constitutional interpretation. If interpretive choice: the mountain is a false summit — the constraint naturalizes what is actually a contested methodological commitment that advantages originalist beneficiaries. This determines whether the constraint exhibits the false summit signature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_of_interpretive_choice, empirical, 'Whether originalism is a neutral method or a choice favoring certain outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eighth_amendment__fixed_original_meaning_reading, 1985, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1985_preoriginalism, eighth_amendment__fixed_original_meaning_reading, theater_ratio, 1985, 0.38).
narrative_ontology:measurement(theater_2000_originalism_ascendant, eighth_amendment__fixed_original_meaning_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(theater_2010_originalism_dominant, eighth_amendment__fixed_original_meaning_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(theater_2025_stable_theater, eighth_amendment__fixed_original_meaning_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(extract_1985_preoriginalism, eighth_amendment__fixed_original_meaning_reading, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement(extract_2000_originalism_ascendant, eighth_amendment__fixed_original_meaning_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(extract_2010_originalism_dominant, eighth_amendment__fixed_original_meaning_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(extract_2025_stable_extraction, eighth_amendment__fixed_original_meaning_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(suppress_1985_preoriginalism, eighth_amendment__fixed_original_meaning_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(suppress_2000_originalism_ascendant, eighth_amendment__fixed_original_meaning_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(suppress_2010_originalism_dominant, eighth_amendment__fixed_original_meaning_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(suppress_2025_stable_suppression, eighth_amendment__fixed_original_meaning_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eighth_amendment__fixed_original_meaning_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eighth_amendment__fixed_original_meaning_reading, eighth_amendment__evolving_standards_reading).

% DUAL FORMULATION NOTE:
% The Eighth Amendment kernel decomposes into two structurally distinct constraint stories: the fixed original meaning reading and the evolving standards reading. Each reading has its own ε (0.58 for fixed original; higher for evolving standards as it permits ongoing reform). They have different beneficiaries, different suppression baselines, and different victim sets. The two constraints are linked via network.affects_constraints because each reading's judicial dominance affects the other's operative scope. The constraint family represents the kernel contest itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eighth_amendment__fixed_original_meaning_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
