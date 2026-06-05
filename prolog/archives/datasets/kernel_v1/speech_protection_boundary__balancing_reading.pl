% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__balancing_reading, []).

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
 *   constraint_id: speech_protection_boundary__balancing_reading
 *   human_readable: Speech Protection Boundary (Balancing Reading)
 *   domain: constitutional_law/free_speech/political_philosophy
 *
 * SUMMARY:
 *   The balancing reading of the speech protection boundary instantiates a
 *   constitutional doctrine in which no single right categorically overrides
 *   others; instead, courts weigh speech protection against competing
 *   interests (dignity, equality, public safety) on a case-by-case basis.
 *   This reading reflects the dominant approach in U.S. free speech
 *   jurisprudence since the mid-20th century, exemplified by strict scrutiny
 *   balancing tests. The doctrine operates as a Tangled Rope: it provides a
 *   coordination mechanism (courts can adjudicate competing claims through
 *   reasoned balancing) while simultaneously institutionalizing asymmetric
 *   extraction (judicial discretion systematically favors powerful speech
 *   (majoritarian coalitions) over marginal speech (dissidents, marginalized
 *   groups)). The constraint's theater_ratio (0.68) reflects that courts
 *   perform the ritual of balancing (citing competing values, examining
 *   proportionality) but outcome patterns suggest balancing serves as a
 *   legitimizing cover for ideology-driven decisions. The suppression value
 *   (0.65) captures that case-by-case adjudication offers no categorical
 *   guarantee to marginalized speakers — their silence can be enforced
 *   through the appearance of neutral balancing.
 *
 * KEY AGENTS:
 *   - Marginalized Speech Targets: Primary victims (powerless/trapped) — have no exit from hate speech or harassment; suppression is total because outcome depends on judge's discretionary balancing judgment
 *   - Majoritarian Safety Coalitions: Primary beneficiary (institutional/arbitrage) — benefits from balancing framework that enables case-by-case restrictions on speech threatening majority interests without requiring categorical amendment
 *   - Judicial Discretion Wielders: Institutional beneficiary (institutional/arbitrage) — granted interpretive authority over competing constitutional claims; experiences balancing doctrine as empowering
 *   - Civil Liberties Advocates (Non-Majoritarian): Secondary victim (moderate/constrained) — constrained by fear that discretionary balancing will be weaponized against dissidents, but also benefit from reasoned adjudication compared to legislative fiat
 *   - Balancing Doctrine Itself: Institutional actor (institutional/constrained) — maintains performative ritual of weighing competing values; institutional inertia preserves doctrine despite degraded functional verification
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the balancing choice as inherent to rights adjudication rather than contingent institutional preference
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.58).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.65).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "Speech Protection Boundary (Balancing Reading)").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "constitutional_law/free_speech/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, '80d31b4c-4944-457e-ae13-b28265202830').
narrative_ontology:cs_kernel_codification('80d31b4c-4944-457e-ae13-b28265202830', fixed_text).
narrative_ontology:cs_authority_grounding('80d31b4c-4944-457e-ae13-b28265202830', lineage).
narrative_ontology:cs_interpretation_layer_present('80d31b4c-4944-457e-ae13-b28265202830').
narrative_ontology:cs_reading_relation('80d31b4c-4944-457e-ae13-b28265202830', speech_protection_boundary__near_absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('80d31b4c-4944-457e-ae13-b28265202830', speech_protection_boundary__dignitary_harm_reading, influences).
narrative_ontology:cs_axiom('80d31b4c-4944-457e-ae13-b28265202830', foundational, no_single_right_categorically_overrides).
narrative_ontology:cs_axiom_status(no_single_right_categorically_overrides, holdable).
narrative_ontology:cs_axiom_grounding('80d31b4c-4944-457e-ae13-b28265202830', no_single_right_categorically_overrides, deontological).
narrative_ontology:cs_axiom('80d31b4c-4944-457e-ae13-b28265202830', foundational, judicial_discretion_institutionalized).
narrative_ontology:cs_axiom_status(judicial_discretion_institutionalized, holdable).
narrative_ontology:cs_axiom_grounding('80d31b4c-4944-457e-ae13-b28265202830', judicial_discretion_institutionalized, conventional).
narrative_ontology:cs_axiom('80d31b4c-4944-457e-ae13-b28265202830', secondary, neutrality_through_reasoned_adjudication).
narrative_ontology:cs_axiom_status(neutrality_through_reasoned_adjudication, overridden).
narrative_ontology:cs_axiom_grounding('80d31b4c-4944-457e-ae13-b28265202830', neutrality_through_reasoned_adjudication, instrumental).
narrative_ontology:cs_reference_frame('80d31b4c-4944-457e-ae13-b28265202830', constitutional_balancing_framework).
narrative_ontology:cs_drift_state('80d31b4c-4944-457e-ae13-b28265202830', contemporary_pluralist_democracy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('80d31b4c-4944-457e-ae13-b28265202830', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, judicial_discretion_wielders).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, majoritarian_safety_coalitions).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, marginalized_speech_targets).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, categorical_rights_believers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED SPEECH TARGET (SNARE) — Has no exit from hate speech or harassment; suppression is total (no categorical protection, outcome depends on judge's balancing judgment). Experiences pure extraction: their dignity/safety are weighed against speaker's freedom case-by-case, with no guarantee they win. Maximum structural entrapment.
constraint_indexing:constraint_classification(speech_protection_boundary__balancing_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL LIBERTIES ADVOCATE / NON-MAJORITARIAN (TANGLED ROPE) — Constrained by fear of establishing precedent that judicial discretion to restrict speech will be weaponized against dissidents. But also benefits from access to courts when speech is restricted — the balancing framework offers some protection through reasoned adjudication rather than legislative fiat. Coordination (access to courts) coexists with extraction (risk that balancing favors majority).
constraint_indexing:constraint_classification(speech_protection_boundary__balancing_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIAL DISCRETION WIELDER (ROPE) — Judges benefit from balancing framework: it grants them interpretive authority to resolve competing constitutional claims without fixed categorical rules. The constraint coordinates judicial power across different cases by institutionalizing judicial discretion as the legitimate mechanism. Net beneficiary; experiences constraint as empowering, not restrictive.
constraint_indexing:constraint_classification(speech_protection_boundary__balancing_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJORITARIAN SAFETY COALITION (ROPE) — Benefits from balancing framework because it enables restrictions on speech that threatens majority interests (public safety, social stability) without requiring categorical amendment to free speech doctrine. The coalition can pursue case-by-case victories without changing the formal rule. Experiences constraint as pure coordination: the balancing mechanism allocates speech permissions to coalitions powerful enough to win judicial arguments.
constraint_indexing:constraint_classification(speech_protection_boundary__balancing_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a civilizational perspective, all rights conflict (speech vs. dignity, liberty vs. safety) and no categorical rule can resolve all cases. Balancing is presented as inherent to rights adjudication — an immutable property of how legal reasoning works. However, structural data contradicts this: balancing is a choice about institutional authority (who decides: judge, legislature, or categorical text), not a law of nature. Engine's false summit detector will flag this.
constraint_indexing:constraint_classification(speech_protection_boundary__balancing_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: BALANCING DOCTRINE (PITON) — The institutional practice of case-by-case balancing in speech jurisprudence is largely performative. Courts declare they are balancing interests but apply similar outcomes across cases — outcomes that correlate with majoritarian preferences rather than genuine weight-assignment. The doctrine persists through institutional inertia (it looks like reasoned adjudication) despite degraded functional verification. Theater ratio captures this performativity.
constraint_indexing:constraint_classification(speech_protection_boundary__balancing_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__balancing_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(speech_protection_boundary__balancing_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(speech_protection_boundary__balancing_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, TR),
    TR >= 0.70.

:- end_tests(speech_protection_boundary__balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The balancing framework extracts from marginalized speakers who bear the cost of case-by-case uncertainty while majoritarian coalitions gain predictable access to speech restrictions. The extraction is not total (balancing sometimes protects marginal speech) but systematic (balancing outcomes correlate with ideology). The trajectory rises over the interval (0.48 → 0.58) as decades of balancing case law accumulate patterns favoring majoritarian preferences. Suppression (0.65): High. Case-by-case adjudication provides no categorical protection; marginalized speakers face suppression unless they can win a balancing argument, which requires demonstrating their interests outweigh the speaker's protected liberty. This is a high barrier — suppression is conditional on judicial discretion rather than structural barrier, but no less real. Theater ratio (0.68): High-moderate. Courts perform elaborate balancing rituals (citing competing values, examining means-ends fit, applying multi-factor tests) that create appearance of neutral adjudication, but outcome patterns suggest balancing serves as legitimizing cover for ideology-driven decisions. The ratio rises over time (0.55 → 0.68) as balancing doctrine accumulates case law and judicial performance becomes more elaborate despite stable outcome patterns.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full range of classification from a single set of base properties. Marginalized speakers experience Snare: no categorical protection, total suppression, pure extraction. Civil liberties advocates experience Tangled Rope: genuine coordination benefit (reasoned adjudication vs. legislative suppression) coexists with extraction risk (discretion weaponized against dissidents). Majoritarian coalitions experience Rope: the balancing framework allocates speech permissions to coalitions powerful enough to win arguments. Judicial discretion wielders experience Rope: balancing grants interpretive authority and empowers judges. Balancing doctrine itself, viewed institutionally, appears as Piton: performative ritual maintained through inertia. The analytical observer risks seeing Mountain: balancing as inherent to rights adjudication, a natural law of constitutional reasoning. However, the structural data contradicts the mountain classification — balancing is a choice about institutional authority (who decides: judge, legislature, or categorical text), not a law of nature. The false summit detector will identify this as naturalization of a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and structural position. Majoritarian safety coalitions (beneficiaries with arbitrage exit) derive low d → negative f(d) → effectively subsidized by the constraint. Marginalized speakers (victims with trapped exit) derive high d → high f(d) → high experienced extraction. Judicial discretion wielders (beneficiaries with arbitrage exit) derive low d, experiencing the constraint as enabling rather than extractive. Civil liberties advocates (moderate power, constrained exit, secondary victims of discretion weaponization) derive d ≈ 0.55, producing moderate experienced extraction. The balancing framework institutionalizes this directionality asymmetry: it appears neutral but systematically advantages those with power to win balancing arguments.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: The balancing reading instantiates genuine Tangled Rope structure — there is a real coordination function (reasoned adjudication of competing constitutional claims) embedded within asymmetric extraction (judicial discretion favors majoritarian coalitions). The reading does not collapse into pure extraction (Snare) or pure coordination (Rope) because both functions are structurally present. The mandatrophy is not 'which type is correct?' but 'how is the coordination function leveraged to mask and legitimize extraction?' The balancing doctrine maintains its Tangled Rope status through performative sophistication — the elaborate ritual of balancing tests creates appearance of neutral adjudication while outcome patterns reveal systematic bias. This is exactly what Tangled Rope structure enables: the coordination mechanism (courts adjudicating competing claims) provides legitimacy cover for the extraction mechanism (discretion systematically favoring powerful speech).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_vs_discretionary_extraction,
    'Does case-by-case balancing constitute legitimate adjudication of genuinely incommensurable values, or does it institutionalize judicial discretion as a mechanism for majoritarian preference-satisfaction?',
    'Empirical analysis of balancing outcomes: correlation between judge identity/court ideology and speech restriction decisions; statistical likelihood of marginal speakers losing balancing disputes across jurisdictions; comparison of outcome variance with outcomes under categorical rules',
    'If balancing is functional (outcomes reflect genuine weight assignment): Tangled Rope classification sustained. If balancing is discretionary cover (outcomes correlate with ideology): Snare or Piton classification becomes dominant; doctrine is exposed as extractive theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_vs_discretionary_extraction, empirical, 'Whether balancing reflects adjudication or disguised discretionary extraction').

omega_variable(
    judicial_neutrality_assumption,
    'Can judges applying balancing tests actually achieve neutrality across competing constitutional values, or does the absence of categorical rules inevitably bias outcomes toward majoritarian coalitions?',
    'Comparative constitutional analysis: countries with categorical vs. balancing speech doctrines; longitudinal study of marginalized speech protection rates; analysis of how balancing doctrine differs between strong-majoritarian and competitive-pluralist democracies',
    'If judges are neutral: Balancing sustains Tangled Rope legitimacy. If structural bias exists: Balancing is extraction mechanism (higher suppression, higher effective extractiveness for marginalized speakers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_neutrality_assumption, empirical, 'Whether judicial neutrality in balancing tests is achievable').

omega_variable(
    categorical_alternative_feasibility,
    'Can categorical rules (near-absolutist or dignitary-harm-boundary readings) actually resolve speech conflicts without producing perverse outcomes or requiring equally discretionary line-drawing at the boundary?',
    'Historical case analysis of categorical doctrine failures; comparison of dispute resolution costs under categorical vs. balancing frameworks; analysis of boundary cases in near-absolutist regimes (incitement, fighting words, true threats)',
    'If categorical rules fail similarly: balancing choice is not about reducing extraction but about distributing discretion. If categorical rules succeed: balancing reading''s claim that no single right categorically overrides others is exposed as contingent institutional preference, not logical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_alternative_feasibility, conceptual, 'Whether categorical speech rules can practically replace balancing').

omega_variable(
    reading_identity_commitment,
    'Does this balancing reading constitute a genuine reading of the kernel (speech protection boundary), or does it constitute a methodological choice about how courts should adjudicate competing rights?',
    'Textual analysis of First Amendment language (''Congress shall make no law''); comparison with how other constitutional rights (equal protection, due process) are adjudicated; examination of whether balancing is required by the text or selected as institutional strategy',
    'If balancing is inherent to the text: reading instantiates an unavoidable interpretation of the kernel. If balancing is institutional strategy: reading is evidence of method choice, not constitutional necessity; the near-absolutist and dignitary-harm readings are equally defensible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_commitment, conceptual, 'Whether balancing is a reading of the kernel or a choice of judicial methodology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_bal_tr_t0, speech_protection_boundary__balancing_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(speech_bal_tr_t10, speech_protection_boundary__balancing_reading, theater_ratio, 10, 0.62).
narrative_ontology:measurement(speech_bal_tr_t20, speech_protection_boundary__balancing_reading, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(speech_bal_be_t0, speech_protection_boundary__balancing_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(speech_bal_be_t10, speech_protection_boundary__balancing_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(speech_bal_be_t20, speech_protection_boundary__balancing_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(speech_bal_su_t0, speech_protection_boundary__balancing_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(speech_bal_su_t10, speech_protection_boundary__balancing_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(speech_bal_su_t20, speech_protection_boundary__balancing_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__near_absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__dignitary_harm_reading).

% DUAL FORMULATION NOTE:
% The speech protection boundary kernel has three structurally distinct readings with different extractiveness values and victim sets. This balancing reading (ε=0.58) instantiates the dominant institutional approach; sibling readings have different ε values reflecting different victim distributions and suppression structures. All three are readings of the same kernel but should be decomposed into separate constraint stories to avoid averaging ε across incomparable institutional frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_boundary__balancing_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
