% ============================================================================
% CONSTRAINT STORY: textualist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_textualist_reading, []).

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
 *   constraint_id: textualist_reading
 *   human_readable: Textualist Constitutional Reading: Plain Text at Ratification as Authority Source
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   The textualist reading of the U.S. Constitution asserts that
 *   constitutional meaning is fixed at the plain text's ordinary meaning at
 *   the time of ratification, independent of framers' subjective intent and
 *   independent of evolving contemporary values. This constraint instantiates
 *   ONE reading of the contested kernel: us_constitution_text. The textualist
 *   reading competes with originalism (which mines framers' intent) and
 *   living constitutionalism (which privileges evolving values) as
 *   alternative authority sources for constitutional meaning. The textualist
 *   constraint exhibits Tangled Rope structure: it provides genuine
 *   coordination on textual meaning (constraining judges to the document's
 *   words rather than their policy preferences) while simultaneously
 *   extracting interpretive authority from alternative reading methods and
 *   from constituencies whose interests were not textualized into the 1787
 *   document or its amendments. The measurement trajectory shows increasing
 *   theater_ratio (0.40 → 0.65) as textualism has become institutionalized in
 *   legal education and federal judiciary, and increasing base_extractiveness
 *   as the constraint's reliance on neutrality-claims has become more
 *   sophisticated at rationalizing outcomes that favor institutional legal
 *   authority over populist originalism and progressive activism. The
 *   false-summit signature will fire because textualism claims
 *   linguistic/logical necessity while benefiting specific agents (legal
 *   profession, textualist jurists) who control textual interpretation
 *   expertise.
 *
 * KEY AGENTS:
 *   - Legal Profession & Textualist Judiciary: Primary beneficiary (institutional/arbitrage) — control interpretive authority through claimed expertise in textual analysis; experience the constraint as coordination (neutral methodology) but benefit from restricting interpretive tools available to competitors
 *   - Excluded Constituencies: Primary victim (powerless/trapped) — marginalized groups (those unrepresented at ratification, those whose interests were not textualized) have no exit from textualist logic; structurally absent from plain text means their interests cannot be recognized through textual reading
 *   - Progressive Constitutional Advocates: Secondary victim (organized/constrained) — civil rights groups, progressive law professors find textualism forecloses rights-expansion arguments; constrained exit through amendment or doctrinal shift, but textualist dominance raises costs
 *   - Originalist Legal Theorists: Secondary victim (moderate/constrained) — framers' intent interpretivists find their method marginalized as speculation; textualism claims superior neutrality and restricts originalist advocacy to alternative theoretical spaces
 *   - The Amendment Mechanism: Structural safety valve (organized/mobile) — provides genuine exit for organized agents dissatisfied with textualist outcomes; sunset is implicit in amendment's availability
 *   - Analytical Observer: Risks naturalizing institutional choice as linguistic necessity (analytical/analytical) — temptation to see textualism as immutable feature of rule-of-law constitutionalism rather than as contingent institutional dominance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(textualist_reading, 0.52).
domain_priors:suppression_score(textualist_reading, 0.58).
domain_priors:theater_ratio(textualist_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(textualist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(textualist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(textualist_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(textualist_reading, tangled_rope).
narrative_ontology:human_readable(textualist_reading, "Textualist Constitutional Reading: Plain Text at Ratification as Authority Source").
narrative_ontology:topic_domain(textualist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(textualist_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(textualist_reading, legal_profession).
narrative_ontology:constraint_beneficiary(textualist_reading, textualist_judiciary).
narrative_ontology:constraint_victim(textualist_reading, originalist_advocates).
narrative_ontology:constraint_victim(textualist_reading, progressive_activists).
narrative_ontology:constraint_victim(textualist_reading, marginalized_constituencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED CONSTITUENCY (SNARE) — Marginalized groups (those without voice at ratification, those whose interests were not textualized into the document) have no exit from textualist logic. The constraint traps them: their interests are structurally absent from the plain text, and textualism offers no mechanism to recognize or remedy that absence. Maximum suppression, zero coordination function for this agent.
constraint_indexing:constraint_classification(textualist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROGRESSIVE CONSTITUTIONAL ADVOCATES (TANGLED ROPE) — Organized agents (civil rights groups, progressive law professors, activist litigators) experience both coordination and extraction. Textualism constrains their interpretive toolkit and raises barriers to rights-expansion arguments, but they also benefit from textualism's constraint on arbitrary executive power and its demands for textual fidelity. Constrained exit: they can argue for amendment or shift doctrinal emphasis, but textualism's dominance in contemporary jurisprudence raises the cost.
constraint_indexing:constraint_classification(textualist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORIGINALIST LEGAL THEORISTS (SNARE) — Originalists who argue for framers' subjective intent find their interpretive move foreclosed by textualism. They experience extraction: textualism claims the high ground of interpretive neutrality and marginalizes originalist intent-mining as illegitimate speculation. High suppression: textualist dominance in federal judiciary (post-Scalia) makes originalist intent arguments appear peripheral. But originalism remains organized as an alternative school — some exit exists through selective circuit-court appointments and scholarly networks.
constraint_indexing:constraint_classification(textualist_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: LEGAL PROFESSION / TEXTUALIST JUDICIARY (ROPE) — Federal judges adopting textualism experience the constraint as coordination: it provides a neutral, rule-like interpretive methodology that binds them (they cannot simply substitute personal policy preferences). The professional community benefits from textualism's clarity and its claim to expertise (textual analysis is a lawyerly skill). Arbitrage exit: legal professionals can adopt or reject textualism strategically depending on circuit/era, and textualism has become the institutional orthodoxy, offering career advancement and professional legitimacy.
constraint_indexing:constraint_classification(textualist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE AMENDMENT MECHANISM (SCAFFOLD) — The constitutional amendment process itself provides a sunset clause for textualism's constraints. If textualist readings produce intolerable outcomes (e.g., blocking civil rights through narrow textual interpretation), organized agents can pursue amendment to revise the text itself. This is a low-extraction path with genuine exit: change the text and textualism must follow. Theater is low because amendment is genuinely difficult but structurally available. Sunset is implicit: as texts age without amendment, their silence becomes increasingly theatrical (e.g., silence on digital privacy).
constraint_indexing:constraint_classification(textualist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: TEXTUALISM AS INSTITUTIONAL PRACTICE (PITON) — At a civilizational timescale, textualism appears as a largely performative institutional practice. The claim to neutrality and objectivity ('follow the words') obscures that textual interpretation always involves judgment about meaning, context, and implication. Contemporary textualism is maintained through educational institutions (law schools teaching textual analysis as primary methodology), professional norms (judicial opinions citing statutory/constitutional text), and theater (the appearance of constraint by objective meaning). The actual function — constraining judicial discretion — is degraded because textualism has become sophisticated enough to rationalize nearly any outcome through linguistic analysis (originalist textualism, progressive textualism, textualist living constitutionalism). Theater ratio is high because the appearance of constraint exceeds its actual force.
constraint_indexing:constraint_classification(textualist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / LINGUISTIC NECESSITY VIEW (MOUNTAIN) — From an analytical/universal perspective, textualism may appear as a constraint derived from linguistic necessity itself: constitutional law must be grounded in some text, and if grounded in text, that text's literal meaning provides a natural floor for interpretation. This perspective sees textualism as an immutable feature of rule-of-law constitutionalism — you cannot have binding law without textual grounding. However, the structural data contradicts mountain classification: textualism is an institutional choice made by the legal profession to claim interpretive authority and authority over alternative reading methods. The false-summit signature will fire.
constraint_indexing:constraint_classification(textualist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(textualist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(textualist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(textualist_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(textualist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(textualist_reading, TR),
    TR >= 0.70.

:- end_tests(textualist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The textualist constraint extracts interpretive authority from originalism, living constitutionalism, and marginalized constituencies whose interests were not textualized. The extraction is not maximal (0.70+) because textualism does provide genuine constraint on judicial discretion — judges cannot simply substitute policy preferences for textual meaning. But the extraction is significant because textualism achieves this constraint while simultaneously claiming neutrality and marginalizing alternative methodologies as illegitimate or speculative. Suppression (0.58): Moderate-high. Barriers to exit from textualism include: (1) institutional dominance in federal judiciary post-Scalia, (2) legal education curricula privileging textual analysis, (3) professional norms treating textualism as the legitimate baseline, (4) the apparent objectivity of textual meaning (which masks interpretive judgment). But suppression is not total (0.70+) because alternative readings remain available in scholarly work, some circuits, and public debate. Theater ratio (0.65): Moderate-high. Textualism performs neutrality and objectivity ('just follow the words') while engaging in sophisticated interpretive judgment about what the words mean, what context informs them, what silences imply. The theater has increased over the measurement interval as textualism has become more academically sophisticated at defending its choices (semantic analysis, historical usage databases, formal logic of interpretation). The contemporary era (time_point=10) has maximal theater because textualism must now defend itself against charges that it merely rationalizes predetermined outcomes while claiming objectivity.
 *
 * PERSPECTIVAL GAP:
 *   The textualist constraint produces a maximum perspectival gap. The legal profession (rope perspective) experiences genuine coordination — textualism provides neutral constraint. Excluded constituencies (snare perspective) experience pure extraction — they have no voice in the text and no exit from its constraints. Originalists (snare perspective) experience marginalization — their method is foreclosed. Progressives (tangled rope perspective) experience mixed outcomes — some coordination (constraint on executive power) with significant extraction (constraint on rights-expansion). The amendment mechanism (scaffold perspective) offers a structured exit path with sunset. The institutional practice (piton perspective) shows increasing theater as sophistication grows. The analytical observer (mountain perspective) risks naturalizing contingent institutional choice. These gaps are not observer-error but rather reflect structural facts: textualism genuinely benefits some agents (legal professionals), genuinely harms others (excluded constituencies), and provides mixed outcomes for organized agents (progressives, originalists) with different power levels.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from the agent's structural position relative to the textualist constraint. Legal professionals benefit from textualism (d ≈ 0.10, low extraction experienced) because it provides professional legitimacy and interpretive authority. Excluded constituencies are harmed (d ≈ 0.95, maximum extraction) because their interests are structurally absent from the text and textualism offers no remedy. Originalist theorists are harmed (d ≈ 0.75, high extraction) because textualism marginalizes their method. Progressive advocates are partially harmed but partially enabled (d ≈ 0.60, moderate extraction) because textualism constrains executive power but also constrains rights-expansion. The amendment mechanism provides exit (d ≈ 0.40, constrained extraction) because it allows organized agents to modify the text. The analytical observer faces a dilemma: from within textualism's frame, the observer experiences d ≈ 0.00 (observing neutrally from outside), but the observation itself is compromised by identity-lock into textualist methodology — the analytical frame depends on textualist tools.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that textualism contains both genuine coordination function (constraining judges to text rather than policy preferences) and genuine extraction function (extracting interpretive authority from alternative methods and from unrepresented constituencies). The coordination and extraction are not separable — textualism's authority derives precisely from its constraint on discretion, which simultaneously constrains alternative readings. The classification as Tangled Rope is stable: the constraint genuinely coordinates on textual meaning while extracting authority from competitors and harming excluded constituencies. The false-summit signature (analytical perspective's mountain classification) is the diagnostic indicator that textualism claims linguistic necessity while benefiting specific institutional actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textualist_vs_originalist_reading_delta,
    'This constraint (textualist_reading) is ONE reading of the us_constitution_text kernel. What structurally distinguishes THIS reading from the originalist_reading sibling?',
    'Comparative analysis of two constraints: textualist_reading vs originalist_reading. Identify which extractiveness, suppression, beneficiary/victim sets, and perspectives differ. Map the delta to specific interpretive moves: textualism rejects framers'' subjective intent as an authority source and relies on plain textual meaning at ratification; originalism mines framers'' intent as the authoritative source. These are logically distinct moves with different extraction profiles.',
    'Textualism forecloses originalist intent-mining and marginalizes it as speculation. Originalism forecloses purely textual meaning and marginalizes it as divorced from constitutional purpose. The reading you instantiate (textualist) imposes costs on originalist advocates that the originalist reading does not impose on textualists. The two readings are structurally asymmetric in their victim sets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textualist_vs_originalist_reading_delta, conceptual, 'Structural difference between textualist and originalist readings of the Constitution kernel').

omega_variable(
    neutral_expertise_claim,
    'Does textualism''s claim to neutrality and objective textual meaning constitute genuine coordination on meaning, or is it an extraction mechanism that benefits the legal profession by claiming interpretive authority?',
    'Historical analysis of textualism''s rise in federal judiciary (post-Scalia); corpus analysis of textualist opinions to identify instances where textual analysis required non-obvious interpretive choices or where equally valid textual readings were rejected; comparison with pre-textualist era to measure whether textualism reduced judicial discretion or merely rationalized it differently.',
    'If neutrality is genuine: textualism is a coordination mechanism constraining judges and is closer to Rope. If neutrality is claimed but not genuine: textualism is an extraction mechanism benefiting lawyers and judges who control textual interpretation expertise, and the constraint is closer to Snare (for excluded constituencies) and higher Tangled Rope (for organized advocates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutral_expertise_claim, empirical, 'Whether textualism''s neutrality claim is genuine coordination or claimed extraction').

omega_variable(
    silences_and_historical_change,
    'As the constitutional text ages (ratified 1787, amended 27 times over 230+ years), does textualism handle silences (e.g., no mention of digital privacy, internet, nuclear weapons, AI) by treating them as implicit delegations to Congress, or does it treat them as constraints on governmental power?',
    'Doctrinal analysis of how textualist judges have handled constitutional silences in specific domains (privacy, technology, executive power expansion). Identify patterns: do silences impose meaningful constraints, or do they become hooks for executive flexibility? Track whether the constraint''s suppression value increases (silences enable extraction) or decreases (silences protect alternative interpretations).',
    'If silences constrain: textualism maintains its suppression level and constraint function. If silences enable executive/legislative flexibility: textualism''s actual suppression of marginalized constituencies increases over time as the text becomes increasingly anachronistic. The measuring point (contemporary era) may understate the constraint''s extractiveness in future generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(silences_and_historical_change, empirical, 'How textualism handles constitutional silences on contemporary issues').

omega_variable(
    competing_reading_existence,
    'Are the originalist_reading and living_constitutionalist_reading sibling constraints with their own distinct extractiveness values, or are they alternative interpretations of the same constraint?',
    'Per the ε-invariance principle: measure each reading''s base extractiveness by the same metric. If originalism (mining intent) produces ε ≈ 0.35 and textualism (plain text) produces ε ≈ 0.52 and living constitutionalism (evolving values) produces ε ≈ 0.68, they are structurally distinct constraints with different beneficiary/victim structures. If all three readings produce similar ε values and beneficiary/victim sets, they are interpretive alternatives of one constraint.',
    'If distinct constraints: the corpus must contain three separate JSON files (textualist_reading, originalist_reading, living_constitutionalist_reading) linked via network.affects_constraints. If alternatives: one constraint is needed with perspectives representing each reading and omegas documenting the interpretive ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_reading_existence, conceptual, 'Whether sibling readings are distinct constraints or interpretive alternatives of one constraint').

omega_variable(
    marginalized_constituency_measurement,
    'Which marginalized groups are most harmed by textualist interpretation, and can we measure the extraction differential between textualism and alternative readings?',
    'Doctrinal analysis of textualist outcomes in key domains: voting rights (textualism constrains Voting Rights Act; compare with originalist/living constitutionalist approaches), reproductive rights (textualism enables fetal personhood restrictions; compare approaches), economic rights (textualism narrows substantive due process; compare approaches), immigrant rights (textualism constrains humanitarian readings of due process; compare approaches). Quantify the victim set size and extraction intensity for each group under each reading.',
    'If specific groups are disproportionately harmed by textualism: the snare perspective''s classification is solid and the victim set should be disaggregated (voting_rights_claimants, reproductive_autonomy_seekers, immigrant_communities). If textualism and alternatives produce similar outcomes for marginalized groups: the snare classification overstates textualism''s unique extraction and the constraint should be reclassified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_constituency_measurement, empirical, 'Measurement of extraction differential for marginalized groups across constitutional readings').

omega_variable(
    false_summit_textualism_as_natural_law,
    'Is textualism a natural law of constitutional interpretation (mountain: immutable, inevitable, zero degrees of freedom), or a contingent institutional choice made by the legal profession to claim interpretive authority?',
    'Historical analysis of constitutional interpretation before textualism''s ascendance (pre-1985). If sophisticated non-textualist interpretation was practiced and legitimate, textualism is contingent. If textualism is asserted as the only valid method post-hoc, it is a false summit — naturalization of an institutional choice. Compare textualism''s rise to other methodological monopolies (strict originalism, living constitutionalism) to identify whether one reading was always dominant or whether dominance is an artifact of institutional power concentration (Scalia''s influence, law school curricula, federal judicial appointments).',
    'If false summit confirmed: the analytical perspective''s mountain classification is invalid. Reclassify as Tangled Rope or Snare depending on whether the constraint genuinely coordinates on meaning (Tangled Rope) or primarily extracts interpretive authority from non-textualist readers (Snare). The engine''s false_summit_mountain signature should trigger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_textualism_as_natural_law, empirical, 'Whether textualism is an immutable natural law or a contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(textualist_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(text_tr_t0, textualist_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(text_tr_t5, textualist_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(text_tr_t10, textualist_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(text_be_t0, textualist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(text_be_t5, textualist_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(text_be_t10, textualist_reading, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(textualist_reading, information_standard).
narrative_ontology:affects_constraint(textualist_reading, originalist_reading).
narrative_ontology:affects_constraint(textualist_reading, living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% The textualist_reading constraint is ONE instantiation of the us_constitution_text kernel. Three competing readings decompose this kernel into structurally distinct constraints. Textualism (ε ≈ 0.52, Tangled Rope) forecloses originalist intent-mining and living constitutionalist value-projection. The three readings form a constraint family where each reading provides benefits for some agents while extracting costs from others. The family structure shows that the kernel itself is not neutral — adopting any reading imposes costs on advocates of alternatives. The false-summit signature on the analytical perspective (which claims textualism is linguistic necessity) reveals this kernel-level structure: textualism is a contingent institutional choice, not an immutable law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(textualist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
