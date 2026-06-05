% ============================================================================
% CONSTRAINT STORY: tenure_contract__academic_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__academic_freedom_reading, []).

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
 *   constraint_id: tenure_contract__academic_freedom_reading
 *   human_readable: Tenure as Academic Freedom Protection: Decoupling Researcher Survival from Political/Institutional Backlash
 *   domain: higher_education/institutional_governance/labor_economics
 *
 * SUMMARY:
 *   The tenure system, viewed through the academic freedom lens, functions as
 *   a coordination mechanism that decouples researcher survival from
 *   institutional displeasure or political backlash. This reading emphasizes
 *   tenure's role in enabling truth-seeking: by guaranteeing employment
 *   security, tenure removes institutional and political incentives to
 *   suppress inconvenient findings, retract controversial research, or avoid
 *   high-risk inquiries. Faculty can pursue questions whose answers might
 *   displease donors, politicians, boards of trustees, or public opinion. The
 *   constraint solves a genuine collective action problem: institutions
 *   require stable researchers to produce impact; researchers require
 *   institutional commitment to undertake multi-year inquiries; and research
 *   integrity requires independence from pressure to reach predetermined
 *   conclusions. This is ONE reading of the contested tenure kernel. Sibling
 *   readings (institutional_extraction_reading,
 *   demographic_reproduction_reading) see the same institutional structure
 *   but emphasize different extraction mechanisms: precarious labor
 *   subsidizing tenured faculty, or demographic gatekeeping via the tenure
 *   track. This reading isolates the academic freedom function and assigns
 *   extractiveness based on how well tenure achieves it, not based on
 *   collateral distributional harms (which are the focus of the sibling
 *   readings).
 *
 * KEY AGENTS:
 *   - Tenured Faculty: Primary beneficiary (powerful/arbitrage) — experiences independence to pursue high-risk research; gains arbitrage advantage within academic labor market
 *   - Early-Career Non-Tenured Academics: Mixed victim/beneficiary (moderate/constrained) — benefits from others' tenure-protected research; bears extraction cost via precarious employment and wage suppression; career path dependency on academic ladder
 *   - Research Community (Disciplinary): Secondary beneficiary (organized/mobile) — stabilized by tenure's protection of researchers across generations; gains coordination benefits; mobile exit to other communities or roles limits extraction
 *   - Threatened External Actors (Political/Corporate/Institutional): Structural loser (powerless in this reading, though actually powerful globally; see override) — cannot suppress inconvenient research; experience tenure as snare that removes their control lever
 *   - Research Universities: Beneficiary/coordinator (institutional/arbitrage) — gain reputation and funding via research excellence; tenure solves coordination problem of stable researchers; arbitrage exit available but costly
 *   - Students: Indirect beneficiary — receive education from active researchers conducting high-risk inquiry; benefit from truth-seeking independent of institutional pressure
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing tenure as epistemically immutable rather than contingent institutional solution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, 0.28).
domain_priors:suppression_score(tenure_contract__academic_freedom_reading, 0.35).
domain_priors:theater_ratio(tenure_contract__academic_freedom_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Tenure as Academic Freedom Protection: Decoupling Researcher Survival from Political/Institutional Backlash").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "higher_education/institutional_governance/labor_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, '0aa607b9-23ab-43b7-a4e4-eeebe3cc2f55').
narrative_ontology:cs_kernel_codification('0aa607b9-23ab-43b7-a4e4-eeebe3cc2f55', formalized).
narrative_ontology:cs_authority_grounding('0aa607b9-23ab-43b7-a4e4-eeebe3cc2f55', lineage).
narrative_ontology:cs_interpretation_layer_present('0aa607b9-23ab-43b7-a4e4-eeebe3cc2f55').
narrative_ontology:cs_reading_relation('0aa607b9-23ab-43b7-a4e4-eeebe3cc2f55', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('0aa607b9-23ab-43b7-a4e4-eeebe3cc2f55', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('0aa607b9-23ab-43b7-a4e4-eeebe3cc2f55', foundational, researcher_independence_enables_truth_seeking).
narrative_ontology:cs_axiom_status(researcher_independence_enables_truth_seeking, holdable).
narrative_ontology:cs_axiom_grounding('0aa607b9-23ab-43b7-a4e4-eeebe3cc2f55', researcher_independence_enables_truth_seeking, deontological).
narrative_ontology:cs_axiom('0aa607b9-23ab-43b7-a4e4-eeebe3cc2f55', secondary, institutional_employment_security_decouples_survival_from_institutional_preference).
narrative_ontology:cs_axiom_status(institutional_employment_security_decouples_survival_from_institutional_preference, holdable).
narrative_ontology:cs_axiom_grounding('0aa607b9-23ab-43b7-a4e4-eeebe3cc2f55', institutional_employment_security_decouples_survival_from_institutional_preference, instrumental).
narrative_ontology:cs_reference_frame('0aa607b9-23ab-43b7-a4e4-eeebe3cc2f55', researcher_independence_epistemic_foundation).
narrative_ontology:cs_drift_state('0aa607b9-23ab-43b7-a4e4-eeebe3cc2f55', contemporary_neoliberal_university, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0aa607b9-23ab-43b7-a4e4-eeebe3cc2f55', '2026-02-27T14:32:00Z').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, faculty_researchers).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, students_via_research_quality).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, disciplinary_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TENURED FACULTY (ROPE) — Experiences tenure as pure coordination mechanism: the constraint solves a legitimate collective action problem (research funding requires institutional commitment; institutions need stable researchers). Faculty benefit from independence to pursue high-risk questions without institutional pressure. Exit options exist (industry, international universities) but arbitrage advantage is substantial within academia. Low experienced extraction because tenure delivers its stated function — decoupling survival from institutional displeasure.
constraint_indexing:constraint_classification(tenure_contract__academic_freedom_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: DISCIPLINARY RESEARCH COMMUNITY (ROPE) — Organized scholarly community benefits from tenure's coordination function: tenured researchers can conduct sustained inquiries, take methodological risks, publish inconvenient findings, and train future researchers without institutional retaliation fear. Generational horizon captures that tenure stabilizes knowledge transmission across research generations. Mobile exit (migrate to other communities or to non-tenured research roles) limits experienced extraction.
constraint_indexing:constraint_classification(tenure_contract__academic_freedom_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: NON-TENURED ACADEMICS (TANGLED ROPE) — Early-career researchers in temporary positions experience tenure as both coordination mechanism (it protects others' research independence) and extraction constraint (they bear the cost of the tenure system through precarious labor conditions and wage suppression). Beneficiary to the coordination function; victim to the distributional extraction that tenure enables by creating a stratified labor market. Constrained exit reflects career path dependency on academic employment.
constraint_indexing:constraint_classification(tenure_contract__academic_freedom_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THREATENED POLITICAL/INSTITUTIONAL ACTORS (SNARE) — Governments, corporations, or institutional authorities that face reputational or political damage from inconvenient research experience tenure as a snare: they cannot suppress or retaliate against tenured researchers without violating academic norms or engaging in overt censorship. Trapped because the constraint exists precisely to prevent their exit strategy (suppressing unwelcome findings). Maximum experienced extraction because tenure removes their primary control lever. However, this perspective's power is actually substantial in global contexts; see directionality override.
constraint_indexing:constraint_classification(tenure_contract__academic_freedom_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: THE RESEARCH UNIVERSITY (ROPE) — Institutions benefit from tenure as a coordination mechanism: stable faculty attract funding, produce research impact, and build institutional reputation. Universities experience tenure as enabling their primary mission (research and knowledge production). Arbitrage exit exists (shift to teaching-only model, become a non-research institution) but opportunity cost is substantial. Tenure's suppression cost (inability to discharge faculty for inconvenient research) is real but calibrated as acceptable for research excellence.
constraint_indexing:constraint_classification(tenure_contract__academic_freedom_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / EPISTEMIC NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, tenure-like mechanisms appear as immutable requirements for knowledge production: any institution that funds high-risk research must insulate researchers from pressure to reach predetermined conclusions. This perspective sees tenure as a natural law of epistemology — if you want truth-seeking, you must decouple survival from institutional preference. However, this risks a false summit: tenure is a specific contingent institutional arrangement, not a law of nature, and other mechanisms (funding autonomy, peer evaluation, exit mobility) might achieve the same epistemic function.
constraint_indexing:constraint_classification(tenure_contract__academic_freedom_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__academic_freedom_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tenure_contract__academic_freedom_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tenure_contract__academic_freedom_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(tenure_contract__academic_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.28): Low-to-moderate. Measured as the degree to which tenure's primary function (protecting truth-seeking) is compromised by collateral extraction mechanisms. The academic freedom reading focuses on HOW WELL tenure achieves its stated function, not on whether tenure creates distributional unfairness. Extractiveness reflects: (1) erosion over time as non-tenured precarity increases (tenant to tenant labor), slightly raising the effective cost of the tenure system; (2) theater ratio (38%) indicates the constraint has some performative element (ritualized peer evaluation, departmental politics) but retains substantial functional coordination. SUPPRESSION (0.35): Moderate. Tenure suppresses the primary suppression mechanism: institutional/political retaliation against inconvenient research. But secondary suppression exists (cultural conformity pressure, self-censorship, career risk in non-tenured positions). The measurement reflects that tenure successfully blocks the institutional lever but weaker suppression mechanisms remain. THEATER RATIO (0.38): Moderate-low. Tenure's primary mechanism (employment security) is functional, not theatrical. The constraint functions as promised. But secondary performative elements exist: peer review processes, departmental evaluation, publication as currency. Theater increases slightly over the interval (0.32 → 0.38) as administrative compliance costs grow relative to actual research independence gains, reflecting bureaucratization of higher education.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is dramatic and instructive. Tenured faculty and research communities see ROPE — pure coordination mechanism delivering independence. Non-tenured academics see TANGLED ROPE — benefits from others' protection plus victim status in a stratified labor system. Threatened external actors see SNARE — tenure removes their suppression lever. The analytical observer risks seeing MOUNTAIN — epistemically immutable requirement for truth-seeking — which is a false summit. This gap reveals the constraint's structural ambiguity: tenure IS a coordination mechanism (enabling research that requires institutional stability), AND it IS an extraction/distributional device (via precarity and labor stratification). The academic freedom reading foregrounds the coordination reading; sibling readings foreground the extraction reading. The readings coexist because they emphasize different aspects of the same institutional structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) represents each agent's structural position relative to extraction flow. TENURED FACULTY (powerful/arbitrage): d ≈ 0.12 — primarily beneficiary; arbitrage exit exists but opportunity cost high; derived χ ≈ -0.08 (institutional/low f(d) ≈ -0.12). RESEARCH COMMUNITY (organized/mobile): d ≈ 0.35 — moderate beneficiary; mobile exit to other communities; derived χ ≈ 0.35. NON-TENURED ACADEMICS (moderate/constrained): d ≈ 0.60 — mixed position (beneficiary to coordination, victim to extraction); constrained exit (career dependency); derived χ ≈ 0.92. EXTERNAL POLITICAL ACTORS: Listed as powerless/trapped in base perspective (d ≈ 0.95, f(d) ≈ 1.42) but OVERRIDE applied: actually powerful globally (governments, corporations can restructure universities, defund research, etc.). Override to d ≈ 0.58 (still experiencing constraint as loss of control but with substantial agency). Derived χ ≈ 0.91 (snare-level extraction in this reading, though they retain power to reshape the constraint). UNIVERSITIES (institutional/arbitrage): d ≈ 0.15 — primarily beneficiary; arbitrage exit available; derived χ ≈ 0.02. The directionality analysis reveals why external actors appear trapped: tenure specifically exists to deny them their preferred exit strategy (suppression). Their experienced extraction is real from within this constraint, even though they retain agency globally.
 *
 * MANDATROPHY ANALYSIS:
 *   The academic freedom reading resolves mandatrophy by isolating the coordination function and measuring extractiveness relative to that function's achievement. If tenure successfully decouples researcher survival from institutional displeasure (high function), extractiveness is low (0.28). If tenure fails (institutional suppression persists or gets worse), extractiveness would be higher. The sibling readings measure different functions: institutional_extraction_reading measures distributional fairness (extractiveness via precarity), demographic_reproduction_reading measures access equity (extractiveness via gatekeeping). One constraint-label (tenure) instantiates three structurally distinct functions with different ε values. The mandatrophy is resolved by decomposing — but the prompt specifies THIS is the academic_freedom_reading only, so we measure tenure relative to its stated purpose: truth-seeking via research independence. No contradiction arises within this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_necessity_vs_institutional_contingency,
    'Is tenure a necessary structural requirement for truth-seeking research, or one contingent institutional arrangement among alternatives?',
    'Comparative analysis of research quality, risk-taking, and inconvenient findings across institutions with permanent employment, contract-based systems with strong funding autonomy, and international variations in academic labor structure. Historical analysis of whether high-risk research existed and flourished before modern tenure systems.',
    'If necessary: mountain classification (Perspective 6) is correct — tenure is epistemically immutable. If contingent: mountain is a false summit, and tenure is a specific (effective but replaceable) institutional solution. This determines whether policy reforms to tenure fundamentally alter research truth-seeking or merely redistribute extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_necessity_vs_institutional_contingency, conceptual, 'Whether tenure is epistemically necessary or institutionally contingent').

omega_variable(
    precarity_extraction_asymmetry,
    'Does the extraction of precarious non-tenured labor systematically exceed the coordination benefits of tenure protection for the researcher population as a whole?',
    'Longitudinal wage and employment security comparison: tenured vs non-tenured cohorts; cost-benefit analysis of research output per unit of labor cost; welfare analysis across all academic workers (not just tenured faculty).',
    'If extraction > coordination: tenure is actually a snare-like system that uses precarity to subsidize protected researchers. If coordination > extraction: rope classification holds and the system is net-beneficial despite distributional unfairness. This affects whether tenure is fundamentally a coordination mechanism or a class-based extraction device.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(precarity_extraction_asymmetry, empirical, 'Whether precarity extraction exceeds tenure''s coordination benefits').

omega_variable(
    political_backlash_suppression_mechanism,
    'Can determined political actors suppress inconvenient tenure-protected research through defunding, reputation attacks, or institutional restructuring, thereby defeating tenure''s suppression-blocking function?',
    'Historical case studies: research suppression attempts against tenured faculty; analysis of effectiveness of defunding, reputation campaigns, or institutional restructuring to silence inconvenient researchers; longitudinal tracking of research independence in politically hostile environments.',
    'If suppression mechanisms are effective: tenure''s suppression coefficient is overstated (0.35 may be too low), and external political actors'' snare classification may shift toward tangled_rope. If tenure is robust: external actors remain snared and unable to suppress despite power. This determines whether tenure achieves its stated function or merely delays suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_backlash_suppression_mechanism, empirical, 'Whether political actors can defeat tenure''s suppression-blocking function').

omega_variable(
    reading_vs_sibling_empirical_gap,
    'This is the academic_freedom_reading. Do the institutional_extraction_reading and demographic_reproduction_reading rest on the same empirical facts but different normative framings, or on genuinely different observable structures?',
    'Comparative constraint analysis: if the sibling readings measure the same extractiveness but assign different evil values (normative judgment), they coexist. If they measure different extraction mechanisms (wage suppression of non-tenured, demographic gatekeeping of women/minorities), they are distinct constraints with different ε values and should be decomposed into separate stories.',
    'Determines whether siblings are coexisting normative readings or separate structural constraints. Affects whether the kernel is truly contested among readings of ONE constraint or whether the label ''tenure'' conflates multiple structurally distinct mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_empirical_gap, conceptual, 'Whether sibling readings differ in empirical structure or only normative frame').

omega_variable(
    false_summit_mountain_epistemic_grounding,
    'Does Perspective 6''s mountain classification rest on a genuine epistemological necessity, or does it naturalize a specific institutional technology whose function could be achieved by other means?',
    'Examine whether alternative institutional forms (contract employment with guaranteed research funding autonomy, peer-evaluated research independence mechanisms, international mobility enabling exit) can achieve equivalent truth-seeking protection without permanent employment. If alternatives exist and function equivalently, the mountain is a false summit.',
    'If true necessity: tenure is immutable, policy reform should focus on access fairness not structure change. If false summit: tenure is contingent institutional design, and policy can experiment with functionally equivalent alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_mountain_epistemic_grounding, conceptual, 'Whether tenure-like protection is epistemically necessary or institutionally contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenure_af_theater_t0, tenure_contract__academic_freedom_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(tenure_af_theater_t5, tenure_contract__academic_freedom_reading, theater_ratio, 5, 0.36).
narrative_ontology:measurement(tenure_af_theater_t10, tenure_contract__academic_freedom_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(tenure_af_extractiveness_t0, tenure_contract__academic_freedom_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(tenure_af_extractiveness_t5, tenure_contract__academic_freedom_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement(tenure_af_extractiveness_t10, tenure_contract__academic_freedom_reading, base_extractiveness, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% The tenure_contract kernel decomposes into three structurally distinct readings with different ε values and different primary functions. The academic_freedom_reading isolates tenure's role in protecting researcher independence (ε=0.28, rope-type coordination). The institutional_extraction_reading isolates tenure's role in sustaining labor stratification (ε would be higher, snare-type). The demographic_reproduction_reading isolates tenure's role in gatekeeping demographic access (ε would be higher, snare-type). All three readings measure aspects of the same institutional structure but emphasize different mechanisms. They are linked via network.affects_constraints to indicate that policy changes to tenure (e.g., abolishing tenure, weakening suppression protection, reforming labor conditions) would shift extractiveness values across all three readings, not just one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__academic_freedom_reading, powerless, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
