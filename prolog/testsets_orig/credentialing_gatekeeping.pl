% ============================================================================
% CONSTRAINT STORY: credentialing_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credentialing_gatekeeping, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: credentialing_gatekeeping
 *   human_readable: Credentialing Gatekeeping in Professional Markets
 *   domain: institutional/professional/economic
 *
 * SUMMARY:
 *   Credentialing gatekeeping represents the institutional constraint that
 *   controls access to professional labor markets through formal
 *   qualification requirements. The constraint combines genuine coordination
 *   functions (reducing information asymmetry, signaling minimum competence,
 *   enabling standardized contracting) with substantial extraction mechanisms
 *   (creating artificial scarcity, concentrating economic rents among
 *   credential holders, raising barriers to market entry and career
 *   mobility). The constraint exhibits multiple structural types depending on
 *   perspective: pure extraction for those trapped outside gatekeeping
 *   systems, mixed coordination-extraction for those navigating between
 *   fields, pure coordination for employers and professional associations,
 *   and increasingly theatrical function for regulatory bodies maintaining
 *   obsolete credential requirements. The rising theater ratio (0.42 → 0.68
 *   over the interval) indicates credential inflation and ritual expansion
 *   outpacing genuine functional verification.
 *
 * KEY AGENTS:
 *   - Aspiring Professionals Without Credentials: Primary victims (powerless/trapped) — face legal prohibition of unlicensed practice and employer hiring filters
 *   - Career-Switching Professionals: Secondary victims (moderate/constrained) — face high costs of credential re-acquisition despite existing competence
 *   - Talent Pool Outside Credential Pathways: Structural victims (powerless/trapped) — excluded from formal labor markets despite potential competence (e.g., self-taught technologists, apprenticeship-trained workers, practitioners from non-accredited training)
 *   - Employers and Professional Associations: Primary beneficiaries (institutional/arbitrage) — experience credentialing as pure coordination mechanism reducing hiring costs
 *   - Incumbent Credential Holders: Secondary beneficiaries (powerful/mobile) — benefit from wage premiums and reduced competition while sharing coordination benefits of professional community
 *   - Regulatory Credentialing Bodies: Institutional actors (organized/constrained) — maintain gatekeeping function through administrative inertia and fee revenue dependence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credentialing_gatekeeping, 0.58).
domain_priors:suppression_score(credentialing_gatekeeping, 0.65).
domain_priors:theater_ratio(credentialing_gatekeeping, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credentialing_gatekeeping, extractiveness, 0.58).
narrative_ontology:constraint_metric(credentialing_gatekeeping, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(credentialing_gatekeeping, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credentialing_gatekeeping, tangled_rope).
narrative_ontology:human_readable(credentialing_gatekeeping, "Credentialing Gatekeeping in Professional Markets").
narrative_ontology:topic_domain(credentialing_gatekeeping, "institutional/professional/economic").

domain_priors:requires_active_enforcement(credentialing_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credentialing_gatekeeping, incumbent_credential_holders).
narrative_ontology:constraint_beneficiary(credentialing_gatekeeping, certifying_bodies).
narrative_ontology:constraint_victim(credentialing_gatekeeping, credential_seekers).
narrative_ontology:constraint_victim(credentialing_gatekeeping, market_entrants).
narrative_ontology:constraint_victim(credentialing_gatekeeping, talent_pool_outside_credential_pathways).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASPIRING PROFESSIONAL (SNARE) — Faces insurmountable barriers to entry: legal/regulatory prohibition of unlicensed practice, employer hiring filters requiring specific credentials, professional association gatekeeping. No meaningful exit option except abandoning career aspirations. Maximum suppression via credential walls; maximum extraction through credential costs (tuition, exam fees, mandatory continuing education). Credential requirements function as pure extraction mechanism with minimal coordination benefit to this agent.
constraint_indexing:constraint_classification(credentialing_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CAREER-SWITCHING PROFESSIONAL (TANGLED ROPE) — Has existing credentials but seeks entry to a new field. Faces genuine coordination benefits (credentials signal competence, reduce transaction costs, enable rapid hiring) alongside extraction costs (must repeat credentialing in new field, foregone earnings during transition, unnecessary re-verification of existing competencies). High exit costs but not insurmountable — can persist in original field or self-employ with lower credentialing requirements. Asymmetric extraction mixed with real coordination function.
constraint_indexing:constraint_classification(credentialing_gatekeeping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMPLOYERS & PROFESSIONAL ASSOCIATIONS (ROPE) — Genuine beneficiaries who experience credentialing as pure coordination mechanism. Credentials reduce hiring costs, filter for baseline competence, enable standardized contracts, reduce litigation exposure for malpractice, provide quality assurance to consumers. Minimal extraction cost to these agents; maximum coordination benefit. Can arbitrage between markets with different credential requirements or deploy credentials strategically.
constraint_indexing:constraint_classification(credentialing_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT CREDENTIAL HOLDERS (TANGLED ROPE) — Benefit from credential gatekeeping through reduced competition, wage premiums, and protected market. But also coordinate through shared standards, professional communities, and collective quality maintenance. Extraction is not pure — they benefit from genuine coordination benefits of their credential system. Mobile exit options (can relocate to lower-credential jurisdictions, can operate across borders) mean moderate suppression. Mixed coordination-extraction at moderate intensity.
constraint_indexing:constraint_classification(credentialing_gatekeeping, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY CREDENTIALING BODIES (PITON) — Large theater ratio indicates performative function. Many regulatory bodies have become administrative machinery maintaining credential standards that no longer reflect actual job requirements. Theater shows up as: credential requirements that exceed necessary competence, exam content mismatched to actual practice, licensing renewal requirements that generate fee revenue without functional verification, turf wars between competing credentialing bodies. The original coordination function (quality assurance) has atrophied; the constraint persists through institutional inertia and regulatory lock-in.
constraint_indexing:constraint_classification(credentialing_gatekeeping, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, credentialing appears as an immutable requirement of complex professional markets: you cannot maintain quality standards without some signal of competence, you cannot reduce information asymmetry between expert and consumer without certification, you cannot scale trust in anonymous markets without credentials. This perspective risks naturalizing what is actually a contingent institutional arrangement. The false summit detection engine should flag this as naturalization of extractive gatekeeping under the guise of inevitable coordination.
constraint_indexing:constraint_classification(credentialing_gatekeeping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credentialing_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credentialing_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credentialing_gatekeeping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credentialing_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(credentialing_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(credentialing_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high with upward drift. The constraint extracts from credential seekers through tuition costs, exam fees, continuing education mandates, and opportunity costs during credential acquisition. The extraction is not maximal (0.66+) because some genuine coordination value persists — credentials do provide signals that reduce employer search costs and some correlation with job performance. The rising trajectory (0.35 → 0.58) reflects credential inflation: requirements have increased faster than actual job complexity, indicating growing rent-extraction overlaid on coordination function. Suppression (0.65): High. Legal prohibition of unlicensed practice in regulated professions (medicine, law, engineering, accounting) creates near-total barriers for non-credentialed individuals. Employer hiring filters enforce credential requirements even in unregulated fields. Continuing education mandates lock in credential holders. Geographic credential reciprocity barriers fragment markets. Theater ratio (0.68): High and rising. Many credential requirements have become performative: licensing exams test knowledge disconnected from actual job tasks, mandatory continuing education fills time without competence verification, credential renewal generates fee revenue without functional assessment, credential board deliberations prioritize incumbent interests over market efficiency. The rising trajectory (0.42 → 0.68) indicates credential bodies have become primarily administrative (maintaining gatekeeping machinery) rather than functional (assessing actual competence).
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximal perspectival disagreement. Aspiring professionals see a snare — pure extraction with no coordination benefit to them. Career-switchers see tangled rope — real coordination functions (professional standards) mixed with extraction (re-credentialing costs). Employers see pure rope — credentials purely reduce their transaction costs with no extraction cost. Incumbents see tangled rope at higher value — they benefit from both the coordination function (professional community) and the extraction mechanism (reduced competition and wage premiums). Regulatory bodies see their own degraded performance (piton) — they maintain gatekeeping machinery through inertia, not effective function. The analytical observer risks a mountain perspective — 'credentialing is inevitable for complex professions' — but the structural data reveals this as naturalization of a contingent institutional arrangement that concentrates rents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) track the structural position of each agent relative to extraction flow. Aspiring professionals without credentials: d ≈ 0.95 (full target); trapped without exit options; experience maximum suppression. Career-switchers: d ≈ 0.65 (target with modest options); constrained exit means they experience moderate-high extraction but retain some agency. Employers: d ≈ 0.10 (beneficiary); credentialing reduces their hiring costs and liability; they experience coordination without extraction cost. Incumbents: d ≈ 0.35 (mixed); benefit from gatekeeping rent but also benefit from genuine professional community coordination; mobile exit options reduce suppression intensity. Regulatory bodies: d ≈ 0.20 (beneficiary via fee extraction); they coordinate gatekeeping maintenance but experience limited suppression because they can exit by removing requirements (they are constrained by political economy, not structural inability).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is: 'Is credentialing coordination or extraction?' The answer is simultaneously both, with the proportions determined by the agent's structural position. For those inside the credential system, it is predominantly coordination — a shared professional standard maintaining quality. For those seeking entry, it is predominantly extraction — a barrier preventing market participation. The rising theater ratio (0.68) indicates the extraction mechanism is tightening: credential inflation outpaces genuine job requirement growth, suggesting rent maintenance has become primary function over quality assurance. The analytical observer's mountain perspective (credentialing is inevitable) obscures the contingency: systems like software development, skilled trades in some jurisdictions, and certain creative professions function with minimal credentialing gatekeeping. The constraint is revealed not as natural law but as extractive institutional arrangement defended by arguments of coordination necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_competence_measurement,
    'How much of credential value reflects actual job-relevant competence versus social gatekeeping and rent extraction?',
    'Comparative analysis: job performance of credentialed vs non-credentialed workers with equivalent experience and ability; correlation between credential test scores and work quality; studies of alternative competence signals (portfolios, apprenticeships, demonstrated performance)',
    'If credentials predict performance: tangled_rope classification sustained (real coordination function present). If weak predictive power: snare classification strengthened (pure extraction with minimal coordination benefit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(actual_competence_measurement, empirical, 'Relationship between credentials and actual job competence').

omega_variable(
    alternative_signal_effectiveness,
    'Would alternative competence signals (portfolio screening, skills-based hiring, apprenticeship models, work experience verification) be more efficient than credential gatekeeping for specific professions?',
    'Field experiments comparing hiring outcomes under credential-required vs credential-optional hiring; historical analysis of professions that transitioned from gatekeeping to alternative signals; cross-country comparison of sectors with different credentialing regimes',
    'If alternatives more efficient: credential gatekeeping is revealed as extracted rent. If alternatives perform worse: gatekeeping coordination function is validated. Scope of alternative effectiveness matters: strong in some fields (software, trades), weak in others (medicine, law).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_signal_effectiveness, empirical, 'Whether alternative competence signals could replace credentials efficiently').

omega_variable(
    credential_inflation_mechanism,
    'Is observable credential inflation driven by genuine skill level increase in job requirements or by credential holder extraction (pushing requirements higher to maintain scarcity)?,',
    'Analysis of job postings over time: correlation between credential requirement inflation and actual task complexity; survey of hiring managers on whether credential inflation reflects real job requirements; historical comparison of equivalent jobs with stable vs rising credential requirements',
    'If driven by actual skill growth: inflation validates tangled_rope (coordination responding to real demands). If driven by extraction: inflation confirms snare dynamics (gatekeeping tightening to maintain rent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_inflation_mechanism, empirical, 'Whether credential inflation reflects real job requirement changes or extraction').

omega_variable(
    regulatory_capture_in_credentialing,
    'To what extent are credentialing bodies captured by incumbent credential holders versus functioning as independent quality assurance agents?',
    'Analysis of credential board composition (proportion of incumbents vs external experts); voting records on credential requirement changes; cost-benefit analyses of credential requirements by independent auditors; correlation between credential holder interests and credential body decisions',
    'If heavily captured: regulatory piton classification deepens; extraction mechanism is incumbent control of gatekeeping bodies. If independent: credential bodies maintain genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_in_credentialing, empirical, 'Degree of regulatory capture in credentialing bodies').

omega_variable(
    geographic_credential_arbitrage,
    'Do credential requirement differences across jurisdictions represent genuine local regulatory needs or extractive gating that creates artificial scarcity in local markets?',
    'Comparison of credential requirements across jurisdictions for same profession; analysis of service quality differences (are higher-credentialed jurisdictions better served?); tracking of credential reciprocity and mutual recognition agreements; comparison of consumer outcomes across jurisdictions',
    'If local differences reflect genuine needs: credential systems serve regional coordination function (tangled_rope confirmed). If differences are arbitrary: geographic gatekeeping fragments markets and enables local extraction (snare confirmed at regional scope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_credential_arbitrage, empirical, 'Whether geographic credential differences reflect genuine needs or arbitrary gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credentialing_gatekeeping, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cg_tr_t0, credentialing_gatekeeping, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cg_tr_t20, credentialing_gatekeeping, theater_ratio, 20, 0.58).
narrative_ontology:measurement(cg_tr_t40, credentialing_gatekeeping, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(cg_be_t0, credentialing_gatekeeping, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cg_be_t20, credentialing_gatekeeping, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(cg_be_t40, credentialing_gatekeeping, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credentialing_gatekeeping, identity_coordination).
narrative_ontology:boltzmann_floor_override(credentialing_gatekeeping, 0.12).
narrative_ontology:affects_constraint(credentialing_gatekeeping, professional_wage_inequality).
narrative_ontology:affects_constraint(credentialing_gatekeeping, occupational_mobility_barriers).
narrative_ontology:affects_constraint(credentialing_gatekeeping, market_entry_cost_accumulation).

% DUAL FORMULATION NOTE:
% Credentialing gatekeeping decomposes into discipline-specific constraints based on empirical credential validation. Medical licensing (high competence correlation, genuine coordination function) classifies differently from administrative certification (low performance correlation, primarily extractive). Each profession should be analyzed separately per epsilon-invariance principle, then linked via network edges showing how discipline-level constraints aggregate to this system-level story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(credentialing_gatekeeping, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
