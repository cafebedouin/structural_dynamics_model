% ============================================================================
% CONSTRAINT STORY: later_amendment_eras__progressive_era_amendments
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_later_amendment_eras__progressive_era_amendments, []).

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
 *   constraint_id: later_amendment_eras__progressive_era_amendments
 *   human_readable: Progressive Era Constitutional Amendments (16th, 17th, 18th, 19th)
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The Progressive era amendments (16th, 17th, 18th, 19th: 1913-1920)
 *   represent a seven-year retooling of the constitutional machinery that
 *   fundamentally redistributed power among specific actors. The 16th
 *   Amendment (1913) enabled federal income taxation, nationalizing wealth
 *   extraction and creating a new federal revenue base previously impossible
 *   under tariff-dependent systems. The 17th Amendment (1913) replaced
 *   state-legislature-chosen senators with direct popular election,
 *   suppressing the state-legislative broker class and centralizing electoral
 *   power. The 18th Amendment (1919) prohibited alcohol, mobilizing federal
 *   enforcement against a distributed local economy (saloons, distilleries,
 *   breweries). The 19th Amendment (1920) extended suffrage to women,
 *   doubling the electorate and bringing new voters into both electoral
 *   politics and the federal taxation system. These amendments operated in
 *   tandem: the income tax provided federal revenue for expanded enforcement
 *   apparatus; direct election removed legislative gatekeepers who resisted
 *   federal expansion; prohibition justified federal police power; and
 *   women's enfranchisement created electoral incentives for both progressive
 *   taxation and moral legislation. The constraint exhibits Tangled Rope
 *   structure because the amendments simultaneously coordinate and extract:
 *   they solve genuine collective action problems (how to tax national wealth
 *   without state-by-state negotiation; how to enfranchise women nationally)
 *   while suppressing specific intermediary classes (state legislatures,
 *   saloon economy, wealth concentrators) and extracting resources through
 *   progressive taxation.
 *
 * KEY AGENTS:
 *   - Progressive Reform Coalition: Organized beneficiary (organized/mobile, generational) — women's suffrage groups, labor organizers, temperance advocates, progressive Republicans and Democrats; experiences amendments as pure coordination solution to collective action problems
 *   - National Reform Majorities: Beneficiary (powerful/mobile, biographical) — northern industrial centers with progressive sentiment; benefit from centralized federal authority and progressive taxation that shifts burden to regional wealth concentrators
 *   - Women Voters: Beneficiary (previously powerless, now moderate/mobile post-19th) — newly enfranchised; gain political voice but simultaneously become subject to federal taxation and electoral accountability
 *   - Federal Revenue Authority: Institutional beneficiary (institutional/arbitrage, immediate) — newly empowered by 16th Amendment; extracts wealth through progressive income taxation while providing genuine coordination function (national revenue pooling)
 *   - State Legislatures: Victim/loser (institutional/constrained, biographical) — suppressed by 17th Amendment; lose power to select senators and thus their gatekeeper function in national politics; constrained exit through electoral politics
 *   - Saloon Economy: Victim (moderate/trapped locally, biographical) — suppressed by 18th Amendment; local alcohol distribution networks face federal enforcement and elimination; geographic specificity (concentrated in working-class neighborhoods) makes exit costly
 *   - Gilded Age Wealth Concentrators: Victim (powerful/constrained, biographical) — targeted by 16th Amendment progressive taxation; high incomes and accumulated capital now exposed to federal extraction; constrained exit (relocation possible but costly)
 *   - Constitutional Amendment Apparatus: Institutional actor (institutional/arbitrage, civilizational) — maintains legitimacy narrative while enabling rapid constitutional change; theater increases as amendment rate accelerates
 *   - Analytical Observer: Neutral position (analytical/analytical, civilizational) — observes whether amendments appear as inevitable modernization or as contingent coalition victory over disorganized resistance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(later_amendment_eras__progressive_era_amendments, 0.52).
domain_priors:suppression_score(later_amendment_eras__progressive_era_amendments, 0.48).
domain_priors:theater_ratio(later_amendment_eras__progressive_era_amendments, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(later_amendment_eras__progressive_era_amendments, extractiveness, 0.52).
narrative_ontology:constraint_metric(later_amendment_eras__progressive_era_amendments, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(later_amendment_eras__progressive_era_amendments, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(later_amendment_eras__progressive_era_amendments, tangled_rope).
narrative_ontology:human_readable(later_amendment_eras__progressive_era_amendments, "Progressive Era Constitutional Amendments (16th, 17th, 18th, 19th)").
narrative_ontology:topic_domain(later_amendment_eras__progressive_era_amendments, "political/legal/constitutional").

domain_priors:requires_active_enforcement(later_amendment_eras__progressive_era_amendments).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(later_amendment_eras__progressive_era_amendments, '5d7bcbd2-0789-4cd2-8132-2ba925acbc47').
narrative_ontology:cs_kernel_codification('5d7bcbd2-0789-4cd2-8132-2ba925acbc47', formalized).
narrative_ontology:cs_authority_grounding('5d7bcbd2-0789-4cd2-8132-2ba925acbc47', lineage).
narrative_ontology:cs_interpretation_layer_present('5d7bcbd2-0789-4cd2-8132-2ba925acbc47').
narrative_ontology:cs_reading_relation('5d7bcbd2-0789-4cd2-8132-2ba925acbc47', later_amendment_eras__reconstruction_amendments, influences).
narrative_ontology:cs_reading_relation('5d7bcbd2-0789-4cd2-8132-2ba925acbc47', later_amendment_eras__civil_rights_era_amendments, influences).
narrative_ontology:cs_reading_relation('5d7bcbd2-0789-4cd2-8132-2ba925acbc47', later_amendment_eras__structural_housekeeping_amendments, coexists_with).
narrative_ontology:cs_axiom('5d7bcbd2-0789-4cd2-8132-2ba925acbc47', foundational, federal_wealth_redistribution_through_taxation).
narrative_ontology:cs_axiom_status(federal_wealth_redistribution_through_taxation, holdable).
narrative_ontology:cs_axiom_grounding('5d7bcbd2-0789-4cd2-8132-2ba925acbc47', federal_wealth_redistribution_through_taxation, empirically_contingent).
narrative_ontology:cs_axiom('5d7bcbd2-0789-4cd2-8132-2ba925acbc47', foundational, direct_representation_over_legislative_mediation).
narrative_ontology:cs_axiom_status(direct_representation_over_legislative_mediation, holdable).
narrative_ontology:cs_axiom_grounding('5d7bcbd2-0789-4cd2-8132-2ba925acbc47', direct_representation_over_legislative_mediation, deontological).
narrative_ontology:cs_axiom('5d7bcbd2-0789-4cd2-8132-2ba925acbc47', secondary, moral_legislation_within_federal_jurisdiction).
narrative_ontology:cs_axiom_status(moral_legislation_within_federal_jurisdiction, overridden).
narrative_ontology:cs_axiom_grounding('5d7bcbd2-0789-4cd2-8132-2ba925acbc47', moral_legislation_within_federal_jurisdiction, deontological).
narrative_ontology:cs_reference_frame('5d7bcbd2-0789-4cd2-8132-2ba925acbc47', constitutional_democracy_via_direct_representation_and_progressive_taxation).
narrative_ontology:cs_drift_state('5d7bcbd2-0789-4cd2-8132-2ba925acbc47', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5d7bcbd2-0789-4cd2-8132-2ba925acbc47', '').
narrative_ontology:cs_kernel_id(later_amendment_eras__progressive_era_amendments, later_amendment_eras).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(later_amendment_eras__progressive_era_amendments, national_reform_majorities).
narrative_ontology:constraint_beneficiary(later_amendment_eras__progressive_era_amendments, women_voters).
narrative_ontology:constraint_beneficiary(later_amendment_eras__progressive_era_amendments, federal_revenue_authority).
narrative_ontology:constraint_victim(later_amendment_eras__progressive_era_amendments, gilded_age_intermediaries).
narrative_ontology:constraint_victim(later_amendment_eras__progressive_era_amendments, state_legislatures).
narrative_ontology:constraint_victim(later_amendment_eras__progressive_era_amendments, saloon_economy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED WOMAN (SNARE, pre-19th) — Trapped by constitutional silence on voting rights and state-level suppression of suffrage. No exit mechanism exists within the constitutional framework prior to amendment. Bears full cost of exclusion while bearing (through taxation after 16th) the revenue extraction. Maximum experienced coercion.
constraint_indexing:constraint_classification(later_amendment_eras__progressive_era_amendments, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: STATE LEGISLATURE BROKER (TANGLED ROPE) — State legislatures were the coordinating mechanism for Senate selection AND the gatekeepers for state-local patronage networks. The 17th Amendment (direct election) suppresses their gatekeeper function but benefits them by empowering direct appeal to voters. Constrained by loss of broker position but not eliminated. The extraction is real (loss of appointment power) but mixed with genuine coordination function previously served. Active enforcement required through electoral machinery.
constraint_indexing:constraint_classification(later_amendment_eras__progressive_era_amendments, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LABORER WITHOUT DEDUCTION (SNARE) — The 16th Amendment enables income taxation. High-wage laborers bear extraction through progressive tax without correspondingly benefiting from the coordination function (national revenue pooling) because wage-earner tax withholding structures will later capture their labor income. No exit mechanism — wages are trapped under the new taxing authority. Theater low: taxation is direct mechanism, not performative.
constraint_indexing:constraint_classification(later_amendment_eras__progressive_era_amendments, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: PROGRESSIVE REFORM COALITION (ROPE) — Organized national movement (women's suffrage groups, labor organizers, temperance advocates, progressive Republicans and Democrats) experiences the amendments as pure coordination: they solve collective action problems (how to enfranchise women nationally without state-by-state battles; how to capture wealth concentration through progressive taxation). The coalition has exit options through ballot initiatives and pressure. Low effective extraction because the coalition designed the constraint and benefits from its coordination function.
constraint_indexing:constraint_classification(later_amendment_eras__progressive_era_amendments, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL REVENUE AUTHORITY (TANGLED ROPE) — The 16th Amendment creates the federal taxing power but also creates genuine coordination function: pooling national revenue enables interstate infrastructure, military capacity, and public goods that states cannot provide alone. The authority benefits from expanded extractive capacity (income taxation vs tariff-dependency) AND from coordinating collective provision. Active enforcement required through IRS-equivalent structures. The extraction is real but inseparable from the coordination.
constraint_indexing:constraint_classification(later_amendment_eras__progressive_era_amendments, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: GILDED AGE WEALTH CONCENTRATOR (SNARE) — The 16th Amendment directly targets accumulated capital and high incomes. Wealthy individuals face constraints: relocation possible (exit to other nations, but costly); trusts and legal structures available (constrained exit through evasion rather than true mobility). High extraction: progressive taxation claims portions of previously untaxed accumulation. Suppression high: enforcement through income reporting, asset disclosure, later IRS structures.
constraint_indexing:constraint_classification(later_amendment_eras__progressive_era_amendments, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: PROHIBITION MOVEMENT (SCAFFOLD) — The 18th Amendment represents a temporary coordinating mechanism with explicit sunset clause (21st Amendment repeals it in 1933). Organized movement sees the constraint as a time-limited experiment in moral legislation. Enforcement requires active suppression of saloons and alcohol production, but movement believes the moral consensus will hold (it does not). Chi low because the constraint's own advocates view it as temporary and reversible — the sunset is built in.
constraint_indexing:constraint_classification(later_amendment_eras__progressive_era_amendments, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: CONSTITUTIONAL AMENDMENT APPARATUS (PITON) — The series of seven amendments in seven years (1913-1920) performs constitutional legitimacy. The apparatus itself — Article V procedures, amendment ratification theater, state-by-state ceremonial approval — becomes increasingly performative as amendment rate accelerates. The process maintains constitutional immutability narrative while simultaneously proving the constitution is revisable. Theater high: the ritual of amendment ratification performs continuity while enabling radical change.
constraint_indexing:constraint_classification(later_amendment_eras__progressive_era_amendments, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / CONSTITUTIONAL INEVITABILITY (MOUNTAIN) — From a civilizational perspective, the Progressive amendments appear inevitable: industrial society requires national taxation (16th), democratic legitimacy requires direct representation (17th), moral progress requires suffrage (19th), and public health requires prohibition (18th). From this view, the constraint appears as a structural requirement of modernization — a law of political-economic development. However, the structural data reveal this as a FALSE SUMMIT: the amendments were contingently designed by specific coalitions, suppressed specific actors (state legislatures, the saloon economy), and extracted concentrated wealth. No inevitability — only organized power meeting disorganized resistance.
constraint_indexing:constraint_classification(later_amendment_eras__progressive_era_amendments, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(later_amendment_eras__progressive_era_amendments_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(later_amendment_eras__progressive_era_amendments, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(later_amendment_eras__progressive_era_amendments, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(later_amendment_eras__progressive_era_amendments, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(later_amendment_eras__progressive_era_amendments, TR),
    TR >= 0.70.

:- end_tests(later_amendment_eras__progressive_era_amendments_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The amendments suppress specific actor classes (state legislatures, saloon owners, wealth concentrators) while benefiting organized national coalitions and federal authority. The income tax is the primary extraction mechanism — it claims portions of previously untaxed wealth, starting at 3% in 1913 and rising to 7% by 1920 on high incomes. However, this is not pure extraction because genuine coordination functions are accomplished: the federal government can now fund infrastructure, defense, and social programs that require national scale; women gain political voice through suffrage; rural and urban areas can compete on electoral rather than legislative terms. Suppression (0.48): Moderate. State legislatures face clear suppression through loss of Senate appointment power; saloon economy faces direct federal enforcement; wealth concentrators face progressive taxation with rising rates. However, suppression is not total — state legislatures remain powerful in state politics; saloon owners can adapt to new legal environment (some escape through relocation, some transition to legal trades); wealth concentrators retain arbitrage options (trusts, relocation, legal evasion). The constraints imposed are real but not absolute. Theater ratio (0.35): Low. The amendments are functionally direct mechanisms, not performative masks. Income taxation directly extracts wealth; direct election directly changes senator selection; prohibition directly suppresses alcohol production; suffrage directly includes women in electorate. There is procedural theater around amendment ratification (state-by-state ritual, ceremonial approval), but the substantive mechanisms are transparent and non-theatrical. Rising theater trajectory (0.25 to 0.35) reflects increasing amendment ratification ritual as amendment frequency accelerates.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits radical perspectival divergence. The Progressive reform coalition sees pure coordination (Rope) — they experience the amendments as solving collective action problems that blocked progress. Women voters see liberation (rope-adjacent) — though this coexists with subsequent extraction through taxation. State legislatures see suppression and loss of power (Tangled Rope) — they are part of the coordination mechanism that the amendments disrupt. The saloon economy sees pure extraction and suppression (Snare) — their economic niche is criminalized with no coordination benefit. Wealth concentrators see extraction under the guise of moral progress (Snare) — progressive taxation targets accumulated capital while prohibition and suffrage provide moral legitimacy for federal expansion. The federal revenue authority sees both coordination and extraction (Tangled Rope) — genuine pooling of national resources alongside enhanced extractive capacity. The constitutional amendment apparatus sees its own legitimacy theater increasing (Piton) — the process of amendment becomes more performative as the pace of change accelerates. The analytical observer faces a false summit temptation (Mountain) — to naturalize the amendments as inevitable modernization rather than recognize them as contingent coalition victories.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural position relative to this specific constraint. State legislatures are beneficiaries of a previous (pre-amendment) constitutional order but become victims of the amendment order — their directionality shifts from low (beneficiary) to high (victim) depending on the reference point. Women begin as trapped agents (disenfranchised, no exit from electoral exclusion) and shift to moderate agents with constrained options (newly enfranchised but subject to taxation). Wealth concentrators have arbitrage options (legal evasion, relocation, trust structures) that prevent them from being fully trapped, placing them at constrained rather than trapped exit level. The federal revenue authority benefits from both extraction (increased tax base) and coordination (national resource pooling), creating a mixed directionality that reflects genuine dual function.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy — the reduction of complex coordination to extractive mechanism — is partially resolved in this reading. The income tax and suppression of state legislatures reduce coordination to extraction for specific victims (wealth concentrators, state legislature brokers), but the amendments simultaneously accomplish genuine coordination (national fiscal capacity, direct electoral representation, women's enfranchisement as moral progress). The constraint is genuinely Tangled Rope, not reducible to either pole. The false summit temptation arises in the analytical observer's perspective: treating the amendments as inevitable modernization naturalizes the contingent distribution of beneficiaries and victims. The amendments appear as natural law only if one presupposes that industrialization requires centralized taxation, that democracy requires direct election, that moral progress requires prohibition, and that suffrage requires women. These are real structural pressures but not natural laws — they are contingent on specific configurations of political power, social movements, and constitutional design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_in_taxation,
    'Does the 16th Amendment primarily coordinate national revenue collection (genuine coordination good) or primarily extract wealth from high earners (asymmetric extraction)?',
    'Historical analysis of revenue use: proportion devoted to genuine public goods (infrastructure, defense, entitlements) vs. proportion captured by political rent-seeking or redistributed through patronage. Comparative analysis with pre-amendment tariff revenue allocation.',
    'If primarily coordination: classify as Rope from beneficiary perspectives. If primarily extraction: classify as Snare from wealth-concentrator perspectives. Current classification (Tangled Rope) assumes both functions coexist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_in_taxation, empirical, 'Whether income taxation primarily coordinates national revenue or primarily extracts wealth').

omega_variable(
    suffrage_as_suppression_or_liberation,
    'Does the 19th Amendment suppress women''s previous economic autonomy (e.g., property rights, economic participation) by bringing them into electoral politics and subsequent taxation, or does it liberate them by granting political voice?',
    'Longitudinal analysis of women''s economic participation, property holdings, and tax burden pre- vs post-19th Amendment. Study of simultaneous expansion of federal taxing authority and women''s enfranchisement.',
    'If primarily suppression: women become newly trapped agents (perspective 1 shifts from Snare to Snare-with-participation-theater). If primarily liberation: suppression shifts from extraction to coordination. Current reading assumes liberation with subsequent extraction through taxation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suffrage_as_suppression_or_liberation, empirical, 'Whether suffrage primarily suppresses or liberates women').

omega_variable(
    gilded_age_intermediary_function,
    'Did state-legislature-chosen senators perform genuine coordination functions that their suppression (17th Amendment) actually disrupts, or did they primarily serve rent-extraction and patronage?',
    'Historical case studies: documentation of senate effectiveness and representativeness before/after 17th Amendment. Analysis of how legislatures selected senators vs how direct election changed selection criteria.',
    'If coordination-disruption: 17th Amendment is a Snare against the democratic impulse (suppresses better representation). If rent-elimination: 17th Amendment is successful Rope (abolishes pure extraction). Classification of state legislatures perspective hinges on this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gilded_age_intermediary_function, empirical, 'Whether legislature-chosen senators performed genuine coordination or pure rent-extraction').

omega_variable(
    prohibition_moral_consensus_reality,
    'Was the moral consensus for prohibition (18th Amendment) genuine mass preference or manufactured consent from organized temperance movements and moneyed interests (Rockefeller, Carnegie funding)?',
    'Referendum data on temperance votes pre-18th Amendment; analysis of funding sources for prohibition advocacy; study of repeal movement (21st Amendment) showing actual preference distribution.',
    'If genuine consensus: Scaffold classification of prohibition movement is correct (organized consensus with sunset). If manufactured: Prohibition is actually a Snare against drinkers/saloons with false consensus theater (extractive suppression dressed as moral coordination). Current classification assumes genuine (if minoritarian) consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prohibition_moral_consensus_reality, empirical, 'Whether prohibition consensus was genuine or manufactured').

omega_variable(
    amendment_reading_vs_reconstruction_reading,
    'Are the Progressive amendments a continuation of Reconstruction''s constitutional reform project or a fundamentally different constitutional moment with different beneficiaries and victims?',
    'This is the core kernel omega: does the Progressive reading FORECLOSE the Reconstruction reading (same constitutional logic, different moment), COEXIST WITH it (separate projects), or INFLUENCE it (Progressive amendments change the terms under which Reconstruction''s unfinished work operates)?',
    'If foreclosure: the Progressive reading has naturalized a specific framing of constitutional authority that the Reconstruction reading contradicts. If coexistence: both readings are simultaneously valid; the kernel accommodates both. If influence: the Progressive amendments reshape the landscape on which Reconstruction''s commitments operate (e.g., the income tax changes what ''equal protection'' means in resource terms).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_reading_vs_reconstruction_reading, conceptual, 'Logical and structural relationship between Progressive and Reconstruction amendment readings of the kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(later_amendment_eras__progressive_era_amendments, 1912, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prog_amend_tr_t1912, later_amendment_eras__progressive_era_amendments, theater_ratio, 1912, 0.25).
narrative_ontology:measurement(prog_amend_tr_t1916, later_amendment_eras__progressive_era_amendments, theater_ratio, 1916, 0.3).
narrative_ontology:measurement(prog_amend_tr_t1920, later_amendment_eras__progressive_era_amendments, theater_ratio, 1920, 0.35).

% Extraction over time
narrative_ontology:measurement(prog_amend_be_t1912, later_amendment_eras__progressive_era_amendments, base_extractiveness, 1912, 0.28).
narrative_ontology:measurement(prog_amend_be_t1916, later_amendment_eras__progressive_era_amendments, base_extractiveness, 1916, 0.41).
narrative_ontology:measurement(prog_amend_be_t1920, later_amendment_eras__progressive_era_amendments, base_extractiveness, 1920, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(prog_amend_su_t1912, later_amendment_eras__progressive_era_amendments, suppression_requirement, 1912, 0.35).
narrative_ontology:measurement(prog_amend_su_t1916, later_amendment_eras__progressive_era_amendments, suppression_requirement, 1916, 0.42).
narrative_ontology:measurement(prog_amend_su_t1920, later_amendment_eras__progressive_era_amendments, suppression_requirement, 1920, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(later_amendment_eras__progressive_era_amendments, enforcement_mechanism).
narrative_ontology:affects_constraint(later_amendment_eras__progressive_era_amendments, reconstruction_amendments).
narrative_ontology:affects_constraint(later_amendment_eras__progressive_era_amendments, civil_rights_era_amendments).
narrative_ontology:affects_constraint(later_amendment_eras__progressive_era_amendments, structural_housekeeping_amendments).

% DUAL FORMULATION NOTE:
% The Progressive era amendments are one reading of a contested constitutional kernel spanning 1868-1992. Each amendment era (Reconstruction, Progressive, Civil Rights, Housekeeping) constitutes a separate constraint with distinct ε values, beneficiary/victim structures, and classifying perspectives. They are linked not as cause-effect but as competing readings of the same constitutional authority — the power to amend the fundamental document. The network relationships reflect that later amendments (Progressive, Civil Rights, Housekeeping) operate within and reshape the framework established by Reconstruction, and that the Housekeeping amendments maintain the machinery that Progressive and Civil Rights amendments depend on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(later_amendment_eras__progressive_era_amendments, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
