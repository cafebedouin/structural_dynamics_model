% ============================================================================
% CONSTRAINT STORY: nsl_hk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_hk, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nsl_hk
 *   human_readable: Hong Kong National Security Law (NSL)
 *   domain: political/legal
 *
 * SUMMARY:
 *   The Hong Kong National Security Law (NSL), imposed by Beijing on Hong
 *   Kong in June 2020, represents a structural shift in Hong Kong's political
 *   status from semi-autonomous special administrative region to integrated
 *   component of the PRC security apparatus. The NSL criminalizes four
 *   categories of activity: secession, subversion, terrorism, and foreign
 *   collusion. The law's vague language ('subversion' includes undefined acts
 *   endangering national security), extraterritorial scope (applies to all
 *   persons, entities, and acts outside Hong Kong), and centralized
 *   interpretation (National Security Commission reports to Beijing) create a
 *   suppression mechanism that operates through legal uncertainty rather than
 *   transparent enforcement. By 2024, over 71 individuals had been prosecuted
 *   under NSL, with convictions typically resulting in 5-10 year sentences.
 *   The constraint exhibits radically different structural meanings depending
 *   on the observer's position: Beijing and the pro-Beijing establishment
 *   experience NSL as coordination (integrating Hong Kong into national
 *   security governance), while civil society actors experience it as pure
 *   extraction (suppression of political freedoms with no offsetting
 *   benefit). The theater ratio (0.64) reflects that NSL operates through
 *   legal institutions rather than overt coercion, creating performative
 *   legitimacy through court proceedings while the underlying mechanism is
 *   political suppression. The suppression index (0.78) reflects the law's
 *   breadth, vagueness, and the practical irreversibility of legal jeopardy
 *   (prosecution leaves permanent stigma; acquittal does not erase legal
 *   exposure).
 *
 * KEY AGENTS:
 *   - Beijing Central Authority: Primary beneficiary (institutional/arbitrage) — gains unified security doctrine, political control, deterrence signal to other separatist movements
 *   - Hong Kong Pro-Beijing Establishment: Secondary beneficiary (institutional/arbitrage) — benefits from political clarity, elimination of political uncertainty, governance legitimacy
 *   - Hong Kong Civil Society and Pro-Democracy Activists: Primary victims (powerless/trapped) — face legal jeopardy for political speech and assembly; cannot exit without relocation
 *   - Independent Media and Journalists: Secondary victims (moderate/constrained) — confront self-censorship pressures; costly exit (emigration)
 *   - Academic and Professional Communities: Secondary victims (moderate/constrained) — face institutional pressure and legal exposure for academic freedom and professional independence
 *   - International Liberal Democratic Community: Analytical observer (analytical/analytical) — assesses NSL against international human rights standards and rule-of-law norms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_hk, 0.68).
domain_priors:suppression_score(nsl_hk, 0.78).
domain_priors:theater_ratio(nsl_hk, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_hk, extractiveness, 0.68).
narrative_ontology:constraint_metric(nsl_hk, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(nsl_hk, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_hk, snare).
narrative_ontology:human_readable(nsl_hk, "Hong Kong National Security Law (NSL)").
narrative_ontology:topic_domain(nsl_hk, "political/legal").

domain_priors:requires_active_enforcement(nsl_hk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_hk, beijing_central_authority).
narrative_ontology:constraint_beneficiary(nsl_hk, hong_kong_pro_beijing_establishment).
narrative_ontology:constraint_victim(nsl_hk, hong_kong_civil_society).
narrative_ontology:constraint_victim(nsl_hk, hong_kong_pro_democracy_activists).
narrative_ontology:constraint_victim(nsl_hk, independent_media).
narrative_ontology:constraint_victim(nsl_hk, academic_freedom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HONG KONG CIVIL SOCIETY (SNARE) — Cannot exit without physical relocation or capitulation. NSL creates legal jeopardy for political speech, assembly, and press freedom. Extraction mechanism relies on suppression of exit (trapped exit option) and legal uncertainty. d≈0.92, f(d)≈1.40, σ=0.9 → χ≈0.88. Experiences pure extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(nsl_hk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INDEPENDENT MEDIA (SNARE) — Constrained exit (emigration is costly; staying risks prosecution). NSL allows prosecution of journalism touching on separatism, subversion, or foreign collusion. Theater_ratio is moderate (0.64) because the law operates through legal institutions, not brute force, but the underlying extraction is severe. d≈0.88, f(d)≈1.35, σ=0.9 → χ≈0.76.
constraint_indexing:constraint_classification(nsl_hk, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRO-DEMOCRACY COALITION (TANGLED ROPE) — Organized agents with some coordination function (solidarity, mutual aid, information sharing) but also asymmetric extraction. NSL simultaneously constrains coordination (illegal assembly) and enables it (radicalization, stronger collective identity against suppression). d≈0.70, f(d)≈1.06, σ=0.9 → χ≈0.67. The constraint has a genuine coordination component (in-group bonding) alongside extraction, making Tangled Rope appropriate for this organized perspective.
constraint_indexing:constraint_classification(nsl_hk, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: BEIJING CENTRAL AUTHORITY (ROPE) — Experiences NSL as a coordination mechanism: enforcing unified national security doctrine, integrating Hong Kong into PRC governance structures, and signaling resolve to other separatist movements (Taiwan, Xinjiang). Low exit costs; institutional flexibility to interpret law. d≈0.15, f(d)≈0.02, σ=0.9 → χ≈0.01. Effective extraction is minimal because the beneficiary has high agency.
constraint_indexing:constraint_classification(nsl_hk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: PRO-BEIJING ESTABLISHMENT (ROPE) — Benefits from legal clarity, reduced political uncertainty, and integration with mainland governance. Experiences NSL as coordination. Exit options include mainland opportunities, business continuity under PRC alignment, political leverage. d≈0.20, f(d)≈0.08, σ=0.9 → χ≈0.05.
constraint_indexing:constraint_classification(nsl_hk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: INTERNATIONAL ANALYTICAL OBSERVER (SNARE) — From a global human rights and rule-of-law perspective, NSL exhibits the structural features of a Snare: high suppression (0.78), high extractiveness (0.68), asymmetric power, vague legal definitions ('subversion,' 'foreign collusion'), and irreversibility (constitutional entrenchment). d≈0.68, f(d)≈1.04, σ=1.2 → χ≈0.85. The constraint targets a civil society victim group that cannot exit the jurisdiction or negotiate terms.
constraint_indexing:constraint_classification(nsl_hk, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_hk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nsl_hk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nsl_hk, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_hk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_hk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The NSL transfers political decision-making authority from Hong Kong civil society to Beijing's security apparatus. Civil society actors lose the freedom to advocate for independence, democratic governance, or criticism of CCP policy. The extraction is not total (some economic activity continues) but is severe in the political domain. The measurement trajectory (0.52 → 0.68 over 4 years) reflects escalating enforcement and expanding interpretation of what constitutes NSL violation. Suppression (0.78): High. The mechanism operates through four channels: (1) vague legal definitions that create legal uncertainty, (2) institutional capacity (prosecutors, courts, national security commission), (3) downstream social consequences (employment loss, professional exclusion, family harassment), and (4) irreversibility (prosecution record persists regardless of acquittal). The barrier to exit is substantial — civil society actors cannot openly advocate for Hong Kong independence or organize against the law without risking prosecution. Theater ratio (0.64): Moderate. NSL operates through formal legal institutions (courts, prosecutors, rules of evidence), creating performative legitimacy. However, the prosecutions frequently hinge on vague standards ('foreign collusion' proved by funding international NGOs; 'subversion' proved by social media posts), suggesting the theater serves to legitimize what is fundamentally a political decision made in Beijing. The ratio is not higher (0.80+) because some cases do involve concrete illegal acts (unauthorized assembly, arson). The ratio is not lower (0.40) because the legal apparatus is genuinely deployed, not entirely performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a stark perspectival divergence. Beijing and the pro-Beijing establishment see NSL as Rope: a coordination mechanism that integrates Hong Kong into a unified national security framework and signals resolve to other separatist movements. The law provides clarity on what is prohibited; institutions can operate within those bounds. The extraction is minimal from this perspective because the beneficiary has high agency — Beijing can negotiate the terms of integration, adjust enforcement intensity, and modify the law. Civil society actors see Snare: the law creates legal jeopardy for political speech and assembly with no offsetting coordination benefit. They cannot negotiate the terms (the law is imposed unilaterally), cannot exit without relocation (trapped exit option), and bear the costs asymmetrically. The analytical observer at the international level also sees Snare: from a rule-of-law and human rights perspective, NSL exhibits the signature of a Snare — high suppression, asymmetric power, vague definitions, irreversible legal exposure, targeting of a civil society victim group. The pro-democracy coalition (organized agents) sees Tangled Rope: the law simultaneously constrains coordination (assembly is illegal) and enables it (persecution creates in-group solidarity, radicalization, international support). This perspectival gap is not a measurement ambiguity — it reflects genuine structural differences in how the constraint operates from different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beijing Central Authority: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.01. Net beneficiary. The authority can interpret and modify the law; it faces no legal jeopardy; it gains centralized control. Civil society actors: Victims + trapped → d≈0.92, f(d)≈1.40. Severe extraction. No agency in negotiating terms; legal jeopardy for political speech; cannot exit without relocation. Pro-democracy coalition (organized): Victims + constrained → d≈0.70, f(d)≈1.06. Extraction with some organizational capacity. Can coordinate information sharing and solidarity; cannot openly advocate; some members have emigrated (exit capacity exists but is costly). Independent media: Victims + constrained → d≈0.88, f(d)≈1.35. High extraction. Cannot freely publish political analysis; can emigrate but at significant professional cost. Hong Kong pro-Beijing establishment: Beneficiary + arbitrage → d≈0.18, f(d)≈0.06. Net beneficiary; lesser degree than Beijing because subject to NSL legal scope even if aligned politically. International observer: Analytical → d≈0.68, f(d)≈1.04. Sees structural extraction targeting a victim group.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (ε=0.68 > 0.46): This constraint requires mandatrophy resolution because extractiveness exceeds 0.46, and the earlier classification as 'coordination mechanism' vs 'suppression mechanism' was the core disputed question. RESOLUTION: The empirical evidence (71 prosecutions mostly for speech; vague legal standards; irreversible legal exposure; absence of offsetting coordination benefit for victims; civil society inability to exit or negotiate) confirms the Snare classification. The constraint is NOT a legitimate coordination mechanism for most perspectives. Beijing and the pro-Beijing establishment experience genuine coordination (integration into national security governance), but this is minority-perspective coordination. The majority of Hong Kong residents (pro-democracy activists, independent media, academic institutions) experience pure extraction. The theater ratio (0.64) and vague legal definitions suggest Beijing may attempt to legitimize NSL as coordination (rule-of-law governance) when it is actually suppression (political control). The mandatrophy is resolved by acknowledging both perspectives are structurally real — Beijing genuinely experiences coordination, civil society genuinely experiences extraction — while recognizing that the classification for the constraint as a whole is Snare because the extraction mechanism is primary and the coordination is secondary/minority. The perspective from organized pro-democracy actors (Tangled Rope) captures the hybrid nature: NSL simultaneously suppresses and enables organization, but the suppression is the dominant feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rule_of_law_vs_political_reality,
    'Is NSL primarily a legal instrument enforcing genuine security concerns, or a political suppression mechanism disguised in legal form?',
    'Prosecution pattern analysis: measure ratio of prosecutions for concrete illegal acts (espionage, violent subversion) vs. prosecutions for speech/assembly. Compare to enforcement in similar jurisdictions (Singapore, Israel). Assess whether legal definitions (subversion, foreign collusion) are enforced consistently across political speech.',
    'If genuine security enforcement: classification shifts toward Tangled Rope (security coordination with extraction overhead). If political suppression: confirms Snare classification. The empirical record (71 prosecutions by 2024, mostly speech-based) suggests the latter, but the distinction turns on prosecution intent which is partially observable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rule_of_law_vs_political_reality, empirical, 'Whether NSL functions as security enforcement or political suppression').

omega_variable(
    exit_mobility_for_vulnerable_populations,
    'Are Hong Kong civil society actors actually trapped, or do they have feasible exit options (emigration, diaspora relocation, remote operation)?',
    'Demographic data on emigration rates pre- and post-NSL (2020-2025). Cost-benefit analysis of emigration vs. staying for different actor classes (activists, journalists, academics, lawyers). Survey of actors who chose exit vs. those who remained and why.',
    'If exit is feasible for most actors: exit_options upgrade from ''trapped'' to ''constrained'' or ''mobile'', reducing d values and χ. Classification could shift from Snare toward Tangled Rope for moderate-power agents. If exit is functionally blocked by practical constraints: Snare classification confirmed with d≈0.92-0.95.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_mobility_for_vulnerable_populations, empirical, 'Feasibility and costs of exit options for different actor classes').

omega_variable(
    suppression_mechanism_degradation,
    'Does NSL''s suppression mechanism rely primarily on institutional capacity (court system, prosecution resources, sustained enforcement) or on fear and uncertainty (chilling effects from vague language)?',
    'Comparison of actual enforcement intensity (prosecutions per year, conviction rates, sentences) with perception of threat (survey data on self-censorship, avoidance of NSL-risky activities). Measure lag between suppression capability and suppression perception.',
    'If institutional capacity is the bottleneck: suppression could degrade over time if the court system becomes overwhelmed or political will wanes (Piton pathway). If fear/uncertainty is the mechanism: suppression may persist indefinitely without high enforcement costs, confirming Snare stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_degradation, empirical, 'Whether NSL suppression relies on institutional capacity or fear-based mechanisms').

omega_variable(
    hong_kong_autonomy_residue,
    'Do Hong Kong institutions (judiciary, media regulators, legislature) retain enough autonomy to interpret NSL narrowly, or is interpretation entirely centralized in Beijing?',
    'Analysis of court decisions: measure variance in NSL interpretation across Hong Kong courts. Compare to guidance from Beijing. Assess whether Hong Kong courts ever reject NSL prosecution arguments or interpret definitions narrowly.',
    'If Hong Kong autonomy persists: some coordination function emerges (rule-of-law predictability within constraints), and classification shifts toward Tangled Rope. If interpretation is entirely Beijing-directed: NSL functions as pure imposition without local legitimacy, confirming Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hong_kong_autonomy_residue, empirical, 'Degree of Hong Kong judicial autonomy in NSL interpretation').

omega_variable(
    international_coordination_cost,
    'Does NSL generate international costs (sanctions, diplomatic isolation, capital flight) that offset Beijing''s coordination gains?',
    'Economic data: FDI flows to Hong Kong pre- and post-NSL (2020-2025). Sanctions imposed by US, UK, EU, Australia. Brain drain metrics (emigration of skilled workers, expats). Corporate headquarters relocations out of Hong Kong.',
    'If international costs are high: NSL''s effective extraction (χ) is reduced by opportunity costs, potentially shifting Beijing''s perspective toward Scaffold (temporary coordination) rather than pure Rope. If costs are manageable: Beijing''s Rope perspective is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_coordination_cost, empirical, 'International economic and diplomatic costs of NSL enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_hk, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl_tr_t0, nsl_hk, theater_ratio, 0, 0.5).
narrative_ontology:measurement(nsl_tr_t2, nsl_hk, theater_ratio, 2, 0.58).
narrative_ontology:measurement(nsl_tr_t4, nsl_hk, theater_ratio, 4, 0.64).

% Extraction over time
narrative_ontology:measurement(nsl_be_t0, nsl_hk, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(nsl_be_t2, nsl_hk, base_extractiveness, 2, 0.62).
narrative_ontology:measurement(nsl_be_t4, nsl_hk, base_extractiveness, 4, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_hk, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_hk, hong_kong_civil_service_loyalty).
narrative_ontology:affects_constraint(nsl_hk, hong_kong_judicial_independence).
narrative_ontology:affects_constraint(nsl_hk, hong_kong_media_freedom).
narrative_ontology:affects_constraint(nsl_hk, taiwan_unification_pressure).

% DUAL FORMULATION NOTE:
% NSL functions as both a legal instrument and a political suppression mechanism. These are not separate constraints but rather two perspectives on the same structural phenomenon. The legal analysis (constitutional status, statutory interpretation) and the political analysis (power asymmetry, suppression mechanism) yield the same classification (Snare) from their respective vantage points, confirming the constraint identity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
