% ============================================================================
% CONSTRAINT STORY: columbia_2026_elections
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_columbia_2026_elections, []).

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
 *   constraint_id: columbia_2026_elections
 *   human_readable: 2026 Colombian Presidential Election Structure
 *   domain: political/electoral
 *
 * SUMMARY:
 *   Colombia's 2026 presidential election is structured around a
 *   constitutional one-term limit that forces executive succession. This
 *   constraint simultaneously functions as democratic coordination mechanism
 *   (guaranteeing turnover, preventing indefinite executive power
 *   accumulation), as coordination infrastructure for the incumbent coalition
 *   (managed succession, preservation of party machinery), and as extraction
 *   mechanism (incumbent advantages in campaign resources, administrative
 *   capacity, regional networks). The structure exhibits the full range of DR
 *   classifications depending on the observer's structural position and exit
 *   capacity. For powerless rural voters locked into vote-trading networks
 *   with local caciques, the election appears as pure extraction (snare). For
 *   established opposition parties with organizational capacity, it appears
 *   as a mixed coordination-extraction hybrid (tangled rope). For the
 *   incumbent coalition, it appears as coordination mechanism for managed
 *   succession (rope). The constitutional one-term limit itself appears as
 *   natural law (mountain) from a civilizational analytical perspective, but
 *   this classification requires scrutiny: if the limit persists because both
 *   dominant coalitions prefer its predictability, it is contingent on
 *   current power distribution, not immutable. The measurement trajectory
 *   shows extractiveness increasing from 0.38 to 0.52 over the election
 *   cycle, and theater ratio rising from 0.48 to 0.62, indicating that as the
 *   election approaches, both the performative content and the actual
 *   extraction mechanisms intensify.
 *
 * KEY AGENTS:
 *   - Incumbent Political Coalition: Primary beneficiary (institutional/arbitrage) — controls state resources, enjoys visibility advantages, manages succession through coalition machinery
 *   - Established Opposition Parties: Secondary beneficiary (powerful/mobile) — the one-term limit creates opposition electoral opportunity, but they face incumbent infrastructure advantages
 *   - Regional Power Brokers (Caciques): Tertiary beneficiary (powerful/arbitrage) — control local electoral machinery, extract rents through vote trading, benefit from incumbent machinery coordination
 *   - Marginalized Rural Voters: Primary victim (powerless/trapped) — structurally locked into local political hierarchies, vote preferences filtered through cacique intermediaries, bear extraction costs without agency
 *   - Electoral Commission (CNE): Institutional actor (institutional/arbitrage) — maintains formal independence but depends on coalitions' acceptance for legitimacy, exhibits piton characteristics (performative procedures)
 *   - International Election Observers: Organized external actors (organized/constrained) — temporary presence during campaign creates verification pressure, scaffold logic (sunset after election day)
 *   - Urban Middle-Class Voters: Moderate agent (moderate/mobile) — higher information access, can exit through spoiled ballots or abstention with lower social cost, constrained but not trapped
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the constitutional one-term limit as unchangeable when it depends on political equilibrium
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(columbia_2026_elections, 0.52).
domain_priors:suppression_score(columbia_2026_elections, 0.68).
domain_priors:theater_ratio(columbia_2026_elections, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(columbia_2026_elections, extractiveness, 0.52).
narrative_ontology:constraint_metric(columbia_2026_elections, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(columbia_2026_elections, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(columbia_2026_elections, tangled_rope).
narrative_ontology:human_readable(columbia_2026_elections, "2026 Colombian Presidential Election Structure").
narrative_ontology:topic_domain(columbia_2026_elections, "political/electoral").

domain_priors:requires_active_enforcement(columbia_2026_elections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(columbia_2026_elections, incumbent_political_coalition).
narrative_ontology:constraint_beneficiary(columbia_2026_elections, established_parties).
narrative_ontology:constraint_beneficiary(columbia_2026_elections, regional_power_brokers).
narrative_ontology:constraint_victim(columbia_2026_elections, outsider_candidates).
narrative_ontology:constraint_victim(columbia_2026_elections, electoral_transparency).
narrative_ontology:constraint_victim(columbia_2026_elections, marginalized_constituencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED RURAL VOTERS (SNARE) — Structurally locked into participation without meaningful agency. Election machinery in remote regions depends on local caciques (power brokers) who control information, transportation, and vote counting. Exit via abstention or spoiled ballots incurs social cost. Maximum extraction: voters bear institutional burden while their preferences are filtered through intermediaries.
constraint_indexing:constraint_classification(columbia_2026_elections, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: REGIONAL POLITICAL MOVEMENTS (TANGLED ROPE) — Moderate power but constrained exit. Participate in elections (coordination benefit: legitimate channel for preferences) but face institutional barriers: ballot access requirements, campaign financing concentrated among established parties, media access skewed toward incumbent coalition. Mixed extraction and coordination — some candidates emerge through this process, but the structure favors incumbent-aligned movements.
constraint_indexing:constraint_classification(columbia_2026_elections, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT POLITICAL COALITION (ROPE) — Institutional beneficiary with full arbitrage. The one-term constitutional limit forces transition but also guarantees the party machinery survives intact. Coalition coordinates succession (choosing next-generation leaders), accesses state resources for campaign, leverages incumbent visibility. Net beneficiary — extraction runs toward this coalition. The constraint (one-term limit) is experienced as coordination mechanism for managed succession.
constraint_indexing:constraint_classification(columbia_2026_elections, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ESTABLISHED OPPOSITION PARTIES (TANGLED ROPE) — Powerful institutional actors with some mobility (can field candidates, campaign across regions, access media through party infrastructure) but constrained by incumbent advantages. Electoral rules create mixed signals: the one-term limit opens space for opposition victory (coordination benefit) but incumbent machinery controls voter rolls, regional networks, and administrative resources (extraction mechanism). Effective extraction moderate because established parties have organizational capacity.
constraint_indexing:constraint_classification(columbia_2026_elections, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL COMMISSION (PITON) — Formally independent but structurally dependent on both incumbent and opposition for legitimacy. Theater ratio high (0.58): public validation rituals (voting ceremonies, transparency audits, international observer protocols) persist despite chronic capacity constraints in vote counting, especially in remote regions. The commission maintains institutional inertia — continues formal procedures that have limited verification capacity — because alternatives haven't replaced it and both coalitions prefer predictable theater to unpredictable dispute.
constraint_indexing:constraint_classification(columbia_2026_elections, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL ELECTION OBSERVERS (SCAFFOLD) — Organized external actors (OAS, UN, Carter Center, EU) who validate election legitimacy. Temporary function: observer presence reduces extraction by increasing verification cost and enabling dispute resolution. Constrained exit (must withdraw after election day) but high agency during campaign. Scaffold logic: observers create temporary constraint on obvious manipulation, with sunset when observers leave. Theater ratio drops during observation period (0.42), rises after (0.65).
constraint_indexing:constraint_classification(columbia_2026_elections, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: CONSTITUTIONAL ONE-TERM LIMIT (MOUNTAIN) — From civilizational analytical view, the one-term limit appears as a natural law: a constitutional constraint that is binding, irreversible, and uniform across all agents. Cannot be suspended by either coalition. Engine will flag this as potential false summit — the 'immutable constitutional rule' naturalizes what is actually a political equilibrium: the one-term limit persists because both dominant coalitions prefer the predictability of managed succession to the chaos of authoritarian persistence. If one coalition gained absolute power, they would revise the constitution. The mountainness depends on actual binding force, not formal status.
constraint_indexing:constraint_classification(columbia_2026_elections, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(columbia_2026_elections_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(columbia_2026_elections, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(columbia_2026_elections, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(columbia_2026_elections, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(columbia_2026_elections, TR),
    TR >= 0.70.

:- end_tests(columbia_2026_elections_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The incumbent coalition and regional power brokers extract significant rents from the election: control of campaign timing, access to administrative resources, ability to condition rural voters' participation through local hierarchies. However, the extraction is not total (snare-level 0.70+) because the constitutional one-term limit does force genuine succession — some positions become available to opposition-aligned candidates, and voter preferences are partially expressed through electoral channels. The 0.52 value reflects that the structure has both coordination function (legitimate channel for preferences) and extraction mechanism (incumbent advantages). Suppression (0.68): High. Multiple barriers constrain electoral competition: (1) voter registration mechanisms concentrated in urban areas favor known constituencies, (2) campaign financing asymmetry (incumbent has state resources), (3) media access skewed toward coalition-aligned candidates, (4) vote-buying networks in rural areas reduce meaningful choice, (5) ballot access requirements screen out new candidates. Theater ratio (0.58): Moderate-high. Public validation rituals (voting ceremonies, observer presence, official result announcements) are substantial, but verification capacity lags in remote regions. Urban polling stations have higher transparency; rural stations have lower. Election Commission maintains formal procedures (vote counting oversight, observer protocols) despite chronic underfunding. Theater increases over campaign cycle as pressure mounts for legitimacy performance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival diversity. The incumbent coalition sees coordination (rope); rural voters see extraction (snare); established opposition sees mixed signals (tangled rope); electoral commission sees degraded ritual (piton); international observers see temporary pressure (scaffold); and the constitutional limit appears as natural law (mountain) to civilization-scale analysis. This is a diagnostic exemplar of why indexical classification requires perspectival multiplicity: no single 'correct' type. The same structural arrangement generates six different experiences depending on power level, exit capacity, time horizon, and scope. The mandatrophy is resolved by recognizing that all six readings are simultaneously true — they describe the same constraint structure from different observation posts.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) encode each agent's structural relationship to the extraction flow. Incumbent coalition and caciques derive d from beneficiary status + arbitrage exit options → low d → low/negative f(d) → negative or minimal χ (they experience the constraint as coordination, not extraction). Established opposition derives d from mixed beneficiary/victim status + mobile exit options → moderate d → moderate f(d) → moderate χ (they benefit from the succession opportunity but constrained by incumbent advantages). Marginalized rural voters derive d from victim status + trapped exit options → high d → high f(d) → high χ (maximum experienced extraction). Electoral commission derives d from institutional position + arbitrage exit options, modified by captured status → moderate d, overrideable upward if analysis suggests actual capture. Urban middle-class voters derive d from moderate power + mobile exit → moderate d → moderate χ. The engine computes these from the beneficiary/victim declarations and exit capacity. Directionality overrides are not needed here because the structural relationships are clear: beneficiaries have arbitrage or mobile options with low d; victims have trapped or constrained options with high d.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint is classified as tangled rope because it possesses both genuine coordination function (legitimate channel for electoral preferences, guaranteed succession preventing indefinite power accumulation, inter-party transition mechanism) and asymmetric extraction (incumbent advantages in resources, cacique vote-trading networks, regional administrative control, information asymmetry). All three required elements are present: (1) beneficiaries declared (incumbent_political_coalition, established_parties, regional_power_brokers) provide coordination function; (2) victims declared (outsider_candidates, electoral_transparency, marginalized_constituencies) experience extraction; (3) requires_active_enforcement true (the constraint requires active electoral machinery, campaign coordination, vote-counting procedures). The tangled rope classification prevents false reductions in either direction: this is neither pure coordination (rope) nor pure extraction (snare). It is genuinely both. The perspectival gap is resolved by noting that different agents experience different ratios of coordination to extraction: incumbents experience high coordination/low extraction (rope from their viewpoint); powerless rural voters experience minimal coordination/high extraction (snare from their viewpoint); moderate opposition actors experience balanced mixed (tangled rope). The mountain classification from civilizational perspective is rejected as a false summit — the one-term limit appears unchangeable only because both dominant coalitions currently prefer its stability. It is contingent on this political equilibrium, not immutable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_revision_risk,
    'Under what conditions would a dominant coalition attempt constitutional revision to allow presidential reelection?',
    'Historical precedent from Peru (1992), Venezuela (1999, 2009), Bolivia (2009). Monitoring of political concentration — if one coalition achieves supermajority control of legislature, revision pressure increases. Polling on public appetite for reelection.',
    'If revision probability > 0.15: the mountain classification is false; one-term limit is contingent on current power balance, not inherent. If probability < 0.05: mountain holds — limit is durable across foreseeable political scenarios.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_revision_risk, empirical, 'Constitutional revision probability as function of political concentration').

omega_variable(
    regional_extraction_opacity,
    'In regions without independent verification infrastructure, what fraction of extractive activity (vote trading, cacique leverage, ballot fraud) remains unobserved and unquantified?',
    'Post-election audits in high-marginalization departments; comparison of pre-announced voting intentions vs official results; qualitative interviews with regional observers and voters. Measurement of vote-buyer effectiveness (how many votes can be purchased per unit expenditure in different regions).',
    'If opacity > 0.60: suppression index may be understated (should be 0.75+). Extraction may be snare-level (not tangled rope) for rural victims. If opacity < 0.30: current tangled-rope classification from moderate regional perspective is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_extraction_opacity, empirical, 'Opacity of regional extraction mechanisms').

omega_variable(
    observer_effectiveness_paradox,
    'Do international observers reduce extraction by increasing verification cost, or do they provide symbolic legitimation that enables extraction by reducing domestic scrutiny?',
    'Comparison of vote irregularities in observed vs unobserved elections. Analysis of electoral disputes: are observers'' presence correlated with fewer or more fraud allegations? Exit surveys in regions with vs without observer presence.',
    'If observers reduce extraction: scaffold classification confirmed. If observers enable extraction through legitimation: scaffold is theater-heavy false classification, extraction persists underneath observer protocols, true classification for powerless agents should be snare (not tangled rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(observer_effectiveness_paradox, empirical, 'Whether observers reduce or legitimize extraction').

omega_variable(
    successor_autonomy,
    'To what degree is the designated successor (chosen by incumbent coalition) bound to fulfill commitments made to coalition allies, vs. exercising genuine presidential autonomy once in office?',
    'Historical analysis of Colombian successions: do new presidents execute predecessor''s coalition agreements? Defection rate and consequences. Comparison with peer democracies (Mexico, Chile) with similar constraints.',
    'If successor autonomy high: one-term limit genuinely blocks indefinite extraction accumulation (true coordinating function). If autonomy low: successor is puppet, extraction continues through alternate mechanism (limit is fake — piton rather than mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(successor_autonomy, empirical, 'Successor autonomy relative to incumbent coalition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(columbia_2026_elections, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(col2026_tr_t0, columbia_2026_elections, theater_ratio, 0, 0.48).
narrative_ontology:measurement(col2026_tr_t6, columbia_2026_elections, theater_ratio, 6, 0.58).
narrative_ontology:measurement(col2026_tr_t12, columbia_2026_elections, theater_ratio, 12, 0.62).

% Extraction over time
narrative_ontology:measurement(col2026_be_t0, columbia_2026_elections, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(col2026_be_t6, columbia_2026_elections, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(col2026_be_t12, columbia_2026_elections, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(columbia_2026_elections, enforcement_mechanism).
narrative_ontology:affects_constraint(columbia_2026_elections, colombia_rural_vote_buying).
narrative_ontology:affects_constraint(columbia_2026_elections, colombia_campaign_finance_asymmetry).
narrative_ontology:affects_constraint(columbia_2026_elections, colombia_media_access_disparity).

% DUAL FORMULATION NOTE:
% The 2026 election structure is upstream of three subordinate constraints: (1) rural vote-buying networks that operationalize extraction in local contexts, (2) campaign finance asymmetry that manifests incumbent advantage, (3) media access disparity that skews information flows. Each subordinate constraint has higher ε values reflecting more concentrated extraction at the implementation level. The election structure itself has moderate ε (0.52) because it integrates both coordination (succession mechanism) and extraction (incumbent advantage). Downstream constraints have ε > 0.60, indicating that the structural vulnerabilities of the election system are exploited more severely at regional implementation levels.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
