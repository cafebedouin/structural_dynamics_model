% ============================================================================
% CONSTRAINT STORY: sotu_1997_clinton_campaign_finance_reform_soft_money_ban
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1997_clinton_campaign_finance_reform_soft_money_ban, []).

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
 *   constraint_id: sotu_1997_clinton_campaign_finance_reform_soft_money_ban
 *   human_readable: McCain-Feingold Campaign Finance Reform: Soft Money Ban and Contribution Limits
 *   domain: governance/electoral_regulation
 *
 * SUMMARY:
 *   The McCain-Feingold Bipartisan Campaign Reform Act (BCRA, enacted 2002,
 *   proposed framework articulated 1997) establishes a structural constraint
 *   on campaign finance through soft-money restrictions, corporate
 *   contribution prohibitions, and noncitizen donation bans. The constraint
 *   attempts to address electoral inequality by blocking unlimited corporate
 *   and aggregate individual contributions to political parties (soft money)
 *   while maintaining hard-money contribution limits to candidates. The
 *   constraint's declared purpose is to reduce incumbent fundraising
 *   advantage, equalize challenger access to capital, and prevent
 *   interest-group capture of elected officials. Structurally, it creates
 *   formal parity in campaign finance architecture while informal extraction
 *   mechanisms (527 organizations, dark money vehicles, trade association
 *   bundling, executive bundling) have partially restored the asymmetries the
 *   law intended to eliminate. The constraint exhibits the full spectrum of
 *   DR types depending on observer position: challengers experience residual
 *   snare dynamics (formal parity but persistent suppression); organized
 *   challenger movements experience genuine tangled rope (coordination
 *   benefit + extraction cost); the FEC experiences pure coordination (rope);
 *   incumbents experience tangled rope (coordination benefit + severe
 *   extraction); corporations experience snare (trapped without workarounds);
 *   reform coalitions experience scaffold (temporary intervention with
 *   constitutional sunset); the civilizational analytical observer risks
 *   naturalizing political money dynamics as immutable law (false summit
 *   mountain).
 *
 * KEY AGENTS:
 *   - Challenger Candidates: Primary beneficiary (powerless/trapped) — appear to benefit from soft-money restrictions but remain suppressed by residual structural barriers and incumbent organizational networks
 *   - Incumbent Fundraisers: Primary victim (powerful/constrained) — experience extraction through loss of soft-money channels; retain constrained exit through dark-money workarounds
 *   - Organized Challenger Movement: Secondary beneficiary (organized/constrained) — experience genuine coordination gain from small-donor infrastructure enabled by soft-money restrictions; also experience extraction through compliance costs
 *   - Federal Election Commission: Institutional coordinator (institutional/arbitrage) — experience pure coordination function through rule clarification and enforcement standardization
 *   - Corporate Political Speech: Victim (institutional/trapped) — face maximal suppression through prohibition of direct corporate contributions and soft-money corporate donations to parties
 *   - Campaign Finance Reform Coalition: Organized advocates (organized/mobile) — see the constraint as temporary scaffold with constitutional sunset; have mobile exit to alternative models
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing money-in-politics as immutable feature of electoral systems rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, 0.52).
domain_priors:suppression_score(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, 0.65).
domain_priors:theater_ratio(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, tangled_rope).
narrative_ontology:human_readable(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, "McCain-Feingold Campaign Finance Reform: Soft Money Ban and Contribution Limits").
narrative_ontology:topic_domain(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, "governance/electoral_regulation").

domain_priors:requires_active_enforcement(sotu_1997_clinton_campaign_finance_reform_soft_money_ban).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, challenger_candidates).
narrative_ontology:constraint_beneficiary(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, electoral_parity_advocates).
narrative_ontology:constraint_beneficiary(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, small_donors).
narrative_ontology:constraint_victim(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, incumbent_fundraising_capacity).
narrative_ontology:constraint_victim(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, corporate_political_speech).
narrative_ontology:constraint_victim(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, wealthy_individual_aggregators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHALLENGER CANDIDATE (SNARE) — Appears to benefit from soft-money restrictions, but remains trapped within suppressive structures. Incumbent fundraising networks (bundled corporate executives, union leadership, industry PACs) operate despite restrictions through workarounds (527 organizations, dark money conduits, grasstops campaigns). Challenger faces both explicit legal restrictions and the residual incumbent structural advantage. The constraint creates formal parity while informal extraction mechanisms persist. Suppression remains high because meaningful campaign requires exponentially larger personal fundraising effort from challengers with no organizational platform.
constraint_indexing:constraint_classification(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED CHALLENGER MOVEMENT (TANGLED ROPE) — Experienced coordination benefit from leveling: small-donor aggregation infrastructure (grassroots fundraising, volunteer networks, digital aggregation) becomes strategically viable for the first time when incumbent bundling is legally constrained. Also experiences extraction: legal compliance costs, donation tracking overhead, and periodic challenge to the constitutionality of the restrictions themselves create uncertainty and constraint on fundraising strategy. Coordination function is genuine (enables distributed small-donor model); extraction is asymmetric (enforcement burden falls heaviest on small-donor aggregators with fewest compliance resources).
constraint_indexing:constraint_classification(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL ELECTION COMMISSION (ROPE) — Experiences the soft-money ban as pure coordination mechanism: clarifies enforcement jurisdiction, provides bright-line rules (hard money vs soft money distinction), and establishes shared reporting standards. FEC coordination function is legitimate — candidates and donors need legal clarity. FEC has arbitrage option (can litigate boundaries, update rules, adapt to challenge); experiences no meaningful extraction from the constraint itself. Acts as referee implementing coordination rules rather than as extracted agent.
constraint_indexing:constraint_classification(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT FUNDRAISER (TANGLED ROPE) — Experiences genuine coordination benefit: soft-money ban clarifies fundraising architecture and creates consistent rules across all candidates (functional coordination). Also experiences severe extraction: prior soft-money access channels (unlimited corporate contributions to leadership PACs, unlimited individual aggregation to party committees, industry bundling networks) are now legally blocked. Incumbent has constrained exit — can attempt workarounds (527 organizations, dark money vehicles, trade association giving) but these incur legal risk and operational friction. Both coordination and extraction are substantial and asymmetric.
constraint_indexing:constraint_classification(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CORPORATE POLITICAL SPEECH (SNARE) — Trapped within a restriction that eliminates direct corporate contribution channels (hard-money corporate donations remain prohibited; soft-money corporate contributions to parties are banned). Corporation has no structural exit: cannot donate directly, cannot aggregate donations, cannot legally fund political speech through traditional channels. Suppression is maximal — the constraint's entire purpose is to prevent this agent's political participation. However, corporations retain adaptive capacity through 501(c)(4) dark money organizations, trade association bundling, and executive bundling — the snare is real but not impermeable. Classification reflects the constraint's immediate structural position (prohibition of direct corporate participation), not the broader dark-money ecosystem that has partially restored extraction capacity.
constraint_indexing:constraint_classification(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: CAMPAIGN FINANCE REFORM COALITION (SCAFFOLD) — Organized actors (Common Cause, League of Women Voters, electoral parity advocates) see the soft-money ban as temporary structural intervention with a sunset clause embedded in constitutional law: Buckley v. Valeo (1976) established that spending limits require the reform to survive strict scrutiny, and as campaign finance jurisprudence evolves, the sunset becomes inevitable. The coalition has mobile exit (can shift to public financing advocacy, dark-money transparency rules, alternative models) and genuine sunset logic: the McCain-Feingold framework has already been partially struck down (Citizens United v. FEC, 2010), and the coordination function (electoral parity) is being addressed through alternative mechanisms (state-level public financing, ranked-choice voting). Theater is moderate — reform rhetoric is significant but functional gains are partial.
constraint_indexing:constraint_classification(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, the soft-money ban appears to reflect an immutable constraint: political speech requires funding, funding creates asymmetry, and asymmetry creates access inequality. This perspective naturalizes the dynamics as inherent to electoral competition itself — money always flows to power, restrictions always develop workarounds, and the constraint landscape is unchangeable. However, the structural data reveals this as a false summit: identifiable beneficiaries (challengers, small donors) and victims (incumbents, corporate speakers) exist, indicating that the constraint is constructed, not natural. The 'money always wins' narrative is itself extractive framing.
constraint_indexing:constraint_classification(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1997_clinton_campaign_finance_reform_soft_money_ban_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sotu_1997_clinton_campaign_finance_reform_soft_money_ban_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint represents substantial extraction from incumbents and corporate speakers through hard-money contribution limits and soft-money prohibitions. However, the extractiveness is not severe (0.68 at T=0) because meaningful workarounds exist (527 organizations, dark money, trade association bundling) that partially restore extraction capacity, and the constraint's constitutional foundation is contested. By T=4 (2002, post-BCRA enactment), extractiveness drops to 0.55 as dark-money mechanisms mature; by T=8 (2008), it stabilizes at 0.52 as the ecosystem reaches equilibrium between legal restrictions and functional workarounds. Suppression (0.65): Moderate-high. Suppression operates along multiple axes: (1) legal prohibition (hard money contribution limits, soft-money bans, corporate donation restrictions) create formal barriers; (2) compliance costs and legal uncertainty create operational friction; (3) incumbent organizational networks and bundling infrastructure persist despite restrictions, maintaining informal suppression of challengers; (4) the disclosure requirements themselves become suppressive (donations become publicly trackable, reducing donor privacy). Suppression does not decline significantly over the interval because workarounds do not eliminate the formal restrictions. Theater ratio (0.58): Moderate. The constraint has significant performative content: the McCain-Feingold narrative emphasizes electoral parity and corruption prevention while dark-money mechanisms functionally restore asymmetries. By T=4, theater rises to 0.58 as the gap between reform rhetoric and functional outcomes becomes visible. The theater is not extreme (not 0.72+) because the constraint does produce measurable real effects (disclosure requirements, fundraising friction, campaign strategy shifts).
 *
 * PERSPECTIVAL GAP:
 *   The incumbent and challenger perspectives invert: incumbent sees extraction (tangled rope, χ high), challenger sees persistent suppression despite formal parity (snare, χ high from different direction). The FEC perspective is orthogonal to both (rope, χ ≈ 0). The reform coalition perspective has temporal structure — scaffold implies χ ≤ 0.30 at present but understanding that the constraint's time horizon is bounded. The mountain perspective is a false summit: the 'money always wins' narrative naturalizes what is actually a contingent institutional arrangement (soft-money channels, incumbent networks, corporate bundling) as immutable law. The perspectival divergence is not reducible to measurement error or insufficient clarity — it reflects the constraint's actual structure: it redistributes political access through legal mechanism while informal extraction mechanisms partially restore the prior asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from structural relationship to the constraint. Incumbent fundraisers are victims with constrained exit (high d, ~0.60-0.70) — can attempt workarounds but face legal risk; derive high f(d) → high experienced extractiveness. Challengers are nominal beneficiaries but remain trapped by informal structures (mixed d, ~0.45-0.55) — benefit formally from soft-money restrictions but cannot exercise the benefit due to persistent suppression; derive moderate f(d) → moderate experienced extraction persists. Organized challenger movements are mixed beneficiaries-victims with constrained exit (d ~0.55) — coordinate small-donor infrastructure (benefit) while managing compliance costs (extraction); derive moderate f(d). FEC is pure coordinator with arbitrage option (d ~0.35) — no meaningful extraction, regulation is their function; derive low f(d) → negative or near-zero χ. Corporations are victims with trapped exit (high d, ~0.85-0.95) — cannot exit the prohibition without violating law; derive very high f(d) → high suppressive force. Reform coalition are beneficiaries with mobile exit (low d, ~0.20-0.30) — see the constraint as enabling alternative models; derive low/negative f(d) → negative or near-zero χ. Scope modifier σ(S) = 1.0 (national scope, standard modifier).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY AVOIDANCE: This constraint resolves potential mandatrophy (confusion between pure extraction and mixed extraction-coordination) by explicitly declaring dual function. McCain-Feingold HAS a genuine coordination function (clarifying campaign finance rules, establishing consistent reporting standards, creating parity architecture) AND asymmetric extraction (blocking incumbent soft-money channels, suppressing corporate speech, creating compliance burdens). The tangled-rope classification is correct because both elements are structural — the coordination function isn't theater, and the extraction isn't incidental. The measured decline in extractiveness (0.68 → 0.52) reflects equilibrium-seeking as dark-money workarounds develop, not success of the constraint in eliminating extraction. The theater ratio (0.58) indicates meaningful gap between reform rhetoric and functional outcomes, but the gap is not extreme because some real effects (disclosure, fundraising friction) persist. The constraint avoids mandatrophy by refusing false purity: it is not 'a reform that works' (would be rope) nor 'pure incumbent protection theater' (would be piton), but genuinely both coordination mechanism AND extraction redistribution mechanism operating simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    soft_money_workaround_sufficiency,
    'Do workaround mechanisms (527 organizations, dark money conduits, trade association bundling) restore the soft-money extraction advantage to incumbents, functionally negating the ban?',
    'Comparative analysis of incumbent fundraising timelines: pre-McCain-Feingold soft-money flows vs. post-2004 dark-money flows (post-BCRA, pre-Citizens United). If dark money achieves 70%+ of prior soft-money volumes, workarounds are functionally sufficient; ban is Piton. If dark money achieves <50% of prior volumes, ban retains extractive force; ban is Snare/Tangled Rope.',
    'If sufficient: constraint reclassifies toward Piton (degraded theater, inertial enforcement). If insufficient: constraint remains Snare/Tangled Rope (genuine extraction mechanism). Structural dependency: campaign finance ecosystem''s adaptability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(soft_money_workaround_sufficiency, empirical, 'Whether dark money and workarounds functionally restore soft-money advantage').

omega_variable(
    challenger_mobilization_threshold,
    'What proportion of defeated challengers would have won if soft-money restrictions had been in place during their campaigns? Does the constraint actually enable electoral competition, or merely reduce the financial gap without changing outcomes?',
    'Counterfactual modeling: compare actual incumbent margins to modeled margins under hard-money-only constraints. Historical races where fundraising gap was 5:1 vs 3:1; correlation between fundraising reduction and margin reduction.',
    'If margin reduction > 5%: constraint has genuine electoral effect (Rope/Tangled Rope confirmed). If margin reduction < 2%: constraint is primarily theatrical (Piton). If no effect on outcomes but changes campaign character (grassroots mobilization, small-donor engagement): constraint is Scaffold with sunset (enabling alternative models).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(challenger_mobilization_threshold, empirical, 'Whether soft-money restrictions enable challenger electoral success').

omega_variable(
    constitutional_inevitability_of_sunset,
    'Is the McCain-Feingold framework''s eventual constitutional strike-down inevitable, or is the Scaffold sunset clause contingent on political choices?',
    'Constitutional law trajectory analysis: Buckley v. Valeo (1976) established strict scrutiny for campaign spending limits; Citizens United (2010) struck down key BCRA provisions. If precedent continues to narrow campaign finance regulation, sunset is constitutional inevitability (structural Scaffold). If courts halt or reverse (via new appointments, doctrinal shift), sunset is contingent (Tangled Rope without terminal time horizon).',
    'If inevitable: Scaffold classification is robust (coordination mechanism with guaranteed sunset). If contingent: Scaffold is aspirational (Tangled Rope with reform coalition wishcasting). Affects strategic analysis of whether the constraint solves the coordination problem or merely delays extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_inevitability_of_sunset, conceptual, 'Whether BCRA''s sunset via constitutional litigation is structural or contingent').

omega_variable(
    small_donor_infrastructure_dependency,
    'Does the soft-money ban actually enable small-donor fundraising, or does it merely create space for small-donor infrastructure that would have developed regardless of the ban?',
    'Comparative analysis: small-donor aggregation platforms (ActBlue founded 2004, post-BCRA) vs pre-BCRA donor democratization efforts. If small-donor infrastructure emerges specifically in response to incumbent soft-money blocking, causation is established (constraint enables coordination). If small-donor infrastructure emerges regardless (parallel development in political technology), constraint is correlative rather than causal.',
    'If enabling: constraint functions as Rope/Tangled Rope (genuine coordination mechanism). If correlative: constraint is theater masking technological change (Piton). Strategic consequence: understanding whether ban drove democratization or merely coincided with it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(small_donor_infrastructure_dependency, empirical, 'Whether soft-money ban causally enables small-donor infrastructure').

omega_variable(
    beneficiary_asymmetry_definition,
    'Do ''challengers'' and ''electoral parity'' constitute a real beneficiary group, or is the beneficiary designation aspirational (presuming the constraint *should* help challengers without evidence it *does*)?',
    'Empirical tracking: post-BCRA challenger success rates vs pre-BCRA baseline. If challenger win-rate improves after BCRA implementation (controlling for macro political conditions, candidate quality), beneficiary status is confirmed. If win-rates remain unchanged, beneficiary status is rhetorical (constraint is designed for challengers but doesn''t functionally benefit them).',
    'If confirmed: beneficiary/victim declarations are structural (directionality derives from real extraction flow). If rhetorical: declarations are narrative framing; true beneficiaries may be organized reform advocates rather than challengers. Affects classification: if true beneficiaries are organized advocates, constraint may be Rope with theater rather than Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_asymmetry_definition, empirical, 'Whether challengers are genuine or aspirational beneficiaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cfr_theater_ratio_1997, sotu_1997_clinton_campaign_finance_reform_soft_money_ban, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cfr_theater_ratio_2002_post_bcra, sotu_1997_clinton_campaign_finance_reform_soft_money_ban, theater_ratio, 4, 0.58).
narrative_ontology:measurement(cfr_theater_ratio_2008_pre_citizens_united, sotu_1997_clinton_campaign_finance_reform_soft_money_ban, theater_ratio, 8, 0.58).

% Extraction over time
narrative_ontology:measurement(cfr_base_extractiveness_1997, sotu_1997_clinton_campaign_finance_reform_soft_money_ban, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(cfr_base_extractiveness_2002_post_bcra, sotu_1997_clinton_campaign_finance_reform_soft_money_ban, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(cfr_base_extractiveness_2008_pre_citizens_united, sotu_1997_clinton_campaign_finance_reform_soft_money_ban, base_extractiveness, 8, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, resource_allocation).
narrative_ontology:affects_constraint(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, interest_group_access_capture).
narrative_ontology:affects_constraint(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, campaign_spending_asymmetry).
narrative_ontology:affects_constraint(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, dark_money_ecosystem_growth).

% DUAL FORMULATION NOTE:
% McCain-Feingold soft-money ban is structurally decomposable into coordination (rule clarification, parity architecture) and extraction (incumbent channel restriction, corporate suppression, compliance burden). The two functions are simultaneous and irreducible. Historical precedent: Buckley v. Valeo (1976) struck down spending limits while upholding contribution limits, establishing the constitutional substrate within which McCain-Feingold operates and eventually constrains its sunset (Citizens United, 2010). Dark-money ecosystem growth is downstream consequence: as soft-money channels close, capital flows to 501(c)(4) organizations, creating new extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, powerful, 0.65).
constraint_indexing:directionality_override(sotu_1997_clinton_campaign_finance_reform_soft_money_ban, powerless, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
