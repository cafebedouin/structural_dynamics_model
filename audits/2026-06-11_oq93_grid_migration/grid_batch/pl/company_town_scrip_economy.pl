% ============================================================================
% CONSTRAINT STORY: company_town_scrip_economy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_company_town_scrip_economy, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: company_town_scrip_economy
 *   human_readable: Company Town Scrip Economy: Wage Monetization and Class-Level Exit Collapse
 *   domain: labor/economic_extraction
 *
 * SUMMARY:
 *   A company town moves wages entirely into scrip (non-convertible company
 *   currency) and establishes a monopoly merchant system through the company
 *   store. Individual wage workers face rising stakes as nominal wages rise
 *   but purchasing power declines through scrip exchange rates and merchant
 *   price markups. Simultaneously, class-level alternatives are
 *   systematically dismantled: competing employers are excluded through
 *   territorial agreements or buyouts, independent merchants are undercut and
 *   driven out, unions are prohibited by employment contract, and state
 *   regulation codifies the scrip system. The constraint exhibits a stark
 *   coercion-grid divergence: individual-level suppression and stakes
 *   inflation rise dramatically (workers are isolated and increasingly
 *   desperate), while class-level resistance capacity declines (the scale at
 *   which workers could coordinate their opposition is eliminated by design).
 *   The scrip system is presented as a coordination mechanism (solving
 *   currency scarcity and working-capital logistics), but structural analysis
 *   reveals it as extraction with coordination theater: geographic isolation
 *   is real but is deepened by institutional choices; the merchant monopoly
 *   is justified by efficiency but is maintained by exclusionary practices;
 *   wage reductions are framed as necessary adjustments but accumulate as
 *   transfer of worker earnings to company ownership through the
 *   exchange-rate mechanism.
 *
 * KEY AGENTS:
 *   - wage_workers: Powerless (immediate), trapped by survival needs and geographic isolation; identity-locked (generational) through multi-generational company-town inhabitation
 *   - company_ownership: Institutional beneficiary (arbitrage exit), experiences scrip as coordination; captures accumulated extraction through exchange rates and merchant control
 *   - monopoly_merchant_class: Moderate institutional power (constrained exit), benefits from scrip adoption but is structured to extract on company's behalf; some independent merchants are driven out (victims)
 *   - state_authority: Institutional regulatory actor (mobile exit), maintains scrip-enabling legal framework long after coordination rationale atrophies; sees own regulations as degraded theater
 *   - regional_labor_movement: Organized agent (constrained exit), organizes against scrip but organizing is criminalized; class-level alternative is systematically eliminated by company design
 *   - independent_employment_alternatives: Structural victim (absent from town), systematically excluded through territorial agreements, buyouts, and competitor suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(company_town_scrip_economy, 0.78).
domain_priors:suppression_score(company_town_scrip_economy, 0.82).
domain_priors:theater_ratio(company_town_scrip_economy, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(company_town_scrip_economy, extractiveness, 0.78).
narrative_ontology:constraint_metric(company_town_scrip_economy, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(company_town_scrip_economy, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(company_town_scrip_economy, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(company_town_scrip_economy, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(company_town_scrip_economy, snare).
narrative_ontology:human_readable(company_town_scrip_economy, "Company Town Scrip Economy: Wage Monetization and Class-Level Exit Collapse").
narrative_ontology:topic_domain(company_town_scrip_economy, "labor/economic_extraction").

domain_priors:requires_active_enforcement(company_town_scrip_economy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(company_town_scrip_economy, company_ownership).
narrative_ontology:constraint_beneficiary(company_town_scrip_economy, monopoly_merchant_class).
narrative_ontology:constraint_victim(company_town_scrip_economy, wage_workers).
narrative_ontology:constraint_victim(company_town_scrip_economy, regional_alternative_employment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(company_town_scrip_economy, company_store_merchants).
narrative_ontology:constraint_victim(company_town_scrip_economy, company_store_merchants).
narrative_ontology:constraint_victim(company_town_scrip_economy, regional_labor_movement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exchange labor for scrip wages; purchase goods at company store using scrip at rates set by company; accumulate debt through mortgages (company housing), store credit, and fines. Cannot refuse scrip, cannot shop elsewhere, cannot leave without forfeiting housing and accumulated credit. Survival depends entirely on continued company employment at wages the company sets. Nominal wages rise but scrip exchange rates decline, producing net wage loss over time. Family identity is constituted through generations of company employment; children expect to work for the company.
narrative_ontology:constraint_stakeholder(company_town_scrip_economy, wage_workers, payer,
    powerless, biographical, trapped, local).

% Sets wage rates, controls scrip issue, controls merchant pricing through company store, controls scrip-to-cash exchange rates. Manages labor force through debt relationships (mortgages, store credit). Can relocate capital and management at any time; can abandon the town if extraction opportunities decline. Receives accumulated transfer from workers through the mechanism of scrip exchange-rate differential: workers earn scrip worth less at redemption than nominal value suggests.
narrative_ontology:constraint_stakeholder(company_town_scrip_economy, company_ownership, agenda_setter,
    institutional, immediate, arbitrage, global).

% Operate stores using company capital; set prices within parameters set by company; receive guaranteed customer base (workers have no alternative). Benefit from lack of competition (independent merchants are excluded). Simultaneously, merchants are constrained by the company (cannot raise prices unilaterally, cannot refuse scrip, operate on company property). Some independent merchants attempted to compete but were undercut and driven out; remaining merchants are integrated into company structure.
narrative_ontology:constraint_stakeholder(company_town_scrip_economy, company_store_merchants, beneficiary,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(company_town_scrip_economy, company_store_merchants, payer).

% Maintains legal framework treating scrip as valid wage payment; codifies company-store monopoly protections; enforces company employment contracts that prohibit union organizing; prosecutes and deports union organizers; provides company police with legal authority to suppress dissent. Receives lobbying pressure from company ownership. The regulatory role persists long after the coordination rationale (currency scarcity) is solved by state banking. Regulation is maintained through institutional inertia and political capture.
narrative_ontology:constraint_stakeholder(company_town_scrip_economy, state_regulatory_authority, agenda_setter,
    institutional, generational, mobile, national).

% Organizes workers across multiple company towns; builds regional union structures to coordinate labor power against company exploitation. Faces violent suppression: organizers are fired, blacklisted, arrested, and sometimes physically assaulted by company police. Union meetings are infiltrated; union literature is confiscated. Regional coordination is the scale at which class-level resistance is possible, but the company systematically prevents regional alternative employment (buys out or excludes competing employers) so that fired organizers cannot find work elsewhere. The labor movement's capacity to mount sustained regional action is constrained by the geographic isolation of the company towns and the company's control over all employment in the region.
narrative_ontology:constraint_stakeholder(company_town_scrip_economy, regional_labor_movement, payer,
    organized, generational, constrained, regional).

% Would provide competitive employment alternatives that would allow workers to exit the company town or increase negotiating power. Systematically prevented from entering the region through company territorial agreements with neighboring firms, acquisition of potential competitors, and political pressure on state authority to exclude them. The absence of this structural alternative is the single most important factor enabling the snare classification — if workers could find alternative employment within reasonable distance, the trap would collapse immediately.
narrative_ontology:constraint_stakeholder(company_town_scrip_economy, independent_alternative_employers, excluded,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_non_agent(company_town_scrip_economy, independent_alternative_employers).

% Not yet born, but structurally shaped by the company town: education provided by company (teaching company loyalty); social identity constituted through company association; geographic knowledge limited to company town geography (limited understanding of alternatives exists); inheritance of family debt (parents' mortgages may be transferred). Workers' children expect to work for the company as their parents did; exit imagination is preemptively foreclosed before birth.
narrative_ontology:constraint_stakeholder(company_town_scrip_economy, workers_future_generations, excluded,
    powerless, generational, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(company_town_scrip_economy, company_ownership).
narrative_ontology:fixing_cost_class(company_town_scrip_economy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The scrip system solves a genuine coordination problem at its founding: in the 1880–1920 era, remote company towns face currency scarcity (hard money is scarce in resource-extraction regions) and working-capital liquidity (companies need to distribute wages without precious-metal transfer, and need to manage merchant credit). Scrip enables both parties to operate without precious metals and provides a local currency unit. The company store solves a merchant coordination problem: workers need access to goods, and independent merchants cannot profitably serve small remote populations; the company store provides guaranteed inventory and pricing stability.
% TRANSFER_FUNCTION: The arrangement transfers worker earnings to company ownership through four mechanisms: (1) scrip exchange-rate differential (workers earn scrip worth less at redemption than nominal value); (2) company-store merchant markups (prices are higher than would prevail in competitive markets); (3) debt relationships (mortgages, store credit accumulate at terms favorable to company); (4) wage suppression (company controls wage rates without market competition). The transfer also flows from independent merchants to company store (competitive merchants are undercut and driven out, or absorbed into company structure). The transfer flows from workers to state authority through regulatory codification (state receives political benefits from codifying company employment practices, gaining authority and legitimacy through 'stabilizing' labor markets).
% ABSENT_VOICES: Independent-goods merchants (competitors to company store who are excluded or driven out before they can participate in the arrangement); workers' descendants (future workers whose alternatives are preemptively foreclosed); neighboring-region alternative employers (systematically excluded from the region through territorial agreements); union organizers (violently suppressed before they can voice opposition in company forums). The constraint's structure intentionally silences: if workers could hear from union organizers regularly, if competing merchants could operate openly, if workers' children had genuine knowledge of alternatives, if neighboring employers could recruit, the consent narrative would collapse immediately.
% DISAPPEARANCE_RATIONALE: If the scrip system disappeared overnight, the regional labor market would rearrange: (1) workers would seek alternative employment in neighboring regions now that the geographic trap is removed; (2) independent merchants would enter the market attracted by the freed customer base; (3) housing markets would develop as company housing loses its monopoly; (4) the state would face loss of a regulatory lever (the scrip framework) and would need to renegotiate labor relations. The company's operational efficiency would decline (working-capital costs would rise; workforce management would become harder without debt leverage). The world would not settle into a new stable equilibrium immediately — regional unemployment would likely spike as workers leave faster than alternative employers can absorb them — but the previous stable configuration (workers trapped in company town through scrip and merchant monopoly) would be permanently destroyed.
% FOUNDING_PROBLEM: In remote resource-extraction company towns (1880–1920s), two genuine problems exist: (1) Currency scarcity — hard money is difficult to transport to remote locations and is hoarded for external transactions; (2) Merchant availability — independent merchants cannot profitably serve small, isolated populations. The company town solves both by issuing scrip as local currency and establishing a company store as the sole merchant. These are real coordination problems with no obvious market solution at the time.
% FOUNDING_PROBLEM_CORROBORATION: By mid-20th century, state banking infrastructure solves currency scarcity (state-issued fiat money circulates freely; banks operate in remote regions). Public transportation and mail systems reduce merchant scarcity (goods can be imported from regional centers; independent small merchants can serve towns using transportation networks). The founding problems are acknowledged as solved by contemporary economic historians, by business schools that teach company-town systems as historical artifacts, and by workers themselves (workers' correspondence and union organizer testimony from 1930s onward describes the system as obsolete for coordination, perpetuated only for extraction). The only voices that continue to justify the system on coordination grounds are company ownership and captured state regulators — both benefit from the extraction mechanism.
narrative_ontology:disappearance_verdict(company_town_scrip_economy, world_rearranges).
narrative_ontology:founding_problem_status(company_town_scrip_economy, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE WORKER (SNARE) — Trapped by immediate survival needs and geographic isolation. Scrip is legally enforced as wages; alternatives have been systematically eliminated (competing employers excluded, independent merchants undercut). The worker perceives the constraint as unchangeable within their lifetime. Maximum experienced extraction with zero exit capacity.
constraint_indexing:constraint_classification(company_town_scrip_economy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: WAGE WORKER'S FAMILY (SNARE / IDENTITY_LOCKED) — Trapped not only by immediate economic dependency but by identity fusion: multi-generational inhabitation of the company town has constituted the worker's and their family's self-concept around the role of 'company employee.' Exit would require abandoning family identity and community belonging. The identity lock persists even where structural escape paths exist — the worker cannot psychologically exercise constrained-level exit capacity because their identity is the company town itself. This perspective reveals the binding mechanism is cognitive capture, not mere economic coercion.
constraint_indexing:constraint_classification(company_town_scrip_economy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 3: INDEPENDENT MERCHANTS (TANGLED_ROPE) — Moderate power actors at regional scale. They coordinate through scrip acceptance (genuine coordination function: scrip enables local commerce without precious metal scarcity). Simultaneously, the merchant class is extracted from through the company's price-setting on scrip redemption and through exclusion of competitors. Some independent merchants benefit from scrip adoption; others are undercut and driven out. This perspective exhibits the hybrid structure: real coordination (scrip as currency substitute) layered with asymmetric extraction (the company sets the exchange rate and controls merchant access).
constraint_indexing:constraint_classification(company_town_scrip_economy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: COMPANY MANAGEMENT (ROPE) — Institutional actors with arbitrage options (capital mobility, management relocation). They experience the scrip system as pure coordination: centralizing wage payment and merchant relationships solves logistics and working-capital problems. The labor force is geographically captive and operationally simplified. Extraction flows toward this seat; management perceives the system as low-friction coordination, not as extraction. The classification is rope because the constraint genuinely solves coordination problems the company faces.
constraint_indexing:constraint_classification(company_town_scrip_economy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 5: STATE AUTHORITY (PITON) — The state maintains scrip-enabling regulations and labor laws (no-strike agreements, scrip legal tender provisions, company-store monopoly protections) long after the genuine coordination problem has atrophied. Modern alternatives (banking infrastructure, state-issued currency, labor mobility) eliminate the coordination rationale, but the regulatory theater persists through institutional inertia and lobbying. State authority sees its own regulations as degraded — a vestigial framework maintained for political rather than functional reasons. Theater ratio is moderate because enforcement is real (arrests for scrip-rejection, deportations of union organizers) but the underlying function (lack of banking alternatives) is obsolete.
constraint_indexing:constraint_classification(company_town_scrip_economy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: REGIONAL LABOR MOVEMENT (SNARE) — Organized agents with constrained exit (organizing is legal but faces violent suppression, infiltration, and firing of participants). The movement sees the scrip system as pure extraction dressed in coordination language. Unlike individual workers, the movement has capacity to name the mechanism and mount resistance. However, the company town structure systematically eliminates the scale at which the movement operates — regional organizing cannot match local captivity. The movement's perspective reveals that the constraint is snare, not rope: the coordination story is cover, and persistence depends on coercion.
constraint_indexing:constraint_classification(company_town_scrip_economy, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN CLAIM / FALSE SUMMIT RISK) — From a civilizational perspective, geographic isolation and wage-labor immobility appear to be natural constraints on labor market function — 'workers in remote areas have limited options by geography alone.' This framing naturalizes the company town as an inevitable response to spatial constraints. However, the structural data reveals this as a false summit: geographic isolation is real, but the scrip system, merchant exclusions, and regulatory protections are human choices that *deepen* immobility. The mountain classification confuses natural constraints (distance) with constructed constraints (monopoly enforcement). The engine's false-summit detector should flag this.
constraint_indexing:constraint_classification(company_town_scrip_economy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(company_town_scrip_economy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(company_town_scrip_economy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(company_town_scrip_economy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(company_town_scrip_economy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(company_town_scrip_economy, TR),
    TR >= 0.70.

:- end_tests(company_town_scrip_economy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High and rising over the interval. At t0=0, scrip adoption is partial and extraction is masked as efficiency (0.55). By t30=30, the system is fully mature and extraction is clear (0.80): the company controls wage payment, merchant pricing, and scrip exchange rates. The extraction accumulates because workers' nominal wages rise (theater) while real purchasing power declines (actual extraction flow to company through exchange-rate differential). Theater ratio (0.45, rising to 0.48): Moderate because the coordination story is partially true (scrip does solve logistics problems) but is used to justify much larger extraction flows. Theater is lower than a pure piton because enforcement is active and visible (not just institutional inertia); the theater is the 'efficiency' framing around genuine coercion. Suppression (0.82, built from suppression_requirement trajectory rising from 0.65 to 0.85): High and intensifying. The company eliminates class-level alternatives through exclusionary practices (not merely competition); union prohibition is enforced through employment contracts and company-police violence; state authority codifies and defends the scrip system through legal frameworks. Accessibility collapse (0.88): Very high at end of interval. Individual escape routes collapse as workers accumulate company debt (housing mortgages, store credit); organizational alternatives are eliminated (unions prohibited); class-level coordination is prevented (regional labor organizing is suppressed); structural alternatives (competing employers in the region) are systematically destroyed. Resistance (0.35 overall, falling from 0.38 at t0 to 0.22 at class-level t30): Declining despite rising extraction. Individual resistance falls as desperation grows and hope atrophies. Class-level resistance falls fastest as the organizational capacity (labor movement) is suppressed and scattered. This inverse relationship (rising extraction, falling resistance) is the coercion-grid signature of a snare moving into deeper entrenchment.
 *
 * PERSPECTIVAL GAP:
 *   The scrip economy exhibits maximum perspectival divergence. The company ownership perceives rope (coordination of logistics and working capital). The wage worker perceives snare (no exit, rising costs, survival desperation). The state authority perceives piton (degraded legal theater). The regional labor movement perceives snare-with-class-structure (coordinated extraction resisting class-level resistance). The geographic isolation observer risks perceiving mountain (immobility is natural given remoteness). The analytical observer sees a false summit: geographic isolation is real but is NOT the binding constraint; the constructed scrip system, merchant monopoly, and organized elimination of alternatives are. The perspectival divergence is not mere disagreement but reflects structural differentiation: beneficiaries and victims have inverted experiences of the same mechanism because the mechanism actively extracts from one set and subsidizes another.
 *
 * DIRECTIONALITY LOGIC:
 *   Wage workers (powerless/trapped): d = 1.0 (full target). They are declared victims with zero exit capacity. The engine derives d from victim status + trapped exit, producing maximum directionality toward extraction. Company ownership (institutional/arbitrage): d = 0.05 (near-full beneficiary). Beneficiary status + arbitrage exit = minimal directionality; the company can leave at any time (exit is costly but possible; capital is mobile). Regional labor movement (organized/constrained): d = 0.72 (moderately targeted). Victim status (extracted from through suppression) + constrained exit (organizing is possible but faces violent retaliation and firing) = high directionality. Independent merchants (moderate/constrained): d = 0.55 (symmetric). Some benefit from scrip adoption (coordination); some are excluded; those remaining are constrained by dependency on company-store traffic. The grid shows directionality diverging MOST at individual and class levels: individual workers face d ≈ 1.0 and stakes rising from 0.58 to 0.85; class-level alternatives face d = 1.0 (structural exclusion) and accessibility collapse rising from 0.68 to 0.92. The company's directionality remains fixed at near-zero (arbitrage mobility protects them from deepening constraint).
 *
 * MANDATROPHY ANALYSIS:
 *   The company-town scrip system exhibits degraded mandate. FOUNDING PROBLEM: In 1880–1920 era, company towns in resource-extraction industries (mining, steel, timber) solved genuine logistics problems — workers were hired from rural populations with no local housing stock, and company towns provided housing + currency liquidity (hard money was scarce in remote areas). The coordination mandate was real. CONTEMPORARY STATE: By mid-20th century, state banking infrastructure exists; currency scarcity is solved by state-issued money; housing markets have developed; public transportation connects remote employment to urban centers. The coordination mandate is dead. Yet the scrip system persists: by t30=30, it is maintained purely for extraction (exchange-rate manipulation, merchant monopoly control), no longer for coordination. The system exhibits all piton signals at the state-authority level (legal theater, regulatory inertia) but exhibits all snare signals at the worker level (active coercion, suppression of alternatives, impossible exit). This is not a single degraded institution but a two-tier system: degraded at the structural level (the mandate is obsolete) and active at the individual level (extraction is intensifying). The mandatrophy is resolved by recognizing that piton and snare are not mutually exclusive across levels: the state sees theater and inertia (piton); the worker sees entrenchment and extraction (snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scrip_coordination_vs_extraction,
    'Is scrip a genuine coordination mechanism (solving currency/logistics problems) that carries extractive overhead, or is the coordination story a cover for pure extraction?',
    'Comparative analysis: scrip systems in regions WITH banking infrastructure vs WITHOUT. If scrip is adopted voluntarily in banking-rich regions, it is coordination + extraction. If adoption requires legal mandate and coercion, it is extraction with coordination cover.',
    'If coordination: reclassify from snare to tangled_rope (mixed structure justified). If cover: snare classification holds; the ''efficiency'' rationale is theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scrip_coordination_vs_extraction, empirical, 'Whether scrip is a genuine coordination mechanism or extraction cover story').

omega_variable(
    identity_lock_mechanism_durability,
    'Does the identity lock (generational self-concept as company employee) persist after workers exit the company town, or does it dissolve once the structural mechanism is removed?',
    'Longitudinal study of workers who left company towns: post-exit identity persistence, psychological reorientation speed, reintegration into broader labor market. Comparison with workers who never experienced company-town identity lock.',
    'If persists: identity lock is internalized; suppression is higher than structural measures suggest (workers carry the lock with them). If dissolves: identity lock is context-dependent; suppression declines rapidly post-exit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_durability, empirical, 'Durability of identity lock post-exit from company-town structure').

omega_variable(
    class_level_alternatives_destruction,
    'Are regional employment alternatives eliminated by deliberate company action (buying out competitors, excluding unions, lobbying for monopoly protection) or by geographic/economic factors beyond company control?',
    'Historical analysis of company communications, testimony, lobbying records. Reconstruction of pre-company-town regional employment landscape and comparison with post-dominance landscape. Attribution of alternative elimination to deliberate strategy vs market forces.',
    'If deliberate: suppression is high and intentional; snare classification is robust. If market-driven: some suppression is structural; the constraint carries less extracted intent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(class_level_alternatives_destruction, empirical, 'Attribution of class-level alternative destruction to deliberate strategy vs structural factors').

omega_variable(
    false_summit_natural_vs_constructed,
    'Is geographic isolation itself (natural) the binding constraint, or does the company town system (constructed) deepen isolation beyond geographic limits?',
    'Comparison of mobility rates: workers in remote areas WITH independent employment options vs workers in company towns (same geographic distance, different institutional structure). If mobility is similar, geographic constraint is binding; if company-town mobility is dramatically lower, the constructed system is the binding mechanism.',
    'If geographic: mountain classification partially justified. If constructed dominates: false summit confirmed; snare classification is correct; the ''natural limit'' framing is ideological cover.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_vs_constructed, empirical, 'Attribution of immobility to geographic constraints vs constructed company-town system').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is suppression of exit and resistance primarily structural (legal barriers, geographic isolation, economic dependency) or internalized (workers have internalized the company''s legitimacy claims, believe exit is impossible, fear retaliation preemptively)?',
    'Analysis of resistance attempts and their suppression: when workers attempt collective action, does suppression come from external force (company violence, legal enforcement, deportation) or from internal demoralization, anticipatory compliance, identity fusion? Post-suppression trajectory: does resistance recur despite previous defeats, or does it decline as internalized helplessness grows?',
    'If primarily structural: suppression is tractable — removing barriers could enable exit. If primarily internalized: suppression carries post-exit persistence — workers may not exercise exit even when barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Suppression mechanism: structural vs internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(company_town_scrip_economy, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scrip_tr_t0, company_town_scrip_economy, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(scrip_tr_t0, observed).
narrative_ontology:measurement(scrip_tr_t10, company_town_scrip_economy, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(scrip_tr_t10, observed).
narrative_ontology:measurement(scrip_tr_t20, company_town_scrip_economy, theater_ratio, 20, 0.45).
narrative_ontology:measurement_basis(scrip_tr_t20, observed).
narrative_ontology:measurement(scrip_tr_t30, company_town_scrip_economy, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(scrip_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(scrip_be_t0, company_town_scrip_economy, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(scrip_be_t0, observed).
narrative_ontology:measurement(scrip_be_t10, company_town_scrip_economy, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(scrip_be_t10, observed).
narrative_ontology:measurement(scrip_be_t20, company_town_scrip_economy, base_extractiveness, 20, 0.78).
narrative_ontology:measurement_basis(scrip_be_t20, observed).
narrative_ontology:measurement(scrip_be_t30, company_town_scrip_economy, base_extractiveness, 30, 0.8).
narrative_ontology:measurement_basis(scrip_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(scrip_su_t0, company_town_scrip_economy, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(scrip_su_t0, observed).
narrative_ontology:measurement(scrip_su_t10, company_town_scrip_economy, suppression_requirement, 10, 0.75).
narrative_ontology:measurement_basis(scrip_su_t10, observed).
narrative_ontology:measurement(scrip_su_t20, company_town_scrip_economy, suppression_requirement, 20, 0.82).
narrative_ontology:measurement_basis(scrip_su_t20, observed).
narrative_ontology:measurement(scrip_su_t30, company_town_scrip_economy, suppression_requirement, 30, 0.85).
narrative_ontology:measurement_basis(scrip_su_t30, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=30
narrative_ontology:measurement(scrip_grid_01, company_town_scrip_economy, accessibility_collapse(class), 0, 0.68).
narrative_ontology:measurement(scrip_grid_02, company_town_scrip_economy, accessibility_collapse(class), 30, 0.92).
narrative_ontology:measurement(scrip_grid_03, company_town_scrip_economy, accessibility_collapse(individual), 0, 0.72).
narrative_ontology:measurement(scrip_grid_04, company_town_scrip_economy, accessibility_collapse(individual), 30, 0.88).
narrative_ontology:measurement(scrip_grid_05, company_town_scrip_economy, accessibility_collapse(organizational), 0, 0.55).
narrative_ontology:measurement(scrip_grid_06, company_town_scrip_economy, accessibility_collapse(organizational), 30, 0.78).
narrative_ontology:measurement(scrip_grid_07, company_town_scrip_economy, accessibility_collapse(structural), 0, 0.45).
narrative_ontology:measurement(scrip_grid_08, company_town_scrip_economy, accessibility_collapse(structural), 30, 0.62).
narrative_ontology:measurement(scrip_grid_09, company_town_scrip_economy, resistance(class), 0, 0.38).
narrative_ontology:measurement(scrip_grid_10, company_town_scrip_economy, resistance(class), 30, 0.22).
narrative_ontology:measurement(scrip_grid_11, company_town_scrip_economy, resistance(individual), 0, 0.28).
narrative_ontology:measurement(scrip_grid_12, company_town_scrip_economy, resistance(individual), 30, 0.15).
narrative_ontology:measurement(scrip_grid_13, company_town_scrip_economy, resistance(organizational), 0, 0.42).
narrative_ontology:measurement(scrip_grid_14, company_town_scrip_economy, resistance(organizational), 30, 0.35).
narrative_ontology:measurement(scrip_grid_15, company_town_scrip_economy, resistance(structural), 0, 0.45).
narrative_ontology:measurement(scrip_grid_16, company_town_scrip_economy, resistance(structural), 30, 0.38).
narrative_ontology:measurement(scrip_grid_17, company_town_scrip_economy, stakes_inflation(class), 0, 0.52).
narrative_ontology:measurement(scrip_grid_18, company_town_scrip_economy, stakes_inflation(class), 30, 0.88).
narrative_ontology:measurement(scrip_grid_19, company_town_scrip_economy, stakes_inflation(individual), 0, 0.58).
narrative_ontology:measurement(scrip_grid_20, company_town_scrip_economy, stakes_inflation(individual), 30, 0.85).
narrative_ontology:measurement(scrip_grid_21, company_town_scrip_economy, stakes_inflation(organizational), 0, 0.42).
narrative_ontology:measurement(scrip_grid_22, company_town_scrip_economy, stakes_inflation(organizational), 30, 0.68).
narrative_ontology:measurement(scrip_grid_23, company_town_scrip_economy, stakes_inflation(structural), 0, 0.35).
narrative_ontology:measurement(scrip_grid_24, company_town_scrip_economy, stakes_inflation(structural), 30, 0.58).
narrative_ontology:measurement(scrip_grid_25, company_town_scrip_economy, suppression(class), 0, 0.68).
narrative_ontology:measurement(scrip_grid_26, company_town_scrip_economy, suppression(class), 30, 0.88).
narrative_ontology:measurement(scrip_grid_27, company_town_scrip_economy, suppression(individual), 0, 0.62).
narrative_ontology:measurement(scrip_grid_28, company_town_scrip_economy, suppression(individual), 30, 0.79).
narrative_ontology:measurement(scrip_grid_29, company_town_scrip_economy, suppression(organizational), 0, 0.48).
narrative_ontology:measurement(scrip_grid_30, company_town_scrip_economy, suppression(organizational), 30, 0.72).
narrative_ontology:measurement(scrip_grid_31, company_town_scrip_economy, suppression(structural), 0, 0.55).
narrative_ontology:measurement(scrip_grid_32, company_town_scrip_economy, suppression(structural), 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(company_town_scrip_economy, resource_allocation).
narrative_ontology:affects_constraint(company_town_scrip_economy, company_town_housing_debt_trap).
narrative_ontology:affects_constraint(company_town_scrip_economy, scrip_banking_exclusion).
narrative_ontology:affects_constraint(company_town_scrip_economy, union_prohibition_enforcement).

% DUAL FORMULATION NOTE:
% The company-town scrip economy is upstream of three downstream constraints: housing debt (the company mortgage traps workers geographically); banking exclusion (scrip prevents access to state banking and credit); union prohibition (employment contracts prohibit organizing). The three stories decompose the different vectors of the constraint family. The scrip economy is the coordinating mechanism through which all three downstream constraints operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(company_town_scrip_economy, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
