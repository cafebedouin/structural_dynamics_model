% ============================================================================
% CONSTRAINT STORY: work_dignity_automation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_work_dignity_automation, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: work_dignity_automation
 *   human_readable: Work Dignity Under Automation
 *   domain: theological_ethics/technology_governance/political_economy
 *
 * SUMMARY:
 *   The automation of work presents a structural tension between productivity
 *   gains and human dignity. Catholic Social Doctrine, articulated in Laudato
 *   Si' and subsequent Magisterial teaching, holds that work is not merely a
 *   means to income but a site of human development, relationship, and
 *   contribution to the common good. Automation that systematically
 *   eliminates jobs without creating dignified alternatives violates the
 *   principle that the person is an end, not a means. This constraint
 *   operates across theological ethics, technology governance, and political
 *   economy domains. The Magisterial reading grounds dignity in imago Dei
 *   (humanity created in God's image) and claims unique interpretive
 *   authority through apostolic succession. Secular humanist,
 *   techno-optimist, and pluralist pragmatic readings offer alternative
 *   groundings and governance frameworks. The constraint exhibits
 *   tangled_rope structure: genuine coordination gains (productivity, reduced
 *   drudgery) exist alongside substantial extraction (wealth concentration,
 *   job quality degradation, worker instrumentalization). The theater_ratio
 *   (0.48) reflects that retraining programs and corporate responsibility
 *   initiatives are partially performative — underfunded, misaligned with
 *   actual labor market needs, and often serving as legitimation mechanisms
 *   rather than effective transitions. Measurements show extraction
 *   accumulation over 40 years (1980-2020): base_extractiveness rising from
 *   0.35 to 0.58 as automation gains are captured asymmetrically,
 *   theater_ratio rising from 0.25 to 0.48 as retraining rhetoric outpaces
 *   results, and suppression_requirement rising from 0.45 to 0.62 as exit
 *   options narrow (geographic immobility, skill obsolescence, weakened labor
 *   power).
 *
 * KEY AGENTS:
 *   - Displaced Workers: Primary victims (powerless/trapped) — face skill obsolescence, geographic immobility, absence of retraining infrastructure; biographical time horizon makes exit impossible
 *   - Workers' Families: Secondary victims (powerless/trapped) — bear intergenerational costs of income loss, community decline, and reduced opportunity
 *   - Young People Entering Labor Market: Victims (powerless/identity_locked) — face systematic elimination of entry-level positions, gig-economy precarity, and educational debt; identity-locked by meritocracy narrative
 *   - Capital Owners / Shareholders: Primary beneficiaries (institutional/arbitrage) — capture productivity gains through labor cost reduction; global arbitrage options across sectors and geographies
 *   - Automation Technology Vendors: Beneficiaries (institutional/mobile) — profit from selling labor-saving systems; face competitive pressure to prioritize cost reduction over dignity enhancement
 *   - Mid-Career Professionals: Mixed position (moderate/constrained) — benefit from augmentation tools but face deskilling pressure and wage stagnation
 *   - Labor Unions / Worker Advocacy Coalitions: Organized agents (organized/mobile) — see automation as coordination problem requiring transitional support; scaffold perspective with sunset logic
 *   - Magisterial Teaching Authority: Institutional actor (institutional/constrained) — benefits from moral authority to critique technocratic paradigm but constrained by lack of enforcement power; mixed beneficiary-victim
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination gains and substantial extraction; tangled_rope classification from comprehensive perspective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(work_dignity_automation, 0.58).
domain_priors:suppression_score(work_dignity_automation, 0.62).
domain_priors:theater_ratio(work_dignity_automation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(work_dignity_automation, extractiveness, 0.58).
narrative_ontology:constraint_metric(work_dignity_automation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(work_dignity_automation, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(work_dignity_automation, tangled_rope).
narrative_ontology:human_readable(work_dignity_automation, "Work Dignity Under Automation").
narrative_ontology:topic_domain(work_dignity_automation, "theological_ethics/technology_governance/political_economy").

domain_priors:requires_active_enforcement(work_dignity_automation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(work_dignity_automation, 'd2f6db1d-8559-49d0-a5c5-88236af1f6fe').
narrative_ontology:cs_kernel_codification('d2f6db1d-8559-49d0-a5c5-88236af1f6fe', formalized).
narrative_ontology:cs_authority_grounding('d2f6db1d-8559-49d0-a5c5-88236af1f6fe', lineage).
narrative_ontology:cs_interpretation_layer_present('d2f6db1d-8559-49d0-a5c5-88236af1f6fe').
narrative_ontology:cs_reading_relation('d2f6db1d-8559-49d0-a5c5-88236af1f6fe', work_dignity_automation__secular_humanist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2f6db1d-8559-49d0-a5c5-88236af1f6fe', work_dignity_automation__techno_optimist_reading, forecloses).
narrative_ontology:cs_reading_relation('d2f6db1d-8559-49d0-a5c5-88236af1f6fe', work_dignity_automation__pluralist_pragmatic_reading, influences).
narrative_ontology:cs_axiom('d2f6db1d-8559-49d0-a5c5-88236af1f6fe', foundational, work_as_participation_in_creation).
narrative_ontology:cs_axiom_status(work_as_participation_in_creation, holdable).
narrative_ontology:cs_axiom_grounding('d2f6db1d-8559-49d0-a5c5-88236af1f6fe', work_as_participation_in_creation, theological).
narrative_ontology:cs_axiom('d2f6db1d-8559-49d0-a5c5-88236af1f6fe', foundational, person_as_end_not_means).
narrative_ontology:cs_axiom_status(person_as_end_not_means, holdable).
narrative_ontology:cs_axiom_grounding('d2f6db1d-8559-49d0-a5c5-88236af1f6fe', person_as_end_not_means, deontological).
narrative_ontology:cs_axiom('d2f6db1d-8559-49d0-a5c5-88236af1f6fe', secondary, universal_destination_of_goods).
narrative_ontology:cs_axiom_status(universal_destination_of_goods, holdable).
narrative_ontology:cs_axiom_grounding('d2f6db1d-8559-49d0-a5c5-88236af1f6fe', universal_destination_of_goods, theological).
narrative_ontology:cs_axiom('d2f6db1d-8559-49d0-a5c5-88236af1f6fe', secondary, subsidiarity_principle).
narrative_ontology:cs_axiom_status(subsidiarity_principle, holdable).
narrative_ontology:cs_axiom_grounding('d2f6db1d-8559-49d0-a5c5-88236af1f6fe', subsidiarity_principle, conventional).
narrative_ontology:cs_reference_frame('d2f6db1d-8559-49d0-a5c5-88236af1f6fe', imago_dei_anthropology_pre_industrial).
narrative_ontology:cs_drift_state('d2f6db1d-8559-49d0-a5c5-88236af1f6fe', contemporary_ai_automation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d2f6db1d-8559-49d0-a5c5-88236af1f6fe', '2026-06-08T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(work_dignity_automation, capital_owners_reducing_labor_costs).
narrative_ontology:constraint_beneficiary(work_dignity_automation, automation_technology_vendors).
narrative_ontology:constraint_beneficiary(work_dignity_automation, shareholders_demanding_efficiency).
narrative_ontology:constraint_victim(work_dignity_automation, displaced_workers).
narrative_ontology:constraint_victim(work_dignity_automation, workers_families).
narrative_ontology:constraint_victim(work_dignity_automation, young_people_facing_unemployment).
narrative_ontology:constraint_victim(work_dignity_automation, communities_dependent_on_manufacturing).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(work_dignity_automation, capital_owner_shareholder).
narrative_ontology:constraint_beneficiary(work_dignity_automation, automation_technology_vendor).
narrative_ontology:constraint_beneficiary(work_dignity_automation, mid_career_professional_automating_sector).
narrative_ontology:constraint_beneficiary(work_dignity_automation, magisterial_teaching_authority).
narrative_ontology:constraint_victim(work_dignity_automation, displaced_manufacturing_worker).
narrative_ontology:constraint_victim(work_dignity_automation, workers_family_members).
narrative_ontology:constraint_victim(work_dignity_automation, young_person_entering_labor_market).
narrative_ontology:constraint_victim(work_dignity_automation, mid_career_professional_automating_sector).
narrative_ontology:constraint_victim(work_dignity_automation, community_dependent_on_manufacturing).
narrative_ontology:constraint_vindicates(work_dignity_automation, labor_cost_minimization_imperative).
narrative_ontology:constraint_vindicates(work_dignity_automation, technological_inevitability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Faces job elimination in mid-career with industry-specific skills that have become obsolete. Geographic immobility due to homeownership and family ties. Retraining programs are underfunded and misaligned with actual labor market needs. New jobs available are lower wage, lower status, and offer less autonomy. Cannot exit the constraint — automation is industry-wide and alternative employment in the region is scarce.
narrative_ontology:constraint_stakeholder(work_dignity_automation, displaced_manufacturing_worker, payer,
    powerless, biographical, trapped, national).

% Bear intergenerational costs of income loss, community economic decline, and reduced educational and employment opportunities for children. The family unit absorbs the shock of displacement through reduced consumption, delayed retirement, and foregone investments in children's development. No exit from the constraint — family ties bind them to the displaced worker's fate.
narrative_ontology:constraint_stakeholder(work_dignity_automation, workers_family_members, payer,
    powerless, generational, trapped, national).

% Faces systematic elimination of entry-level positions that historically provided pathways to stable careers. Gig-economy precarity and credential inflation require higher education investment, creating debt burden. Identity-locked by meritocracy narrative ('learn to code', 'adapt or fail') that frames structural unemployment as individual failure. Structurally mobile (could theoretically retrain, relocate) but psychologically and financially trapped by educational debt and internalized responsibility for labor market outcomes.
narrative_ontology:constraint_stakeholder(work_dignity_automation, young_person_entering_labor_market, payer,
    powerless, generational, identity_locked, national).

% Captures productivity gains from automation through labor cost reduction and increased returns on investment. Competitive pressure requires adoption of labor-saving technologies to maintain market position. Arbitrage exit options across sectors, asset classes, and geographies. Experiences automation as pure coordination: efficiency gains are distributed to shareholders as intended, and the system functions as designed.
narrative_ontology:constraint_stakeholder(work_dignity_automation, capital_owner_shareholder, beneficiary,
    institutional, immediate, arbitrage, global).

% Profits from selling labor-saving systems to firms seeking cost reduction. Faces competitive pressure to prioritize features that maximize labor displacement (ROI for clients) over features that enhance worker dignity or create new roles. Mobile exit options across industries and geographies. Benefits from the constraint's operation but also faces reputational risk if automation is perceived as socially harmful.
narrative_ontology:constraint_stakeholder(work_dignity_automation, automation_technology_vendor, beneficiary,
    institutional, immediate, mobile, global).

% Benefits from productivity tools that augment work (data analysis, communication, project management) but faces extraction through deskilling pressure (automation of complex tasks reduces skill premium) and wage stagnation (labor cost savings do not translate to wage increases). Constrained by mortgage obligations, family responsibilities, and industry-specific expertise. Can exit at significant cost (career change, relocation, retraining) but not easily. Dual-positioned: genuine beneficiary of augmentation tools, genuine victim of deskilling and wage pressure.
narrative_ontology:constraint_stakeholder(work_dignity_automation, mid_career_professional_automating_sector, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(work_dignity_automation, mid_career_professional_automating_sector, beneficiary).

% Organized agents advocating for retraining programs, portable benefits, job guarantees, and profit-sharing mechanisms. See automation as a coordination problem requiring transitional support structures. Have agency to build alternative institutions (worker cooperatives, sectoral bargaining, portable benefit funds) and political power to demand policy changes. Mobile exit options: can shift advocacy strategies, form new coalitions, and adapt to changing labor market conditions. Set the agenda for what 'dignified automation' would require.
narrative_ontology:constraint_stakeholder(work_dignity_automation, labor_union_worker_advocacy_coalition, agenda_setter,
    organized, generational, mobile, national).

% Benefits from moral authority to critique technocratic paradigm and guide AI development toward common good. Sets the agenda for what dignity principles require in automation governance. Constrained by lack of enforcement power (relies on voluntary adoption by states, firms, and individuals) and dependence on secular translation of theological claims. Dual-positioned: genuine agenda-setter with unique interpretive authority within Catholic tradition, but also beneficiary of institutional legitimacy derived from addressing technological crisis.
narrative_ontology:constraint_stakeholder(work_dignity_automation, magisterial_teaching_authority, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(work_dignity_automation, magisterial_teaching_authority, beneficiary).

% Entire regional economies built around manufacturing employment face collapse when automation eliminates anchor industries. Tax base erodes, public services decline, property values fall, and social fabric frays. The community as a collective entity cannot exit — it is geographically fixed and lacks resources to attract alternative industries. Bears intergenerational costs through reduced opportunity for young people and declining quality of life for all residents.
narrative_ontology:constraint_stakeholder(work_dignity_automation, community_dependent_on_manufacturing, payer,
    powerless, generational, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Automation solves the genuine problem of increasing productivity, reducing production costs, and eliminating dangerous or repetitive physical labor. It enables firms to remain competitive in global markets and can free human workers from drudgery for higher-value activities.
% TRANSFER_FUNCTION: Automation transfers labor costs from firms to workers (job elimination, wage stagnation), transfers productivity gains from workers to capital owners (increased returns on investment), and transfers economic risk from firms to workers (precarity, loss of stable employment). Money flows from displaced workers (lost wages, reduced consumption) to capital owners (increased profits, shareholder returns) and automation vendors (sales revenue).
% ABSENT_VOICES: Displaced workers in regions without strong labor unions or political representation. Workers in the Global South facing automation of manufacturing jobs that were previously offshored. Future generations who will inherit a labor market with fewer pathways to dignified work. Indigenous communities whose traditional economies are disrupted by automated resource extraction. These voices are absent from corporate boardrooms, technology design processes, and policy forums where automation decisions are made. They are excluded by lack of institutional access, geographic distance from decision-making centers, and power asymmetries.
% DISAPPEARANCE_RATIONALE: If automation constraints disappeared overnight (no labor-saving technology, no productivity tools), the entire structure of modern production would collapse. Firms would face dramatically higher labor costs, supply chains would break, and global trade patterns would shift. Workers would regain bargaining power through scarcity of labor, but productivity would fall and living standards would decline. The world is deeply organized around automation — it is not a natural fact but a constructed arrangement that shapes employment, wages, firm structure, and economic geography. Disappearance would force massive rearrangement.
% FOUNDING_PROBLEM: The founding problem was genuine: in the early industrial era (1800s-1900s), dangerous and backbreaking physical labor caused widespread injury, early death, and human suffering. Automation of hazardous tasks (mining, heavy manufacturing, chemical processing) was a moral imperative to reduce harm. The problem was: how do we eliminate dangerous drudgery while preserving human dignity and economic security?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is contested between capital and labor. Capital owners and technology vendors claim the problem is still live: dangerous and repetitive work still exists, and automation continues to solve it (corroborated by workplace safety statistics showing injury reduction in automated sectors). Labor advocates and displaced workers claim the problem has shifted: the dangerous drudgery has largely been eliminated in developed economies, but automation now targets cognitively complex and relationally meaningful work that was never 'drudgery' (corroborated by job quality studies showing automation of skilled trades, professional services, and creative work). The Magisterial teaching authority claims the founding problem is dead in its original form but a new problem has emerged: automation now serves profit maximization rather than human flourishing, and the arrangement persists not because it solves the original problem but because it concentrates power (corroborated by theological analysis and CST principles, but contested by secular economists who argue efficiency gains benefit society broadly).
narrative_ontology:disappearance_verdict(work_dignity_automation, world_rearranges).
narrative_ontology:founding_problem_status(work_dignity_automation, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED WORKER (SNARE) — Trapped by geographic immobility, skill obsolescence, and absence of retraining infrastructure. Automation eliminates dignified work without creating accessible alternatives. The coordination story (efficiency gains benefit society) is cover for extraction. Maximum experienced extraction from biographical perspective.
constraint_indexing:constraint_classification(work_dignity_automation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-CAREER PROFESSIONAL (TANGLED ROPE) — Constrained by mortgage obligations, family responsibilities, and industry-specific expertise. Benefits from productivity tools that augment rather than replace, but faces extraction through deskilling pressure and wage stagnation. Mixed coordination (genuine efficiency gains) and extraction (asymmetric distribution of gains).
constraint_indexing:constraint_classification(work_dignity_automation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL OWNER (ROPE) — Experiences automation as pure coordination: labor cost reduction increases returns, competitive pressure requires adoption, efficiency gains are distributed to shareholders. Arbitrage exit options across sectors and geographies. Net beneficiary with negligible experienced extraction.
constraint_indexing:constraint_classification(work_dignity_automation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR COALITION (SCAFFOLD) — Organized agents see automation as a coordination problem requiring transitional support structures: retraining programs, portable benefits, job guarantees, profit-sharing mechanisms. The constraint is temporary if social protections are built. Sunset logic: automation becomes dignity-compatible when gains are shared and transitions are supported.
constraint_indexing:constraint_classification(work_dignity_automation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: MAGISTERIAL AUTHORITY (TANGLED ROPE) — The Church benefits from moral authority to critique technocratic paradigm and guide development toward common good, but is constrained by lack of enforcement power and dependence on voluntary adoption. Genuine coordination function (articulating dignity principles, convening dialogue) exists alongside extraction (institutional legitimacy derived from crisis). Mixed beneficiary-victim position.
constraint_indexing:constraint_classification(work_dignity_automation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: YOUNG PERSON (SNARE) — Identity-locked by educational debt, credential requirements, and internalized meritocracy narrative. Faces systematic elimination of entry-level positions and gig-economy precarity. The 'learn to code' coordination story is cover: retraining programs are underfunded, new jobs are lower quality, and the promise of dignified work through adaptation is largely theatrical. High extraction from generational perspective.
constraint_indexing:constraint_classification(work_dignity_automation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, automation presents genuine coordination gains (productivity, reduced drudgery, potential for human flourishing) alongside substantial extraction (concentration of wealth, elimination of dignified work, instrumentalization of persons). The constraint is tangled_rope: both functions are real. The coordination function is not mere cover, but the extraction is not incidental. Requires active enforcement of dignity principles to prevent collapse into pure extraction.
constraint_indexing:constraint_classification(work_dignity_automation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(work_dignity_automation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(work_dignity_automation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(work_dignity_automation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(work_dignity_automation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(work_dignity_automation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial. Automation produces genuine productivity gains, but the distribution is highly asymmetric. Capital owners capture the majority of gains through labor cost reduction and increased returns. Workers bear the costs through job elimination, wage stagnation, and degraded job quality. The 'rising tide lifts all boats' coordination story is partially true (some new jobs are created, some workers benefit from augmentation) but substantially cover for extraction. The value reflects that coordination function is real but secondary to extraction. Suppression (0.62): Moderate-high. Workers face significant barriers to exit: geographic immobility (housing markets, family ties), skill obsolescence (industry-specific expertise becomes worthless), absence of adequate retraining infrastructure (programs are underfunded and misaligned), weakened labor power (union decline, at-will employment), and credential inflation (new jobs require higher education). Suppression has increased over the 40-year interval as labor power has eroded and exit options have narrowed. Theater ratio (0.48): Moderate. Retraining programs, corporate social responsibility initiatives, and 'future of work' task forces are partially performative. Some programs are effective, but many are underfunded, have low completion rates, and fail to place workers in jobs of comparable dignity and compensation. The theater serves legitimation function: it allows capital and policymakers to claim they are addressing displacement while extraction continues. Theater has increased over the interval as the gap between retraining rhetoric and outcomes has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the full indexical classification range. Capital owners see pure coordination (Rope): automation solves the legitimate problem of reducing costs and increasing efficiency; gains are distributed to shareholders as intended. Labor coalitions see a temporary problem with a sunset (Scaffold): automation can be dignity-compatible if social protections are built and gains are shared. Mid-career professionals see mixed coordination and extraction (Tangled Rope): productivity tools augment their work, but deskilling pressure and wage stagnation extract from them. Displaced workers and young people see pure extraction (Snare): the coordination story is cover; automation eliminates dignified work without creating accessible alternatives; retraining programs are theatrical. The Magisterial authority sees tangled_rope from a different angle: genuine coordination potential exists, but extraction dominates in practice due to technocratic paradigm and absence of dignity principles in governance. The analytical observer sees tangled_rope at civilizational scope: both coordination and extraction are real structural features requiring active enforcement to prevent collapse into pure extraction. The perspectival gap is not 'which type is correct?' but 'which structural position are you measuring from?' The presheaf over observation sites IS the answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Capital owners are declared beneficiaries with arbitrage exit options — they experience low or negative effective extraction (the constraint subsidizes them). Displaced workers are declared victims with trapped exit options — they experience maximum effective extraction. Mid-career professionals are in a mixed position: they appear in neither beneficiary nor victim lists explicitly, but their moderate power and constrained exit options place them in the middle of the directionality range. Young people are victims with identity_locked exit options — they are structurally mobile (could theoretically retrain, relocate) but identity-fused with meritocracy narrative and educational debt, making exit psychologically and financially unthinkable. Labor coalitions are organized agents with mobile exit options — they have agency to build alternative structures, reducing their experienced extraction. The Magisterial authority is institutional with constrained exit — it benefits from moral authority but is constrained by lack of enforcement power, placing it in a mixed beneficiary-victim position. No directionality overrides are needed; the structural declarations produce accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that tangled_rope is the correct classification from the analytical perspective: both coordination and extraction are real. The coordination function is not mere cover — automation does produce genuine productivity gains, reduce drudgery, and create some new opportunities. But the extraction is not incidental — the asymmetric distribution of gains, systematic elimination of dignified work, and instrumentalization of persons are structural features, not bugs. The Magisterial reading's claim that automation violates dignity principles is validated by the victim perspectives (snare from displaced workers, identity_locked snare from young people) but must acknowledge the genuine coordination gains visible from other perspectives. The secular humanist and pluralist pragmatic readings can engage with the coordination-extraction mix without accepting theological foundations. The techno-optimist reading's pure coordination claim is falsified by the victim perspectives and the extraction accumulation measurements. The mandatrophy is resolved by showing that the constraint's type depends on the observer's structural position, and the analytical classification (tangled_rope) integrates the perspectival diversity rather than collapsing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_threshold_automation,
    'What threshold of job quality, wage level, and worker agency constitutes ''dignified work'' such that automation creating jobs below this threshold violates dignity principles?',
    'Empirical measurement of worker autonomy, income adequacy, skill utilization, and relational goods in new vs. eliminated jobs; cross-cultural validation of dignity indicators; longitudinal tracking of worker well-being across automation transitions',
    'If threshold is high (e.g., living wage + autonomy + skill development): most current automation violates dignity, strengthening snare classification from worker perspectives. If threshold is low (e.g., any income-generating activity): automation is coordination, strengthening rope classification from capital perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_threshold_automation, conceptual, 'Threshold definition for dignified work under automation').

omega_variable(
    retraining_effectiveness_empirical,
    'Do existing retraining programs successfully transition displaced workers into jobs of comparable dignity and compensation, or are they primarily theatrical compliance mechanisms?',
    'Longitudinal outcome tracking: employment rates, wage trajectories, job satisfaction, and skill utilization for program participants vs. control groups; analysis of program funding adequacy and completion rates; comparison of promised vs. actual job placement outcomes',
    'If effective: scaffold perspective is validated, automation can be dignity-compatible with proper support. If theatrical: snare classification strengthened, retraining narrative is cover for extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retraining_effectiveness_empirical, empirical, 'Empirical effectiveness of worker retraining programs').

omega_variable(
    technological_determinism_vs_choice,
    'Is labor-saving automation technologically inevitable, or is it a policy choice reflecting power asymmetries between capital and labor?',
    'Historical analysis of automation trajectories under different regulatory regimes; comparative study of labor-enhancing vs. labor-saving innovation in different institutional contexts; examination of R&D funding priorities and patent incentives',
    'If inevitable: mountain classification from some perspectives (natural law of technological progress). If choice: snare/tangled_rope classification strengthened (extraction mechanism disguised as necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism_vs_choice, empirical, 'Technological determinism vs. policy choice in automation direction').

omega_variable(
    magisterial_authority_scope,
    'Does the Magisterium''s interpretive authority over human dignity principles extend legitimately to binding technical specifications for AI systems, or only to moral principles requiring secular translation?',
    'Theological debate within Catholic tradition on boundaries of natural law reasoning; ecumenical dialogue on shared vs. tradition-specific dignity claims; practical observation of whether non-Catholic actors adopt Magisterial technical guidance',
    'If binding authority extends to technical specs: Magisterial reading can claim enforcement power, reducing extraction. If authority is limited to principles: secular translation layer required, increasing risk of dignity principles being diluted in implementation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(magisterial_authority_scope, conceptual, 'Scope of Magisterial authority over technical AI governance').

omega_variable(
    cs_framing_kernel_ambiguity,
    'Is the kernel ''human dignity as imago Dei'' or the broader ''Catholic Social Doctrine principles'' (common good, subsidiarity, solidarity, etc.)? The former is theological and fixed; the latter is a developed tradition with internal tensions.',
    'Analysis of which element the Magisterial reading treats as immutable vs. which elements are subject to development of doctrine; examination of how sibling readings engage with imago Dei claim vs. CST principles separately',
    'If kernel is imago Dei alone: secular readings foreclose it entirely (no shared kernel). If kernel is CST principles: secular readings can engage with subsidiarity, common good, etc. without accepting theological foundation, creating overlapping consensus space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_ambiguity, conceptual, 'Kernel identity ambiguity: imago Dei vs. CST principles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(work_dignity_automation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(work_dignity_tr_t0, work_dignity_automation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(work_dignity_tr_t10, work_dignity_automation, theater_ratio, 10, 0.32).
narrative_ontology:measurement(work_dignity_tr_t20, work_dignity_automation, theater_ratio, 20, 0.38).
narrative_ontology:measurement(work_dignity_tr_t30, work_dignity_automation, theater_ratio, 30, 0.44).
narrative_ontology:measurement(work_dignity_tr_t40, work_dignity_automation, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(work_dignity_be_t0, work_dignity_automation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(work_dignity_be_t10, work_dignity_automation, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(work_dignity_be_t20, work_dignity_automation, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(work_dignity_be_t30, work_dignity_automation, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(work_dignity_be_t40, work_dignity_automation, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(work_dignity_su_t0, work_dignity_automation, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(work_dignity_su_t10, work_dignity_automation, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(work_dignity_su_t20, work_dignity_automation, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(work_dignity_su_t30, work_dignity_automation, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(work_dignity_su_t40, work_dignity_automation, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(work_dignity_automation, resource_allocation).
narrative_ontology:affects_constraint(work_dignity_automation, technocratic_paradigm_resistance).

% DUAL FORMULATION NOTE:
% This constraint is downstream of technocratic_paradigm_resistance. The upstream constraint describes the broader paradigm that treats efficiency and technological progress as self-justifying, independent of human dignity considerations. This constraint instantiates that paradigm in the specific domain of work automation. The upstream constraint's extractiveness reflects the epistemic closure and institutional capture of technocratic thinking; this constraint's extractiveness reflects the material consequences for workers and families. Both are tangled_rope: genuine coordination functions (efficiency gains, productivity) exist alongside substantial extraction (concentration of power, elimination of alternatives, instrumentalization of persons).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
