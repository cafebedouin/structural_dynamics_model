% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__beneficiary_maintained_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_as_natural_default__beneficiary_maintained_reading
 *   human_readable: Market-as-Natural-Default: Beneficiary-Maintained Reading
 *   domain: political_economy/ideology
 *
 * SUMMARY:
 *   Market naturalization is the active institutional project by which
 *   finance, corporations, and property owners present market allocation as
 *   inevitable, efficient, and natural—a product of human nature or economic
 *   laws—rather than as one possible coordinating mechanism among many. This
 *   reading treats that naturalization as ACTIVELY DEFENDED post-hoc:
 *   beneficiaries fund the narratives, capture institutions that could
 *   question them, and suppress alternatives from policy and academic
 *   discourse. The founding problem (how to coordinate without feudalism) was
 *   live in the 18th century; it is now dead. The constraint persists not
 *   because the problem persists but because beneficiaries maintain its
 *   framing. Alternatives are neither forgotten nor impossible; they are
 *   actively reframed as unthinkable. This is distinct from the
 *   hybrid_amnesia_reading (initial forgetting creates openness to capture)
 *   and the lapsed_alternative_reading (alternatives fade from cultural
 *   memory). Here, alternatives are visible—worked examples exist
 *   (cooperatives, state provision, commons)—and are actively labeled as
 *   marginal or failed.
 *
 * KEY AGENTS:
 *   - Financial sector: institutional agenda-setter; funds think tanks, chairs economics departments, produces naturalizing research; collects rents from the arrangement; arbitrage-exit capable.
 *   - Labor-dependent populations: powerless payers; trapped in wage systems; told alternatives are impossible; identity-locked into market participation.
 *   - Heterodox economists and cooperative institutions: excluded from policy tables and mainstream funding; would argue alternatives are viable; systematic erasure from conversation.
 *   - Property owners: beneficiaries insulated from questioning legitimacy of private property; participate in capture; have mobile exit but defend the system.
 *   - Subsistence communities: identity-locked payers; forced integration into market systems; own institutions erased intellectually and legally; exit unthinkable.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, 0.48).
domain_priors:suppression_score(market_as_natural_default__beneficiary_maintained_reading, 0.62).
domain_priors:theater_ratio(market_as_natural_default__beneficiary_maintained_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__beneficiary_maintained_reading, "Market-as-Natural-Default: Beneficiary-Maintained Reading").
narrative_ontology:topic_domain(market_as_natural_default__beneficiary_maintained_reading, "political_economy/ideology").

domain_priors:requires_active_enforcement(market_as_natural_default__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__beneficiary_maintained_reading, 'd97859e7-e55e-4b48-9d51-181814e7fdba').
narrative_ontology:cs_kernel_codification('d97859e7-e55e-4b48-9d51-181814e7fdba', fixed_text).
narrative_ontology:cs_authority_grounding('d97859e7-e55e-4b48-9d51-181814e7fdba', extraction).
narrative_ontology:cs_interpretation_layer_present('d97859e7-e55e-4b48-9d51-181814e7fdba').
narrative_ontology:cs_reading_relation('d97859e7-e55e-4b48-9d51-181814e7fdba', market_as_natural_default__hybrid_amnesia_reading, influences).
narrative_ontology:cs_reading_relation('d97859e7-e55e-4b48-9d51-181814e7fdba', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_axiom('d97859e7-e55e-4b48-9d51-181814e7fdba', foundational, market_naturalization_actively_defended).
narrative_ontology:cs_axiom_status(market_naturalization_actively_defended, holdable).
narrative_ontology:cs_axiom_grounding('d97859e7-e55e-4b48-9d51-181814e7fdba', market_naturalization_actively_defended, empirically_contingent).
narrative_ontology:cs_axiom('d97859e7-e55e-4b48-9d51-181814e7fdba', foundational, alternatives_are_suppressed_not_forgotten).
narrative_ontology:cs_axiom_status(alternatives_are_suppressed_not_forgotten, holdable).
narrative_ontology:cs_axiom_grounding('d97859e7-e55e-4b48-9d51-181814e7fdba', alternatives_are_suppressed_not_forgotten, empirically_contingent).
narrative_ontology:cs_reference_frame('d97859e7-e55e-4b48-9d51-181814e7fdba', markets_as_natural_inevitable_default).
narrative_ontology:cs_drift_state('d97859e7-e55e-4b48-9d51-181814e7fdba', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d97859e7-e55e-4b48-9d51-181814e7fdba', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, financial_sector).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, property_owners).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, labor_dependent_populations).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, small_producers).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, subsistence_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, policy_maker_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the narrative infrastructure that presents market logic as inevitable and natural. Funds think tanks, chairs departments of economics, sponsors media narratives, and produces research justifying market outcomes as equilibrium states. Actively funds counter-narratives to alternative economic arrangements (cooperative ownership, state provision, commons-based allocation). Collects rents from the arrangement via favorable tax, regulatory, and monetary policy.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, financial_sector, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Benefit from the naturalization of property rights, contract enforcement, and profit extraction as baseline institutional structures. Are insulated from questioning whether these arrangements are inevitable or desirable. Participate in institutional capture (regulatory capture, university governance, think tank boards) that maintains the closed framing of what 'the market' is and whether alternatives are thinkable.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, multinational_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from the naturalization of private property as the only legitimate form of resource control. Land, capital, and claims are treated as natural objects to be owned privately; common ownership, stewardship, and usufruct arrangements are presented as exotic or failed alternatives rather than viable historical systems.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, property_owners, beneficiary,
    organized, generational, mobile, national).

% Must accept market-determined wages and conditions as natural and inevitable. Have access to alternative economic arrangements (worker cooperatives, mutual aid, commons-based production) suppressed in mainstream discourse and policy. Face active reframing: alternatives are labeled inefficient, utopian, or unscalable by the same institutions that benefit from market naturalization. Experience the constraint as having no exit because 'there is no alternative.'
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, labor_dependent_populations, payer,
    powerless, biographical, trapped, global).

% Compete in markets structured to favor scale and capital intensity. Are told this is natural competitive selection rather than a designed arrangement. Cannot access the institutional supports (narrative, regulatory, financial) that large firms use to naturalize the market system. Exit via cooperative or commons-based production requires swimming against institutional framing and policy.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, small_producers, payer,
    moderate, biographical, constrained, local).

% Many are forcibly integrated into market economies by colonial and post-colonial policy. Their own institutions of resource management and production are not just suppressed but erased from the possibility space—intellectually through anthropology and development theory that marks them as primitive, institutionally through legal systems that do not recognize commons or usufruct claims. Identity fusion with their own institutions makes exit from market logic psychologically and socially incoherent.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, subsistence_communities, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, subsistence_communities, excluded).

% Produce sustained scholarly critique of market naturalization—institutional economics, ecological economics, feminist economics, Marxist political economy. Are systematically excluded from: mainstream journal publication, university funding, media platforms, policy advisory roles. Must front-load their critique with apologies for challenging 'established theory.' Their absence from the agenda-setting table is what the enforcement machinery exists to maintain.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, heterodox_economists, excluded,
    moderate, biographical, constrained, national).

% Cooperative ownership structures, commons-based resource management, state provision, planned economies, gift economies—these institutional forms historically existed and continue to exist in pockets. They are not forgotten but actively reframed as marginal, failed, or impossible. Their existence is not suppressed; their viability and scalability are suppressed.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, alternative_institutional_models, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(market_as_natural_default__beneficiary_maintained_reading, alternative_institutional_models).

% Operate within the constraint's framing: they design policy to tweak markets, not to question whether the market is the appropriate coordinating mechanism for a given domain. Central banks, finance ministries, and trade negotiators are staffed by economists trained in the naturalized framing. Their capacity to imagine non-market alternatives for healthcare, education, housing, or resource allocation is structurally limited by the institutional capture.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, policy_maker_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, policy_maker_institutions, payer).

% Amplify the market-as-natural narrative through: framing economic crises as failures of regulation rather than design, covering labor issues as supply-and-demand stories rather than power dynamics, treating privatization as technical improvement rather than resource redistribution. Are funded by advertising from financial and corporate beneficiaries. Frame the naturalization as neutral reporting.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, media_systems, agenda_setter,
    institutional, generational, mobile, global).

% Sees the full structural picture: market naturalization is neither natural nor accidental, but an ongoing institutional project. Identifies which institutions defend it, by what mechanisms, at what cost to non-beneficiaries. Can model what the world would look like if this constraint vanished—what alternatives would surface, what rents would decompress, what arrangements would re-emerge.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, observer_frame, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(market_as_natural_default__beneficiary_maintained_reading, observer_frame).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__beneficiary_maintained_reading, financial_sector).
narrative_ontology:fixing_cost_class(market_as_natural_default__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, universal language (price, profit, property) for allocating resources across heterogeneous populations without explicit negotiation or centralized planning authority. Solves the problem: how to coordinate production and distribution at global scale when feudal authority is gone and democratic negotiation is infeasible. This coordination function is genuine and was historically necessary.
% TRANSFER_FUNCTION: Moves control over production, capital accumulation, and resource allocation to property owners and financial intermediaries. Moves labor and rents to capital holders. Extracts from workers (via wages below productivity), small producers (via scale disadvantage), and subsistence communities (via forced commodification). Concentrates in financial sector, multinational corporations, and property owners.
% ABSENT_VOICES: Heterodox economists (institutional, ecological, feminist, Marxist traditions) who argue markets are one mechanism among many, not natural law. Cooperative movements and commons stewardship traditions arguing for viable alternatives. Subsistence producers whose institutions are erased. These voices would argue: the founding problem is solved; alternatives are proven viable; naturalization is ideological cover for rent extraction. They are excluded from mainstream academic funding, policy advising, and media platforms—systematically, not accidentally.
% DISAPPEARANCE_RATIONALE: If beneficiary-maintained naturalization stopped (if policy recovered the capacity to ask 'is the market appropriate for this domain?' for healthcare, education, housing; if media reported alternatives as viable; if heterodox economics received funding equal to orthodox; if legal/financial barriers to cooperatives were removed), the constraint would collapse within a decade. Cooperative ownership would explode. State provision would return to education and healthcare. Rents flowing to finance would compress dramatically. The naturalization is what makes alternatives seem impossible; removing it makes them inevitable. The world would rearrange because the entire institutional logic depends on market naturalization being unchallengeable.
% FOUNDING_PROBLEM: In the 18th–19th centuries, feudal authority systems collapsed and mercantilist coordination mechanisms broke down. Global trade and production required a coordinating mechanism that didn't depend on personal loyalty, heredity, or centralized planning. Markets emerged as a solution: price signals aggregate distributed knowledge without requiring anyone to plan the whole system. The problem was real and acute. The constraint solved it.
% FOUNDING_PROBLEM_CORROBORATION: We now have multiple working coordination mechanisms: state bureaucracies (healthcare, education, infrastructure), international institutions (UN, trade agreements), digital platforms (open source, Wikipedia), cooperative enterprises (Mondragon, credit unions), and blended systems (Nordic economies). The founding problem—how to coordinate without feudal authority—was acute in 1750. It is now solved. Market mechanisms are one of the solutions, not the inevitable solution. Historical economists document this shift: Polanyi on the embedding of markets in social systems; Hirschman on the mutual dependence of markets and state; Zuboff on the emergence of new extraction mechanisms within market naturalization. Comparative institutional analysis by economists outside the mainstream (North, Acemoglu, Ostrom) all document that viable alternatives exist and operate successfully at scale. The persistence of market naturalization as dominant framing is not explained by the founding problem's continued acuteness—it is explained by beneficiary maintenance.
narrative_ontology:disappearance_verdict(market_as_natural_default__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__beneficiary_maintained_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__beneficiary_maintained_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__beneficiary_maintained_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.32 in 1980) when alternatives were still partially visible in policy discourse (labor unions, state industries, cooperative movements were still seen as live options). It rises as institutional capture deepens: financial deregulation, neoliberal policy shifts, consolidation of media ownership, and defunding of public institutions all operate within the beneficiary-maintained reading. The measurement grid shows that at the INDIVIDUAL level, accessibility collapse rises from 0.58 to 0.72—workers individually perceive fewer alternatives. At STRUCTURAL level, it's already high and stays high (0.68→0.76) because the system itself naturalizes market logic. Suppression rises across all levels (individual 0.38→0.58, structural 0.46→0.64), peaking around 2008 when the financial crisis could have shattered market naturalization, but beneficiaries mounted an active counter-narrative (too big to fail, market failures require more markets, austerity is inevitable). Theater ratio climbs (0.38→0.51) as the machinery of naturalization becomes more sophisticated: peer-reviewed journals that exclude heterodox work are presented as neutral science; policy recommendations from beneficiary-funded economists are framed as technical expertise; 'innovation' in finance is covered as beneficial disruption rather than new extractive mechanisms. The 2008 peak and slight decline reflects the moment when naturalization was most vulnerable (crisis visible, alternatives briefly thinkable) and beneficiaries had to intensify defense—hence theater rises sharply. Post-2008 stabilization at 0.48 extractiveness reflects a quasi-equilibrium: suppression and theater hold alternatives at bay; extractiveness cannot rise much further without provoking mass exit or coalition-building, so beneficiaries stabilize the constraint rather than ratchet it tighter.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (financial sector, multinational corporations, property owners), the market is a natural, efficient, inevitable system—deviations from it are failures that require more market (deregulation, privatization). They experience the constraint as having no enforcement cost because they control the narrative infrastructure; it feels like natural fact. From the payer seats (labor, small producers), the constraint appears as active enforcement: media that never questions market logic, policy that treats alternatives as unthinkable, educational systems that teach market fundamentals as truth rather than framework. The excluded seats (heterodox economists, cooperative movements) see the full machinery: funded PR, institutional capture, suppression of alternatives. The engine computes these seat-specific classifications from power/exit/directionality: beneficiaries at high power with arbitrage exit and low d→low/negative χ (they experience positive subsidy); payers at low power with trapped/identity-locked exit and high d→high χ (extraction is felt as oppressive inevitability); excluded seats experience observational divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Financial sector d≈0.05 (full beneficiary): controls the narrative machinery, arbitrage-capable (can move capital if domestic policy shifts), institutional power. Multinational corporations d≈0.1: benefit from naturalization of property and contract, but some labor/regulatory pressure reduces pure beneficiary status. Property owners d≈0.25: benefit substantially but have political/social constraints (legitimacy questions in some regions); mobile exit but ideologically locked to property defense. Labor d≈0.85: trapped and identity-locked (their survival depends on markets); high suppression; alternatives suppressed. Small producers d≈0.8: constrained exit (cooperatives and commons alternatives exist but require institutional support they don't have); moderate power but low relative to market-dominating firms. Subsistence communities d≈0.95: identity-locked; alternatives not just suppressed but erased; powerless. Directionality overrides: none needed—the derivation chain (beneficiary/victim + power + exit) produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is LIVE tangled_rope, not a false mountain. The founding problem (feudalism → market coordination) was real and acute in 1750. It is now dead (we have multiple coordination mechanisms; markets are no longer novel). The founding_problem_status is 'dead' and disappearance_verdict is 'world_rearranges'—this is the classic mandatrophy signature. Beneficiaries maintain the constraint not because it still solves the founding problem, but because it distributes rents in their favor. The measuring difference between mandatrophy_resolved=false (the constraint persists despite dead founding problem) and theater_ratio rising (performance becoming the primary function) is visible in the grid: suppression rises despite stable/declining extractiveness (the rents don't inflate; the defense effort does). The constraint can be classified as tangled_rope (genuine coordination kernel + asymmetric extraction + active enforcement) AND marked for mandatrophy review: the coordination benefit has decoupled from the distribution, and beneficiaries now defend primarily the distribution, not the coordination. A policy intervention that recovered alternatives (cooperative financing, public banking, worker ownership) would collapse the constraint; it persists only because that recovery is suppressed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_intent_vs_structural_outcome,
    'Do beneficiaries consciously and deliberately maintain market naturalization as an ideological project, or do they genuinely believe in it and maintain it out of conviction rather than strategic interest?',
    'Internal documentation from beneficiary institutions (leaked strategy memos, archived planning documents, oral histories from insiders). Pattern matching on rhetorical behavior: deliberate decoupling of private belief and public narrative is a strong indicator of strategic maintenance.',
    'If deliberate: the constraint is a snare with ideological cover—classification demotes to pure extraction. If conviction-driven: the constraint is a genuine tangled_rope with active-but-sincere enforcement. If mixed: mandatrophy is acute (beneficiaries defend a dead founding problem believing it''s alive).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_intent_vs_structural_outcome, empirical, 'Whether market naturalization is actively maintained out of strategic interest or sincere conviction that markets are natural.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression primarily internalized—individuals believe no alternatives are thinkable—or is it structural—barriers make alternatives actually inaccessible?',
    'Post-exit trajectory: when individuals exit suppressive arrangements and gain access to alternative narratives and institutions (cooperative workplaces, heterodox education, autonomous communities), does suppression persist? Rapid recovery of alternative-considering capacity = internalized; persistence = structural.',
    'If internalized: suppression is psychological, not just institutional; higher effective suppression than the scalar metric suggests (target carries it with them after exit). If structural: removing barriers (funding heterodox economics, legal/financial support for cooperatives, media pluralism) directly collapses suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of alternatives is internalized (psychological) or structural (institutional barriers).').

omega_variable(
    kernel_reading_alternative_viability,
    'Are alternative institutional forms (cooperatives, commons, state provision) actually scalable and functionally viable at the scale markets operate, or are they genuine niche solutions unsuitable for global coordination?',
    'Comparative analysis of performance and cost for hybrid systems that blend market mechanisms, state provision, and cooperative/commons components (e.g., Nordic economies, Mondragon industrial complex, municipal utilities). Scaled trials where barriers are removed and alternatives receive equivalent institutional/financial support.',
    'If viable: the constraint''s suppression of alternatives is unjustified by structural necessity; alternatives are suppressed purely to maintain beneficiary rents (snare reclassification). If niche: market naturalization has partial truth; constraint is correctly classified as tangled_rope (necessary coordination with asymmetric benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_viability, conceptual, 'Whether alternative institutional forms are genuinely scalable or constrained to specific contexts.').

omega_variable(
    founding_problem_genuine_revival,
    'Could the founding problem (coordination without centralized authority) re-emerge under conditions of state collapse, digital fragmentation, or post-centralization? Is the constraint poised to become functionally necessary again?',
    'Historical scenario analysis and institutional fragmentation studies. If state-level authority collapses or digital networks fundamentally fragment, do markets re-emerge as the primary coordination mechanism?',
    'If re-emergence likely: the constraint''s persistence is forward-looking, not merely beneficiary-maintained; it would reclassify as scaffold (temporary support for a transition now complete, but institutionally poised for re-emergence). If re-emergence unlikely: the constraint''s persistence is purely beneficiary-maintained and mandatrophy is complete.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_genuine_revival, conceptual, 'Whether the founding problem (coordination without centralized authority) could re-emerge and justify market naturalization prospectively.').

omega_variable(
    kernel_reading_contention,
    'Is the lapsed_alternative_reading (market dominance results from historical forgetting without active defense) empirically distinguishable from this beneficiary_maintained_reading, or are they describing the same constraint from different temporal angles?',
    'Documentary evidence of active defense mechanisms (funded narratives, institutional capture) vs. passive forgetting. If beneficiary-funded think tanks and academic programs emerged BEFORE alternatives faded from public discourse, active defense is primary. If forgetting preceded active defense machinery, forgetting is primary.',
    'If distinguishable (active defense is primary): this reading''s classification as tangled_rope + mandatrophy is correct. If the readings describe the same constraint with different causal framings: the two readings coexist_with rather than influencing each other; they are perspectival rather than structural variants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contention, conceptual, 'Whether active beneficiary maintenance and passive historical forgetting are distinguishable mechanisms or descriptive frames for the same constraint.').

omega_variable(
    identity_lock_reversibility,
    'For agents locked into market participation via identity fusion (labor-dependent populations whose self-concept is ''I am a wage earner''), is the identity lock structurally irreversible or contingent on continuous reinforcement from narrative institutions?',
    'Longitudinal study of individuals who exit wage-dependent markets and gain sustained access to cooperative/autonomous production: does identity reconstitute around non-wage labor? If yes, lock is reversible; if no, lock is identity-fused and quasi-irreversible.',
    'If reversible: removing the constraint (funding alternatives, recovering cooperative tradition) allows rapid identity reconstruction and mass exit. If irreversible: constraint persists even if enforcement machinery is dismantled, because targets'' self-concept enforces it internally.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity lock to market participation is reversible when institutional reinforcement is removed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__beneficiary_maintained_reading, 1980, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1980, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 1980, 0.38).
narrative_ontology:measurement(mark_tr_t1990, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 1990, 0.44).
narrative_ontology:measurement(mark_tr_t2000, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2000, 0.48).
narrative_ontology:measurement(mark_tr_t2008, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2008, 0.55).
narrative_ontology:measurement(mark_tr_t2016, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2016, 0.51).
narrative_ontology:measurement(mark_tr_t2026, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2026, 0.51).

% Extraction over time
narrative_ontology:measurement(mark_be_t1980, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 1980, 0.32).
narrative_ontology:measurement(mark_be_t1990, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(mark_be_t2000, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement(mark_be_t2008, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2008, 0.51).
narrative_ontology:measurement(mark_be_t2016, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2016, 0.48).
narrative_ontology:measurement(mark_be_t2026, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2026, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1980, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 1980, 0.42).
narrative_ontology:measurement(mark_su_t1990, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 1990, 0.48).
narrative_ontology:measurement(mark_su_t2000, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(mark_su_t2008, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2008, 0.66).
narrative_ontology:measurement(mark_su_t2016, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2016, 0.62).
narrative_ontology:measurement(mark_su_t2026, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2026, 0.62).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1980, tn=2026
narrative_ontology:measurement(mark_grid_01, market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse(class), 1980, 0.65).
narrative_ontology:measurement(mark_grid_02, market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse(class), 2026, 0.74).
narrative_ontology:measurement(mark_grid_03, market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse(individual), 1980, 0.58).
narrative_ontology:measurement(mark_grid_04, market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse(individual), 2026, 0.72).
narrative_ontology:measurement(mark_grid_05, market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse(organizational), 1980, 0.62).
narrative_ontology:measurement(mark_grid_06, market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse(organizational), 2026, 0.76).
narrative_ontology:measurement(mark_grid_07, market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse(structural), 1980, 0.68).
narrative_ontology:measurement(mark_grid_08, market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse(structural), 2026, 0.76).
narrative_ontology:measurement(mark_grid_09, market_as_natural_default__beneficiary_maintained_reading, resistance(class), 1980, 0.58).
narrative_ontology:measurement(mark_grid_10, market_as_natural_default__beneficiary_maintained_reading, resistance(class), 2026, 0.62).
narrative_ontology:measurement(mark_grid_11, market_as_natural_default__beneficiary_maintained_reading, resistance(individual), 1980, 0.48).
narrative_ontology:measurement(mark_grid_12, market_as_natural_default__beneficiary_maintained_reading, resistance(individual), 2026, 0.44).
narrative_ontology:measurement(mark_grid_13, market_as_natural_default__beneficiary_maintained_reading, resistance(organizational), 1980, 0.56).
narrative_ontology:measurement(mark_grid_14, market_as_natural_default__beneficiary_maintained_reading, resistance(organizational), 2026, 0.58).
narrative_ontology:measurement(mark_grid_15, market_as_natural_default__beneficiary_maintained_reading, resistance(structural), 1980, 0.52).
narrative_ontology:measurement(mark_grid_16, market_as_natural_default__beneficiary_maintained_reading, resistance(structural), 2026, 0.54).
narrative_ontology:measurement(mark_grid_17, market_as_natural_default__beneficiary_maintained_reading, stakes_inflation(class), 1980, 0.55).
narrative_ontology:measurement(mark_grid_18, market_as_natural_default__beneficiary_maintained_reading, stakes_inflation(class), 2026, 0.66).
narrative_ontology:measurement(mark_grid_19, market_as_natural_default__beneficiary_maintained_reading, stakes_inflation(individual), 1980, 0.48).
narrative_ontology:measurement(mark_grid_20, market_as_natural_default__beneficiary_maintained_reading, stakes_inflation(individual), 2026, 0.64).
narrative_ontology:measurement(mark_grid_21, market_as_natural_default__beneficiary_maintained_reading, stakes_inflation(organizational), 1980, 0.52).
narrative_ontology:measurement(mark_grid_22, market_as_natural_default__beneficiary_maintained_reading, stakes_inflation(organizational), 2026, 0.68).
narrative_ontology:measurement(mark_grid_23, market_as_natural_default__beneficiary_maintained_reading, stakes_inflation(structural), 1980, 0.58).
narrative_ontology:measurement(mark_grid_24, market_as_natural_default__beneficiary_maintained_reading, stakes_inflation(structural), 2026, 0.68).
narrative_ontology:measurement(mark_grid_25, market_as_natural_default__beneficiary_maintained_reading, suppression(class), 1980, 0.44).
narrative_ontology:measurement(mark_grid_26, market_as_natural_default__beneficiary_maintained_reading, suppression(class), 2026, 0.64).
narrative_ontology:measurement(mark_grid_27, market_as_natural_default__beneficiary_maintained_reading, suppression(individual), 1980, 0.38).
narrative_ontology:measurement(mark_grid_28, market_as_natural_default__beneficiary_maintained_reading, suppression(individual), 2026, 0.58).
narrative_ontology:measurement(mark_grid_29, market_as_natural_default__beneficiary_maintained_reading, suppression(organizational), 1980, 0.42).
narrative_ontology:measurement(mark_grid_30, market_as_natural_default__beneficiary_maintained_reading, suppression(organizational), 2026, 0.62).
narrative_ontology:measurement(mark_grid_31, market_as_natural_default__beneficiary_maintained_reading, suppression(structural), 1980, 0.46).
narrative_ontology:measurement(mark_grid_32, market_as_natural_default__beneficiary_maintained_reading, suppression(structural), 2026, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__beneficiary_maintained_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__beneficiary_maintained_reading, 0.18).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default__hybrid_amnesia_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default__lapsed_alternative_reading).

% DUAL FORMULATION NOTE:
% The kernel 'market_as_natural_default' decomposes into three readings, each with distinct structural properties and ε values. This reading (beneficiary_maintained) treats naturalization as actively engineered by identifiable beneficiaries; it emphasizes narrative work and institutional capture. The hybrid_amnesia_reading treats initial forgetting as creating openness to capture. The lapsed_alternative_reading treats forgetting itself as the primary mechanism, without active beneficiary defense. All three share the same referent (the standing arrangement: market allocation as natural default) but author different mechanisms for its persistence. The engine computes per-reading classifications; this constraint instantiates the beneficiary_maintained mechanism and should not be averaged or fused with sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_as_natural_default__beneficiary_maintained_reading, powerless, 0.92).
constraint_indexing:directionality_override(market_as_natural_default__beneficiary_maintained_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
