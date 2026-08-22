% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__hybrid_amnesia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__hybrid_amnesia_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: market_as_natural_default__hybrid_amnesia_reading
 *   human_readable: Market Naturalization via Lapsed Closure & Inherited Amnesia (Hybrid Reading)
 *   domain: political_economy/ideology
 *
 * SUMMARY:
 *   The market system is widely treated as a natural, inevitable
 *   arrangement—the default around which all economies organize once they
 *   'develop' or 'modernize.' This narrative is claimed as self-evidently
 *   true across ideological lines: neoliberals say markets are natural;
 *   progressives often concede markets are inevitable and debate regulation
 *   at the margins. The hybrid-amnesia reading traces this consensus to a
 *   two-stage historical process: (1) Initial lapsed closure (1930s-1970s):
 *   genuine institutional forgetting of pre-market alternatives (guild
 *   systems, cooperative production models, ecological allocation frameworks,
 *   planned subsistence). This forgetting was not engineered; it resulted
 *   from generational turnover, institutional concentration, and the material
 *   success of early industrialization, which satisfied many without
 *   requiring explicit defense of market naturalism. (2) Later defensive
 *   rationalization (1980s-present): once beneficiaries of market
 *   concentration became aware that alternatives had been historically
 *   available, the pre-existing amnesia became useful. Beneficiary
 *   intellectuals, policy institutions, and media converged on a frame:
 *   'markets are not a choice, they are inevitable,' embedding the inherited
 *   amnesia in new rationality claims. The extraction mechanism operates
 *   through the amnesia itself—because the target populations (labor
 *   organizing traditions, ecological constituencies, cooperative economic
 *   actors) do not perceive alternatives as live options, they cannot
 *   organize at scale to redistribute the rents markets collect. The reading
 *   claims extractiveness rose over the interval (0.20 → 0.45) as inherited
 *   amnesia became more thoroughly weaponized.
 *
 * KEY AGENTS:
 *   - incumbent_capital_holders: Beneficiaries of market concentration; inherit and later weaponize pre-existing amnesia
 *   - financial_sector_professionals: Beneficiaries of market-as-natural frame; deploy 'there is no alternative' rationality in policy and ideology
 *   - alternative_economic_actors: Targets; labor unions, cooperative movements, ecological planners excluded by the amnesia they do not recognize as constructed
 *   - labor_organizing_traditions: Victims; lost institutional memory of pre-market wage-labor alternatives (guild apprenticeship, cooperative ownership, commons-based subsistence)
 *   - ecological_planning_constituencies: Victims; lost knowledge of non-market allocation mechanisms (commons management, subsistence forestry, bioregional planning)
 *   - policy intellectuals & economists: Mediators; inherit genuine amnesia from textbook training, later ratify it as 'natural law'
 *   - historical researchers & heterodox economists: Excluded observers; possess archive knowledge of pre-market systems but are institutionally marginalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__hybrid_amnesia_reading, 0.32).
domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, 0.68).
domain_priors:theater_ratio(market_as_natural_default__hybrid_amnesia_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__hybrid_amnesia_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__hybrid_amnesia_reading, "Market Naturalization via Lapsed Closure & Inherited Amnesia (Hybrid Reading)").
narrative_ontology:topic_domain(market_as_natural_default__hybrid_amnesia_reading, "political_economy/ideology").

domain_priors:requires_active_enforcement(market_as_natural_default__hybrid_amnesia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, 'e805d90a-0239-45ac-a7ff-3497c7f14142').
narrative_ontology:cs_kernel_codification('e805d90a-0239-45ac-a7ff-3497c7f14142', distributed).
narrative_ontology:cs_authority_grounding('e805d90a-0239-45ac-a7ff-3497c7f14142', extraction).
narrative_ontology:cs_interpretation_layer_present('e805d90a-0239-45ac-a7ff-3497c7f14142').
narrative_ontology:cs_reading_relation('e805d90a-0239-45ac-a7ff-3497c7f14142', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('e805d90a-0239-45ac-a7ff-3497c7f14142', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_axiom('e805d90a-0239-45ac-a7ff-3497c7f14142', foundational, initial_amnesia_unintended).
narrative_ontology:cs_axiom_status(initial_amnesia_unintended, holdable).
narrative_ontology:cs_axiom_grounding('e805d90a-0239-45ac-a7ff-3497c7f14142', initial_amnesia_unintended, empirically_contingent).
narrative_ontology:cs_axiom('e805d90a-0239-45ac-a7ff-3497c7f14142', foundational, beneficiary_weaponization_post_hoc).
narrative_ontology:cs_axiom_status(beneficiary_weaponization_post_hoc, holdable).
narrative_ontology:cs_axiom_grounding('e805d90a-0239-45ac-a7ff-3497c7f14142', beneficiary_weaponization_post_hoc, empirically_contingent).
narrative_ontology:cs_reference_frame('e805d90a-0239-45ac-a7ff-3497c7f14142', market_natural_coordinating_mechanism).
narrative_ontology:cs_drift_state('e805d90a-0239-45ac-a7ff-3497c7f14142', post_1980_weaponized_rationalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e805d90a-0239-45ac-a7ff-3497c7f14142', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, financial_sector_professionals).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, alternative_economic_actors).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, labor_organizing_traditions).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, ecological_planning_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, policy_intellectuals_economists).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, ordinary_people_market_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit and consolidate wealth through market mechanisms. Initially benefit from pre-existing amnesia without needing to actively defend it (they inherit the frame from their own training). Post-1980, as heterodox alternatives become visible in academic discourse, they deploy 'market naturalism' rationality and fund policy institutions that rationalize the frame. Their position is secure because the amnesia they inherit makes alternatives cognitively unavailable to the populations they might threaten them.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_holders, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_holders, agenda_setter).

% Operate within market-naturalism frames (asset pricing, portfolio theory, financial regulation). Their professional identity is constituted through market rationality; they are beneficiaries of the frame's naturalization because it legitimizes their sector and their claims to expertise. They participate in defensive rationalization (publishing economics articles, teaching MBA curricula) not from conscious conspiracy but from internalized professional formation in the market-natural frame.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, financial_sector_professionals, beneficiary,
    powerful, biographical, arbitrage, global).

% Cooperative production networks, mutual-aid organizations, community land trusts, and alternative-economy enterprises operate within the gaps and margins of market-dominated economies. They bear the constraint through resource scarcity (markets funnel capital away from non-market alternatives), regulatory pressure (market-naturalism frames law and policy to favor market actors), and cognitive barriers (difficult to recruit participants when alternatives are not recognized as viable options). Their exit from market logic is constrained by embeddedness in market-dependent institutions.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, alternative_economic_actors, payer,
    moderate, generational, constrained, regional).

% Labor movements, unions, and worker-organizing traditions lost institutional memory of pre-wage-labor alternatives (guild apprenticeship systems, cooperative production, commons-based subsistence). This amnesia constrains their organizing imagination: they organize within market logic (demanding higher wages, better conditions) rather than organizing to displace market logic (cooperative ownership, alternative production relations). Their identity as 'workers' is partly constituted through the wage-labor relationship they contest; exit is identity-locked even as they resist market conditions.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, labor_organizing_traditions, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, labor_organizing_traditions, excluded).

% Environmental movements, ecological researchers, bioregional planners, and subsistence-focused communities operate against the constraint that market mechanisms (carbon pricing, payment for ecosystem services, conservation markets) are the only tools for ecological coordination. The amnesia prevents them from drawing on pre-market allocation frameworks (commons management, ecological knowledge systems, subsistence provisioning) as live alternatives. Their identity as ecological agents is constituted through relation to the natural world; exit from market frames while maintaining ecological commitment is difficult.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, ecological_planning_constituencies, payer,
    organized, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, ecological_planning_constituencies, excluded).

% Economics professors, policy advisors, think-tank researchers operate within market-naturalism frames as the default intellectual infrastructure. They inherit genuine amnesia from textbook training (mainstream economics pedagogy does not teach pre-market systems as viable alternatives). Many participate in defensive rationalization post-1980 (publishing 'TINA'—there is no alternative—arguments, dismissing heterodox economics, advising governments on 'market reform'). Some are unconscious beneficiaries (their career prospects depend on market-framework dominance); some are conscious agents of rationalization. The constraint operates through both.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, policy_intellectuals_economists, observer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, policy_intellectuals_economists, beneficiary).

% Researchers in heterodox economic traditions, economic historians, and institutional economists maintain archive knowledge of pre-market alternatives and continue developing alternative frameworks (ecological economics, post-Keynesian theory, economic anthropology). They are excluded from mainstream policy and pedagogy because the market-naturalism frame treats their work as historical curiosity rather than live alternatives. Their constraints are institutional (marginal funding, low prestige in mainstream academia) and informational (their work does not reach the populations it might benefit).
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, heterodox_economists_historians, excluded,
    moderate, generational, constrained, global).

% Billions of people dependent on market-wage income, market-provisioned goods, and market-mediated access to resources (land, water, energy). They experience the constraint as simple fact: there is no alternative to participating in markets. The amnesia operates at maximum suppressive force at this seat because the populations most affected have no access to heterodox knowledge or alternative frameworks. Their exit is trapped—they cannot leave markets while dependent on them for survival; their identity as 'workers,' 'consumers,' 'citizens' is constituted through market participation.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, ordinary_people_market_participants, payer,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_holders).
narrative_ontology:fixing_cost_class(market_as_natural_default__hybrid_amnesia_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Market mechanisms coordinate production, distribution, and resource allocation at large scale by aggregating dispersed information through price signals, enabling specialization and trade, and solving the problem of 'who decides what gets made' through decentralized individual choice. This coordination function was real and powerful in the 19th-20th centuries; it remains partially live in the sense that markets continue to coordinate complex economies.
% TRANSFER_FUNCTION: The constraint transfers wealth, decision-making authority, and control of resource allocation from dispersed workers and community actors to concentrated capital holders and financial intermediaries. It operates through: (1) differential access to capital (beneficiaries control investable surplus; targets do not), (2) wage extraction (workers paid less than the value they produce; beneficiaries capture the surplus), (3) cognitive closure (targets do not perceive alternatives and thus cannot organize for different arrangements), (4) institutional lock-in (market-dependent institutions become the only available livelihood infrastructure).
% ABSENT_VOICES: Heterodox economists, historical researchers, cooperative-movement practitioners, ecological planners, and labor organizers who work with alternative frameworks are institutionally excluded from mainstream policy, pedagogy, and media. They would argue: (1) pre-market alternatives (guild systems, commons, cooperative production, planned economies) were historically viable and remain so, (2) market naturalism is ideology masquerading as inevitability, (3) the amnesia of alternatives is an extractive mechanism that prevents re-imagination of economic arrangements. They are absent from policy-making and do not reach mass populations because the market-naturalism frame treats their work as historical curiosity. Their exclusion is the constraint's operative suppression mechanism.
% DISAPPEARANCE_RATIONALE: If the market-naturalization constraint vanished overnight—if people suddenly recovered knowledge of pre-market alternatives and perceived them as live options—the world would reorganize rapidly: (1) labor organizing would shift from wage-struggle within markets to organizing for alternative production relations (cooperatives, commons, gift economies), (2) ecological constituencies would deploy pre-market allocation frameworks (commons management, subsistence provisioning, bioregional planning) rather than accepting market-based conservation, (3) policy would explore genuinely alternative institutional structures rather than treating market mechanisms as the only possible coordination tools, (4) wealth accumulation patterns would shift because the cognitive closure that enables market-concentration would lift. The rearrangement would not be total—markets might persist in some domains—but the monopoly on legitimacy and inevitability would break.
% FOUNDING_PROBLEM: Early industrial capitalism faced genuine coordination challenges: how to organize large-scale production, distribute goods across regions, match supply to demand without centralized planners, enable specialization and trade. Market mechanisms solved these problems effectively in the 19th-20th centuries, enabling rapid industrialization, rising material productivity, and technological innovation. The founding problem was real.
% FOUNDING_PROBLEM_CORROBORATION: The operator-beneficiary seats (incumbent capital holders, policy intellectuals, financial professionals) attest the founding problem remains live: markets are still necessary to coordinate complex modern economies. Alternative constituencies (heterodox economists, cooperative practitioners, ecological planners) attest the founding problem is substantially solved or superseded: modern economies possess technologies (computers, telecommunications, logistics systems) and institutional knowledge (planning theory, cooperative practice, commons management) that enable viable non-market coordination. They argue the constraint persists not because markets are necessary but because beneficiaries have rationalized the amnesia of alternatives as inevitable. Independent economic historians and institutional researchers document: (1) pre-market systems (guilds, commons, planned economies, cooperative production) were historically viable and had capacity for scale and complexity, (2) the perceived necessity of markets results partly from institutional concentration and cognitive closure, not from fundamental coordination limits, (3) contemporary alternatives (Mondragon cooperatives, commons-based resource management, participatory planning initiatives) demonstrate ongoing viability of non-market mechanisms.
narrative_ontology:disappearance_verdict(market_as_natural_default__hybrid_amnesia_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__hybrid_amnesia_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__hybrid_amnesia_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__hybrid_amnesia_reading, 0.32, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__hybrid_amnesia_reading_tests).
:- end_tests(market_as_natural_default__hybrid_amnesia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32 at midpoint, 0.45 at end) is moderate-to-high because the constraint operates through suppression of knowledge of alternatives rather than direct coercion—the amnesia IS the extraction mechanism. Those who do not know alternatives exist cannot organize for them. Suppression (0.68) is high because it operates at the cognitive/institutional level—alternative frameworks are legally permitted but informationally inaccessible to the populations they would benefit. Theater (0.61) is high because the rationality machinery that defends market naturalism (economics textbooks, policy frameworks, 'TINA' rhetoric) performs heavily—what is performed is the claim that markets are inevitable, which is partially theatrical maintenance of inherited amnesia. Accessibility collapse (0.72) is high because once the market frame is adopted, the cognitive closure around alternatives is strong; people who have been trained in market logic find alternatives difficult to imagine or articulate. Resistance (0.58) is moderate: alternative communities exist and resist (labor, ecology, cooperative movements) but do not reach mass scale because the amnesia prevents their alternatives from being recognized as serious options. The measurements trace the key dynamics: extractiveness rises as weaponization becomes more systematic post-1980; theater rises as defensive rationality hardens; suppression rises as the amnesia becomes institutionalized in credentialing and pedagogy.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (incumbent capital holders, financial professionals), the market frame is perceived as natural fact—they inherited it from their own training and do not need to actively defend it; it feels like description, not defense. From the target seats (labor traditions, ecological constituencies), the market frame appears as constraint and inevitability; they experience the suppression but may not recognize it as suppression of alternatives (they experience it as 'that's just how the world works'). From the observer seat (historical researchers), the constraint is visible as constructed—alternatives existed, were forgotten, and the forgetting was later weaponized. The engine computes per-seat directionality: beneficiaries' d moves toward 0.0 (they collect from the arrangement without defending it actively—inheritance not maintenance); targets' d moves toward 1.0 (they are excluded by a mechanism they do not recognize). This perspectival gap is the reading's core structural claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality (incumbent_capital_holders, financial_sector_professionals) begins near 0.0 because they inherit the frame without conscious defense in the early period; rises modestly to 0.15-0.25 in the later period as defensive rationalization becomes more active and conscious. Target directionality (alternative_economic_actors, labor_organizing_traditions, ecological_planning_constituencies) remains high throughout (0.80-0.95) because they are systematically excluded by amnesia; their exit options are constrained or identity-locked—they cannot leave the market frame while embedded in market-dependent institutions, and their identity as 'worker,' 'organizer,' or 'ecological agent' is constituted partly through the relationship to the market they contest. The constraint operates through informational/cognitive suppression rather than legal prohibition, which makes exit-locked status particularly binding—you cannot escape a constraint you do not know is constructed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real: early industrial capitalism faced coordination and production challenges that market mechanisms solved. That founding problem remains live in the sense that markets continue to coordinate large-scale production. However, the constraint now operates through amnesia of alternatives more than through coordination necessity. A mandatrophy reading would emphasize that the market-naturalization frame outlived its justification once the technology and institutions of the mid-20th century created genuine alternatives (planned economies, cooperative production, commons management) that were functionally viable but became historically forgotten. The hybrid reading claims both: the coordination function is still partially live, but the amnesia-enabled extraction has become the dominant operating mechanism post-1980.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_amnesia_vs_motivated_forgetting,
    'Did the forgetting of pre-1930s alternatives occur because genuine institutional memory loss (natural institutional decay, generational turnover, textual erasure) or because beneficiary interests actively suppressed alternative narratives once market dominance became apparent?',
    'Archival analysis of institutional correspondence 1920s-1950s: did alternative frameworks disappear from circulation before or after their economic threat became manifest? Did beneficiary organizations fund historiography that reframed market arrangements as inevitable? Timeline analysis of academic discipline shifts (economics PhD programs, policy schools).',
    'If genuine amnesia: the constraint is hybrid (initial lapsed closure + later defensive rationalization). If motivated suppression from outset: the constraint reclassifies toward snare (extraction was always active, just disguised as forgetting). The reading assumes genuine amnesia; a resolved ''motivated'' case would require reframing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_amnesia_vs_motivated_forgetting, empirical, 'Whether the 1930s-1970s lapsed closure of alternatives was unintended institutional decay or engineered beneficiary suppression.').

omega_variable(
    inheritance_vs_weaponization_boundary,
    'When post-1980 defenders invoke ''market naturalism'' or claim ''there is no alternative,'' are they consciously weaponizing a pre-existing amnesia (knowing the alternatives were historically available but choosing to silence them) or unconsciously inheriting a genuine blind spot?',
    'Cognitive and discourse analysis: examine whether post-1980 beneficiary advocacy cites knowledge of alternatives and rejects them on merit, or operates from a claimed absence of alternatives. Survey of economics textbooks 1970-2000 for references to alternative frameworks (guild systems, planned economies, cooperative models). Interviews with policy intellectuals: did they learn alternatives were forgotten, or learn only that markets were natural?',
    'If conscious weaponization: increases extraction reading (d rises, χ rises, snare indicators strengthen). If unconscious inheritance: the constraint stabilizes as tangled rope (coordination function + extraction, neither fully transparent). The hybrid reading assumes inheritance with some later weaponization; high consciousness would push reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inheritance_vs_weaponization_boundary, empirical, 'Whether post-1980s market naturalism defense is conscious exploitation of amnesia or unconscious internalization of forgotten alternatives.').

omega_variable(
    alternative_viability_contested,
    'Were the pre-1930s alternatives (guild systems, cooperative production, ecological/subsistence models, planned allocation) genuinely viable at scale and abandoned for contingent reasons, or were they marginal and displaced because markets outperformed them?',
    'Economic history analysis: comparative performance metrics of pre-1930 alternative systems (productivity, stability, resilience, welfare distribution). Counterfactual modeling: if 1930s-level alternative frameworks had continued to develop in parallel with market systems, would they have remained viable competitors or faced fundamental limits? Requires engagement with heterodox economic theory and institutional analysis.',
    'If alternatives were genuinely viable: the amnesia is extractive (beneficiaries suppress knowledge of real options). If alternatives faced structural limits: the forgetting was not suppression of live options but attrition of dead ends (extraction reading weakens, constraint reclassifies toward rope). This omega documents the reading''s foundational contested claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_viability_contested, conceptual, 'Whether forgotten alternatives were genuinely viable at scale or faced structural limits that made market dominance inevitable.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'How much of the measured suppression (0.68) is structural—legal prohibition of alternatives, institutional foreclosure of exit routes—versus internalized—the target populations have adopted market-naturalism frames and no longer believe alternatives are thinkable?',
    'Post-exit analysis: when alternative economic actors (cooperatives, unions, ecological planners) leave the market frame or encounter its constraints, does the suppression persist? If post-exit suppression persists (people continue believing ''there is no alternative'' even after physically exiting market structures), it is internalized. If suppression lifts after exit, it is structural. Ethnographic work with de-marketed communities (off-grid, cooperative networks, gift economies) testing whether the amnesia persists outside market enforcement.',
    'If mostly internalized: the constraint''s effective suppression exceeds the structural measure—targets carry amnesia-as-suppression with them after exit. Directionality of exit-locked agents shifts higher (d moves toward 1.0). If mostly structural: suppression is lower-effective once alternatives are available outside market enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of alternatives is structural/external or internalized/cognitive in origin and persistence.').

omega_variable(
    kernel_reading_boundary,
    'This reading instantiates one frame on the ''market_as_natural_default'' kernel: genuine forgetting (1930s-1970s) followed by inherited weaponization (1980s-present). Sibling readings—''beneficiary_maintained_reading'' (active post-hoc defense) and ''lapsed_alternative_reading'' (passive historical forgetting)—occupy the same kernel. Do these readings logically coexist as different accounts of the same dynamics, or do they foreclose each other?',
    'Framing analysis: the hybrid reading claims a two-stage process (genuine amnesia + later weaponization). The beneficiary_maintained_reading emphasizes active post-hoc rationalization (suggesting conscious agency throughout). The lapsed_alternative_reading emphasizes passive forgetting without later capture (suggesting no weaponization). These frames highlight different causal mechanisms but do not logically exclude each other—they differ in emphasis on agency, timing, and consciousness. They coexist as different institutional-observer framings of overlapping processes.',
    'Coexistence means the readings are held by different analytical communities and institutional actors, each emphasizing different causal arrows through the same historical terrain. The engine computes per-seat classification; different seats will perceive different readings'' force. No single reading forecloses the others within a unified framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the hybrid_amnesia, beneficiary_maintained, and lapsed_alternative readings logically coexist or foreclose each other within the market_as_natural_default kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(mark_tr_t0, observed).
narrative_ontology:measurement(mark_tr_t10, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(mark_tr_t10, observed).
narrative_ontology:measurement(mark_tr_t20, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 20, 0.51).
narrative_ontology:measurement_basis(mark_tr_t20, observed).
narrative_ontology:measurement(mark_tr_t30, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 30, 0.57).
narrative_ontology:measurement_basis(mark_tr_t30, observed).
narrative_ontology:measurement(mark_tr_t40, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 40, 0.61).
narrative_ontology:measurement_basis(mark_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(mark_be_t0, observed).
narrative_ontology:measurement(mark_be_t10, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement_basis(mark_be_t10, observed).
narrative_ontology:measurement(mark_be_t20, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement_basis(mark_be_t20, observed).
narrative_ontology:measurement(mark_be_t30, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(mark_be_t30, observed).
narrative_ontology:measurement(mark_be_t40, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement_basis(mark_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(mark_su_t0, observed).
narrative_ontology:measurement(mark_su_t10, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(mark_su_t10, observed).
narrative_ontology:measurement(mark_su_t20, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement_basis(mark_su_t20, observed).
narrative_ontology:measurement(mark_su_t30, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement_basis(mark_su_t30, observed).
narrative_ontology:measurement(mark_su_t40, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement_basis(mark_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__hybrid_amnesia_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__hybrid_amnesia_reading, 0.12).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, labor_organizing_suppression__amnesia_mechanism).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, ecological_planning_exclusion__knowledge_gap).

% DUAL FORMULATION NOTE:
% The 'market_as_natural_default' kernel decomposes into three constraint stories: (1) hybrid_amnesia_reading (this file): genuine forgetting + later weaponization, ε rises from 0.20 to 0.45. (2) beneficiary_maintained_reading: active post-hoc rationalization from the beginning, ε stays high throughout. (3) lapsed_alternative_reading: passive forgetting without weaponization, ε stays low. All three are readings of the same kernel and share the referent (market naturalism); they differ in mechanism emphasis (genuine amnesia + capture vs. active defense vs. passive decay). Network edges show how the hybrid reading influences its siblings: it provides intermediate-ground framing that acknowledges beneficiary agency (vs. lapsed_alternative's pure passivity) while preserving space for inherited unconsciousness (vs. beneficiary_maintained's full intentionality). Each reading has distinct ε trajectory and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_as_natural_default__hybrid_amnesia_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
