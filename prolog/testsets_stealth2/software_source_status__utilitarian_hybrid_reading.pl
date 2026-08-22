% ============================================================================
% CONSTRAINT STORY: software_source_status__utilitarian_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__utilitarian_hybrid_reading, []).

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
 *   constraint_id: software_source_status__utilitarian_hybrid_reading
 *   human_readable: Utilitarian Hybrid Reading of Software Source Status — Context-Dependent Licensing Allocation
 *   domain: economic/political/technological
 *
 * SUMMARY:
 *   This story instantiates the utilitarian_hybrid_reading of the
 *   software_source_status kernel: the claim that software licensing should
 *   maximize aggregate welfare, with open and proprietary models each serving
 *   the contexts where they produce the most total value. The standing
 *   arrangement under contest — the referent epsilon is authored against — is
 *   the actual mixed ecosystem as it evolved from 1998 to 2026: proprietary
 *   vendors collecting license and subscription revenue from buyers with
 *   costly exits, alongside an open-source infrastructure commons (operating
 *   systems, databases, build tooling) sustained by corporate contribution
 *   and individual labor. Assessed by this reading's own lights, the
 *   arrangement contains no categorical injustice: extraction is real but
 *   bounded and context-dependent — concentrated where switching costs trap
 *   buyers — and partially self-corrects as open substitutes mature.
 *   CONSTRAINT FAMILY: this is one of four readings of the same kernel
 *   (siblings: freedom_imperative_reading, pragmatic_development_reading,
 *   property_rights_reading), each authoring its own epsilon over the
 *   identical referent; they are separate constraints linked via
 *   network.affects_constraints, not averaged here. KEY AGENTS (by structural
 *   relationship): - proprietary_software_vendors: Agenda-setting beneficiary
 *   (institutional/arbitrage) — sets licensing terms, collects the fee stream
 *   - locked_in_enterprise_licensees: Primary target (powerful/trapped) —
 *   bears the largest fee and audit burden, cannot coordinate exit -
 *   small_business_software_buyers: Secondary target (moderate/constrained) —
 *   pays list prices without leverage - independent_downstream_developers:
 *   Target with partial subsidy (moderate/constrained) — pays platform cuts,
 *   rides ecosystems - open_source_foundations: Beneficiary
 *   (organized/constrained) — receives the contribution stream the hybrid
 *   channels - open_source_contributors: Dual-positioned labor input
 *   (moderate/mobile) — gains career capital, bears maintenance burden -
 *   public_sector_procurement_offices: Excluded voice
 *   (institutional/constrained) — prefers open formats, rarely holds a seat -
 *   competition_regulators: Analytical observer (institutional/analytical) —
 *   sees the full structure
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: agenda-setting beneficiary (institutional/arbitrage) — controls licensing terms and collects the fee stream
 *   - locked_in_enterprise_licensees: primary target (powerful/trapped) — bears the heaviest fees and audit exposure, exit blocked by integration depth
 *   - small_business_software_buyers: secondary target (moderate/constrained) — pays without negotiating leverage but retains shallower-exit mobility
 *   - independent_downstream_developers: target with partial subsidy (moderate/constrained) — pays platform and SDK costs, benefits from documented APIs and installed bases
 *   - open_source_foundations: beneficiary (organized/constrained) — stewards the commons, funded by the contribution stream the hybrid legitimizes
 *   - open_source_contributors: dual-positioned labor input (moderate/mobile) — supplies unpaid and semi-paid maintenance, captures reputation and employment
 *   - public_sector_procurement_offices: excluded voice (institutional/constrained) — would argue for open formats and sovereign control, bound out of the conversation by legacy contracts
 *   - competition_regulators: analytical observer (institutional/analytical) — investigates lock-in and interoperability refusal from outside the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, 0.52).
domain_priors:suppression_score(software_source_status__utilitarian_hybrid_reading, 0.42).
domain_priors:theater_ratio(software_source_status__utilitarian_hybrid_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Utilitarian Hybrid Reading of Software Source Status — Context-Dependent Licensing Allocation").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "economic/political/technological").

domain_priors:requires_active_enforcement(software_source_status__utilitarian_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, '477e7f87-9447-4102-ae1d-9a5ed3f54da1').
narrative_ontology:cs_kernel_codification('477e7f87-9447-4102-ae1d-9a5ed3f54da1', distributed).
narrative_ontology:cs_authority_grounding('477e7f87-9447-4102-ae1d-9a5ed3f54da1', distributed).
narrative_ontology:cs_reading_relation('477e7f87-9447-4102-ae1d-9a5ed3f54da1', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('477e7f87-9447-4102-ae1d-9a5ed3f54da1', software_source_status__pragmatic_development_reading, influences).
narrative_ontology:cs_reading_relation('477e7f87-9447-4102-ae1d-9a5ed3f54da1', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_axiom('477e7f87-9447-4102-ae1d-9a5ed3f54da1', foundational, welfare_maximization_as_sole_licensing_criterion).
narrative_ontology:cs_axiom_status(welfare_maximization_as_sole_licensing_criterion, holdable).
narrative_ontology:cs_axiom_grounding('477e7f87-9447-4102-ae1d-9a5ed3f54da1', welfare_maximization_as_sole_licensing_criterion, instrumental).
narrative_ontology:cs_axiom('477e7f87-9447-4102-ae1d-9a5ed3f54da1', foundational, context_dependent_model_selection).
narrative_ontology:cs_axiom_status(context_dependent_model_selection, holdable).
narrative_ontology:cs_axiom_grounding('477e7f87-9447-4102-ae1d-9a5ed3f54da1', context_dependent_model_selection, instrumental).
narrative_ontology:cs_reference_frame('477e7f87-9447-4102-ae1d-9a5ed3f54da1', aggregate_welfare_maximization_framework).
narrative_ontology:cs_drift_state('477e7f87-9447-4102-ae1d-9a5ed3f54da1', contemporary_ai_frontier, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('477e7f87-9447-4102-ae1d-9a5ed3f54da1', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, open_source_foundations).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, locked_in_enterprise_licensees).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, small_business_software_buyers).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, independent_downstream_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, independent_downstream_developers).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, open_source_contributors).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, open_source_contributors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish commercial software under paid licenses and subscriptions; set license terms, run deployment audits, and decide component by component what to open and what to keep closed. Collect recurring fees from enterprise and small-business customers. Because they hold the code, their exit is repositioning: open-core pivots, dual licensing, or shifting delivery to hosted services.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__utilitarian_hybrid_reading, proprietary_software_vendors, beneficiary).

% Large organizations running core operations on licensed databases, ERP suites, and design tools. Pay substantial annual fees and periodic audit settlements; migration away requires multi-year rewrites, data conversion, and retraining, so most renew despite price increases. They negotiate discounts individually but have been unable to coordinate a collective walk-away.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, locked_in_enterprise_licensees, payer,
    powerful, biographical, trapped, global).

% Smaller firms buying off-the-shelf licenses and SaaS seats at list price. Fees are a visible cost line; they have no audit leverage and negotiate nothing. Their integrations are shallow compared to large enterprises, so cheaper or open alternatives remain reachable, at the cost of retooling workflows.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, small_business_software_buyers, payer,
    moderate, biographical, constrained, national).

% Small studios and freelance developers building products on licensed platforms, SDKs, and app stores. Pay platform cuts and toolkit licensing; in exchange they get documented APIs, distribution, and installed bases they could never build alone. Leaving means rebuilding on another stack and surrendering reach.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, independent_downstream_developers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__utilitarian_hybrid_reading, independent_downstream_developers, beneficiary).

% Nonprofit stewards hosting shared infrastructure projects. Receive corporate contributions, grants, and membership dues that the mixed ecosystem channels toward commons maintenance. Their funding depends on continued corporate participation, which ties their budgets to the health of the hybrid arrangement they do not control.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, open_source_foundations, beneficiary,
    organized, generational, constrained, global).

% Individual engineers contributing code to shared projects, employed or volunteer. Gain reputation, skills, and employment pathways; bear uncompensated maintenance load and burnout risk when heavy corporate users take more than they return. Exit is real — they can stop contributing or move to other projects — at cost to the commons and to their own networks.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, open_source_contributors, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__utilitarian_hybrid_reading, open_source_contributors, payer).

% Government agencies buying software at scale. Many would prefer open formats and sovereign control for cost and resilience reasons, but are bound by legacy contracts, compatibility with incumbent systems, and procurement law. Their preference reaches licensing debates mainly through occasional policy mandates rather than a negotiated seat.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, public_sector_procurement_offices, excluded,
    institutional, generational, constrained, national).

% Antitrust and digital-market authorities examining lock-in, refusal to interoperate, and self-preferencing in software markets. Take testimony from the other seats, commission market studies, and can impose interoperability remedies that alter how licensing terms bind. They neither collect nor pay under the arrangements they examine.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, competition_regulators, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__utilitarian_hybrid_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_source_status__utilitarian_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates software production between shared-infrastructure and commercially-funded modes: decides, context by context, which code is developed as a commons and which is funded through exclusive licenses, so that infrastructure is built once and specialized tools attract sustained investment.
% TRANSFER_FUNCTION: Moves license and subscription revenue from software users — enterprises, small businesses, downstream developers — to proprietary vendors; moves contributed engineering labor from individual developers and corporate teams into open-source infrastructure whose benefits flow to all users, including the vendors' competitors.
% ABSENT_VOICES: End users without purchasing power — students, hobbyists, users in lower-income economies — are counted in the aggregate in principle but hold no seat in any licensing decision; their interests arrive only as advocates' estimates. Future developers locked out of closed formats are similarly unrepresented. Public-sector procurers hold articulated preferences but rarely a negotiated seat, entering through episodic mandates instead.
% DISAPPEARANCE_RATIONALE: If context-sensitive licensing governance vanished overnight and one pole became universal: universal propertization would force the shared infrastructure stack — operating systems, compilers, web servers, databases — to be rebuilt behind paywalls, raising costs economy-wide; universal open sourcing would strip specialized tools of their funding base and shrink professional tooling. Either way the software economy reorganizes painfully, which is the operational meaning of arrangements depending on the mixed allocation.
% FOUNDING_PROBLEM: When software detached from hardware as a sellable product (the 1969 unbundling) and again when the internet made copying free, producers and users faced the problem of sustaining software production without either underproducing the commons or excluding users from code they depend on: which software should be owned, which shared, and who decides?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the innovation-economics literature on open-source production (empirical studies of developer motivation and commons sustainability), competition-authority market studies on cloud and software lock-in, and public-procurement total-cost-of-ownership analyses all attest the allocation problem remains unresolved. The newest frontier — AI model weights — reopened it with no settled answer. Industry parties do not dispute that the problem exists; they dispute the answer, which is what makes the status live rather than dead.
narrative_ontology:disappearance_verdict(software_source_status__utilitarian_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__utilitarian_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__utilitarian_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__utilitarian_hybrid_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__utilitarian_hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__utilitarian_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.52, referent fixed: the standing mixed ecosystem 1998-2026, assessed by this reading's own lights. Extraction is real — fee levels decoupled from marginal cost in concentrated categories, audit settlements, per-core licensing games — but bounded: open substitutes discipline pricing in infrastructure layers, and the hybrid's own corrective loop (mature open alternative -> vendor repricing) is visible in the post-2018 easing. Hence mid-range, not categorical. Suppression 0.42: enforcement machinery (EULAs, activation, audits, historically DRM) is real, but exits are legal and increasingly viable; note suppression is a raw structural property, unscaled by power or scope — only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater 0.26: open-washing, compliance pages, and community branding grow, but the underlying functions (commons maintenance, funded tooling) are genuine. Accessibility_collapse 0.45: understanding the arrangement does not collapse alternatives — open substitutes exist and improve — though they materially degrade in some categories (enterprise ERP, professional design suites). Resistance 0.55: copyleft enforcement actions, public procurement mandates, free-software campaigning, and interoperability regulation all press against the proprietary pole. TEMPORAL SERIES: all three metrics run on one shared 8-point grid (1998, 2003, 2008, 2013, 2018, 2021, 2024, 2026) — no metric is sampled on a private grid, and no end-state value is backfilled into earlier rows. Base_extractiveness accumulates through the consolidation-and-audit era, peaks around 2018, then eases as the cloud-native open stack matures. Suppression_requirement is authored because enforcement-capacity change is a traced dynamic here: it rises with the DMCA/activation/audit ratchet through 2018, then falls as the SaaS shift converts active copy-policing into architectural access control. Theater creeps upward with the marketing value of 'open'. COALITION NOTE: enterprise licensees are individually powerful yet collectively unable to coordinate exit — the classic buyer-side coordination failure that keeps the fee stream intact; small-business buyers are unorganized and weaker still.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the vendor seat the arrangement is the coordination it administers: it wrote the terms, funds the tooling, and can reposition (open-core, dual license) — low directionality, subsidized. From the trapped enterprise seat the same structure operates as enforced payment: renewal under audit threat with no credible exit — near-full-target directionality. The SMB seat computes a softer version of the same extraction: no leverage, but shallower lock-in. Contributors sit near symmetric — the hybrid subsidizes their careers while capturing their maintenance labor. Foundations are subsidized stewards. The regulator's analytical seat sees a functional-but-contested allocation rather than either pole's caricature. Same-level dynamics: enterprises and SMBs hold the same nominal buyer position but differ in exit depth (integration gravity, audit exposure, ability to fund migration), which is why equal nominal standing yields different effective positions. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map cleanly onto structural relationships, so no directionality_overrides are authored. Proprietary vendors derive near the beneficiary pole: they collect the transfer and hold arbitrage-grade exit (they control the code and can relicense or pivot). Trapped enterprise licensees derive near the full-target end: victim declaration plus trapped exit is the maximal-amplification combination. SMB buyers derive high but below enterprises: victim declaration with constrained (not trapped) exit. Downstream developers derive high with partial damping from their secondary beneficiary position. Open-source foundations derive near the beneficiary pole: subsidized by the contribution stream. Contributors derive near symmetric from their dual role — deliberately NOT overridden, because overrides key on the power atom and would misfire across the other moderate-power seats (SMB buyers, who are genuine targets). Excluded and observer seats do not drive chi. Scope: most seats are global, which modestly amplifies effective extraction for targets via harder verification; the regulator's continental scope and the excluded procurer's national scope are authored as held.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents both mislabelings. Reading the hybrid as pure rope would erase the lock-in rents the trapped buyer seats demonstrably pay — a coordination story used as cover. Reading it as pure snare would erase the genuine funding-allocation function that built and sustains the open infrastructure commons — treating the coordination as cover when it is load-bearing. Tangled rope preserves both halves: real coordination (allocating production modes so infrastructure is built once and specialized tools get funded) AND asymmetric extraction (vendor fee streams from buyers whose exits are costly), held together by active enforcement (license law, audits, contractual lock-in). Mandatrophy status: the founding problem — which software to own, which to share, who decides — is live, not dead: the AI-weights frontier reopened it with no settled answer. Accordingly mandatrophy_resolved is NOT declared, and the R5 mismatch consumer finds status=live paired with verdict=world_rearranges — a consistent pairing producing no zombie flag. The receipt surface corroborates the tangled reading: gains demonstrably accrue to the named vendor seat (not diffuse), and fixing is prohibitive for any single fixer — collapsing to either pole strands enormous invested capital on one side or the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story authors epsilon for the standing mixed-ecosystem arrangement as assessed by the utilitarian hybrid reading; would the identical arrangement classify differently under the sibling readings of the software_source_status kernel?',
    'Compare compiled classifications across the four sibling stories (freedom_imperative, pragmatic_development, property_rights). Divergence in epsilon and victim sets across readings over the identical referent is the expected signature of a contested kernel, not a defect.',
    'If classifications converge across readings, the kernel is less contested than the discourse suggests; if they diverge sharply (expected — the freedom reading authors categorical victims and much higher epsilon, the property reading lower epsilon), cross-reading comparison becomes the primary analytic product and no single story''s verdict should be reported standalone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification over a shared kernel; sibling stories carry the other readings.').

omega_variable(
    welfare_metric_underdetermination,
    'Which welfare measure governs ''maximize aggregate welfare'' for a given licensing decision — consumer surplus, producer surplus, innovation option value, resilience, or some weighted composite?',
    'Welfare-economic analysis of natural experiments: post-open-sourcing trajectories of formerly proprietary infrastructure, and entry of open substitutes into concentrated categories, to see which measures predict observed welfare-relevant outcomes.',
    'If no stable measure exists, the hybrid criterion risks collapsing into status-quo rationalization — whatever mix exists gets declared optimal — and the reading loses its discriminating content, drifting toward a piton-like performance of optimization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_metric_underdetermination, conceptual, 'Operational content of the aggregate-welfare criterion.').

omega_variable(
    saas_enforcement_trajectory,
    'Does the shift from possessed copies to hosted access permanently lower the enforcement burden of proprietary control, or do attestation, telemetry licensing, and usage metering rebuild active enforcement inside the service relationship?',
    'Track license-audit volume, attestation and telemetry deployment, and compliance-dispute rates over the next decade alongside architecture surveys.',
    'Continued decline supports drift toward lighter-enforcement coordination; re-ratcheting through telemetry would push effective suppression back up and harden the extraction asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saas_enforcement_trajectory, empirical, 'Whether the measured fall in suppression_requirement is durable or cyclical.').

omega_variable(
    ai_weights_extension,
    'Does the hybrid allocation extend to AI model weights (open weights for research infrastructure, proprietary for products), or do training-data provenance and safety externalities break welfare aggregation for this asset class?',
    'Observe regulatory treatment and licensing practice for frontier models 2026-2032: whether decisions track context-specific welfare analysis or categorical open/closed rules.',
    'Successful extension confirms the reading''s reference frame; breakdown would split a new kernel (model_source_status) whose readings re-run this contest with different stakes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ai_weights_extension, empirical, 'Whether the kernel''s allocation logic survives the AI-models frontier.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 1998, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1998, software_source_status__utilitarian_hybrid_reading, theater_ratio, 1998, 0.14).
narrative_ontology:measurement_basis(soft_tr_t1998, observed).
narrative_ontology:measurement(soft_tr_t2003, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2003, 0.16).
narrative_ontology:measurement_basis(soft_tr_t2003, observed).
narrative_ontology:measurement(soft_tr_t2008, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement_basis(soft_tr_t2008, observed).
narrative_ontology:measurement(soft_tr_t2013, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2013, 0.2).
narrative_ontology:measurement_basis(soft_tr_t2013, observed).
narrative_ontology:measurement(soft_tr_t2018, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2018, 0.22).
narrative_ontology:measurement_basis(soft_tr_t2018, observed).
narrative_ontology:measurement(soft_tr_t2021, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2021, 0.24).
narrative_ontology:measurement_basis(soft_tr_t2021, observed).
narrative_ontology:measurement(soft_tr_t2024, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2024, 0.25).
narrative_ontology:measurement_basis(soft_tr_t2024, observed).
narrative_ontology:measurement(soft_tr_t2026, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2026, 0.26).
narrative_ontology:measurement_basis(soft_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t1998, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 1998, 0.44).
narrative_ontology:measurement_basis(soft_be_t1998, observed).
narrative_ontology:measurement(soft_be_t2003, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2003, 0.48).
narrative_ontology:measurement_basis(soft_be_t2003, observed).
narrative_ontology:measurement(soft_be_t2008, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2008, 0.53).
narrative_ontology:measurement_basis(soft_be_t2008, observed).
narrative_ontology:measurement(soft_be_t2013, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2013, 0.57).
narrative_ontology:measurement_basis(soft_be_t2013, observed).
narrative_ontology:measurement(soft_be_t2018, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2018, 0.59).
narrative_ontology:measurement_basis(soft_be_t2018, observed).
narrative_ontology:measurement(soft_be_t2021, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2021, 0.56).
narrative_ontology:measurement_basis(soft_be_t2021, observed).
narrative_ontology:measurement(soft_be_t2024, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2024, 0.53).
narrative_ontology:measurement_basis(soft_be_t2024, observed).
narrative_ontology:measurement(soft_be_t2026, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2026, 0.52).
narrative_ontology:measurement_basis(soft_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1998, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 1998, 0.34).
narrative_ontology:measurement_basis(soft_su_t1998, observed).
narrative_ontology:measurement(soft_su_t2003, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2003, 0.43).
narrative_ontology:measurement_basis(soft_su_t2003, observed).
narrative_ontology:measurement(soft_su_t2008, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2008, 0.54).
narrative_ontology:measurement_basis(soft_su_t2008, observed).
narrative_ontology:measurement(soft_su_t2013, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2013, 0.61).
narrative_ontology:measurement_basis(soft_su_t2013, observed).
narrative_ontology:measurement(soft_su_t2018, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2018, 0.63).
narrative_ontology:measurement_basis(soft_su_t2018, observed).
narrative_ontology:measurement(soft_su_t2021, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2021, 0.58).
narrative_ontology:measurement_basis(soft_su_t2021, observed).
narrative_ontology:measurement(soft_su_t2024, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2024, 0.52).
narrative_ontology:measurement_basis(soft_su_t2024, observed).
narrative_ontology:measurement(soft_su_t2026, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2026, 0.46).
narrative_ontology:measurement_basis(soft_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__utilitarian_hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__property_rights_reading).

% DUAL FORMULATION NOTE:
% The colloquial debate 'should software be open or proprietary' decomposes, per the epsilon-invariance principle, into four structurally distinct readings of one kernel (software_source_status), each with its own epsilon over the shared referent (the standing mixed ecosystem): this utilitarian hybrid reading (epsilon ~0.52, no categorical victims, context-dependent allocation), the freedom-imperative reading (categorical injustice, high epsilon, categorical victim set), the pragmatic-development reading (instrumental quality argument, moderate epsilon), and the property-rights reading (entitlement-legitimated restriction, low epsilon). The label conflated them; the framework splits them. Family members link via affects_constraints (IDs follow the kernel__reading convention). Citation structure runs from the pragmatic and property readings into policy and procurement, with this reading functioning as the umbrella frame that absorbs the others' arguments into welfare terms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
