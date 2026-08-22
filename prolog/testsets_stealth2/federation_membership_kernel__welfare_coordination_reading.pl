% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__welfare_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__welfare_coordination_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: federation_membership_kernel__welfare_coordination_reading
 *   human_readable: Welfare-Coordination Reading of EU Free Movement
 *   domain: political economy/federalism/migration policy/welfare state theory
 *
 * SUMMARY:
 *   Free movement in the EU is administered not as a single supranational
 *   welfare state but as a coordination layer over 27 national systems: rules
 *   that pick one applicable legislation per worker, aggregate insurance
 *   periods across borders, and honor each state's benefit design while
 *   setting a common floor against social dumping. This story instantiates
 *   the welfare-coordination reading of the membership kernel: its substance
 *   is the coordination machinery (Regulation 883/2004, the Posted Workers
 *   Directive and its 2018 revision, the enforcement-directive inspection
 *   networks) plus the posting economy grown through it. By this reading's
 *   own lights the arrangement is a genuine portability achievement carrying
 *   a cost-competition overlay: the 24-month home-scheme retention that lets
 *   posting operators attach levy bills to cheap home systems, wage packages
 *   that undercut host collective agreements, and a persistent gap between
 *   declared anti-dumping rules and delivered inspection capacity. Posted
 *   workers, host-state labor markets, and host welfare funds bear the
 *   overlay's costs; posting operators, client firms, and sending-state
 *   treasuries collect its gains; ordinary mobile citizens collect the
 *   portability gains. This file is one reading of a contested kernel — the
 *   integration and member-sovereignty readings are separate constraints
 *   linked in network.affects_constraints — and its epsilon is authored for
 *   the standing coordination-plus-posting arrangement as this reading
 *   assesses it. KEY AGENTS (by structural relationship): -
 *   posting_agency_multinationals: Primary beneficiary (powerful/arbitrage) —
 *   captures the posting margin - posted_workers: Primary target
 *   (powerless/constrained) — bears levy-exemption and undercutting costs -
 *   host_state_domestic_workers: Secondary target (organized/constrained) —
 *   faces wage-and-terms competition - host_state_welfare_funds:
 *   Institutional target (institutional/constrained) — forgoes contributions,
 *   absorbs residuals - emigration_source_regions: Diffuse target
 *   (powerless/trapped) — loses workers without fiscal compensation -
 *   sending_state_insurance_funds: Secondary beneficiary
 *   (institutional/constrained) — collects levies from workers abroad -
 *   cost_competitive_host_employers: Secondary beneficiary
 *   (powerful/arbitrage) — buys posted labor to cut wage bills -
 *   mobile_union_citizens: Coordination beneficiary (moderate/mobile) —
 *   collects portability gains - eu_commission_employment_directorate: Agenda
 *   setter (institutional/constrained) - eu_court_of_justice: Agenda setter,
 *   adjudicative arm (institutional/constrained) -
 *   third_country_migrant_workers: Excluded voice (powerless/trapped) -
 *   comparative_welfare_researchers: Analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.61).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.66).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "Welfare-Coordination Reading of EU Free Movement").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political economy/federalism/migration policy/welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, 'a02b23c9-a15b-4ad9-b468-655950557175').
narrative_ontology:cs_kernel_codification('a02b23c9-a15b-4ad9-b468-655950557175', fixed_text).
narrative_ontology:cs_authority_grounding('a02b23c9-a15b-4ad9-b468-655950557175', lineage).
narrative_ontology:cs_interpretation_layer_present('a02b23c9-a15b-4ad9-b468-655950557175').
narrative_ontology:cs_reading_relation('a02b23c9-a15b-4ad9-b468-655950557175', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('a02b23c9-a15b-4ad9-b468-655950557175', federation_membership_kernel__member_sovereignty_reading, influences).
narrative_ontology:cs_axiom('a02b23c9-a15b-4ad9-b468-655950557175', foundational, welfare_design_autonomy_preserved).
narrative_ontology:cs_axiom_status(welfare_design_autonomy_preserved, holdable).
narrative_ontology:cs_axiom_grounding('a02b23c9-a15b-4ad9-b468-655950557175', welfare_design_autonomy_preserved, conventional).
narrative_ontology:cs_axiom('a02b23c9-a15b-4ad9-b468-655950557175', foundational, anti_dumping_floor_sufficiency).
narrative_ontology:cs_axiom_status(anti_dumping_floor_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('a02b23c9-a15b-4ad9-b468-655950557175', anti_dumping_floor_sufficiency, instrumental).
narrative_ontology:cs_reference_frame('a02b23c9-a15b-4ad9-b468-655950557175', welfare_pluralism_through_coordination).
narrative_ontology:cs_drift_state('a02b23c9-a15b-4ad9-b468-655950557175', post_enlargement_posting_boom, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a02b23c9-a15b-4ad9-b468-655950557175', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, posting_agency_multinationals).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, cost_competitive_host_employers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, sending_state_insurance_funds).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, mobile_union_citizens).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, host_state_domestic_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, host_state_welfare_funds).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, emigration_source_regions).
narrative_ontology:constraint_vindicates(federation_membership_kernel__welfare_coordination_reading, portability_of_social_rights_principle).
narrative_ontology:constraint_vindicates(federation_membership_kernel__welfare_coordination_reading, anti_social_dumping_floor_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run cross-border placement chains: register workers with a home-state social-security scheme, contract them out to client firms in higher-wage states, and invoice the difference between the host-market rate and the posted package. The 24-month home-scheme retention keeps each deployment's levy bill attached to the cheaper home system. Their business model is regulatory arbitrage across 27 contribution regimes; re-registering in another member state is routine and cheap.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posting_agency_multinationals, beneficiary,
    powerful, biographical, arbitrage, continental).

% Client firms in construction, agriculture, logistics, and care that staff projects with posted labor instead of local hires. They save the spread between local collective-agreement wages and posted packages, and can scale crews up and down across borders without carrying local employment obligations. Hiring locally remains open to them but costs more, which is why they buy posting.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, cost_competitive_host_employers, beneficiary,
    powerful, biographical, arbitrage, national).

% Collect contributions from nationals who work and consume services abroad under home-scheme retention, and from emigrants who keep voluntary home coverage. Contribution inflows arrive without corresponding service outlays during working years, though pension liabilities accrue. Scheme solvency arithmetic increasingly depends on exported workers' payments.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_state_insurance_funds, beneficiary,
    institutional, generational, constrained, national).

% Ordinary mobile citizens — nurses, engineers, students, retirees — whose moves the coordination machinery makes workable: one applicable legislation instead of overlapping claims, insurance periods added together across states, health coverage honored abroad. They pay the same levies wherever they work and draw the rights the rules promise. Their exit is real: they move, and the rules follow them.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, mobile_union_citizens, beneficiary,
    moderate, biographical, mobile, continental).

% Sent by agencies to work in higher-wage states under home-scheme coverage for up to 24 months. They contribute at home rates while living at host prices, accrue reduced or delayed host entitlements, and often pay agency-arranged housing and transport deductions out of posted wages. Changing employer mid-chain or staying past the assignment means renegotiating from zero; returning home means the unemployment they left. Language barriers, recruiter debt, and tied accommodation narrow the practical choice set further.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_workers, payer,
    powerless, biographical, constrained, continental).

% Work beside posted crews under collective agreements the posting package undercuts. Their unions and works councils defend terms through inspections, blockades, and litigation — defense that courts have sometimes ruled unlawful restraint of service freedom. Their representative institutions are fused with the bargaining model they defend, so defense takes the form of escalating legal and industrial conflict rather than exit; individually, the alternative is accepting the lowered going rate or leaving the trade.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, host_state_domestic_workers, payer,
    organized, biographical, constrained, national).

% Forgo contribution income for every worker-month spent under home-scheme retention while standing ready to absorb residual claims — workplace accidents, employer insolvency, top-up benefits — when posting chains dissolve. Over the long run they also inherit pension and health costs of workers who settle after years of home-scheme contributions. Their boards can adjust benefit schedules but cannot levy the foreign registrations that draw down their base.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, host_state_welfare_funds, payer,
    institutional, generational, constrained, national).

% Districts and counties that export working-age residents to posting chains and permanent migration alike. Remittances arrive; tax bases, school cohorts, and care-economy contributors do not. No fiscal-compensation mechanism offsets the population loss, and the region cannot retain workers whose home wages are a fraction of posted wages abroad.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, emigration_source_regions, payer,
    powerless, generational, trapped, regional).

% Drafts and revises the coordination instruments, polices transposition through infringement proceedings, runs the administrative network linking national liaison bodies, and brokered the 2018 posted-workers revision. Its discretion is bounded by member-state coalitions in Council and by treaty text; it administers the balance between service freedom and the anti-dumping floor but cannot rewrite either alone.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_commission_employment_directorate, agenda_setter,
    institutional, generational, constrained, continental).

% Adjudicates where service freedom ends and host-state defense begins. Its case law struck down union blockades and public-contract clauses as disproportionate obstacles, defining how much defensive space the anti-dumping floor actually leaves. It binds the other seats through preliminary rulings; its own position is fixed by the treaties it interprets.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_court_of_justice, agenda_setter,
    institutional, generational, constrained, continental).

% Work the same sites and wards as posted crews under regular-migration or asylum-channel statuses, usually with weaker documentation and no home-scheme safety net. The coordination framework's privileges attach to EU-nationality chains; nothing in the negotiating forums represents them. They would press for equal terms and open channels if seated.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, third_country_migrant_workers, excluded,
    powerless, biographical, trapped, continental).

% Track contribution flows, entitlement gaps, enforcement statistics, and posting volumes across regimes; publish the incidence analyses the policy debate cites. No stake in the arrangement's survival; their exit is disciplinary, not geographic.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, comparative_welfare_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__welfare_coordination_reading, posting_agency_multinationals).
narrative_ontology:fixing_cost_class(federation_membership_kernel__welfare_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the portability problem created by 27 separate compulsory social-security systems: a person working in several states would otherwise be double-charged, uncovered, or stripped of accrued rights at each border. Regulation 883/2004 picks a single applicable legislation per worker, aggregates insurance periods across states, and routes data between liaison bodies; the Posted Workers Directive sets a floor of host-state terms that must follow a worker temporarily abroad. Each state keeps writing its own benefit schedules, contribution rates, and eligibility rules.
% TRANSFER_FUNCTION: Moves labor and services from lower-wage to higher-wage member states; during posting, moves social contributions from the state where work is consumed to the state where the worker is insured; moves the wage-bill difference between host collective-agreement rates and posted packages to posting operators and their client firms; moves enforcement attention and administrative-cooperation traffic toward host-state inspectorates.
% ABSENT_VOICES: Third-country nationals working the same construction sites, farms, and care homes have no seat: the coordination framework privileges EU-national posting chains over the regular migration channels open to them, and they are absent where the rules are written. Posted workers themselves are represented only through home-state governments whose treasuries collect their contributions — an agent with mixed incentives. In non-coordinated economies before the Laval litigation, host unions lacked procedural standing to defend terms against posting.
% DISAPPEARANCE_RATIONALE: If the coordination instruments vanished overnight, cross-border service contracts would become legally uninsurable risks; millions of aggregated pension and insurance records would strand; posting chains would dissolve into either permanent migration or withdrawal; host-state construction and agriculture wages would reprice upward while sending-state remittance flows collapsed. The rearrangement would be large, fast, and unevenly distributed across seats.
% FOUNDING_PROBLEM: Mobile workers falling between national systems: double liability for contributions, loss of coverage at borders, forfeiture of accrued rights — frictions that fragmented the common labor market the Community was created to build. Addressed first by early coordination regulations, consolidated in Regulations 1408/71 and 883/2004, with the Posted Workers Directive (1996, revised 2018) adding the anti-dumping floor.
% FOUNDING_PROBLEM_CORROBORATION: Host-state trade unions and labor inspectorates — adversarial to the posting regime — still attest the portability problem is live: their own members depend on aggregation of periods and single-applicable-legislation rules. European Court of Auditors special reports and the academic social-security-law literature corroborate both continued need and enforcement gaps. No party, including the arrangement's critics, claims the portability problem is solved.
narrative_ontology:disappearance_verdict(federation_membership_kernel__welfare_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__welfare_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__welfare_coordination_reading, 0.61, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__welfare_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__welfare_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.61 is a blend: the portability function delivers broad diffuse benefits to millions of mobile citizens, while the posting overlay concentrates costs — the 24-month levy exemption functions as a per-worker-month subsidy to posting operators relative to local hires, and posted packages undercut host collective agreements. Suppression 0.66 is authored as a raw structural property (unscaled by power or scope): the Laval, Viking, and Rüffert line of case law removed host-state defensive options (blockades, public-contract clauses), and the enforcement machinery has hardened steadily since the 2014 enforcement directive — hence the rising suppression_requirement series. Theater ratio 0.34 reflects declared-versus-delivered enforcement: inspection capacity and transposition quality lag the anti-dumping rules' text, but the enforcement activity that exists is real, and the 2018 revision converted part of the performative floor into operative equal-pay rules. Accessibility collapse 0.48: welfare-design autonomy and bilateral options persist, and Brexit demonstrated exit is possible at high cost, but within the single market a member state cannot refuse free movement or posting without treaty breach. Resistance 0.68: Swedish union blockades, years of litigation, 'Polish plumber' electoral politics, Brexit itself, and the host-state legislative coalition that forced the 2018 revision — that coalition's success is direct evidence that victim-side coalition power partially caps extraction, which is why the extractiveness series bends down after t=20 rather than continuing to climb. All three tracked metrics run on one shared seven-point grid (t=0..30, five-year steps mapped to 1996..2026); scalar base_properties values are the interval-end steady state, measured after the 2018 revision's effects began to land.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats should compute different types from the same structure. From the posted worker's position — constrained exit, tied accommodation, home-rate levies against host-price living — the arrangement operates as enforced extraction wearing a coordination shell. From the posting operator's position — arbitrage-grade exit across 27 regimes — the same structure is the price system that makes cross-border service provision financeable at all. From the Commission's and Court's positions it is the achievable compromise between service freedom and welfare pluralism, defended precisely because the alternatives (harmonization or exclusion) are worse by their lights. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. posting_agency_multinationals and cost_competitive_host_employers hold arbitrage-grade exit and sit nearest the beneficiary end (damped chi). sending_state_insurance_funds is a declared beneficiary with low d — note the deliberate seat split inside the same member state: the treasury seat collects from exported workers while the emigration_source_regions seat pays in lost population with trapped exit and high d; the expected structural delta ('sending states lose workers without fiscal compensation') lands on the region seat, not the fund seat. posted_workers (payer, constrained), host_state_domestic_workers (payer, constrained), host_state_welfare_funds (payer, constrained), and emigration_source_regions (payer, trapped) derive high d, amplified by scope where verification is hardest. mobile_union_citizens (beneficiary, mobile) sits near the beneficiary end. The two agenda setters are not declared beneficiaries; they collect legitimacy and precedent rather than rents, and the derivation should place them near symmetric-administrative. No directionality overrides were needed: the declarations plus exit options reproduce the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — portability across incompatible national systems — is live and externally corroborated, so no zombie flag applies to the arrangement as a whole. The classification work is keeping two mislabels apart. Reading the whole arrangement as pure extraction erases the portable-rights benefit that millions of ordinary mobile citizens collect and that even adversarial host-state unions rely on; reading it as pure coordination erases the posting overlay's asymmetric extraction, which is concentrated, measurable, and enforced. Tangled rope holds both: a real coordination function, real asymmetric extraction through the same structure, and active enforcement required to maintain the hybrid. The mandatrophy risk is localized in the anti-dumping enforcement layer specifically: if inspection capacity decays further, the floor becomes theatrical while the levy-exemption channel keeps paying, and the arrangement slides toward extraction-with-portability-residue. The theater_ratio series tracks exactly that decay-and-partial-restoration dynamic; the t=20 peak marks maximum gap between declared rules and delivered enforcement before the 2018 revision began closing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_index_epsilon_scope,
    'This constraint is one reading of federation_membership_kernel; would the sibling readings author a different epsilon for the same standing arrangement?',
    'Generate the sibling files and compare authored epsilon over the identical referent (coordination instruments plus the posting economy as operated); the divergence locates the disagreement structurally.',
    'The integration reading would likely author lower epsilon (costs absorbed as the price of a constitutive right); the member-sovereignty reading higher epsilon (imposed mobility reads as erosion of solidarity institutions). Cross-reading epsilon spread measures kernel contest, not authoring error.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_index_epsilon_scope, conceptual, 'Reading-indexed epsilon over a shared referent; sibling readings would author different values.').

omega_variable(
    levy_retention_cost_incidence,
    'Who ultimately bears the cost of 24-month home-scheme retention — posted workers (reduced or delayed host entitlements), host funds (forgone contributions), or neither (priced into posting fees and home-host rate differentials)?',
    'Actuarial comparison of entitlement trajectories for posted versus locally hired equivalents; contribution-flow accounting between liaison bodies.',
    'If workers bear it, the posting overlay draws from the mobile poor; if host funds, from territorial solidarity; if priced out, it is a pure operator margin. Each answer redistributes effective extraction across seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(levy_retention_cost_incidence, empirical, 'Incidence of the home-scheme retention cost across posted workers, host funds, and operator pricing.').

omega_variable(
    sham_posting_share,
    'What share of postings are genuine cross-border service provision versus letterbox arrangements (no real establishment, worker effectively permanently deployed in the host state)?',
    'Inspectorate establishment-substance audits; comparison of declared versus observed posting durations and return rates.',
    'A high sham share means much of the measured extraction is abuse riding the coordination rail (remediable by enforcement); a low share means the design itself prices undercutting in (requiring redesign). The remediation path differs completely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sham_posting_share, empirical, 'Prevalence of genuine service posting versus artificial arrangements.').

omega_variable(
    kernel_codification_frame,
    'Is the kernel best codified as fixed_text (treaty articles and regulations interpreted by the Court) or as distributed acquis (accumulated practice with no single adjudicating authority)?',
    'Examine whether member states and Commission treat specific textual provisions as controlling, or whether practice outruns text with legitimacy following the practice.',
    'fixed_text plus lineage supports the declared interpretation layer (the Court absorbs drift without surfaced revision); a distributed reading removes the designated interpreter and reroutes drift analysis toward practice-based authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_codification_frame, conceptual, 'Framing under-determination in the kernel''s codification: authoritative text versus accumulated practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fed_welfare_coord_tr_t0, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(fed_welfare_coord_tr_t0, observed).
narrative_ontology:measurement(fed_welfare_coord_tr_t5, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(fed_welfare_coord_tr_t5, observed).
narrative_ontology:measurement(fed_welfare_coord_tr_t10, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(fed_welfare_coord_tr_t10, observed).
narrative_ontology:measurement(fed_welfare_coord_tr_t15, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement_basis(fed_welfare_coord_tr_t15, observed).
narrative_ontology:measurement(fed_welfare_coord_tr_t20, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(fed_welfare_coord_tr_t20, observed).
narrative_ontology:measurement(fed_welfare_coord_tr_t25, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 25, 0.36).
narrative_ontology:measurement_basis(fed_welfare_coord_tr_t25, observed).
narrative_ontology:measurement(fed_welfare_coord_tr_t30, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement_basis(fed_welfare_coord_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(fed_welfare_coord_be_t0, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(fed_welfare_coord_be_t0, observed).
narrative_ontology:measurement(fed_welfare_coord_be_t5, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(fed_welfare_coord_be_t5, observed).
narrative_ontology:measurement(fed_welfare_coord_be_t10, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(fed_welfare_coord_be_t10, observed).
narrative_ontology:measurement(fed_welfare_coord_be_t15, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(fed_welfare_coord_be_t15, observed).
narrative_ontology:measurement(fed_welfare_coord_be_t20, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(fed_welfare_coord_be_t20, observed).
narrative_ontology:measurement(fed_welfare_coord_be_t25, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(fed_welfare_coord_be_t25, observed).
narrative_ontology:measurement(fed_welfare_coord_be_t30, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(fed_welfare_coord_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(fed_welfare_coord_su_t0, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(fed_welfare_coord_su_t0, observed).
narrative_ontology:measurement(fed_welfare_coord_su_t5, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(fed_welfare_coord_su_t5, observed).
narrative_ontology:measurement(fed_welfare_coord_su_t10, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(fed_welfare_coord_su_t10, observed).
narrative_ontology:measurement(fed_welfare_coord_su_t15, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement_basis(fed_welfare_coord_su_t15, observed).
narrative_ontology:measurement(fed_welfare_coord_su_t20, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(fed_welfare_coord_su_t20, observed).
narrative_ontology:measurement(fed_welfare_coord_su_t25, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 25, 0.64).
narrative_ontology:measurement_basis(fed_welfare_coord_su_t25, observed).
narrative_ontology:measurement(fed_welfare_coord_su_t30, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement_basis(fed_welfare_coord_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__welfare_coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__member_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the federation_membership_kernel per the epsilon-invariance principle: the colloquial label 'EU free movement' conflates three structurally distinct claims with different epsilon values, beneficiary structures, and failure modes. This file instantiates the welfare-coordination reading (coordination instruments + posting economy, epsilon authored for the standing arrangement as this reading assesses it). The integration reading (citizenship-right framing) and the member-sovereignty reading (territorial-welfare-capacity framing) are separate stories; each authors its own epsilon over the same referent. Edges here link the family; upstream/downstream pressure between readings is documented in cs_structure.reading_relations and commentary.kernel_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
