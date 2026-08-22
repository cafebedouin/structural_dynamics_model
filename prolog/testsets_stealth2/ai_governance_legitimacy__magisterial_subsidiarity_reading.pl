% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__magisterial_subsidiarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__magisterial_subsidiarity_reading, []).

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
 *   constraint_id: ai_governance_legitimacy__magisterial_subsidiarity_reading
 *   human_readable: Magisterial Subsidiarity Reading of AI Governance Legitimacy
 *   domain: theological/political_theology/technology_governance
 *
 * SUMMARY:
 *   Since Rerum Novarum (1891) the Magisterium has claimed authority to
 *   interpret the moral order of economic and technical life; that claim now
 *   extends to artificial intelligence through Caritas in Veritate (2009),
 *   Laudato Si' (2015), the Rome Call for AI Ethics (2020), and Antiqua et
 *   Nova (2025). The standing arrangement under contest is the operative
 *   magisterial influence structure: doctrinal criteria (common good,
 *   subsidiarity, solidarity, universal destination of goods) define
 *   legitimate AI governance; the Magisterium authoritatively interprets
 *   them; and an applicative apparatus — the Pontifical Academy for Life, the
 *   RenAIssance Foundation, interfaith investor coalitions, Catholic
 *   institutional procurement and investment screening — applies pressure
 *   through moral suasion, shareholder activism, and convening power. The
 *   arrangement delivers a genuine transnational coordination function (a
 *   dignity framework and a voice for populations legal governance does not
 *   reach) while extracting real compliance and reputational costs from
 *   technology firms, defense contractors, and financial institutions, and
 *   concentrating interpretive authority and moral capital in Rome. KEY
 *   AGENTS (by structural relationship): - roman_curia_magisterium:
 *   Authorizing center (institutional/identity_locked) — sets doctrine,
 *   convenes coalitions, collects interpretive authority -
 *   catholic_institutional_network: Implementing beneficiary
 *   (organized/identity_locked) — applies standards, gains identity, bears
 *   operating costs - precarious_platform_workers: Protected class
 *   (powerless/trapped) — named beneficiary, receives advocacy more than
 *   relief - global_south_communities: Protected class
 *   (powerless/constrained) — gains transnational voice through Church
 *   channels - catholic_family_households: Protected class
 *   (moderate/constrained) — receives normative backing for family authority
 *   - vulnerable_marginalized_populations: Protected class
 *   (powerless/trapped) — design-for-first criterion, enforcement via proxy -
 *   private_tech_monopolies: Primary cost-bearing seat
 *   (institutional/arbitrage) — pays compliance and reputational costs,
 *   manages via absorption - military_industrial_ai_complex: Cost-bearing
 *   seat (institutional/constrained) — condemned categorically, reached
 *   weakest - extractive_finance_institutions: Cost-bearing seat
 *   (institutional/arbitrage) — most concretely pressured via investor
 *   coalitions - secular_ai_ethics_bodies: Excluded rival
 *   (institutional/constrained) — no interpretive standing, objects from
 *   outside - multilateral_governance_fora: Analytical observer
 *   (institutional/analytical) — weighs the arrangement's voice without being
 *   governed by it The claim/metric split is deliberate: the claimed type is
 *   authored from the authoring seat as structurally true; the metrics are
 *   authored as descriptively true of the arrangement's actual operation; the
 *   engine computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - roman_curia_magisterium: authorizing center — sets and interprets doctrine, convenes the coalition, collects interpretive authority and convening power
 *   - catholic_institutional_network: implementing beneficiary with payer costs — hospitals, universities, Caritas agencies, interfaith investor coalitions
 *   - precarious_platform_workers: protected class — algorithmically managed, receives advocacy voice more than material relief
 *   - global_south_communities: protected class — supplies labor and absorbs environmental costs, gains Church-channel representation
 *   - catholic_family_households: protected class — receives normative backing for family authority over technology in the home
 *   - vulnerable_marginalized_populations: protected class — the preferential-option design criterion, enforced by proxy advocacy
 *   - private_tech_monopolies: primary cost-bearing seat — compliance and reputational demands, managed through selective signature and absorption
 *   - military_industrial_ai_complex: cost-bearing seat — categorical condemnation, weakest enforcement reach
 *   - extractive_finance_institutions: cost-bearing seat — most concretely pressured through shareholder activism and capital screening
 *   - secular_ai_ethics_bodies: excluded rival — produces competing frameworks, holds no interpretive standing inside the arrangement
 *   - multilateral_governance_fora: analytical observer — UN and regional bodies that amplify or marginalize the arrangement's reach
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.5).
domain_priors:suppression_score(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.4).
domain_priors:theater_ratio(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__magisterial_subsidiarity_reading, "Magisterial Subsidiarity Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__magisterial_subsidiarity_reading, "theological/political_theology/technology_governance").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__magisterial_subsidiarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'b6edabd4-7939-4bd0-953b-37f6b8902e84').
narrative_ontology:cs_kernel_codification('b6edabd4-7939-4bd0-953b-37f6b8902e84', formalized).
narrative_ontology:cs_authority_grounding('b6edabd4-7939-4bd0-953b-37f6b8902e84', lineage).
narrative_ontology:cs_interpretation_layer_present('b6edabd4-7939-4bd0-953b-37f6b8902e84').
narrative_ontology:cs_reading_relation('b6edabd4-7939-4bd0-953b-37f6b8902e84', ai_governance_legitimacy__technocratic_optimization_reading, forecloses).
narrative_ontology:cs_reading_relation('b6edabd4-7939-4bd0-953b-37f6b8902e84', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6edabd4-7939-4bd0-953b-37f6b8902e84', ai_governance_legitimacy__market_libertarian_reading, forecloses).
narrative_ontology:cs_axiom('b6edabd4-7939-4bd0-953b-37f6b8902e84', foundational, technology_subordinate_to_human_dignity).
narrative_ontology:cs_axiom_status(technology_subordinate_to_human_dignity, holdable).
narrative_ontology:cs_axiom_grounding('b6edabd4-7939-4bd0-953b-37f6b8902e84', technology_subordinate_to_human_dignity, deontological).
narrative_ontology:cs_axiom('b6edabd4-7939-4bd0-953b-37f6b8902e84', foundational, magisterium_holds_ai_interpretive_authority).
narrative_ontology:cs_axiom_status(magisterium_holds_ai_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('b6edabd4-7939-4bd0-953b-37f6b8902e84', magisterium_holds_ai_interpretive_authority, theological).
narrative_ontology:cs_axiom('b6edabd4-7939-4bd0-953b-37f6b8902e84', secondary, universal_destination_of_goods_limits_property).
narrative_ontology:cs_axiom_status(universal_destination_of_goods_limits_property, holdable).
narrative_ontology:cs_axiom_grounding('b6edabd4-7939-4bd0-953b-37f6b8902e84', universal_destination_of_goods_limits_property, deontological).
narrative_ontology:cs_reference_frame('b6edabd4-7939-4bd0-953b-37f6b8902e84', magisterially_ordered_common_good).
narrative_ontology:cs_drift_state('b6edabd4-7939-4bd0-953b-37f6b8902e84', contemporary_frontier_ai_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b6edabd4-7939-4bd0-953b-37f6b8902e84', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, precarious_platform_workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_communities).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_family_households).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, vulnerable_marginalized_populations).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_ai_complex).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_institutional_network).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_institutional_network).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_social_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, principle_of_subsidiarity).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, universal_destination_of_goods).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, preferential_option_for_the_poor).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, human_dignity_supremacy_over_efficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the doctrinal corpus — from Rerum Novarum through Caritas in Veritate and Laudato Si' to Antiqua et Nova — that defines legitimate technology governance, and operates the dicasteries, pontifical academies, and summits that interpret it for the AI era. Convenes the Rome Call coalition and receives the deference, consultative access, and moral authority that flow to the arrangement's authorizing center. Cannot relinquish the teaching office's claim without dissolving the office itself; exit from its own doctrine is not available to it.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, roman_curia_magisterium, agenda_setter,
    institutional, generational, identity_locked, global).

% Hospitals, universities, development agencies, schools, and faith-based investor coalitions that implement the doctrine in procurement, investment screening, and service delivery. Gains a distinctive identity, moral positioning, and access to coalitions unavailable to secular peers; simultaneously bears the operating costs of applying the standards — screening vendors, auditing algorithms, declining profitable lines of business.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_institutional_network, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_institutional_network, payer).

% Deliver, drive, moderate content, and label training data under algorithmic management whose evaluations and wage-setting they cannot see or contest. The arrangement names them a protected class and claims their cause; what reaches them concretely is advocacy, documentation, and occasional investor pressure on their behalf — thin relative to the day-to-day pressures they experience, but a voice they would otherwise lack entirely.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, precarious_platform_workers, beneficiary,
    powerless, immediate, trapped, global).

% Supply the annotation labor, moderation workforce, and mineral inputs behind AI systems while absorbing the environmental costs of compute, and are the least represented in the forums where AI norms are set. Church networks — bishops' conferences, Caritas agencies, parish structures — give them a transnational channel for their interests that formal AI governance processes do not.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_communities, beneficiary,
    powerless, generational, constrained, global).

% Parents and households navigating children's exposure to persuasive and companion AI systems. The arrangement backs their authority as the first society — insisting that technology governance respect family prerogatives rather than route around them — and supplies practical guidance documents; the backing is normative rather than material.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_family_households, beneficiary,
    moderate, biographical, constrained, global).

% Elderly, disabled, refugee, and poor populations whom automated eligibility, scoring, and care-allocation systems most often fail quietly. The arrangement's preferential-option-for-the-poor criterion obliges governors to design for them first; enforcement on their behalf depends almost entirely on third-party advocacy, since they rarely sit in the rooms where systems are specified.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, vulnerable_marginalized_populations, beneficiary,
    powerless, biographical, trapped, global).

% Operate the platforms and foundation models the doctrine targets. Face demands for transparency, participatory oversight, and limits on applications; respond with selective signature of ethics compacts, internal ethics boards, and narrative absorption that converts critique into marketing. Their scale lets them treat the demanded concessions as a manageable reputational line item, and their mobility lets them shift operations toward friendlier jurisdictions when pressure localizes.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies, payer,
    institutional, generational, arbitrage, global).

% Develops autonomous targeting, battlefield decision-support, and surveillance AI that the doctrine condemns categorically. Operates largely beyond the reach of moral suasion — procurement is sovereign, programs are classified — and feels the arrangement mainly as diplomatic friction, investor-divestment noise, and the occasional episcopal condemnation attached to weapons fairs.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_ai_complex, payer,
    institutional, generational, constrained, national).

% Run high-frequency trading, algorithmic credit scoring, and predatory lending products that the universal-destination-of-goods criterion marks as disordered. Feel the arrangement most acutely through interfaith investor coalitions — shareholder resolutions, dialogue programs, and screening from Catholic institutional capital — which have extracted concrete policy concessions here more often than anywhere else.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance_institutions, payer,
    institutional, immediate, arbitrage, global).

% Standards institutes, multilateral working groups, and academic centers that produce rival frameworks for legitimate AI governance. The arrangement assigns them no interpretive standing — its criteria are authoritatively fixed elsewhere — so their objections about pluralistic legitimacy can only be lodged from outside, in competition for the same firms' and states' attention.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, secular_ai_ethics_bodies, excluded,
    institutional, generational, constrained, global).

% United Nations and regional bodies weighing which voices count in AI norm-setting. Take testimony from the Church alongside states and industry, adopt or decline its vocabulary in instruments, and can amplify or marginalize the arrangement's reach without themselves being governed by it.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, multilateral_governance_fora, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__magisterial_subsidiarity_reading, roman_curia_magisterium).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__magisterial_subsidiarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a transnational collective-action problem that neither markets nor states currently solve: AI harms — algorithmic management of workers, extractive data practices, autonomous weapons — cross jurisdictions faster than any regulator, and affected populations lack organized voice. The arrangement supplies shared criteria for legitimate AI governance, convenes firms, states, and civil society around them, and organizes investor and consumer pressure where legal enforcement is absent.
% TRANSFER_FUNCTION: Moves compliance obligations (transparency reporting, participatory-oversight structures, application restrictions) and reputational standing from technology firms, defense contractors, and financial institutions toward protected populations; moves moral authority, convening power, and institutional relevance toward the Magisterium and its affiliated network.
% ABSENT_VOICES: Secular and non-Christian AI-ethics bodies, market-libertarian theorists, and non-believing affected populations have no authorized seat: the arrangement grants interpretive authority exclusively to the Magisterium, so their objection — that legitimacy cannot rest on a tradition-specific interpretive monopoly — can only be voiced from outside. Frontier AI engineers are likewise addressed as objects of the arrangement rather than participants in its formulation; their knowledge of feasibility constraints enters only as filtered through ecclesial consultation.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, interfaith investor coalitions would lose their coordinating frame; Catholic health, education, and development networks would lose procurement and investment guidance; the Rome Call coalition would lose its charter and its annual summitry; Global South advocacy would lose one of its few civilizational-scale amplifiers; and the AI-ethics discourse would lose a vocabulary that currently disciplines both corporate and state framings. Visible arrangements depend on it.
% FOUNDING_PROBLEM: Modern Catholic Social Doctrine was built from Rerum Novarum (1891) onward to defend workers against industrial capitalism's excesses; its extension to AI addresses the technocratic paradigm — the risk that efficiency-maximizing technical systems subordinate human dignity, concentrate decision-making away from communities (violating subsidiarity), and concentrate goods away from the poor (violating the universal destination of goods).
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: secular empirical literature on algorithmic workplace management and on Global South data-labeling and content-moderation labor conditions attests the harm pattern the doctrine names; UNESCO's Recommendation on the Ethics of AI and the OECD AI Principles independently identify concentration and accountability deficits; industry actors' own ethics-board formations concede the deficit. What no outside party corroborates is the remedy's interpretive monopoly — secular bodies attest the problem, not the Magisterium's exclusive authority to define its solution.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__magisterial_subsidiarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__magisterial_subsidiarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__magisterial_subsidiarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_governance_legitimacy__magisterial_subsidiarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.50 (mid of the expected 0.45–0.55 band): the demanded conduct — transparency, participatory oversight, application limits — substantially tracks genuine harm reduction, but parts of the demand are decoupled from service rendered, notably the interpretive authority asserted over non-consenting actors and compliance costs levied regardless of uptake quality. Suppression is 0.40 as a raw, unscaled structural property: enforcement is suasion-based (reputational sanctioning, coalition gatekeeping, investor campaigns, internal ecclesial discipline) rather than legal coercion, but dissenting Catholics and non-signaling firms face real coordinated cost. Theater ratio is 0.32: functional content (procurement changes, investor dialogues that have produced concrete policy shifts, the teaching apparatus itself) is mixed with ritual (summitry, signatures without operational change, diocesan statements as liturgy of concern). Accessibility collapse is low (0.25) because nothing collapses on understanding the arrangement — three rival readings of the same kernel remain fully live and actively contested. Resistance is 0.58: industry evasion and selective adoption, explicit market-libertarian rejection of the solidarity mandates, secular rejection of the interpretive monopoly, and intra-Church friction over specific applications. The temporal series run on ONE shared grid (t = 0,4,8,12,16,20,24, spanning roughly 2001–2025: from Ethics in Internet, through Caritas in Veritate and Laudato Si', to the Rome Call era and Antiqua et Nova); all three metrics are authored at every point, all points observed. Extractiveness, theater, and enforcement capacity all rise monotonically — the arrangement matured from exhortation into organized machinery, accumulating compliance demands and ethics-washing surface together.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the roman_curia_magisterium's position the arrangement is the faithful extension of a 130-year teaching office protecting persons from concentrated technical power — coordination it stewards at its own expense. From private_tech_monopolies' position the same structure operates as a reputational tax with arbitrary interpretive authority, payable through signature, ethics boards, and narrative absorption. From precarious_platform_workers' position it is a promised protection that arrives mostly as rhetoric — the arrangement coordinates on their behalf far more than with them. From extractive_finance_institutions' position it is the one external pressure that has actually moved policy. Same structure, four different experienced realities; the engine computes this divergence from power, exit, and directional position, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The four protected classes are structurally subsidized (d near the beneficiary pole): they receive protection, voice, and normative backing at little cost to themselves, though delivery is thin and mediated. The three cost-bearing seats carry high d: private_tech_monopolies and extractive_finance_institutions with arbitrage-grade exit (their effective burden is damped by mobility and absorption capacity), military_industrial_ai_complex with constrained exit (sovereign procurement shields it from suasion, but it cannot relocate its mission). The roman_curia_magisterium sits nearest the beneficiary pole as both agenda-setter and principal collector — the interpretive-authority rent lands on its seat. The catholic_institutional_network sits slightly above it: net beneficiary (identity, moral capital, coalition access) but bearing real implementation costs. Multilateral_governance_fora are near-symmetric observers. No directionality overrides were needed: the beneficiary/victim declarations plus exit profiles already differentiate the seats correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — technocratic concentration subordinating persons — is live and intensifying with frontier capability, so no mandatrophy is declared. The tangled_rope classification prevents two symmetrical mislabels. Calling the arrangement a snare would erase its genuine coordination function: a transnational dignity framework, investor leverage that has produced documented policy concessions, and the only civilizational-scale voice consistently speaking for populations outside formal governance. Calling it a rope would erase the asymmetric extraction: interpretive monopoly rents accruing to Rome, compliance costs borne by firms for standards they did not author, and moral capital concentrating at the center while protected classes receive advocacy rather than relief. The rising theater series (0.10 to 0.32) is the early-warning signal: ethics-washing converts function into credential, and if theater crosses roughly 0.5 while enforcement capacity decays, drift toward inertial theatrical maintenance becomes the live hypothesis. For now the cost asymmetry does not obtain — the administrator could not cheaply abandon the arrangement without dissolving its own office, and the function it performs is still performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_monopoly_separability,
    'Is the Magisterium''s exclusive interpretive authority a necessary part of the arrangement''s coordination function, or an extractive rent layered onto principles that ecumenical or secular bodies could interpret equivalently?',
    'Compare coordination outcomes where materially identical principles are interpreted by non-Magisterial bodies — e.g., adoption and compliance behavior under the OECD AI Principles or UNESCO recommendation versus under Rome Call commitments by the same firms.',
    'If separable, the interpretive-monopoly component is extraction riding on a genuine coordination function and the effective burden on non-consenting actors rises; if inseparable, part of the measured extraction is the price of doctrinal integrity and the arrangement''s coordination claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_separability, conceptual, 'Whether doctrinal interpretive authority is functionally load-bearing or a rent.').

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of the ai_governance_legitimacy kernel; sibling readings relocate the entire extraction structure — the market_libertarian_reading classifies this reading''s solidarity mandates as the extraction and casts technology firms as beneficiaries. Where does the disagreement''s resolution live?',
    'None internal to this story: resolution exists only at the meta-level comparison across the four linked family stories, each with its own stable epsilon over its own referent.',
    'Cross-reading epsilon comparison is the corpus''s measurement; no within-story resolution is possible, and any attempt to average readings inside this file would destroy its epsilon invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer-frame omega: this file is one reading; sibling files carry the displaced structures.').

omega_variable(
    soft_enforcement_ceiling,
    'Can suasion-based enforcement bind the most consequential AI developers — state-aligned laboratories and classified military programs — whose operations lie beyond ecclesial and investor reach?',
    'Track compliance deltas between reachable actors (Rome Call signatories, screened portfolio firms) and unreachable ones across the next capability cycle.',
    'If the ceiling binds, effective extraction stays below doctrinal intent for the strongest cost-bearing seats, capping their computed burden and leaving the arrangement advisory precisely where stakes are highest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soft_enforcement_ceiling, empirical, 'Whether moral suasion scales to frontier and sovereign actors.').

omega_variable(
    ethics_washing_conversion,
    'Are corporate adoptions of the framework genuine uptake, or performative capture that converts the arrangement into legitimation cover for unchanged practice?',
    'Audit signatory operational behavior — procurement decisions, product launches, lobbying activity — against public commitments over a multi-year window.',
    'If capture dominates, the theater_ratio series understates drift and the arrangement slides toward theatrical maintenance faster than the authored trajectory shows; if uptake is real, the coordination function is consolidating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethics_washing_conversion, empirical, 'Uptake versus capture in corporate adoption of the framework.').

omega_variable(
    subsidiarity_solidarity_tension,
    'The arrangement carries an internal tension: subsidiarity pushes decisions down toward families and local communities (limiting central oversight), while solidarity pulls protective obligation up toward centralized mandates. Which principle dominates as AI capability grows?',
    'Observe which principle successive Magisterial documents invoke and elaborate as capabilities escalate — whether Antiqua et Nova''s successors thicken central accountability demands or devolve authority to local and familial levels.',
    'Central-dominant resolution raises the arrangement''s suppression profile over time; devolutionary resolution lowers it and redistributes enforcement to identity-locked local seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_solidarity_tension, conceptual, 'Internal doctrinal tension determining the arrangement''s future enforcement shape.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_gov_mag_sub_tr_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(ai_gov_mag_sub_tr_t0, observed).
narrative_ontology:measurement(ai_gov_mag_sub_tr_t4, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement_basis(ai_gov_mag_sub_tr_t4, observed).
narrative_ontology:measurement(ai_gov_mag_sub_tr_t8, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement_basis(ai_gov_mag_sub_tr_t8, observed).
narrative_ontology:measurement(ai_gov_mag_sub_tr_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement_basis(ai_gov_mag_sub_tr_t12, observed).
narrative_ontology:measurement(ai_gov_mag_sub_tr_t16, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement_basis(ai_gov_mag_sub_tr_t16, observed).
narrative_ontology:measurement(ai_gov_mag_sub_tr_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement_basis(ai_gov_mag_sub_tr_t20, observed).
narrative_ontology:measurement(ai_gov_mag_sub_tr_t24, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement_basis(ai_gov_mag_sub_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(ai_gov_mag_sub_be_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(ai_gov_mag_sub_be_t0, observed).
narrative_ontology:measurement(ai_gov_mag_sub_be_t4, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 4, 0.22).
narrative_ontology:measurement_basis(ai_gov_mag_sub_be_t4, observed).
narrative_ontology:measurement(ai_gov_mag_sub_be_t8, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement_basis(ai_gov_mag_sub_be_t8, observed).
narrative_ontology:measurement(ai_gov_mag_sub_be_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 12, 0.34).
narrative_ontology:measurement_basis(ai_gov_mag_sub_be_t12, observed).
narrative_ontology:measurement(ai_gov_mag_sub_be_t16, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement_basis(ai_gov_mag_sub_be_t16, observed).
narrative_ontology:measurement(ai_gov_mag_sub_be_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement_basis(ai_gov_mag_sub_be_t20, observed).
narrative_ontology:measurement(ai_gov_mag_sub_be_t24, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement_basis(ai_gov_mag_sub_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_gov_mag_sub_su_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(ai_gov_mag_sub_su_t0, observed).
narrative_ontology:measurement(ai_gov_mag_sub_su_t4, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 4, 0.18).
narrative_ontology:measurement_basis(ai_gov_mag_sub_su_t4, observed).
narrative_ontology:measurement(ai_gov_mag_sub_su_t8, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 8, 0.22).
narrative_ontology:measurement_basis(ai_gov_mag_sub_su_t8, observed).
narrative_ontology:measurement(ai_gov_mag_sub_su_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 12, 0.26).
narrative_ontology:measurement_basis(ai_gov_mag_sub_su_t12, observed).
narrative_ontology:measurement(ai_gov_mag_sub_su_t16, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 16, 0.31).
narrative_ontology:measurement_basis(ai_gov_mag_sub_su_t16, observed).
narrative_ontology:measurement(ai_gov_mag_sub_su_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement_basis(ai_gov_mag_sub_su_t20, observed).
narrative_ontology:measurement(ai_gov_mag_sub_su_t24, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement_basis(ai_gov_mag_sub_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__magisterial_subsidiarity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, market_libertarian_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'AI governance legitimacy' decomposes into four structurally distinct constraints per the epsilon-invariance principle: this magisterial reading, the technocratic optimization reading, the democratic pluralist reading, and the market libertarian reading. Each locates extraction differently — this reading finds it in unaccountable technical power and answers it with doctrinal interpretive authority; the market reading classifies this reading's solidarity mandates themselves as the extraction; the technocratic reading treats doctrinal criteria as friction against welfare; the democratic reading rejects any tradition's interpretive monopoly as the extraction. The four stories form one constraint family linked through affects_constraints; influence between them runs through shared corporate and multilateral audiences (firms and states choose among the readings' legitimacy offers) rather than through logical dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
