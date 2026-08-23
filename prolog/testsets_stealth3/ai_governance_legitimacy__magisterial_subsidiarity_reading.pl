% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__magisterial_subsidiarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   domain: theological/political/technological
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the ai_governance_legitimacy
 *   kernel: the magisterial subsidiarity reading, on which AI governance
 *   arrangements are legitimate insofar as they conform to Catholic Social
 *   Doctrine principles (common good, subsidiarity, solidarity, universal
 *   destination of goods) as authoritatively interpreted by the Magisterium,
 *   with technology subordinated through transparent accountability,
 *   participatory governance, and protection of the vulnerable. The epsilon
 *   referent is the standing arrangement under contest — the magisterial
 *   legitimacy regime as it actually operates across encyclical teaching,
 *   Rome Call-style coalition-building, and international-law advocacy —
 *   assessed by this reading's own lights; the reading's endorsed alternative
 *   arrangement is NOT the referent. Interval 0-15 maps approximately to
 *   2010-2025 (Caritas in Veritate baseline through Laudato Si' 2015, Rome
 *   Call and Fratelli Tutti 2020, Antiqua et Nova 2025). Claim and metrics
 *   are independent authored facts: the constraint is CLAIMED as tangled_rope
 *   (coordination and extraction entangled in one structure) while the
 *   metrics describe its actual operation; the engine computes per-seat
 *   classifications from the structural data and any divergence is the
 *   measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - magisterium: agenda-setter and receipt-seat (institutional / identity_locked) — sets the interpretive agenda, collects authority and convening power; cannot exit its own teaching claim
 *   - private_ai_monopolies: primary target (institutional / mobile) — bears compliance demands and censure, exits cheaply by disregard
 *   - military_industrial_complex and extractive_data_finance: secondary targets (institutional|powerful / mobile|arbitrage) — censured business models, minimal material exposure
 *   - workers_facing_automation and global_south_communities: intended beneficiaries (organized|powerless / trapped) — provisions framed for them, thin seats in governance
 *   - families_and_vulnerable_populations: stated protection priority (moderate / constrained)
 *   - catholic_social_institutions: operational beneficiaries (organized / constrained) — run the framework's research and advocacy machinery
 *   - secular_regulators: dual-positioned co-governors (institutional / constrained) — gain vocabulary and allies, pay in deferred interpretive authority
 *   - catholic_technologists: identity-locked dual-positioned insiders (moderate / identity_locked) — live the constraint daily
 *   - non_catholic_ethicists: excluded voice (organized / constrained) — object to the interpretive monopoly from outside adjudication
 *   - ai_governance_analysts: analytical observer (analytical / analytical) — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.52).
domain_priors:suppression_score(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.4).
domain_priors:theater_ratio(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__magisterial_subsidiarity_reading, "Magisterial Subsidiarity Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__magisterial_subsidiarity_reading, "theological/political/technological").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__magisterial_subsidiarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__magisterial_subsidiarity_reading, '5b5a34e6-e997-4524-a7f5-d1a6f32710fe').
narrative_ontology:cs_kernel_codification('5b5a34e6-e997-4524-a7f5-d1a6f32710fe', fixed_text).
narrative_ontology:cs_authority_grounding('5b5a34e6-e997-4524-a7f5-d1a6f32710fe', lineage).
narrative_ontology:cs_interpretation_layer_present('5b5a34e6-e997-4524-a7f5-d1a6f32710fe').
narrative_ontology:cs_reading_relation('5b5a34e6-e997-4524-a7f5-d1a6f32710fe', ai_governance_legitimacy__technocratic_optimization_reading, influences).
narrative_ontology:cs_reading_relation('5b5a34e6-e997-4524-a7f5-d1a6f32710fe', ai_governance_legitimacy__democratic_pluralist_reading, influences).
narrative_ontology:cs_reading_relation('5b5a34e6-e997-4524-a7f5-d1a6f32710fe', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('5b5a34e6-e997-4524-a7f5-d1a6f32710fe', foundational, technology_subordinate_to_human_dignity).
narrative_ontology:cs_axiom_status(technology_subordinate_to_human_dignity, holdable).
narrative_ontology:cs_axiom_grounding('5b5a34e6-e997-4524-a7f5-d1a6f32710fe', technology_subordinate_to_human_dignity, deontological).
narrative_ontology:cs_axiom('5b5a34e6-e997-4524-a7f5-d1a6f32710fe', foundational, magisterium_authoritative_interpreter_of_social_doctrine).
narrative_ontology:cs_axiom_status(magisterium_authoritative_interpreter_of_social_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('5b5a34e6-e997-4524-a7f5-d1a6f32710fe', magisterium_authoritative_interpreter_of_social_doctrine, theological).
narrative_ontology:cs_axiom('5b5a34e6-e997-4524-a7f5-d1a6f32710fe', secondary, universal_destination_of_goods_governs_ai_benefits).
narrative_ontology:cs_axiom_status(universal_destination_of_goods_governs_ai_benefits, holdable).
narrative_ontology:cs_axiom_grounding('5b5a34e6-e997-4524-a7f5-d1a6f32710fe', universal_destination_of_goods_governs_ai_benefits, theological).
narrative_ontology:cs_reference_frame('5b5a34e6-e997-4524-a7f5-d1a6f32710fe', magisterial_common_good_framework).
narrative_ontology:cs_drift_state('5b5a34e6-e997-4524-a7f5-d1a6f32710fe', contemporary_ai_governance_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5b5a34e6-e997-4524-a7f5-d1a6f32710fe', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers_facing_automation).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_communities).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, families_and_vulnerable_populations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_social_institutions).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_ai_monopolies).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_data_finance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, secular_regulators).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_technologists).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, secular_regulators).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_technologists).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, common_good_primacy_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, subsidiarity_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, solidarity_obligation).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, universal_destination_of_goods).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues encyclicals and authoritative interpretations (Laudato Si', Fratelli Tutti, Antiqua et Nova, the Rome Call for AI Ethics) that define what legitimate AI governance requires, convenes technology executives and diplomats, and builds advocacy coalitions in international forums. Interpretive authority, convening power, and agenda-setting influence flow to it from the framework's operation. It cannot relinquish the teaching claim without dissolving the teaching office itself.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Universities, justice-and-peace commissions, and faith-based NGOs operationalize the doctrine through research centers, ethics guidelines, and advocacy campaigns. Funding, institutional relevance, and standing in policy venues flow to them from the framework's authority. Leaving the framework would mean abandoning their institutional mission.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_social_institutions, beneficiary,
    organized, biographical, constrained, global).

% Bear displacement risk from automation and algorithmic management. Labor-dignity provisions (just wage, worker participation, transition protection) are framed for their benefit and occasionally advance their interests through union alliances with faith-based organizers. They cannot exit labor markets and depend on advocacy structures to be heard.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers_facing_automation, beneficiary,
    organized, biographical, trapped, global).

% Data extraction, supply-chain harms, and unequal access to AI benefits fall disproportionately on them. Universal destination of goods and solidarity provisions are framed for their benefit and invoked in their name in international advocacy. They hold few seats in either corporate or ecclesial governance despite being the framework's stated priority.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_communities, beneficiary,
    powerless, generational, trapped, continental).

% Children online, the elderly, the disabled, and the poor are the constraint's stated protection priority. Protective provisions reach them rhetorically and sometimes concretely through platform-pressure campaigns and policy advocacy. Their own capacity to enforce anything is minimal.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, families_and_vulnerable_populations, beneficiary,
    moderate, generational, constrained, national).

% Large AI labs and platform firms bear compliance demands (transparency, accountability, participatory oversight), moral censure of extractive business models, and exclusion from legitimacy narratives built on the framework. They can and mostly do disregard ecclesial authority at negligible material cost, engaging selectively where signature events buy reputational cover.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_ai_monopolies, payer,
    institutional, biographical, mobile, global).

% Autonomous-weapons and defense-AI programs are primary objects of the framework's censure. Dignity-based demands against lethal autonomy conflict with their core business. They disregard the interpretive authority entirely and face no material consequence for doing so.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex, payer,
    institutional, biographical, mobile, global).

% Data-extractive advertising and financialized data economies are condemned by the universal destination of goods teaching. Under advocacy pressure they make marginal adjustments and arbitrage jurisdictional gaps rather than altering the underlying model; quarterly horizons dominate any longer obligation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_data_finance, payer,
    powerful, immediate, arbitrage, global).

% National and supranational regulators gain a ready-made dignitary vocabulary, civil-society allies, and framing support for AI oversight. They pay by facing sustained pressure to ground legitimacy in an interpretive authority they do not recognize and cannot control, and by absorbing advocacy campaigns routed through ecclesial channels into their rulemaking processes.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, secular_regulators, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__magisterial_subsidiarity_reading, secular_regulators, payer).

% Engineers, executives, and researchers inside the tradition are called to subordinate their daily work to doctrines they partly affirm. They receive vocational meaning, community, and a moral framework for their craft while bearing recurring conflict between employer incentives and doctrinal demands. Leaving means exiting either the profession or the church.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_technologists, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_technologists, beneficiary).

% Secular ethicists, other faith traditions, and democratic theorists hold that AI governance legitimacy cannot route through a single tradition's interpretive monopoly. They stand outside the framework's adjudication: their objections register as external criticism and never as inputs to authoritative interpretation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, non_catholic_ethicists, excluded,
    organized, generational, constrained, global).

% Scholars of technology governance and political theology map how dignity language, subsidiarity claims, and ecclesial advocacy move through AI policy venues. They see the whole structure — the coordination content, the interpretive-authority flows, and the enforcement limits — without collecting from or paying into it.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__magisterial_subsidiarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates states, firms, and civil society around a shared standard of legitimate AI governance: decisions taken at the lowest competent level (subsidiarity), burdens shared with the worst-off (solidarity), AI benefits broadly distributed (universal destination of goods), and technology subordinated to human dignity through transparency, participation, and protection of the vulnerable. Without it, AI governance fragments into rival legitimacy claims with no common evaluative vocabulary.
% TRANSFER_FUNCTION: Moves interpretive authority and agenda-setting power to the Magisterium and its aligned institutions; moves compliance obligations — transparency, accountability mechanisms, participatory oversight, vulnerability protections — onto AI developers, platforms, defense contractors, and extractive finance; moves protective framing and occasional concrete advocacy gains toward workers, Global South communities, and vulnerable populations.
% ABSENT_VOICES: Non-Catholic traditions, secular ethicists, and the governed firms themselves had no seat in determining what the authoritative interpretation requires — interpretation flows from the teaching office, not from those bound by its outputs. Global South communities are invoked as beneficiaries but rarely seated as interpreters. Their absence is what makes the unanimity of the framework's self-presentation possible.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, AI governance discourse would lose one of its principal principled frameworks: Rome Call-style multi-stakeholder coalitions would dissolve, dignity-and-subsidiarity language would thin out of international AI negotiations, faith-based advocacy networks would lose their coordinating doctrine, and secular regulators would lose an ally vocabulary — the field would reorganize around the remaining technocratic, market, and pluralist legitimacy claims.
% FOUNDING_PROBLEM: The industrial-era social question first addressed by Rerum Novarum (1891) — how to order transformative productive technology so it serves human dignity rather than subordinating persons to technical-economic logics — extended by successive pontificates to each new technological order, now applied to artificial intelligence.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration of the problem's liveness exists outside the beneficiary set: secular political-theology scholarship, UNESCO and Council of Europe AI instruments invoking human dignity and subsidiarity independently of Magisterial authority, and non-Catholic signatories of Rome Call-style declarations all attest that dignity-governance of transformative technology is a live problem. No one outside the tradition attests the Magisterium's standing to adjudicate it — market-libertarian and technocratic commentators explicitly deny that interpretive monopoly. The problem is corroborated; the adjudicating authority is not.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__magisterial_subsidiarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__magisterial_subsidiarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.52: substantial asymmetric extraction — interpretive authority, agenda-setting power, and institutional relevance accrue to the teaching office and its aligned institutions, while compliance burdens, moral censure, and legitimacy exclusion fall on actors (most AI developers, defense contractors, extractive finance) who never consented to that interpretive authority. Genuine coordination content keeps epsilon below snare range: subsidiarity, solidarity, and dignity protections address a real fragmentation problem in AI governance and occasionally deliver concrete advocacy wins. Suppression 0.40: enforcement is suasion-based — delegitimation, moral censure, civil-society pressure, diplomatic advocacy — with no coercive force over non-consenting actors; suppression is authored as a raw structural property and is NOT scaled by power or scope (only extractiveness is scaled, by directionality and scope, in the engine). Theater 0.30: declaratory surplus (symposia, declarations, signature events) visibly outpaces verifiable protections, but the teaching and convening functions remain substantively real. Accessibility collapse 0.25: rival readings remain fully available; the constraint forecloses almost nothing. Resistance 0.55: active rejection from market-libertarian and technocratic camps, indifference from industry, contestation from secular ethicists. All three tracked series run on one shared time grid (points 0,3,6,9,12,15) so every metric is authored at every examined time point; the rising base_extractiveness trajectory records extraction accumulation — sector-specific AI demands layered onto the general social-doctrine framework — as an abductive hypothesis for investigation, not a reclassification.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute different types from identical structure. From the magisterium's position the arrangement is faithful transmission of a coherent anthropological framework — the interpretive-authority flows that payers read as rent are, from inside, indistinguishable from the teaching office itself, and the identity lock makes the distinction undecidable from that seat. From the mobile payer seats (private_ai_monopolies, military_industrial_complex) the constraint is externally imposed moral taxation without representation: obligations and censure flow from an authority they never recognized, exit is materially cheap but reputationally priced for engaged firms. Intended-beneficiary seats compute partial delivery — rhetorical priority with thin material transfer. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (workers_facing_automation, global_south_communities, families_and_vulnerable_populations, catholic_social_institutions) drive derived directionality toward the beneficiary end — the constraint subsidizes them, damping or inverting effective extraction; the trapped exit of workers and Global South communities matters less here because they sit on the subsidized side. Victim declarations (private_ai_monopolies, military_industrial_complex, extractive_data_finance) drive derived directionality toward the target end — amplified effective extraction, moderated somewhat by their mobile/arbitrage exits, which sit nearer the beneficiary end than trapped targets would. The magisterium, as agenda-setter with identity_locked exit, sits nearest the full-beneficiary end: it collects the framework's principal yields and structurally cannot exit. Dual-positioned agents (secular_regulators, catholic_technologists) derive mid-range directionality; the identity lock on catholic_technologists pushes their effective extraction upward relative to otherwise similar moderates, since locked targets sit nearer the full-target end than mobile ones. Global spatial scope amplifies effective extraction modestly for the target seats (verification is harder at scale).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ordering transformative technology to serve dignity — is contested-live: the industrial social question was substantially absorbed by the welfare-state settlement, but the tradition has repeatedly re-grounded itself on each new technological order, and outside parties (UNESCO, Council of Europe, secular scholars) corroborate the problem's liveness even while denying the adjudicating authority. The mismatch consumer reads founding_problem_status=contested x disappearance_verdict=world_rearranges: no dead-mandate zombie flag fires; mandatrophy is not resolved. The tangled_rope classification prevents mislabeling in both directions: read as pure snare, one misses the real coordination (subsidiarity and dignity protections solve a genuine fragmentation problem and deliver occasional concrete protections); read as pure rope, one misses the asymmetric extraction (non-consenting actors fund an interpretive-authority claim through compliance and censure while the receipt concentrates in one seat). Long-term piton-drift risk is recorded: if declaratory output keeps outpacing delivered protection, theater_ratio crosses 0.5 and the coordination story becomes cover — the omega on the delivery gap tracks exactly this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_rent_vs_content_extraction,
    'How much of the measured extraction flows from the Magisterium''s claim to authoritative interpretation, versus from the substantive demands of the principles themselves?',
    'Counterfactual comparison with the same principles advanced under distributed interpretation (democratic-pluralist mode): if compliance burdens and censure asymmetries persist without the interpretive monopoly, extraction is content-driven; if they drop, it is interpretive-authority rent.',
    'Rent-driven extraction supports tangled_rope with strong capture coloring toward the magisterium seat; content-driven extraction would indicate the burden is intrinsic to dignity-governance demands and would soften the capture reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_rent_vs_content_extraction, conceptual, 'Whether extraction is interpretive-authority rent or intrinsic to the principles'' demands.').

omega_variable(
    binding_scope_over_nonconsenting_actors,
    'Is the constraint binding on non-Catholic AI governance actors, or operative only within the ecclesial community plus voluntary signatories?',
    'Track uptake channels: if international-law advocacy converts suasion into treaty obligations reaching non-parties, binding scope extends; if uptake remains voluntary signature events and intra-ecclesial application, scope stays consensual.',
    'Universal binding scope widens the victim set to all secular actors and raises effective extraction across the board; intra-ecclesial scope narrows victims to identity-locked members and leaves mobile outsiders essentially untouched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_scope_over_nonconsenting_actors, empirical, 'Whether the constraint''s authority claim reaches actors who never recognized it.').

omega_variable(
    subsidiarity_self_application_tension,
    'Does centralized Magisterial interpretation violate the reading''s own subsidiarity principle, which locates decisions at the lowest competent level?',
    'Examine the tradition''s official self-understanding (whether subsidiarity is scoped to civil-society/state relations with doctrinal authority exempted) and observe whether interpretive functions devolve to local churches and lay experts over time.',
    'If the exemption is principled, the coordination claim stands intact; if the exemption is self-serving, part of the coordination function is theatrical and long-run piton-drift risk rises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidiarity_self_application_tension, conceptual, 'Internal consistency of subsidiarity with centralized interpretation.').

omega_variable(
    enforcement_hardening_trajectory,
    'Will enforcement remain suasion-based, or harden into legal and treaty mechanisms with coercive force?',
    'Monitor international AI governance negotiations for adoption of dignity and subsidiarity language with compliance machinery, and ecclesial diplomatic investment in binding instruments.',
    'Hardening raises suppression and pushes the constraint toward snare-flavored tangled_rope; persistent suasion keeps suppression low but feeds theater_ratio growth as declaratory activity substitutes for binding force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_hardening_trajectory, empirical, 'Trajectory of enforcement from moral suasion toward binding mechanisms.').

omega_variable(
    vulnerable_protection_delivery_gap,
    'Do the constraint''s operations deliver measurable protection to workers, Global South communities, and vulnerable populations, or primarily declaratory and reputational goods?',
    'Audit concrete outcomes traceable to the constraint''s advocacy (policy adoptions, enforcement actions, resource transfers) against declaratory output volume over the interval.',
    'A widening delivery gap confirms extraction accumulating on top of decaying coordination — the piton-drift precursor; delivered protection validates the coordination half of the tangled_rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_protection_delivery_gap, empirical, 'Gap between protective claims and delivered protections.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(ai_g_tr_t0, observed).
narrative_ontology:measurement(ai_g_tr_t3, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 3, 0.2).
narrative_ontology:measurement_basis(ai_g_tr_t3, observed).
narrative_ontology:measurement(ai_g_tr_t6, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 6, 0.23).
narrative_ontology:measurement_basis(ai_g_tr_t6, observed).
narrative_ontology:measurement(ai_g_tr_t9, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 9, 0.26).
narrative_ontology:measurement_basis(ai_g_tr_t9, observed).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(ai_g_tr_t12, observed).
narrative_ontology:measurement(ai_g_tr_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement_basis(ai_g_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(ai_g_be_t0, observed).
narrative_ontology:measurement(ai_g_be_t3, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 3, 0.39).
narrative_ontology:measurement_basis(ai_g_be_t3, observed).
narrative_ontology:measurement(ai_g_be_t6, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 6, 0.43).
narrative_ontology:measurement_basis(ai_g_be_t6, observed).
narrative_ontology:measurement(ai_g_be_t9, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 9, 0.47).
narrative_ontology:measurement_basis(ai_g_be_t9, observed).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement_basis(ai_g_be_t12, observed).
narrative_ontology:measurement(ai_g_be_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(ai_g_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(ai_g_su_t0, observed).
narrative_ontology:measurement(ai_g_su_t3, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 3, 0.26).
narrative_ontology:measurement_basis(ai_g_su_t3, observed).
narrative_ontology:measurement(ai_g_su_t6, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 6, 0.3).
narrative_ontology:measurement_basis(ai_g_su_t6, observed).
narrative_ontology:measurement(ai_g_su_t9, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 9, 0.33).
narrative_ontology:measurement_basis(ai_g_su_t9, observed).
narrative_ontology:measurement(ai_g_su_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 12, 0.37).
narrative_ontology:measurement_basis(ai_g_su_t12, observed).
narrative_ontology:measurement(ai_g_su_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement_basis(ai_g_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__magisterial_subsidiarity_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI governance legitimacy' decomposes into four structurally distinct readings of one kernel, each with its own stable epsilon, beneficiary/victim structure, and classification (epsilon-invariance decomposition). This member instantiates the magisterial subsidiarity reading; the upstream doctrinal corpus (fixed social-encyclical text plus living Magisterium) supplies the legitimacy claim that this reading presses into AI governance venues, creating downstream pressure on the technocratic and democratic-pluralist siblings' operating environments without foreclosing either. The market-libertarian sibling shares no structural channel with this reading beyond direct discursive repudiation in both directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
