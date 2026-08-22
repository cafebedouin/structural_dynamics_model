% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__territorial_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__territorial_sovereignty_reading, []).

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
 *   constraint_id: gdpr_article_3_scope__territorial_sovereignty_reading
 *   human_readable: GDPR Article 3 Extraterritorial Reach — Territorial Sovereignty Reading
 *   domain: technology governance / international law / privacy regulation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   gdpr_article_3_scope: the territorial_sovereignty_reading, which holds
 *   that legitimate regulatory authority is bounded by territorial
 *   sovereignty and that GDPR Article 3(2)'s targeting/monitoring extension
 *   therefore exceeds legitimate authority. Per the kernel-reading epsilon
 *   rule, the referent is the STANDING ARRANGEMENT under contest — Article
 *   3(2)'s extraterritorial application regime as operated since May 2018 —
 *   assessed by this reading's own lights, NOT the territorial-bound
 *   arrangement this reading would put in place. From this seat the regime
 *   delivers real protection to EU residents (the coordination half) while
 *   binding non-consenting foreign actors and overriding non-EU states'
 *   regulatory autonomy (the asymmetric half); the constituency this reading
 *   vindicates — non-EU state regulatory independence — sits in the
 *   victim/excluded seats of the standing arrangement and would be the prime
 *   beneficiary of the endorsed alternative. Claimed_type (tangled_rope) and
 *   the metrics are authored independently: the claim states what I believe
 *   is structurally true, the metrics what I believe is descriptively true;
 *   the engine computes per-seat classifications from the structural data.
 *   KEY AGENTS (by structural relationship): -
 *   eu_data_protection_authorities: Agenda setter (institutional/mobile) —
 *   writes the scope guidelines, enforces across borders, collects fines and
 *   mandate expansion - eu_residents_data_subjects: Coordination beneficiary
 *   (moderate/constrained) — receives enforceable rights; carries indirect
 *   withdrawal/price costs - eu_incumbent_firms: Dual-positioned
 *   beneficiary/payer (powerful/constrained) — compliance moat against
 *   foreign entrants, ongoing domestic compliance costs -
 *   us_multinational_platforms: Primary target (powerful/constrained) — bears
 *   fine exposure and transfer suspensions; cannot exit the EU market -
 *   non_eu_smes_exporters: Secondary target (moderate/constrained) —
 *   proportionally heaviest burden, weakest resources -
 *   non_eu_state_regulators: Excluded institutional party
 *   (institutional/identity_locked) — autonomy overridden, resists via
 *   localization and blocking statutes - public_international_law_scholars:
 *   Analytical observer (analytical/analytical) — supplies the doctrinal
 *   arguments for every camp
 *
 * KEY AGENTS:
 *   - eu_data_protection_authorities: Agenda setter (institutional/mobile) — administers and enforces the extraterritorial scope; collects fines and converts precedents into mandate expansion
 *   - us_multinational_platforms: Primary target (powerful/constrained) — bears multi-billion-euro exposure; exit would forfeit a major revenue share
 *   - non_eu_smes_exporters: Secondary target (moderate/constrained) — heaviest proportional burden, thinnest compliance capacity
 *   - non_eu_state_regulators: Excluded institutional party (institutional/identity_locked) — domestic autonomy overridden; answers with localization, blocking statutes, rival adequacy networks
 *   - eu_residents_data_subjects: Coordination beneficiary (moderate/constrained) — genuine rights received; indirect costs where services withdraw
 *   - eu_incumbent_firms: Dual beneficiary/payer (powerful/constrained) — sunk compliance becomes a moat against foreign entrants
 *   - public_international_law_scholars: Analytical observer (analytical/analytical) — arbitrates the doctrinal contest from no material stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.72).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.68).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Article 3 Extraterritorial Reach — Territorial Sovereignty Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "technology governance / international law / privacy regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, '7d635293-5df8-4dcc-9862-c0a523ee41e1').
narrative_ontology:cs_kernel_codification('7d635293-5df8-4dcc-9862-c0a523ee41e1', fixed_text).
narrative_ontology:cs_authority_grounding('7d635293-5df8-4dcc-9862-c0a523ee41e1', lineage).
narrative_ontology:cs_interpretation_layer_present('7d635293-5df8-4dcc-9862-c0a523ee41e1').
narrative_ontology:cs_reading_relation('7d635293-5df8-4dcc-9862-c0a523ee41e1', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('7d635293-5df8-4dcc-9862-c0a523ee41e1', gdpr_article_3_scope__market_access_reading, influences).
narrative_ontology:cs_axiom('7d635293-5df8-4dcc-9862-c0a523ee41e1', foundational, sovereign_jurisdiction_terminates_at_territorial_border).
narrative_ontology:cs_axiom_status(sovereign_jurisdiction_terminates_at_territorial_border, holdable).
narrative_ontology:cs_axiom_grounding('7d635293-5df8-4dcc-9862-c0a523ee41e1', sovereign_jurisdiction_terminates_at_territorial_border, conventional).
narrative_ontology:cs_axiom('7d635293-5df8-4dcc-9862-c0a523ee41e1', foundational, cross_border_authority_requires_express_state_consent).
narrative_ontology:cs_axiom_status(cross_border_authority_requires_express_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('7d635293-5df8-4dcc-9862-c0a523ee41e1', cross_border_authority_requires_express_state_consent, deontological).
narrative_ontology:cs_reference_frame('7d635293-5df8-4dcc-9862-c0a523ee41e1', westphalian_territorial_jurisdiction).
narrative_ontology:cs_drift_state('7d635293-5df8-4dcc-9862-c0a523ee41e1', contemporary_extraterritorial_enforcement_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7d635293-5df8-4dcc-9862-c0a523ee41e1', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, eu_residents_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, eu_incumbent_firms).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, us_multinational_platforms).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_smes_exporters).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, eu_incumbent_firms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce Article 3(2): publish guidelines on when foreign controllers fall within scope, coordinate cross-border proceedings through the EDPB, and impose corrective orders and fines that reach non-EU headquarters. Fine revenue and settled precedents expand their budgets and mandate. They can recalibrate enforcement intensity, prioritize sectors, or negotiate adequacy arrangements, and no external actor can compel them to stop asserting reach.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, mobile, continental).

% Receive enforceable privacy rights against foreign websites and platforms that target or monitor them, backed by complaint channels and NGO litigation. They carry indirect costs where services withdraw from the EU market or raise prices to fund compliance, and they cannot opt out of being monitored by processors located abroad.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_residents_data_subjects, beneficiary,
    moderate, biographical, constrained, continental).

% Already operate inside the EU legal order, so the compliance burden lands on foreign rivals as a new fixed cost of market entry while their own sunk compliance spend becomes a competitive asset. They also pay ongoing compliance and legal costs themselves, and leaving the EU market is not a realistic option for their business models.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_incumbent_firms, beneficiary,
    powerful, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__territorial_sovereignty_reading, eu_incumbent_firms, payer).

% Process data of hundreds of millions of EU residents from non-EU headquarters; face multi-billion-euro fine exposure, transfer suspensions after Schrems II, and supervision they had no part in legislating. Litigation, corporate restructuring, and lobbying are their levers; abandoning the EU market would forfeit a large share of global revenue, so full exit is off the table — a few smaller services have simply blocked EU visitors instead.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, us_multinational_platforms, payer,
    powerful, biographical, constrained, global).

% Small foreign retailers, SaaS vendors, and publishers selling into the EU discover that the regulation reaches them through website targeting and marketing analytics. Compliance overhead is proportionally heaviest for them; their realistic responses are paying for counsel, geo-blocking EU traffic, or absorbing legal risk — none of which is costless.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_smes_exporters, payer,
    moderate, immediate, constrained, global).

% Agencies such as the US Federal Trade Commission, India's data-protection authority, and China's cyberspace administration find EU rules de facto governing processing on their territory and reshaping their domestic policy space. They answer with blocking statutes, data localization mandates, and rival adequacy networks, but they had no vote in the rules they respond to, and retreating from the territorial-authority premise would dissolve the constitutional foundation of their own offices.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators, excluded,
    institutional, generational, identity_locked, global).

% Debate whether the targeting and monitoring tests reconcile with the Lotus presumption and sovereign equality, track jurisdictional-conflict escalation across the CLOUD Act, blocking-statute, and localization episodes, and supply the doctrinal arguments both camps deploy. They bear no compliance costs and collect no fines.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, public_international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__territorial_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single harmonized data-protection baseline for any processing that touches EU residents, wherever the processor sits — addressing the fact that data flows ignore borders, so a purely territorial rule would leave EU residents' data unprotected abroad and fragment the single digital market.
% TRANSFER_FUNCTION: Moves compliance costs, fine exposure (up to 4% of worldwide turnover), and regulatory deference from non-EU controllers and processors — and from non-EU states' regulatory autonomy — toward EU institutions, EU residents' legal protection, and EU-market incumbents.
% ABSENT_VOICES: Non-EU state regulators and non-EU processing firms had no seat in the EU legislative process that wrote Article 3(2); they object from outside (CLOUD Act, blocking statutes, localization mandates) but cannot vote on the rules that bind them. Non-EU data subjects whose governments' privacy-policy choices are preempted are likewise unrepresented in the arrangement's rule-making.
% DISAPPEARANCE_RATIONALE: If the extraterritorial application regime vanished overnight, protection for EU residents' data processed abroad would collapse to whatever bilateral arrangements exist, the cross-border compliance industry would restructure, pending enforcement actions would evaporate, jurisdictional conflicts would de-escalate, and non-EU states would regain de facto regulatory primacy over processing on their territory — the transnational data economy would reorganize around territorial and negotiated-jurisdiction principles.
% FOUNDING_PROBLEM: Data processing migrated beyond the reach of any single regulator while harms to EU residents remained local; a purely territorial rule would have created a protection vacuum and rewarded relocating processing offshore. Article 3(2) was built to close that gap by extending the regulation to processing that targets or monitors EU residents from anywhere.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and resident-advocacy NGOs attest the protection gap is live, citing ongoing cross-border tracking and offshore data brokerage. Corroboration from outside the benefiting parties: non-EU state regulators and multinational firms concede the underlying gap is real while disputing the mechanism — US officials negotiated the EU-US Data Privacy Framework precisely because some cross-border standard was needed, even while objecting to unilateral assertion. International-law scholarship documents both the gap and the legitimacy dispute. No party seriously attests the founding problem is dead; the contest is over whether the current mechanism is a legitimate solution.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__territorial_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__territorial_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the regime's costs land substantially on actors outside the enacting polity: fine exposure decoupled from any process those actors could vote in, and compliance burdens that fall hardest on small foreign exporters. Suppression (0.68) is authored as a raw structural property — it is NOT scaled by power or scope in the engine's computation — and reflects the coercive machinery the arrangement requires: coordinated EDPB enforcement, transfer suspension as leverage after Schrems II, and contractual propagation of obligations down supply chains via standard contractual clauses. Theater_ratio (0.30) is moderate-low: most enforcement is functional (real orders, real fines), but a growing share of activity on both sides is performative sovereignty signaling — headline announcements timed to jurisdictional disputes, retaliatory statutes aimed at audiences at home. Accessibility_collapse (0.52): alternatives persist but are costly — geo-blocking the EU, data localization, market withdrawal — so understanding the regime does not collapse the option set the way a natural law would. Resistance (0.62) is substantial and organized: the CLOUD Act, blocking statutes, localization mandates, adequacy brinkmanship, and a sustained scholarly campaign. The measurement series run on ONE shared nine-point grid (years since entry into application, 2018–2026) with all three metrics authored at every point; the trajectory is cumulative rather than oscillating — episodic shocks (Schrems II at t≈2, the EU-US Data Privacy Framework at t≈5) produce step-changes, not a repeating cycle, so intermittent reinforcement is not the mechanism here. Coalition note: the victim seats are heterogeneous (firms vs. states), but if non-EU states coordinate a localization bloc, their combined institutional power could shift the balance — the analysis treats coalition formation as the main upside risk to the current trajectory.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat (EU DPAs), the arrangement is a protection regime it built and legitimately administers; from the payer seats (platforms, SMEs), the same structure operates as obligation imposed without representation; from the excluded institutional seat (non-EU regulators), it is a sovereignty violation answered with countermeasures; from the observer seat, both descriptions are simultaneously accurate, which is precisely the tangled-rope signature. Same-level dynamics: EU DPAs and non-EU state regulators hold the same nominal power atom (institutional) yet sit at opposite ends of the arrangement — what differentiates them is not power but position relative to the rule-writing process: one wrote the rule, the other was written about. Identity-lock dynamics: the non-EU regulator seat is authored identity_locked because the fusion is institutional — a sovereignty-based regulator's office is constituted by the territorial-authority premise; conceding extraterritoriality as legitimate would dissolve the foundation of its own mandate, so exit from the conflict is unthinkable even where retreat would be cheap. If that identity frame broke (e.g., a state regulator reconstituted as a market-access agency), its computed directionality would drop sharply and the conflict would de-escalate.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to the low-d end: eu_data_protection_authorities sit nearest the beneficiary pole (collect fines, expand mandate, control the rules); eu_incumbent_firms sit low-to-mid (moat gains partly offset by their own compliance spend); eu_residents_data_subjects sit mid-low (genuine protection received, diffuse indirect costs where services withdraw or reprice). Victims map to the high-d end: non_eu_smes_exporters sit nearest the target pole (heaviest proportional burden, thinnest exit); us_multinational_platforms sit high (enormous absolute stakes, but litigation and restructuring resources dampen effective extraction relative to trapped agents); non_eu_state_regulators sit high with the identity_lock pushing them toward the full-target end despite institutional power — trapped-or-locked targets sit nearer full-target than mobile ones. The excluded role of the non-EU regulators is the structural heart of this reading: the arrangement's rule-making process is exactly where their objection would have been voiced, and their absence from it is what the reading alleges as the legitimacy defect.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is declared: the founding problem — offshore processing of EU residents' data outside any regulator's reach — remains live, so the arrangement has not outlived its function; the contest is over the legitimacy of the mechanism, not obsolescence of the mission. The classification discipline prevents two symmetric errors. Flattening the arrangement to a snare would deny the genuine coordination half: EU residents really do receive enforceable protection they would largely lose under a strict territorial rule, and the single-market harmonization function is real. Excusing it as a rope would deny the asymmetric half: the costs concentrate on actors who never consented and cannot vote, and non-EU state autonomy is overridden as a side effect of enforcement. Tangled_rope preserves both halves and routes the dispute to the correct axis — the representation/legitimacy question carried in the omegas — rather than letting either camp's label settle it. The theater_ratio tracks the posturing share of enforcement activity, not the arrangement's function, so performative sovereignty signaling on either side cannot masquerade as coordination success or extraction failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gdpr_article_3_kernel_reading,
    'This constraint is the territorial_sovereignty_reading of kernel gdpr_article_3_scope; would instantiating the effects_jurisdiction_reading or market_access_reading instead produce a different beneficiary/victim structure and a different epsilon over the same referent?',
    'Comparative classification across the three sibling stories: if the effects reading computes materially lower effective extraction for the non-EU seats over the identical arrangement, the disagreement is located in the legitimacy premise rather than in the facts, and the family''s divergence is fully attributed to reading selection.',
    'Switching readings reassigns the beneficiary set (EU regulators and residents versus non-EU states) and moves epsilon from this reading''s high assessment toward the sibling''s; classifications computed from this file are valid only within this reading and must not be averaged across the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gdpr_article_3_kernel_reading, conceptual, 'Committer-frame omega: this story is one reading of the gdpr_article_3_scope kernel; sibling readings instantiate different constraints from the same text.').

omega_variable(
    westphalian_bound_status,
    'Is the territorial boundedness of jurisdiction a constitutive convention of the interstate order (constructed but settled) or a contestable policy preference losing force as data flows denationalize?',
    'State-practice and opinio-juris survey: do non-EU states'' objections assert the Westphalian premise itself, or do they merely bargain for better terms within an effects-based order they have otherwise accepted?',
    'If the premise is eroding, this reading''s axioms trend toward overridden and the constraint family converges on the effects/market-access settlements; if the premise is robust, jurisdictional conflict persists and escalates, and this reading remains the live challenger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(westphalian_bound_status, conceptual, 'Whether the reading''s foundational premise is a stable constitutive convention or a declining policy preference.').

omega_variable(
    representation_deficit_or_market_incident,
    'Does the burden on non-EU actors reflect a legitimacy defect (obligations without representation) or the ordinary incident of any polity setting terms for access to its market, as import regulation always has?',
    'Doctrinal comparison with settled effects-based jurisdictions that democracies accept (antitrust, securities enforcement): if the targeting/monitoring test is structurally analogous to those accepted exercises, the deficit claim weakens; if it binds actors who made no election of market presence, the claim strengthens.',
    'Resolution toward ''ordinary incident'' collapses this reading''s extraction assessment toward the market_access_reading''s framing; resolution toward ''defect'' hardens the tangled_rope profile at this seat and strengthens the case for consent-based restructuring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(representation_deficit_or_market_incident, conceptual, 'The core legitimacy dispute: unrepresented obligation versus ordinary market-conditioning.').

omega_variable(
    localization_resistance_efficacy,
    'Does data localization as a resistance mechanism actually reduce the burden on non-EU actors, or does fragmentation raise costs for all parties and entrench the conflict?',
    'Natural experiments: Russian and Chinese localization outcomes, India''s draft-cycle iterations, and intra-EU responses to the US CLOUD Act — measure compliance-cost deltas and jurisdictional-dispute frequency before and after localization adoption.',
    'If localization reduces net burden, resistance is functional and the regime''s suppression requirement should ratchet upward in response; if it merely fragments, the escalation dynamic dominates and both blocs'' costs rise — confirming the jurisdictional-conflict-escalation structural delta this reading predicts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(localization_resistance_efficacy, empirical, 'Whether the predicted resistance mechanism works or merely escalates the conflict.').

omega_variable(
    adequacy_consent_or_coercion,
    'Do adequacy decisions convert the extraterritorial assertion into consensual jurisdiction (mitigating the legitimacy defect) or extend it through asymmetric leverage dressed as consent?',
    'Compare negotiation records and post-adequacy behavior: the terms accepted by Japan, the UK, and South Korea versus the suspension leverage the EU exercised during the Schrems II fallout — does partner-state assent survive when the leverage is removed?',
    'A consent reading lowers effective extraction for adequacy partners and softens this reading''s assessment for them; a coercion reading extends the target set and reinforces the high-extraction profile authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adequacy_consent_or_coercion, preference, 'Whether the adequacy web is consent-based jurisdiction or leveraged extension of the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(gdpr_tr_t1, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 1, 0.19).
narrative_ontology:measurement(gdpr_tr_t2, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement(gdpr_tr_t3, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 3, 0.24).
narrative_ontology:measurement(gdpr_tr_t4, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(gdpr_tr_t5, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement(gdpr_tr_t6, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(gdpr_tr_t7, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 7, 0.29).
narrative_ontology:measurement(gdpr_tr_t8, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 8, 0.3).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gdpr_be_t1, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 1, 0.58).
narrative_ontology:measurement(gdpr_be_t2, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2, 0.63).
narrative_ontology:measurement(gdpr_be_t3, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 3, 0.66).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 4, 0.67).
narrative_ontology:measurement(gdpr_be_t5, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 5, 0.69).
narrative_ontology:measurement(gdpr_be_t6, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 6, 0.7).
narrative_ontology:measurement(gdpr_be_t7, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 7, 0.71).
narrative_ontology:measurement(gdpr_be_t8, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 8, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(gdpr_su_t1, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 1, 0.53).
narrative_ontology:measurement(gdpr_su_t2, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2, 0.58).
narrative_ontology:measurement(gdpr_su_t3, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(gdpr_su_t4, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(gdpr_su_t5, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(gdpr_su_t6, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(gdpr_su_t7, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 7, 0.67).
narrative_ontology:measurement(gdpr_su_t8, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 8, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__territorial_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__market_access_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'GDPR extraterritoriality' decomposes into three structurally distinct readings of kernel gdpr_article_3_scope, per the epsilon-invariance principle: effects_jurisdiction_reading (protection follows effects on EU residents; beneficiaries EU residents and regulators), market_access_reading (conditional market access; Brussels Effect standard-setting rather than jurisdictional assertion), and this territorial_sovereignty_reading (jurisdiction territorially bounded; the standing arrangement overrides non-EU state regulatory independence, which this reading vindicates and which data localization operationalizes as resistance). All three share the referent — Article 3's operative scope regime — and author different epsilon from their own lights; upstream CJEU and EDPB interpretive practice feeds all three, which is why the family links run from this story to both siblings. Conflict-escalation episodes (Schrems litigation, CLOUD Act, blocking statutes, localization mandates) are the observable surface where the readings collide.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
