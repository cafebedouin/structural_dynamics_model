% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__capital_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__capital_supremacy_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__capital_supremacy_reading
 *   human_readable: Trade Agreement Capital Supremacy: Domestic Regulatory Subordination
 *   domain: international_trade/regulatory_federalism/political_economy
 *
 * SUMMARY:
 *   Under the capital supremacy reading of the trade agreement kernel, treaty
 *   text constitutes supreme law that overrides domestic regulatory
 *   standards; capital mobility and regulatory harmonization (at minimal
 *   levels) become mandatory treaty obligations, not negotiable policy
 *   choices for signatory states. This reading interprets investor protection
 *   chapters and dispute-resolution clauses as vesting regulatory authority
 *   over labor and environment in the treaty framework and in arbitral
 *   interpretation, not in domestic democratic processes. Domestic labor and
 *   environmental regulatory agencies face subordination to capital's
 *   treaty-protected claim of market access and non-discriminatory treatment.
 *   The constraint extracts regulatory authority upward (from domestic to
 *   supranational) and distributes economic gains to mobile capital while
 *   imposing costs on workers, environmental constituencies, and subnational
 *   governments. This reading is one of three contending interpretations of
 *   the same treaty text; the others (embedded liberalism, sovereignty
 *   primacy) construct different beneficiary/victim structures from the same
 *   kernel.
 *
 * KEY AGENTS:
 *   - multinational_capital: global institutional actor, beneficiary of regulatory optionality and arbitral protection, arbitrage-level exit options
 *   - domestic_labor_standards_enforcement: organized national apparatus, constrained by capital mobility threat and litigation risk, subject to treaty obligations
 *   - environmental_regulatory_agencies: organized national apparatus, identity-locked to environmental protection mission but operationally subordinate to treaty interpretation
 *   - trade_dispute_arbitrators: institutional agenda-setter, interprets treaty text, controls what regulatory action is permissible, benefits from interpretive authority
 *   - national_governments: nominal agenda-setter but constrained by treaty obligations, cannot unilaterally revise or reinterpret
 *   - workers and environmental constituencies: powerless, excluded from dispute mechanism, subject to treaty commitments made without their participation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.81).
domain_priors:suppression_score(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.77).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "Trade Agreement Capital Supremacy: Domestic Regulatory Subordination").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "international_trade/regulatory_federalism/political_economy").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, '035bf935-8977-46dc-af05-22b7b580afb6').
narrative_ontology:cs_kernel_codification('035bf935-8977-46dc-af05-22b7b580afb6', fixed_text).
narrative_ontology:cs_authority_grounding('035bf935-8977-46dc-af05-22b7b580afb6', extraction).
narrative_ontology:cs_interpretation_layer_present('035bf935-8977-46dc-af05-22b7b580afb6').
narrative_ontology:cs_reading_relation('035bf935-8977-46dc-af05-22b7b580afb6', nafta_jurisdictional_boundary__embedded_liberalism_reading, influences).
narrative_ontology:cs_reading_relation('035bf935-8977-46dc-af05-22b7b580afb6', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('035bf935-8977-46dc-af05-22b7b580afb6', foundational, investor_protection_supremacy).
narrative_ontology:cs_axiom_status(investor_protection_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('035bf935-8977-46dc-af05-22b7b580afb6', investor_protection_supremacy, instrumental).
narrative_ontology:cs_axiom('035bf935-8977-46dc-af05-22b7b580afb6', secondary, regulatory_harmonization_at_capital_preferred_levels).
narrative_ontology:cs_axiom_status(regulatory_harmonization_at_capital_preferred_levels, holdable).
narrative_ontology:cs_axiom_grounding('035bf935-8977-46dc-af05-22b7b580afb6', regulatory_harmonization_at_capital_preferred_levels, instrumental).
narrative_ontology:cs_reference_frame('035bf935-8977-46dc-af05-22b7b580afb6', capital_protection_framework).
narrative_ontology:cs_drift_state('035bf935-8977-46dc-af05-22b7b580afb6', contemporary_post_thirty_years_arbitral_jurisprudence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('035bf935-8977-46dc-af05-22b7b580afb6', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_capital).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_dispute_arbitrators).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_standards_enforcement).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, subnational_governments).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, capital_mobility_supremacy).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, harmonized_regulatory_minimalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Multinational corporations and investment funds operating across the treaty zone gain regulatory optionality: they can structure production, sourcing, and operations to exploit the lowest-cost regulatory jurisdiction while claiming treaty-protected rights to market access and non-discriminatory treatment. If a jurisdiction tightens labor or environmental standards, the firm can threaten relocation or file an investor claim for compensation under the treaty's investor protection chapters. The institutional architecture of the treaty (arbitral tribunals, investment chapters, most-favored-nation clauses) is designed to protect capital's interests and enforce non-discrimination among capital sources. Gains flow directly to the capital seat in the form of lower compliance costs, higher returns on investment, and institutional protection against downward regulatory pressure.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_capital, beneficiary,
    institutional, generational, arbitrage, global).

% International arbitral tribunals (investor-state dispute settlement panels) adjudicate claims under the treaty's investment chapters. Arbitrators are drawn from a pool of trade lawyers and former government officials with expertise in investment law. They interpret treaty language, determine whether regulatory measures constitute expropriation or unfair treatment, and issue binding awards. Their professional role makes them gatekeepers of what regulatory action is permissible within the treaty framework. They benefit from high claim volumes (fees, professional visibility, case complexity) and from interpretive authority (clarifying what the treaty means). They enforce the capital-supremacy reading by interpreting treaty language in ways that expand investor protections and constrain domestic regulatory space. Their agenda-setting function means they shape what regulatory authority is permissible; their beneficiary status follows from the direct economic interest in maintaining high-volume dispute activity and from the professional prestige of interpretive authority.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_dispute_arbitrators, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_dispute_arbitrators, beneficiary).

% National ministries and regulatory bodies tasked with enforcing labor law find their authority constrained by treaty obligations. New or strengthened labor standards face challenge under the treaty's investment chapters as potential non-tariff barriers or indirect expropriation. Enforcement against treaty-covered multinational enterprises faces claims of unfair treatment under investor-state dispute settlement. The threat of litigation and compensation claims chills the regulatory appetite: proposed wage increases, workplace safety standards, or union protections are withdrawn after legal analysis reveals treaty vulnerability. The enforcement apparatus persists in form (ministries exist, regulations are drafted) but its scope narrows as capital claims treaty protection against new standards. Workers' interests in raising standards face capital mobility threat: announce a labor standard and capital relocates to a lower-cost neighbor within the treaty zone. The organized nature of the apparatus means the payer function is concentrated and visible, but the apparatus cannot exit the constraint except by national exit from the treaty.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_standards_enforcement, payer,
    organized, biographical, constrained, national).

% Environmental regulators face structurally similar constraints: pollution standards, emissions controls, restrictions on extractive industries, and climate mitigation measures are vulnerable to investor claims for compensation. Agencies retain formal authority to promulgate rules but lack effective enforcement against firms that treat the treaty's investor protections as insurance against regulatory action. Professional identity is locked to the environmental protection mission—career advancement depends on strong environmental outcomes—but the constraint makes that mission subordinate to capital's claim of treaty-protected profitability. The regulatory apparatus persists as theater: environmental impact assessments are conducted, emissions standards are written, protected areas are designated, but enforcement is chilled by litigation threats and the knowledge that regulatory action triggers compensation claims. The identity_lock is the critical distinction: environmental professionals cannot exit the identity (they defined themselves through environmental protection), so they absorb the conflict between mission and constraint rather than switching roles or exiting the apparatus.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_regulatory_agencies, payer,
    organized, biographical, identity_locked, national).

% States and provinces within the treaty zone lose unilateral regulatory capacity. A subnational jurisdiction cannot tighten environmental or labor standards without national government risking investor claims and damages at the national level. If a province passes stricter emissions standards and multinational firms file investor claims, the national government bears the litigation cost and compensation liability. Subnational governments are trapped: exit from the constraint requires exiting the treaty entirely, which is a collective national decision beyond any single province's control. Subnational actors (mayors, state legislators, regional environmental bodies) represent constituencies that support stricter standards, but they cannot act unilaterally. They bear the cost of subordination without the authority to make binding decisions about treaty compliance.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, subnational_governments, payer,
    moderate, generational, trapped, national).

% Signatories to the trade agreement retain nominal authority but operate under binding treaty obligations. They enforce the treaty through their courts and arbitral cooperation mechanisms. Their capacity to regulate is conditioned on treaty compliance: if they pass new standards that capital characterizes as treaty violations, they face arbitral claims and compensation liability. Their regulatory agencies are subordinate to treaty interpretation—domestic courts must recognize arbitral awards and enforce them against the domestic treasury. Exiting the treaty is theoretically possible but economically costly (trade retaliation, investment withdrawal, supply-chain disruption) and politically costly (capital-aligned business interests oppose exit). National governments are powerful in a global sense but are constrained by the treaty's binding nature and by capital mobility threat.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, national_governments, agenda_setter,
    powerful, generational, constrained, national).

% Voters, workers, unions, environmental organizations, and civil-society constituencies whose interests are protected by domestic labor and environmental law have no direct voice in the treaty mechanism. They are bound by treaty commitments made by their national governments without their participation. They can petition their governments to enforce regulations, but those governments face capital exit threats and arbitral claims. Their exclusion from the dispute mechanism is structural: investor-state arbitration involves only states and investors, not citizens or civil-society actors. Workers cannot appear in arbitral proceedings to defend labor standards; environmental organizations cannot appear to defend ecosystems. They are trapped at the subnational and individual level with no mechanism to participate in the supranational decision-making that constrains their regulatory protections.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, workers_and_environmental_constituencies, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_capital).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__capital_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified market zone with enforceable rules on tariffs, intellectual property, and investment treatment; resolves collective-action problems of market access and dispute adjudication that would otherwise require bilateral negotiation.
% TRANSFER_FUNCTION: Transfers regulatory authority upward from subnational and national governments to the level of treaty interpretation; transfers economic gains from labor and environmental constituencies to mobile capital; transfers sovereignty over standards-setting from domestic democratic processes to arbitral tribunals interpreting treaty language.
% ABSENT_VOICES: Workers, environmental organizations, and civil-society constituencies affected by regulatory subordination have no direct standing in the dispute mechanism and no seat at the interpretation table. Non-signatory states and competing trade frameworks (labor-protective, environment-protective, or regional groupings with different values) are structured out. Within signatory states, subnational and local governments are bound by the national commitment without local consent.
% DISAPPEARANCE_RATIONALE: If the treaty text dissolved overnight, capital would lose the legal instruments (investor protection chapters, ISDS mechanisms) that subordinate domestic regulation to investor claims. Labor and environmental standards would revert to domestic democratic control (subject to local political struggle, not capital mobility threat). Regulatory agencies would recover jurisdictional authority. The institutional mechanism (arbitral tribunals interpreting the treaty) would dissolve. Production chains and investment flows would reorganize under different risk/return calculations—some capital would exit low-regulation zones, others would relocate to jurisdictions with stronger standards.
% FOUNDING_PROBLEM: In the 1990s, manufacturers from high-regulation jurisdictions (labor-protective, environmentally stringent) faced competitive pressure from lower-cost jurisdictions with minimal labor/environmental oversight. Governments sought a mechanism to prevent a 'race to the bottom' in labor and environmental standards while ensuring market access for their firms. Trade agreement sought to lock in a level playing field through harmonized (but minimal) standards and dispute resolution for trade complaints.
% FOUNDING_PROBLEM_CORROBORATION: Governments initially claimed the purpose was to prevent regulatory races-to-the-bottom. However, three decades of jurisprudence on investor-state disputes (documented in arbitral awards, trade law scholarship, and government litigation budgets) show that the treaty mechanism has been used primarily to subordinate domestic standards upward pressure, not to coordinate common floors. Labor economists document stagnation in labor standards across the treaty zone since the agreement entered force, coupled with capital mobility to low-regulation jurisdictions—the opposite of the founding problem's intended solution. Environmental studies document regulatory chilling: proposed domestic standards are withdrawn after litigation threats. The founding problem (race-to-the-bottom) persists; the mechanism subordinates attempts to solve it. No credible external source attributes current treaty jurisprudence to the prevention of regulatory races-to-the-bottom; instead, trade scholars, labor advocates, and environmental lawyers document the extractive reading as the live institutional practice.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__capital_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__capital_supremacy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__capital_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) and rising (from 0.45 at t0) because the constraint's operation increasingly subordinates domestic regulatory authority to capital's claims. Early in the interval (t0–t10), the extractive function was masked by the nominal coordination function (market access, dispute resolution); over 30+ years of arbitral jurisprudence, the extractive structure clarified and intensified. Suppression is high (0.77) because the constraint's persistence depends on actively enforcing arbitral awards against domestic governments that attempt to strengthen labor or environmental standards. The enforcement mechanism (arbitration + litigation threat + capital mobility) operates continuously to chill regulatory action. Theater is moderate (0.42): formal regulatory bodies continue to operate and issue rules, but enforcement is chilled by the legal and financial cost of litigation and the threat of capital exit. The measurement series show extraction accumulation (rising base_extractiveness + rising theater_ratio over the interval) consistent with a constraint whose nominal function (coordination) decayed while its extractive function intensified—a mandatrophy candidate. The three time-series share one grid so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the multinational capital seat: the treaty constraint is pure coordination—a framework that solves collective-action problems in market access and dispute resolution, enabling investment and trade that benefits all parties. Regulatory subordination is not extraction but the necessary cost of a level playing field and predictable legal environment. From the environmental agency seat: the constraint is pure extraction—capital's treaty-protected claim to compensation for regulatory action amounts to a veto on standards enforcement, and the arbitral mechanism is a private court enforcing capital's interests against public welfare. The engine's per-seat classification should detect this divergence: capital perceives rope or light tangled-rope (with beneficiary surplus); environmental agencies perceive snare (with target extraction). The authored claim (tangled_rope) reflects the reading's structural assertion: genuine coordination (market access) is bound together with asymmetric extraction (regulatory subordination) in one institutional mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational capital sits at the full beneficiary end (d ≈ 0.1): the constraint subsidizes capital's regulatory optionality and guarantees arbitral recourse against downward regulatory action. Domestic regulatory agencies sit at the target end (d ≈ 0.85): they bear the cost of subordination and constrained authority. Labor and environmental constituencies sit near full target (d ≈ 0.9): constrained exit (trapped at subnational level, excluded from dispute mechanism), powerless to resist, bearing the cost of regulatory subordination. The national governments sit near 0.7: they have some nominal authority (can sign or exit the treaty) but are powerlessly constrained by capital mobility threat and treaty obligation. Arbitrators sit at high d (0.75) in an extractive sense: they extract interpretive authority and professional authority from the constraint's persistence, though their formal role is neutral adjudication. The divergence between beneficiary and payer seats is the key measurement: capital and arbitrators perceive coordination and neutrality; workers and environmental agencies perceive extraction and subordination. The engine computes this divergence from the structural data (beneficiary/victim declarations + power atoms + exit options); the authored claim does not adjudicate which perception is 'correct'—both are locally rational.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem was the race-to-the-bottom in labor and environmental standards; the constraint was supposed to solve it by coordinating common minimum floors. However, thirty years of operation show the founding problem persists while the constraint increasingly subordinates attempts to solve it. The measured extraction rose from 0.45 to 0.81 over the interval—not because new extraction was added, but because the initial coordination function (market access, dispute resolution) decayed into theater while the extraction function (regulatory subordination) clarified and intensified. Theater_ratio rose from 0.15 to 0.42: regulatory agencies continue to issue standards (theater) but enforcement is chilled and compensation claims are constant (extraction). This is mandatrophy: the arrangement's original function (coordination toward common floors) is dead; the mechanism persists to serve its extractive function (capital protection, regulatory subordination). The disappearance_verdict (world_rearranges) confirms: if the treaty dissolved, capital would lose its regulatory protection and enforcement mechanism, production chains would reorganize, and domestic jurisdictions could raise standards without compensation claims—the constraint's death would rearrange the world, not leave it unchanged. The founding_problem_status (dead) and founding_problem_corroboration establish that external sources (labor economists, trade scholars, government litigation records) document the founding problem as unsolved while the mechanism subordinates solutions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_solver_vs_constraint_function,
    'Was the treaty mechanism designed to solve the race-to-the-bottom by coordinating upward pressure on standards, or to protect capital from upward pressure by subordinating domestic regulatory authority?',
    'Textual analysis of legislative history (negotiating records, parliamentary debates) combined with comparison of stated intent to actual jurisprudence. If intent was upward coordination but jurisprudence subordinates it, the constraint exhibits mandatrophy.',
    'If intent was upward coordination (founding_problem_status = live, constraint designed to solve it), then the extractive jurisprudence is a distortion of the treaty''s purpose and the constraint should be reclassified toward piton (atrophied function maintained theatrically). If intent was capital protection (founding_problem_status = dead from the start, claimed falsely), then the capital_supremacy reading is the accurate structural reading and the constraint remains tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_solver_vs_constraint_function, empirical, 'Whether the founding problem was the genuine policy intent or a cover story for capital protection.').

omega_variable(
    arbitral_jurisprudence_as_reading_or_design,
    'Is the capital-subordination jurisprudence an inevitable reading of the treaty text, or is it a discretionary interpretation chosen by arbitrators from among multiple plausible readings?',
    'Comparative legal analysis: examine arbitral awards where tribunals rejected capital claims under the same treaty language, and identify the interpretive choices that produced different outcomes. If different readings of the same text are structurally possible, jurisprudence is chosen, not inevitable.',
    'If inevitable reading: the capital_supremacy_reading is the accurate structural reading of the kernel; other readings are misreadings. If discretionary interpretation: arbitrators'' interpretive choices constitute the agenda-setting function, and the treaty''s meaning is constructed by the dispute mechanism, not settled by text. This would elevate the arbitrators'' role from neutral interpreters to agenda-setters and beneficiaries of the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arbitral_jurisprudence_as_reading_or_design, conceptual, 'Whether capital subordination is textually determined or interpretively chosen.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the regulatory chilling effect on domestic labor and environmental standards structural (external capital mobility threat, litigation risk, compensation claims) or internalized (regulatory agencies internalize the threat and pre-emptively avoid action)?',
    'Post-treaty-renegotiation or post-dispute-mechanism-reform observation: if suppression persists after litigation threat is reduced, suppression is partially internalized (regulatory cultures adapted to subordination). If suppression declines when threat is reduced, suppression was primarily structural.',
    'If internalized: regulatory agencies carry the suppression with them even if legal threats decline; recovery of regulatory authority would require institutional and professional culture change, not just treaty reform. If structural: treaty renegotiation removing investor claims would restore regulatory capacity relatively quickly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of regulatory action is enforced by external mechanisms or internalized in regulatory culture.').

omega_variable(
    reading_contention_structural_or_rhetorical,
    'Does the capital_supremacy reading constitute a genuine contending interpretation of the kernel text, or is it a post-hoc rationalization of arbitral jurisprudence that was never the text''s intended meaning?',
    'Historical analysis of trade negotiation documents, statutory interpretation in national parliaments, and early arbitral decisions (pre-2000) compared with later jurisprudence. If early jurisprudence rejected capital-supremacy readings and later jurisprudence adopted them, the reading is an emergent interpretation, not a textual inevitability. If negotiators and early interpreters clearly endorsed capital protection, the reading is structural.',
    'If emergent interpretation: the capital_supremacy_reading describes a process by which arbitrators and capital interests reshaped the treaty''s meaning, and the reading''s status changed over the interval from ''contested'' to ''institutionally dominant.'' This would support mandatrophy (founding intent subordinated by interpretive drift). If structural: the reading is a valid account of the text''s actual meaning from the start, and institutions simply clarified and enforced it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contention_structural_or_rhetorical, conceptual, 'Whether the capital-supremacy reading is a textually determined interpretation or an institutional remaking of the treaty''s meaning.').

omega_variable(
    victim_identity_lock_reversibility,
    'Can domestic environmental regulators (identity_locked) recover regulatory authority if the treaty''s investor protections were removed, or has institutional and professional identity become fused with subordination such that recovery would require cultural decolonization?',
    'Comparative case study: examine jurisdictions that exited similar treaties or reformed their dispute mechanisms; track whether regulatory agencies recovered capacity and pursued stronger standards, or continued subordinated behavior despite legal permission.',
    'If reversible: removing the constraint would restore regulatory capacity; the constraint''s costs are primarily distributional (capital gains, regulators lose authority). If irreversible: the constraint has induced identity fusion (regulators see themselves as subordinate to capital interests); recovery would require generational institutional change. This affects the constraint''s effective suppression and the agent''s directionality—irreversibility increases effective suppression and locks d toward full target.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_identity_lock_reversibility, empirical, 'Whether identity-locked regulatory agencies can recover functional authority if legal constraints are removed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(naft_tr_t0, observed).
narrative_ontology:measurement(naft_tr_t5, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(naft_tr_t5, observed).
narrative_ontology:measurement(naft_tr_t10, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(naft_tr_t10, observed).
narrative_ontology:measurement(naft_tr_t15, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement_basis(naft_tr_t15, observed).
narrative_ontology:measurement(naft_tr_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(naft_tr_t20, observed).
narrative_ontology:measurement(naft_tr_t25, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(naft_tr_t25, observed).
narrative_ontology:measurement(naft_tr_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(naft_tr_t30, observed).
narrative_ontology:measurement(naft_tr_t35, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(naft_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(naft_be_t0, observed).
narrative_ontology:measurement(naft_be_t5, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(naft_be_t5, observed).
narrative_ontology:measurement(naft_be_t10, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(naft_be_t10, observed).
narrative_ontology:measurement(naft_be_t15, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 15, 0.69).
narrative_ontology:measurement_basis(naft_be_t15, observed).
narrative_ontology:measurement(naft_be_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement_basis(naft_be_t20, observed).
narrative_ontology:measurement(naft_be_t25, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 25, 0.79).
narrative_ontology:measurement_basis(naft_be_t25, observed).
narrative_ontology:measurement(naft_be_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement_basis(naft_be_t30, observed).
narrative_ontology:measurement(naft_be_t35, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 35, 0.81).
narrative_ontology:measurement_basis(naft_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(naft_su_t0, observed).
narrative_ontology:measurement(naft_su_t5, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement_basis(naft_su_t5, observed).
narrative_ontology:measurement(naft_su_t10, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(naft_su_t10, observed).
narrative_ontology:measurement(naft_su_t15, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(naft_su_t15, observed).
narrative_ontology:measurement(naft_su_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(naft_su_t20, observed).
narrative_ontology:measurement(naft_su_t25, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement_basis(naft_su_t25, observed).
narrative_ontology:measurement(naft_su_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement_basis(naft_su_t30, observed).
narrative_ontology:measurement(naft_su_t35, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 35, 0.77).
narrative_ontology:measurement_basis(naft_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__capital_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.25).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% The nafta_jurisdictional_boundary kernel decomposes into three constraint stories representing three contending readings of the same treaty text. capital_supremacy_reading interprets the treaty as subordinating domestic labor and environmental standards to capital's treaty-protected claims. embedded_liberalism_reading interprets the treaty as permitting legitimate domestic regulation when non-discriminatory. sovereignty_primacy_reading interprets the treaty as subordinate to domestic sovereign authority. Each reading instantiates a different constraint with a different ε, different beneficiary/victim structure, and different classification. The three readings are structurally linked: capital_supremacy influences both siblings (shapes the institutional environment in which they operate), and is coexistent with both (different parties hold different readings). Each story's network.affects_constraints array links to the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nafta_jurisdictional_boundary__capital_supremacy_reading, organized, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
