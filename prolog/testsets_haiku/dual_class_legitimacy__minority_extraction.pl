% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__minority_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__minority_extraction, []).

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
 *   constraint_id: dual_class_legitimacy__minority_extraction
 *   human_readable: Dual-Class Share Structure and Minority Governance Extraction
 *   domain: corporate_governance/securities_law
 *
 * SUMMARY:
 *   This constraint represents one reading of the dual-class legitimacy
 *   kernel: the minority_extraction reading asserts that minority
 *   shareholders are entitled to governance proportional to capital and risk
 *   borne. Under this reading, dual-class structures with supervoting shares
 *   transfer governance value from public to founder in breach of that
 *   entitlement. The control discount measurable in equity markets (15-30%
 *   lower valuation multiples for identical fundamentals) operationalizes the
 *   extraction: public shareholders pay the price of their governance
 *   exclusion. This reading contests the founder_stewardship reading (which
 *   frames concentrated control as benefiting all shareholders through
 *   mission continuity) and operates orthogonally to the disclosure_consent
 *   reading (which grounds legitimacy in informed consent to the structure,
 *   not on governance parity). We instantiate the minority_extraction reading
 *   only — the extraction assertion and its structural consequences. The
 *   other readings are separate constraint stories linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - founder_control_bloc: Holder of supervoting equity, agenda-setter, captures governance value through exemptions and control premium
 *   - minority_shareholders_class_a: Class A shareholders, bear full economic risk and dilution exposure but exercise governance only on narrow pre-approved topics
 *   - public_equity_markets: Enforce the governance discount through lower valuation multiples; absorb the extraction cost via lower returns
 *   - institutional_insiders: Managers and board members aligned with founder, negotiate private governance exceptions
 *   - securities_regulators: Administer controlled-company exemptions that permit dual-class companies to skip mandatory independence and compensation scrutiny
 *   - activist_investors_challengers: Excluded from proxy contests and board influence by supervoting structure; their exclusion is structural, not discretionary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, 0.72).
domain_priors:suppression_score(dual_class_legitimacy__minority_extraction, 0.68).
domain_priors:theater_ratio(dual_class_legitimacy__minority_extraction, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, extractiveness, 0.72).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__minority_extraction, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Dual-Class Share Structure and Minority Governance Extraction").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "corporate_governance/securities_law").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, 'fc849552-63f2-48a0-b866-c8607dea0c36').
narrative_ontology:cs_kernel_codification('fc849552-63f2-48a0-b866-c8607dea0c36', formalized).
narrative_ontology:cs_authority_grounding('fc849552-63f2-48a0-b866-c8607dea0c36', extraction).
narrative_ontology:cs_interpretation_layer_present('fc849552-63f2-48a0-b866-c8607dea0c36').
narrative_ontology:cs_reading_relation('fc849552-63f2-48a0-b866-c8607dea0c36', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('fc849552-63f2-48a0-b866-c8607dea0c36', dual_class_legitimacy__disclosure_consent, influences).
narrative_ontology:cs_axiom('fc849552-63f2-48a0-b866-c8607dea0c36', foundational, governance_proportional_to_capital_risk_entitlement).
narrative_ontology:cs_axiom_status(governance_proportional_to_capital_risk_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('fc849552-63f2-48a0-b866-c8607dea0c36', governance_proportional_to_capital_risk_entitlement, deontological).
narrative_ontology:cs_axiom('fc849552-63f2-48a0-b866-c8607dea0c36', foundational, dual_class_violates_proportionality_entitlement).
narrative_ontology:cs_axiom_status(dual_class_violates_proportionality_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('fc849552-63f2-48a0-b866-c8607dea0c36', dual_class_violates_proportionality_entitlement, deontological).
narrative_ontology:cs_reference_frame('fc849552-63f2-48a0-b866-c8607dea0c36', pro_rata_governance_legitimacy).
narrative_ontology:cs_drift_state('fc849552-63f2-48a0-b866-c8607dea0c36', contemporary_encumbered_ownership, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fc849552-63f2-48a0-b866-c8607dea0c36', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, founder_control_bloc).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, institutional_insiders).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, minority_shareholders_class_a).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, public_equity_markets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, minority_shareholders_class_a).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__minority_extraction, founder_long_term_vision).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__minority_extraction, stewardship_efficiency_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds Class B shares (10 votes per share) or equivalent supervoting equity. Retains board supermajority and unilateral strategic authority. Justifies control as necessary to protect the company's founding mission and long-term vision from short-term market pressure. Captures governance value through control premium at acquisition, exemption from mandatory board independence and compensation committee requirements, private anti-dilution agreements, and blocking rights on major transactions. Can exit through founder-initiated sale or recapitalization on their terms.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, founder_control_bloc, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, founder_control_bloc, beneficiary).

% Hold Class A shares (1 vote per share). Bear full economic risk (dilution, downside, acquisition risk) and full capital contribution. Exercise governance only on narrow pre-approved topics (dividend policy, related-party transactions subject to fairness opinions, routine matters). Cannot replace board members, block acquisitions, amend charter, or contest executive compensation. Exit is costly: selling locks in the control discount, creates tax events, and forgoes future upside if founder delivers on mission. Coalition exit (minority shareholders coordinating) faces collective-action problems and retaliatory price pressure.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, minority_shareholders_class_a, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, minority_shareholders_class_a, beneficiary).

% Price Class A shares at a persistent discount to single-class peer companies on identical fundamentals (empirically 15-30% lower valuation multiples). The discount reflects accurate assessment of governance risk: minority shareholders lack voting control and are structurally exposed to founder decisions that benefit founder at their expense. Institutional investors (pension funds, mutual funds, index funds) absorb the discount through lower returns or opt out entirely. The market enforces the extraction through pricing discipline.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, public_equity_markets, payer,
    organized, biographical, mobile, global).

% Senior managers, board directors aligned with founder, and long-term institutional investors (sovereign funds, family offices) negotiate governance exceptions — board seats, veto rights on specific decisions, anti-dilution provisions, information rights — not available to the public market. These side-agreements formalize their subordinate-but-protected status. Their governance privileges are discretionary gifts of the founder; they remain only while aligned.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, institutional_insiders, beneficiary,
    powerful, biographical, arbitrage, global).

% Administer Securities Act disclosure, proxy access rules, and listing standards. Have progressively adopted controlled-company exemptions (NYSE Rule 5635, NASDAQ Rule 5250) permitting dual-class companies to skip mandatory independent board, audit committee, compensation committee, and nominating committee requirements. Justify exemptions as enabling founder long-term vision and attracting capital; critics view as regulatory capture. Each jurisdiction (U.S., EU, UK, Asia-Pacific) makes independent choices; some mandate one-share-one-vote, others permit dual-class indefinitely.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, securities_regulators, observer,
    institutional, generational, analytical, national).

% Cannot conduct successful proxy fights or influence board composition (supervoting makes minority voting power mathematically ineffectual). Can lobby for governance reform, divest holdings, or short the stock. None of these moves alter the governance structure. Their exclusion is structural and permanent unless the founder voluntarily recapitalizes or regulatory mandate forces recapitalization.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, activist_investors_challengers, excluded,
    organized, biographical, trapped, global).

% Proxy advisory firms (ISS, Glass Lewis), labor unions (CalPERS, CalSTRS), ESG rating agencies, and governance advocates arguing for one-share-one-vote and mandatory independence. Can lobby regulators, produce shareholder proposals (which non-binding votes cast by Class A shareholders, founder ignores), and publicize governance concerns. Face structural resistance from founder control bloc and regulatory exemptions framework. Their influence is limited to moral suasion and regulatory reform, not direct governance change.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, governance_reform_coalition, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__minority_extraction, founder_control_bloc).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__minority_extraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables long-horizon capital formation and reduces founder-market conflict by concentrating strategic authority: founder can pursue multi-decade missions (industrial transformation, technological research, mission-driven product development) without quarterly earnings pressure or activist challenge. Public shareholders receive defined economic rights (dividend participation, acquisition proceeds, proxy votes on limited topics) while founder executes strategy unilaterally.
% TRANSFER_FUNCTION: Moves governance value from Class A shareholders to Class B supervoting shareholders. Class A shareholders bear full economic risk and capital contribution but yield all strategic authority; Class B holders capture decision rights, acquisition-value premiums, exemption from independent scrutiny, and early warning systems. The control discount (15-30% lower valuation multiples) operationalizes the transfer: public shareholders pay the governance asymmetry through subordinate equity economics.
% ABSENT_VOICES: Minority shareholders who exit (recognize the discount, liquidate holdings) are not present to contest the structure; activist investors lack voting power and do not appear in governance processes; alternative governance framers (one-share-one-vote advocates) lobby externally but are excluded from charter amendment authority.
% DISAPPEARANCE_RATIONALE: If dual-class structures vanished overnight (mandatory recapitalization to one-share-one-vote), the governance landscape would reorganize entirely: founders would immediately face proxy contest risk from any 5% shareholder coalition, independent board directors would become mandatory (current exemptions void), say-on-pay votes would become binding (current nonbinding votes change authority structure), anti-dilution and blocking rights would evaporate (no longer negotiable exceptions), and strategic decisions would require supermajority alignment or litigation risk. The company governance architecture would shift from founder discretion to multi-party negotiation.
% FOUNDING_PROBLEM: Public equity markets and activist investors impose short-term performance pressure and disruption risk on founders attempting to execute multi-decade strategic visions; dual-class structure decouples founder control from quarterly earnings cycles and activist interference, enabling mission continuity.
% FOUNDING_PROBLEM_CORROBORATION: The founder control bloc and long-term mission advocates attest the founding problem is live: activist pressure (activism peaked 2012-2018 but remains a persistent threat to long-term strategy), quarterly earnings focus (financial community prioritizes quarterly guidance and EPS), and short-term institutional capital (some institutional investors do trade on quarterly performance). Minority shareholders and single-class peer companies attest the founding problem is substantially solved or irrelevant: they cite examples of single-class companies executing decades-long strategies (Berkshire Hathaway under Buffett, Johnson & Johnson long-term drug development, Intel foundry strategy) without governance disruption. They also cite cases where dual-class founders abandoned founding mission once entrenched (Uber pre-recapitalization divergence from sustainability focus, WeWork diversion of capital to personal ventures). Securities law scholars note that say-on-pay votes, long-term performance metrics, and patient institutional capital address founder-market conflict without dual-class suppression of minority governance. The empirical record is mixed: some dual-class founders deliver superior long-term returns and mission adherence (Berkshire if it were structured dual-class; SpaceX's long-term R&D); others diverge (Uber's founder-era capital misallocation). Regulatory consensus is split: some jurisdictions (EU, UK, Singapore) mandate one-share-one-vote or sunset dual-class structures; others (U.S., Canada) permit dual-class indefinitely with exemptions.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__minority_extraction, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__minority_extraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dual_class_legitimacy__minority_extraction, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__minority_extraction, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__minority_extraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dual_class_legitimacy__minority_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the control premium is measurable, persistent, and economically significant — public shareholders demonstrably receive subordinate equity at a discount. Suppression is substantial (0.68) because the voting structure and regulatory exemptions make contested recapitalization or proxy succession mathematically impossible; alternatives (exit, voice, coalition action) are constrained by the economic lock-in and structural dominance. Theater ratio is moderate (0.42) because the arrangement includes genuine elements — founder actually does execute long-term strategy — but an increasing share of the maintained infrastructure (board independence exemptions, compensation committees stuffed with insiders, limited say-on-pay scope) serves extraction rather than mission enablement. The measurement trajectory shows extraction and suppression accumulating over the interval: as the company matures, the founder increasingly consolidates control through charter amendments, founder-friendly institutional investors entrench side-agreements, and regulatory exemptions expand. Theater rises in parallel — more resources devoted to legitimacy narratives (mission statements, founder letters framing decisions as vision-driven rather than extraction-driven) rather than actual structural change. The extraction accumulates because early minority investors accepted the discount believing the founder would deliver superior returns; later cohorts accept the discount because the structure is now path-dependent (recapitalization is legally and politically harder than maintaining it). This is the classic structure-lock-in pattern: initial choice (founder keeps control, public market values it below parity) becomes institutional fact (path-dependent regulation, investor expectation), becomes extractive machine (founder extracts governance value with minimal legitimacy defense).
 *
 * PERSPECTIVAL GAP:
 *   From the founder's seat: the structure is a necessary protection enabling long-term mission execution; the 'extraction' is the price public shareholders rationally accept in return for founder commitment (the stewardship reading). From the public shareholder's seat: the structure transfers governance value that should be proportional to capital contribution; the discount compensates the founder for accepting public scrutiny, not for superior vision (the minority_extraction reading). The engine computes these perspectives from the structural data: founder has high d (beneficiary, arbitrage exit, controls the rules), public shareholder has high d (victim, constrained exit, pays the discount). The divergence in computed types reflects the fundamental asymmetry: what looks like rational coordination from the founder's seat looks like entrenchment from the minority seat. The reading we are instantiating (minority_extraction) asserts the minority seat's entitlement frame, not the founder's justification.
 *
 * DIRECTIONALITY LOGIC:
 *   Founder bloc: d ~ 0.15 (beneficiary by structural derivation: captures control premium, exempted from mandatory scrutiny, arbitrage exit available through sale or recapitalization on terms they control). Minority shareholders: d ~ 0.78 (target by structural derivation: pay the control discount, constrained exit, voting power yields no influence). Public markets: d ~ 0.72 (target: enforce the discount through pricing, no exit except divestment which locks in losses). Institutional insiders: d ~ 0.25 (near-beneficiary: negotiate exceptions, move between founder bloc and minority seat depending on institutional positioning). Regulators and excluded activists carry analytical d (0.5 by convention). The reading's commitment is minority-centric: it names what minority shareholders are entitled to, not what the founder deserves or what regulators should permit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (public equity pressure disrupts long-term strategy) is contested in status. The founder control bloc insists it remains live (quarterly earnings cycles still pressure strategy). Minority shareholders and single-class peer companies attest it is substantially solved by other means (say-on-pay, board compensation committees, long-term performance metrics, index fund patient capital). The constraint persists even as the problem recedes — that mismatch is the mandatrophy trigger. The measured theater ratio rising from 0.25 to 0.42 over the interval signals that maintenance activity increasingly serves extraction rather than mission coordination: founder justifies control by mission necessity, but the enumerated threats (activist pressure, quarterly focus) are contracting (activist pressure is ineffectual at dual-class companies and has declined as a concern; quarterly focus is addressed by long-term metrics). The governance extraction continues because it is now path-dependent: recapitalization would require founder consent (never given), mandatory one-share-one-vote would require legislative action (politically difficult), and the control discount is baked into capital allocation decisions (expected by institutional investors). This is mandatrophic governance: control is extracted because it is entrenched and costly to change, not because the original rationale requires it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    control_value_attribution,
    'Is the measured control discount attributable to dual-class governance asymmetry, or to market mispricing, founder reputation risk, or other factors orthogonal to governance structure?',
    'Matched-pair analysis comparing dual-class companies with single-class peers on identical financial metrics, controlling for founder tenure, industry, growth stage, and profitability. Analyze recapitalization events where control discount compresses or expands post-one-share-one-vote transition.',
    'If the discount is primarily governance-driven, the extraction claim is empirically grounded and the minority entitlement violation is measurable. If the discount reflects founder reputation or company-specific risk, the governance extraction is lower and the reading''s entitlement framing is weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_value_attribution, empirical, 'Attribution of the control discount to governance structure vs. other factors').

omega_variable(
    stewardship_vs_extraction_read,
    'Does the founder''s concentrated control produce superior long-term returns for ALL shareholders, or does it primarily capture governance value for the founder while minimizing minority returns?',
    'Long-term total return analysis (10+ years) comparing dual-class companies (founder-controlled) with single-class peers on identical fundamentals; separate out dividends, buybacks, and acquisition multiples paid by minority shareholders.',
    'If dual-class outperforms materially and broadly (inclusive of minority returns), the stewardship reading''s legitimacy claim is strengthened and extraction is reframed as coordination cost. If returns track single-class peers or minority underperforms, extraction claim is supported and mandatrophy is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stewardship_vs_extraction_read, empirical, 'Whether concentrated founder control generates superior returns across the shareholder base').

omega_variable(
    regulatory_capture_mechanism,
    'Do controlled-company exemptions (NYSE, NASDAQ rules) persist because they serve a genuine governance function that benefits public shareholders, or because founder lobbying and listing exchange competition suppress the rules?',
    'Historical analysis of rule-making processes; examine founder-bloc political contributions, lobbying spending, and exchanges'' rationales for exemptions. Compare exemption adoption across jurisdictions with different regulatory independence.',
    'Evidence of regulatory capture would strengthen the extraction reading (exemptions are manufactured consent, not genuine accommodation of a legitimate governance need) and support recapitalization policy. Evidence that exemptions reflect genuine governance trade-offs would support the stewardship reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Whether controlled-company exemptions reflect genuine governance function or regulatory capture').

omega_variable(
    reading_incompossibility,
    'Are the minority_extraction and founder_stewardship readings logically incompossible (forecloses) or do they represent genuinely coexisting frames that cannot be resolved within one framework?',
    'Examine the core premises: minority_extraction asserts entitlement to governance-parity; founder_stewardship asserts that parity is not necessary for legitimacy if the founder produces superior outcomes. If outcomes are universally superior AND minority shareholders consistently capture proportional returns, both readings can be held (stewardship is extraction that justifies itself through delivery). If outcomes are mixed or if minority returns underperform, the readings contradict and foreclosure is present.',
    'If the readings genuinely coexist, the kernel admits multiple legitimate framings and regulatory policy should clarify which it endorses. If one forecloses the other empirically, the regulatory regime should be restructured to align with the actual facts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_incompossibility, conceptual, 'Whether the minority_extraction and founder_stewardship readings are logically compatible within a single framework').

omega_variable(
    suppression_internalization,
    'Is the measured suppression structural (external barriers: voting rules, regulatory exemptions, exit costs) or internalized (minority shareholders believe they deserve subordinate governance, have accepted the frame, carry suppression after exit)?',
    'Survey and interview minority shareholders on their belief about governance entitlement; track post-exit shareholder activism and venture formation choices (do former dual-class minority shareholders avoid dual-class structures or accept them as normal). Examine institutional investor proxy voting patterns — do they challenge dual-class on every ballot or have they internalized the legitimacy claim?',
    'If suppression is primarily structural, transparency and rule changes (mandatory one-share-one-vote, mandatory say-on-pay) would reduce extraction rapidly. If suppression is internalized, even post-recapitalization minority shareholders would accept governance asymmetry in future investments.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression operates primarily through external structures or internalized beliefs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__minority_extraction, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__minority_extraction, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(dual_tr_t0, observed).
narrative_ontology:measurement(dual_tr_t5, dual_class_legitimacy__minority_extraction, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(dual_tr_t5, observed).
narrative_ontology:measurement(dual_tr_t10, dual_class_legitimacy__minority_extraction, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(dual_tr_t10, observed).
narrative_ontology:measurement(dual_tr_t15, dual_class_legitimacy__minority_extraction, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(dual_tr_t15, observed).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__minority_extraction, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(dual_tr_t20, observed).
narrative_ontology:measurement(dual_tr_t25, dual_class_legitimacy__minority_extraction, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(dual_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__minority_extraction, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(dual_be_t0, observed).
narrative_ontology:measurement(dual_be_t5, dual_class_legitimacy__minority_extraction, base_extractiveness, 5, 0.62).
narrative_ontology:measurement_basis(dual_be_t5, observed).
narrative_ontology:measurement(dual_be_t10, dual_class_legitimacy__minority_extraction, base_extractiveness, 10, 0.67).
narrative_ontology:measurement_basis(dual_be_t10, observed).
narrative_ontology:measurement(dual_be_t15, dual_class_legitimacy__minority_extraction, base_extractiveness, 15, 0.7).
narrative_ontology:measurement_basis(dual_be_t15, observed).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__minority_extraction, base_extractiveness, 20, 0.71).
narrative_ontology:measurement_basis(dual_be_t20, observed).
narrative_ontology:measurement(dual_be_t25, dual_class_legitimacy__minority_extraction, base_extractiveness, 25, 0.72).
narrative_ontology:measurement_basis(dual_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__minority_extraction, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(dual_su_t0, observed).
narrative_ontology:measurement(dual_su_t5, dual_class_legitimacy__minority_extraction, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(dual_su_t5, observed).
narrative_ontology:measurement(dual_su_t10, dual_class_legitimacy__minority_extraction, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(dual_su_t10, observed).
narrative_ontology:measurement(dual_su_t15, dual_class_legitimacy__minority_extraction, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(dual_su_t15, observed).
narrative_ontology:measurement(dual_su_t20, dual_class_legitimacy__minority_extraction, suppression_requirement, 20, 0.67).
narrative_ontology:measurement_basis(dual_su_t20, observed).
narrative_ontology:measurement(dual_su_t25, dual_class_legitimacy__minority_extraction, suppression_requirement, 25, 0.68).
narrative_ontology:measurement_basis(dual_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__minority_extraction, resource_allocation).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__minority_extraction, 0.18).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% The dual_class_legitimacy kernel admits three constraint readings: minority_extraction (this file) asserts minority governance entitlement and measures extraction through control discount; founder_stewardship asserts founder control legitimately serves all shareholders through long-horizon mission, measuring extraction as coordination cost; disclosure_consent asserts legitimacy rests on informed consent via Securities Act rules, not parity. All three readings share the kernel (dual-class share structure with founder supervoting equity) but differ in their legitimacy frames and epsilon referents. The minority_extraction reading's epsilon measures governance value transfer from public to founder under the minority-entitlement frame; the founder_stewardship reading's epsilon would measure the same transfer but call it coordination cost; the disclosure_consent reading's epsilon would measure it against the consent framework rather than entitlement. Each reading produces a different classification from the same structural facts because the epsilon referent changes. The three stories are linked via network.affects_constraints to enable cross-reading analysis and demonstrate that classification divergence reflects frame choice, not measurement error.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_class_legitimacy__minority_extraction, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
