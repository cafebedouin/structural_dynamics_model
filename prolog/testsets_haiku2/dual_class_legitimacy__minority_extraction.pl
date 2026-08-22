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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: dual_class_legitimacy__minority_extraction
 *   human_readable: Dual-Class Share Structure: Minority Extraction via Governance Disenfranchisement
 *   domain: corporate_governance/securities_law
 *
 * SUMMARY:
 *   This constraint instantiates the MINORITY_EXTRACTION reading of the
 *   dual-class legitimacy kernel. The reading asserts that dual-class share
 *   structures (where founder-held Class A shares carry superior voting
 *   rights) constitute systematic extraction from minority public
 *   shareholders. Minority shareholders bear proportional economic risk
 *   (capital loss, dilution, dividend cuts) but hold governance votes that do
 *   not scale with their capital or risk exposure. The founder control
 *   bloc—holding perhaps 5–15% of economic interest but 50–70% of voting
 *   control—can unilaterally decide on acquisitions, capital allocation,
 *   dividend policy, and strategic direction, overriding minority interests
 *   entirely. The constraint's persistence depends on active enforcement:
 *   controlled-company exemptions from mandatory governance standards
 *   (Sarbanes-Oxley committee independence, proxy access rules, say-on-pay
 *   votes), regulatory forbearance, and minority shareholders' imperfect
 *   ability to exit. The reading treats this structure not as legitimate
 *   stewardship (founder_stewardship reading) but as systematic extraction
 *   defended by regulatory capture and asymmetric information.
 *
 * KEY AGENTS:
 *   - founder_control_bloc — holds super-voting Class A shares; sets strategic direction unilaterally; benefits from capital raised without governance surrender
 *   - minority_public_shareholders — hold Class B/C shares; bear proportional economic risk; have minimal voting power; can exit but exit is costly
 *   - institutional_investors — hold large minority stakes; trapped by fiduciary/index mandates; cannot exit; advocate for reform without enforcement power
 *   - securities_regulators — oversee capital markets; have exempted 'controlled companies' from mandatory governance protections; justify exemption as enabling founder vision
 *   - minority_shareholder_advocates — governance reformers excluded from board composition; lack statutory standing to override founder decisions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, 0.82).
domain_priors:suppression_score(dual_class_legitimacy__minority_extraction, 0.71).
domain_priors:theater_ratio(dual_class_legitimacy__minority_extraction, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, extractiveness, 0.82).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__minority_extraction, snare).
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Dual-Class Share Structure: Minority Extraction via Governance Disenfranchisement").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "corporate_governance/securities_law").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, '670cc335-797b-49dc-b4e5-83594953b23f').
narrative_ontology:cs_kernel_codification('670cc335-797b-49dc-b4e5-83594953b23f', formalized).
narrative_ontology:cs_authority_grounding('670cc335-797b-49dc-b4e5-83594953b23f', extraction).
narrative_ontology:cs_interpretation_layer_present('670cc335-797b-49dc-b4e5-83594953b23f').
narrative_ontology:cs_reading_relation('670cc335-797b-49dc-b4e5-83594953b23f', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_reading_relation('670cc335-797b-49dc-b4e5-83594953b23f', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_axiom('670cc335-797b-49dc-b4e5-83594953b23f', foundational, governance_proportional_to_capital_risk).
narrative_ontology:cs_axiom_status(governance_proportional_to_capital_risk, holdable).
narrative_ontology:cs_axiom_grounding('670cc335-797b-49dc-b4e5-83594953b23f', governance_proportional_to_capital_risk, deontological).
narrative_ontology:cs_axiom('670cc335-797b-49dc-b4e5-83594953b23f', secondary, extraction_without_alignment_illegitimate).
narrative_ontology:cs_axiom_status(extraction_without_alignment_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('670cc335-797b-49dc-b4e5-83594953b23f', extraction_without_alignment_illegitimate, deontological).
narrative_ontology:cs_reference_frame('670cc335-797b-49dc-b4e5-83594953b23f', proportional_shareholder_governance).
narrative_ontology:cs_drift_state('670cc335-797b-49dc-b4e5-83594953b23f', contemporary_dual_class_proliferation, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('670cc335-797b-49dc-b4e5-83594953b23f', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, founder_control_bloc).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, minority_public_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, institutional_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds Super-Voting Class A shares (or equivalent multi-vote structure) that grant disproportionate control over board composition, merger/acquisition decisions, dividend policy, and capital allocation. Controls the company's strategic direction despite holding a fraction of economic risk. Benefits from the ability to raise capital without surrendering governance authority; can override minority preferences on all material decisions.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, founder_control_bloc, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Hold economically-weighted Class B or C shares (one-share-one-vote) or common equity that carries proportional risk exposure but minimal governance voice. Bear full economic downside (loss of capital, dividend cuts, dilution from founder-favored issuances) but cannot block or influence decisions that harm their interests. Can exit by selling, but selling signals distrust and may depress share price; exit is costly and imperfect.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, minority_public_shareholders, payer,
    organized, biographical, mobile, global).

% Hold large minority stakes (pension funds, mutual funds, index funds) for diversification or long-term return. Have fiduciary duties that prevent concentration avoidance, so they cannot simply exit controlled companies. Subject to the same governance disenfranchisement as retail shareholders but with the added constraint that index-fund mandates force continued holding. Advocate for governance reform but lack the vote share to impose it.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, institutional_investors, payer,
    powerful, biographical, constrained, global).

% Oversee capital markets disclosure and enforcement. Have largely exempted 'controlled companies' from mandatory governance standards (Sarbanes-Oxley committee independence rules, proxy access, say-on-pay votes). Justify the exemption as allowing founder vision; critics argue it enables systematic extraction from minority shareholders.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, securities_regulators, observer,
    institutional, generational, analytical, national).

% Shareholder advocacy groups, governance reformers, and labor/pension fund coalitions that object to dual-class voting and controlled-company exemptions. Lack statutory standing to block proxies or force board changes; their objections are aired in shareholder meetings and regulatory comment periods but are structurally overridden by founder control.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, minority_shareholder_advocates, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__minority_extraction, founder_control_bloc).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__minority_extraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables founders to secure capital for growth while retaining long-term strategic vision; supposedly prevents short-termist investor pressure that would fragment the company.
% TRANSFER_FUNCTION: Transfers governance value (control over strategic decisions, capital allocation, dividend policy, merger terms) from minority shareholders to founder control bloc, while retaining the economic transfer of capital in the opposite direction: minority shareholders supply capital but receive proportionally diminished voice.
% ABSENT_VOICES: Minority shareholder advocates and governance reformers object to dual-class structures but are structurally excluded from board composition and cannot block founder decisions; labor unions and pension funds argue dual-class exemptions strip mandatory protections but have no vote to enforce reform.
% DISAPPEARANCE_RATIONALE: If dual-class structures and controlled-company exemptions were prohibited overnight, founders would lose the ability to raise capital without surrendering governance authority; capital formation would be affected, existing controlled companies would face pressure to unify voting (or lose access to public capital), and minority shareholders would gain enforceable governance rights and mandatory committee protections. The financial and organizational landscape would reorganize around single-class equity or explicit consent mechanisms.
% FOUNDING_PROBLEM: Early growth-stage founders need patient capital and insulation from activist investors to execute long-horizon strategy; public markets reward short-termism and impose costly governance oversight.
% FOUNDING_PROBLEM_CORROBORATION: Founders and founder-friendly investors attest the problem is live and justified. Institutional investors, governance researchers, and securities regulators (in foreign jurisdictions with single-class mandates) attest the founding problem is overstated: patient capital is available under single-class structures, and the extraction costs to minorities exceed any efficiency gain; empirical cross-national analysis from outside the benefiting parties (comparing controlled-company performance to single-class peers) shows mixed results and does not support systematic efficiency advantage.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__minority_extraction, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__minority_extraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dual_class_legitimacy__minority_extraction, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__minority_extraction, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.82) because the constraint transfers governance value from minority shareholders to founder while maintaining unidirectional capital transfer in the opposite direction: minorities supply capital but receive disproportionately small voice. The constraint is not temporary—it is perpetual by design (multi-vote shares pass to heirs; founder trusts maintain control across generations). Suppression (0.71) reflects active enforcement: controlled-company exemptions strip mandatory protections that would otherwise constrain founder unilateralism; minority shareholders can vote in shareholder meetings but their votes are structurally overridden; exit is mobile for retail shareholders but constrained for institutions. Theater ratio (0.48) reflects that dual-class structures rely partly on founder-mission narratives and stewardship framing (the founder_stewardship reading) but increasingly operate as pure extraction mechanisms—the proportion of enforcement dedicated to maintaining founder vision vs. defending founder financial interests has shifted toward the latter as founders age and companies mature. The measurement series tracks the accumulation of extractiveness from 1980 (era of limited dual-class adoption and stronger minority protections) to 2026 (era of widespread controlled-company exemptions and founder-dominated mega-cap firms). Extractiveness rises as the practice normalizes and the exemption regime solidifies; theater rises as governance theater (proxy contests, say-on-pay advisory votes) increases without changing founder control. Suppression requirement rises as institutional investors and reformers mount objections that require increasingly explicit regulatory and contractual suppression to override.
 *
 * PERSPECTIVAL GAP:
 *   The founder control bloc perceives the structure as legitimate stewardship and long-horizon protection (the founder_stewardship reading); the constraint computes as a mountain of necessity from their seat. Minority shareholders perceive it as systematic extraction; the constraint computes as a snare from their seat. Institutional investors perceive it as a forced holding in an unfavorable structure; the constraint computes as a trap from their seat. Securities regulators perceive it as a legal exemption for controlled companies; the constraint computes as regulatory capture from a governance-reform seat. The engine should compute different type verdicts for each seat because the power atoms and exit options differ: founder control bloc holds powerful + arbitrage; minorities hold organized + mobile (for retail) or constrained (for institutions); regulators hold institutional + analytical. The authored metrics describe the actual structural operation (snare at the control-bloc seat, which extracts unidirectionally); the divergence from the stewardship reading is exactly the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Founder control bloc: d = 0.1 (full beneficiary; collects governance surplus; arbitrage exit = subsidized position). Minority public shareholders: d = 0.85 (near-full target; bear economic downside without governance remedy; mobile exit still costly). Institutional investors: d = 0.88 (even higher target; trapped by mandate despite powerful institutional position; cannot exit). Securities regulators: d = 0.5 (analytical seat, no structural benefit or cost; observer position). The directionality map shows the structural asymmetry: the beneficiary's exit is costless arbitrage (alternative control structures available if needed); the victims' exit is costly and partial (they can sell but selling is economically suboptimal and signals distrust). This asymmetry drives the extraction: minorities cannot credibly threaten exit, so the constraint persists.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (insulating founder vision from short-term investor pressure) remains live but increasingly orphaned from the constraint's actual operation. Early dual-class structures (1980–2000) genuinely served to prevent activist raids on founder-led companies executing genuine long-horizon strategies (Berkshire Hathaway, Ford family control). Over time, controlled companies proliferated to founders seeking indefinite personal control unrelated to any specific mission (tech founders extending control past their own productive careers; financial engineers using dual-class to extract private benefits). The constraint's function shifted from protecting long-horizon execution to enabling extraction, but the stewardship narrative persists unchanged. The theater ratio's rise from 0.25 to 0.48 reflects this: governance committees, say-on-pay votes, and shareholder advisory proxies create the appearance of minority voice while founder voting control remains fixed. The rising suppression_requirement (0.42 to 0.71) reflects that increasingly explicit regulatory exemptions and contract language are required to defend the structure against mounting minority objections. The constraint exhibits classic mandatrophy: the original mandate (protect founder strategy) is dead or drastically narrowed in most cases; the structure persists via regulatory exemptions and minority weakness; the divergence between founding problem (still-live stewardship claim) and disappearance verdict (world_rearranges) marks the zombie constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_stewardship_vs_extraction,
    'Does the founder''s concentrated control produce measurably superior long-horizon outcomes (patent velocity, R&D intensity, patient capital deployment) that could not be achieved under single-class structures with governance minority protections?',
    'Empirical comparison of controlled-company performance (using dual-class) to peer single-class companies, matched by industry, stage, and founder tenure. Isolate founder-specific value creation from control-enabled extraction. Tests: do founder-led single-class companies outperform single-class peers? Do minority shareholders in controlled companies underperform relative to their capital contribution?',
    'If founder control produces measurably superior outcomes beyond noise, part of the measured extraction is the price of legitimate stewardship. If outcomes are equivalent or minorities underperform controlling for capital exposure, the extraction is pure rent without coordination benefit (snare classification holds). If outcomes are worse, controlled companies are doubly extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founder_stewardship_vs_extraction, empirical, 'Whether concentrated founder control generates long-horizon returns that justify governance extraction.').

omega_variable(
    disclosure_sufficiency_for_consent,
    'Does Securities Act disclosure of dual-class structure and voting rights constitute informed consent to the governance asymmetry, or is disclosure insufficient to constitute meaningful consent when exit is costly and alternatives are unavailable?',
    'Behavioral study of investor choice: do retail and institutional investors knowingly and deliberately choose dual-class shares, or do they hold them incidentally (index fund inclusions, concentrated positions in founder-led companies, limited alternatives)? Compare disclosure understanding pre-purchase and post-harm. Test whether informed investors who understand the structure ex ante continue to hold or exit.',
    'If disclosure constitutes sufficient consent (disclosure_consent reading holds), minorities are bound by their choice and extraction is not coercive. If disclosure is insufficient (minorities hold under asymmetric information or constraint), the consent framing collapses and the snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_sufficiency_for_consent, empirical, 'Whether SEC disclosure of dual-class voting creates meaningful investor consent to governance disenfranchisement.').

omega_variable(
    controlled_company_exemption_capture,
    'Are controlled-company exemptions from Sarbanes-Oxley committee independence, proxy access, and say-on-pay rules grounded in genuine regulatory judgment about founder stewardship efficiency, or do they reflect regulatory capture by founder-controlled firms and their advisors?',
    'Policy genealogy: trace the origins of the SEC''s ''controlled company'' exemption (introduced 1992, expanded 2008–2015). Examine SEC comment letters, academic testimony, and founder-industry advocacy during rulemaking. Compare the exemption''s text to empirical evidence on dual-class efficiency. Assess whether jurisdictions without exemptions (EU majority voting rules, UK stewardship code expectations of single-class) show measurably worse capital formation or innovation outcomes.',
    'If exemptions reflect genuine efficiency judgment, some of the suppression is justified regulatory forbearance. If they reflect capture, the suppression is an additional extraction mechanism layered on top of dual-class structure itself (snare is augmented by regulatory capture to increase extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(controlled_company_exemption_capture, empirical, 'Whether controlled-company exemptions are grounded in regulatory judgment or represent regulatory capture.').

omega_variable(
    alternative_founder_alignment_mechanisms,
    'Could founder long-termism be preserved under single-class governance through contractual mechanisms (founder-majority boards negotiated in venture-to-IPO, long-vesting founder equity, super-majority requirements for dissolution, sunset clauses ceding control to professional management) without systematic minority extraction?',
    'Comparative analysis of founder-led companies using contractual long-termism mechanisms (Google pre-IPO negotiation of Brin-Page-Schmidt control; Snap contractual founder control ceding to board majority; Costco supermajority Sinegal model) vs. dual-class permanent control. Measure minority shareholder protections, exit costs, and long-horizon outcome parity. Assess whether contractual alternatives are systematically disfavored by founders or regulators.',
    'If viable alternatives exist and are disfavored by founders despite equivalent long-term benefits, the preference for dual-class is extraction-motivated, not stewardship-motivated. If alternatives are systematically disfavored by regulators or investors, capture is institutionalized. Either resolution deepens the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_founder_alignment_mechanisms, empirical, 'Whether non-extractive contractual mechanisms could substitute for dual-class structures while preserving founder alignment.').

omega_variable(
    reading_frame_ambiguity,
    'Is the legitimacy criterion for dual-class structures located in the founder''s subjective experience of stewardship (founder_stewardship reading), in the investor''s informed choice at purchase (disclosure_consent reading), or in the structural alignment of governance to capital and risk (minority_extraction reading)? Which reading''s legitimacy frame is the operative one in practice?',
    'Examine actual shareholder disputes, proxy contests, and regulatory findings. Trace which legitimacy frame prevails when readings collide (e.g., when a shareholder suit challenges dual-class on governance grounds: do courts apply stewardship deference, informed-consent framing, or proportionality analysis?). Document the frame used in SEC enforcement actions, judicial opinions, and shareholder agreements.',
    'If courts and regulators apply stewardship deference, the snare classification will remain under-enforced. If disclosure consent prevails, minorities are bound and exit-cost suppression is treated as buyer''s responsibility. If proportionality prevails, dual-class becomes harder to defend and minorities gain remedy. The reading frame determines whether the extraction is structurally legitimate or structurally illegitimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_frame_ambiguity, conceptual, 'Which legitimacy frame (stewardship, disclosure-consent, proportionality) is operative in dispute resolution and regulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__minority_extraction, 1980, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t1980, dual_class_legitimacy__minority_extraction, theater_ratio, 1980, 0.25).
narrative_ontology:measurement_basis(dual_tr_t1980, observed).
narrative_ontology:measurement(dual_tr_t1995, dual_class_legitimacy__minority_extraction, theater_ratio, 1995, 0.32).
narrative_ontology:measurement_basis(dual_tr_t1995, observed).
narrative_ontology:measurement(dual_tr_t2005, dual_class_legitimacy__minority_extraction, theater_ratio, 2005, 0.39).
narrative_ontology:measurement_basis(dual_tr_t2005, observed).
narrative_ontology:measurement(dual_tr_t2015, dual_class_legitimacy__minority_extraction, theater_ratio, 2015, 0.44).
narrative_ontology:measurement_basis(dual_tr_t2015, observed).
narrative_ontology:measurement(dual_tr_t2020, dual_class_legitimacy__minority_extraction, theater_ratio, 2020, 0.46).
narrative_ontology:measurement_basis(dual_tr_t2020, observed).
narrative_ontology:measurement(dual_tr_t2026, dual_class_legitimacy__minority_extraction, theater_ratio, 2026, 0.48).
narrative_ontology:measurement_basis(dual_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(dual_be_t1980, dual_class_legitimacy__minority_extraction, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement_basis(dual_be_t1980, observed).
narrative_ontology:measurement(dual_be_t1995, dual_class_legitimacy__minority_extraction, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement_basis(dual_be_t1995, observed).
narrative_ontology:measurement(dual_be_t2005, dual_class_legitimacy__minority_extraction, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement_basis(dual_be_t2005, observed).
narrative_ontology:measurement(dual_be_t2015, dual_class_legitimacy__minority_extraction, base_extractiveness, 2015, 0.72).
narrative_ontology:measurement_basis(dual_be_t2015, observed).
narrative_ontology:measurement(dual_be_t2020, dual_class_legitimacy__minority_extraction, base_extractiveness, 2020, 0.78).
narrative_ontology:measurement_basis(dual_be_t2020, observed).
narrative_ontology:measurement(dual_be_t2026, dual_class_legitimacy__minority_extraction, base_extractiveness, 2026, 0.82).
narrative_ontology:measurement_basis(dual_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t1980, dual_class_legitimacy__minority_extraction, suppression_requirement, 1980, 0.42).
narrative_ontology:measurement_basis(dual_su_t1980, observed).
narrative_ontology:measurement(dual_su_t1995, dual_class_legitimacy__minority_extraction, suppression_requirement, 1995, 0.51).
narrative_ontology:measurement_basis(dual_su_t1995, observed).
narrative_ontology:measurement(dual_su_t2005, dual_class_legitimacy__minority_extraction, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement_basis(dual_su_t2005, observed).
narrative_ontology:measurement(dual_su_t2015, dual_class_legitimacy__minority_extraction, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement_basis(dual_su_t2015, observed).
narrative_ontology:measurement(dual_su_t2020, dual_class_legitimacy__minority_extraction, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement_basis(dual_su_t2020, observed).
narrative_ontology:measurement(dual_su_t2026, dual_class_legitimacy__minority_extraction, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(dual_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__minority_extraction, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__minority_extraction, 0.18).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% The dual-class legitimacy constraint decomposes into three readings: disclosure_consent (legitimacy rests on investor consent under Securities Act disclosure), founder_stewardship (legitimacy rests on founder's long-horizon value creation), and minority_extraction (legitimacy rests on governance proportional to capital/risk). Each reading instantiates a distinct constraint with different ε values, beneficiary/victim structures, and measured extraction. The minority_extraction reading authorizes the highest ε (0.82) because it treats the entire governance structure as extractive rather than as a justified stewardship mechanism. The three readings are linked via this network entry: each influences the others' legitimacy environment without logically foreclosing any. Changes to minority protections (say-on-pay mandates, forced unification votes) affect the enforcement environment for all three readings simultaneously but do not resolve the kernel dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_class_legitimacy__minority_extraction, organized, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
