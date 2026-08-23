% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__governance_skeptic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_valuation_legitimacy__governance_skeptic, []).

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
 *   constraint_id: valuation_legitimacy__governance_skeptic
 *   human_readable: Dual-Class Voting Structure with Controlled-Company Exemptions at a $1.75T Valuation (Governance-Skeptic Reading)
 *   domain: corporate finance/technology governance/space economics
 *
 * SUMMARY:
 *   A publicly listed company operates a dual-class structure: the founder
 *   controls 82.4 percent of votes with 42 percent of equity through 10:1
 *   supervoting shares, and controlled-company exemptions waive the
 *   independent compensation and nominating committees that listing standards
 *   otherwise require. The market prices the enterprise at $1.75T. This story
 *   instantiates the governance_skeptic reading of the valuation_legitimacy
 *   kernel: on this reading a valuation is legitimate only if a governance
 *   structure protecting minority holders stands behind it, and a premium
 *   sustained by unaccountable control is a transfer to the controller rather
 *   than value created for shareholders. The referent of the extractiveness
 *   score is the standing arrangement — the dual-class structure and its
 *   exemptions as they operate on Class A holders at the current valuation —
 *   assessed by this reading's own lights. The three sibling readings are
 *   separate constraints in separate files; nothing about them is averaged
 *   into this story's epsilon.
 *
 * KEY AGENTS:
 *   - - elon_musk_control_block: Primary beneficiary and agenda setter (powerful/constrained) — holds 82.4% of votes on 42% of equity; sets board composition, compensation, and cross-company allocation
 *   - - early_class_b_holders: Secondary beneficiaries (moderate/mobile) — supervoting shares from founding rounds, aligned with the control bloc
 *   - - captured_board_directors: Enforcement administrators who collect from the arrangement (moderate/identity_locked) — administer the exemptions, nominated by and dependent on the control holder
 *   - - class_a_public_shareholders: Primary targets (powerless/constrained) — one vote per share, ballots arithmetically irrelevant, exit only by selling
 *   - - passive_index_fund_managers: Targets without exit (organized/trapped) — must hold per index mandates, stewardship neutralized by vote arithmetic
 *   - - proxy_advisory_firms: Analytical observers (organized/analytical) — rate and recommend against the structure from outside
 *   - - securities_regulators: Institutional observers (institutional/analytical) — review transactions and disclosures; the charter core was set elsewhere
 *   - - musk_constellation_employees: Excluded parties (moderate/constrained) — live inside allocation decisions made above them
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__governance_skeptic, 0.74).
domain_priors:suppression_score(valuation_legitimacy__governance_skeptic, 0.62).
domain_priors:theater_ratio(valuation_legitimacy__governance_skeptic, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, extractiveness, 0.74).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__governance_skeptic, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__governance_skeptic, "Dual-Class Voting Structure with Controlled-Company Exemptions at a $1.75T Valuation (Governance-Skeptic Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__governance_skeptic, "corporate finance/technology governance/space economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__governance_skeptic).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__governance_skeptic, 'ba042638-60e1-4e19-a197-8edc4faf2fa2').
narrative_ontology:cs_kernel_codification('ba042638-60e1-4e19-a197-8edc4faf2fa2', distributed).
narrative_ontology:cs_authority_grounding('ba042638-60e1-4e19-a197-8edc4faf2fa2', expertise).
narrative_ontology:cs_interpretation_layer_present('ba042638-60e1-4e19-a197-8edc4faf2fa2').
narrative_ontology:cs_reading_relation('ba042638-60e1-4e19-a197-8edc4faf2fa2', valuation_legitimacy__dcf_fundamentalist, influences).
narrative_ontology:cs_reading_relation('ba042638-60e1-4e19-a197-8edc4faf2fa2', valuation_legitimacy__real_options_technologist, coexists_with).
narrative_ontology:cs_reading_relation('ba042638-60e1-4e19-a197-8edc4faf2fa2', valuation_legitimacy__musk_cult_believer, coexists_with).
narrative_ontology:cs_axiom('ba042638-60e1-4e19-a197-8edc4faf2fa2', foundational, minority_governance_rights_entitlement).
narrative_ontology:cs_axiom_status(minority_governance_rights_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('ba042638-60e1-4e19-a197-8edc4faf2fa2', minority_governance_rights_entitlement, deontological).
narrative_ontology:cs_axiom('ba042638-60e1-4e19-a197-8edc4faf2fa2', foundational, unaccountable_control_premium_is_extraction).
narrative_ontology:cs_axiom_status(unaccountable_control_premium_is_extraction, holdable).
narrative_ontology:cs_axiom_grounding('ba042638-60e1-4e19-a197-8edc4faf2fa2', unaccountable_control_premium_is_extraction, instrumental).
narrative_ontology:cs_reference_frame('ba042638-60e1-4e19-a197-8edc4faf2fa2', minority_protection_governance_standard).
narrative_ontology:cs_drift_state('ba042638-60e1-4e19-a197-8edc4faf2fa2', contemporary_dualclass_proliferation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ba042638-60e1-4e19-a197-8edc4faf2fa2', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__governance_skeptic, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, elon_musk_control_block).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, early_class_b_holders).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, captured_board_directors).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, class_a_public_shareholders).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, passive_index_fund_managers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds roughly 42 percent of the equity and 82.4 percent of the votes through 10:1 supervoting shares. Appoints the board that approves his compensation, maintains the controlled-company exemptions that waive independent compensation and nominating committees, and decides how capital, talent, and business opportunities are allocated across the half-dozen companies he runs. Selling enough stock to diversify would depress the price of the very asset that backs his borrowing capacity, and the companies' missions are bound up with his personal trajectory, so his practical exit from the arrangement is narrow even though his legal ownership is portable.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, elon_musk_control_block, agenda_setter,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__governance_skeptic, elon_musk_control_block, beneficiary).

% Early investors and insiders who received supervoting shares in founding rounds. Each of their votes counts ten times a public share's, so they share direction-setting with the control bloc without bearing proportional governance friction. Lockups have lapsed and they can convert or sell down gradually, so their position is portable in ways the public float's is not.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, early_class_b_holders, beneficiary,
    moderate, biographical, mobile, global).

% Sit on the board, collect director fees and equity grants, and administer the exemption regime that waives the independent committees a listed company would otherwise maintain. They were nominated by, and remain dependent on the favor of, the control holder; several have ties to his other ventures. Leaving the board means forfeiting the position and the standing attached to it, and their professional identities are invested in the companies' success narratives.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, captured_board_directors, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__governance_skeptic, captured_board_directors, agenda_setter).

% Own one-vote-per-share stock bought on the open market, much of it at prices embedding the current valuation. They have no meaningful say in director nomination, compensation approval, or cross-company allocation: their ballots are arithmetically irrelevant against the supervoting bloc. They can sell at any time, but selling is their only lever, and exercising it forfeits whatever the investment thesis delivers.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, class_a_public_shareholders, payer,
    powerless, biographical, constrained, global).

% Must hold the stock because it sits in the indices they track; dropping it would mean removing it from client portfolios, which their mandates forbid. They run stewardship programs and vote against contested packages, but the 10:1 ratio converts their opposition into arithmetic noise. Their remaining tools are engagement letters, proxy-season statements, and escalation they have so far declined to initiate.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, passive_index_fund_managers, payer,
    organized, generational, trapped, global).

% Publish voting recommendations and governance-quality ratings. Their policy frameworks treat unequal voting structures and waived committee requirements as red flags. They sit outside the company, analyze from disclosed materials, and sell advice; they neither collect from the arrangement nor bear its costs.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, proxy_advisory_firms, observer,
    organized, biographical, analytical, global).

% Review disclosures, investigate related-party transactions and executive stock sales, and adjudicate complaints. The charter design itself — the share classes, the vote ratio, the exemptions — was settled through exchange listing processes and state incorporation choices rather than regulator sign-off, so their reach over the structure's core is indirect.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, securities_regulators, observer,
    institutional, generational, analytical, national).

% Engineers and executives across the founder's companies compete for the same leader's attention and for capital drawn from overlapping pools. Choices about which venture gets a factory, a product line, or a key hire are made above their heads, with no channel for their input. They could leave for competitors, but the missions are scarce and career-defining, so exit carries costs they weigh heavily.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, musk_constellation_employees, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__governance_skeptic, elon_musk_control_block).
narrative_ontology:fixing_cost_class(valuation_legitimacy__governance_skeptic, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates decision authority in a single founder for capital-allocation problems spanning decades — reusable launch, Starship-class development, gigafactory buildout — insulating those commitments from quarterly earnings pressure, activist campaigns, and hostile takeover during the loss-making ramp. The dual-class vote ratio is the mechanism that keeps that authority stable.
% TRANSFER_FUNCTION: Moves governance rights and a slice of enterprise value from public Class A holders to the control bloc: votes disproportionate to capital contributed, compensation set without an independent committee, and a valuation premium attributable to control itself rather than to distributable cash flows. Cross-company opportunity allocation adds transfers that are difficult to observe from outside.
% ABSENT_VOICES: Atomized Class A holders rarely attend meetings and have no organized seat; index-fund stewards speak in standardized statements that the vote ratio neutralizes; employees of the affected companies have no channel into allocation decisions that shape their work; buyers entering at the current valuation are represented nowhere in the process that set it.
% DISAPPEARANCE_RATIONALE: If the supervoting structure and its exemptions vanished overnight, voting collapsed to parity would make proxy contests winnable, an independent compensation committee would have to re-price existing packages, and every cross-company allocation would need an arm's-length justification. The control premium embedded in the valuation would reprice, and the constellation's capital allocation would reorganize around accountable governance.
% FOUNDING_PROBLEM: At the founding and pre-IPO stages the company needed to pursue capital-intensive, decade-spanning goals that public markets punish quarter to quarter; the dual-class structure was adopted to keep strategic authority with the founder through the loss-making years and to foreclose takeover while the mission matured.
% FOUNDING_PROBLEM_CORROBORATION: Proxy-advisor policy research and the academic finance literature on dual-class discounts attest, from outside the benefiting parties, that the accountability problem the structure now poses is real and persistent; the record of disinterested-shareholder votes and court proceedings attests that minority holders repeatedly rejected packages the full vote count approved. No source outside the benefiting parties attests that the original insulation problem is dead; index-fund stewardship groups argue it is substantially mitigated by scale and engagement, which is precisely the contested position.
narrative_ontology:disappearance_verdict(valuation_legitimacy__governance_skeptic, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__governance_skeptic, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__governance_skeptic, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(valuation_legitimacy__governance_skeptic, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__governance_skeptic, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(valuation_legitimacy__governance_skeptic_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(valuation_legitimacy__governance_skeptic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(valuation_legitimacy__governance_skeptic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.74 because, on this reading, the dominant component of the premium is attributable to control itself: compensation approved by a board the controller appoints, opportunity allocation across privately held affiliates, and a price that already assumes the arrangement persists. Suppression is 0.62 and deliberately below extractiveness: the structure suppresses voice (vote ratio, waived committees) far more than exit — holders may sell — so the coercive floor sits lower than arrangements that bar leaving. Theater_ratio 0.58 reflects governance activity that is predominantly predetermined: advisory votes, annual-meeting exchanges, and independent designations whose outcomes the 10:1 arithmetic fixes in advance. Accessibility_collapse 0.45: once the structure is understood, alternatives persist (other equities, engagement channels, litigation), so alternatives do not fully collapse. Resistance 0.60: proxy-advisor opposition, repeated minority-holder rejection votes, and litigation are real and continuous. The temporal series run on one shared grid (t = 0,3,6,9,12,14,16, approximately the 2010 listing era to the present) so every tracked metric is authored at every examined point: base_extractiveness climbs with each private-benefit episode (affiliate merger, mega-package ratification, cross-company expansion), theater_ratio climbs as accountability performance substitutes for absent machinery, and suppression_requirement climbs as enforcement shifts onto vote arithmetic and jurisdiction selection. Claimed type is tangled_rope, stated independently of the metrics: the structure retains a genuine insulation function (long-horizon capital allocation is a real coordination problem) while operating asymmetric extraction through the same architecture, and it requires active enforcement to hold. The reading's own polemic leans toward pure extraction; the structural data retain a coordination substrate, and that divergence is left for the engine to adjudicate.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical facts. From the control bloc's seat the arrangement is infrastructure it built and legitimately operates: the insulation enabled the missions, and the premium is the market's appraisal of that judgment. From the Class A retail seat the same architecture is a machine that takes votes and value while offering sale as the only response. Index managers occupy the sharpest position: extraction they are contractually forbidden to escape, with their stewardship converted to noise by the ratio. Captured directors experience a benign administrative routine. The engine derives these divergent per-seat classifications from power, exit, and directional position; nothing in the authored claim reconciles them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the control bloc, early Class B holders, and captured directors toward the subsidized end (low d): the arrangement pays them. Victim declarations drive Class A holders and index managers toward the target end (high d), with exit modulating the distance — index managers' trapped position pushes them nearer full-target than retail holders, whose sale option damps their effective extraction. One override is declared: the powerful atom is set to d = 0.06 rather than the derived ~0.0 because the control holder is not a costless beneficiary — his wealth is undivided inside the arrangement, large sales would impair the asset backing his position, and his time is split across ventures at real opportunity cost. The override applies cleanly because the control bloc is this story's only powerful-seat agent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting decade-spanning capital allocation from quarterly-market discipline — is genuinely not dead: the missions still outrun public-market patience, and that residual liveness is what keeps this a tangled_rope rather than a snare in which the coordination story is pure cover. Mandatrophy is therefore not declared resolved. The classification prevents two symmetric mislabels: reading the structure as pure extraction ignores the insulation function the architecture still performs; reading it as pure coordination ignores the measured accumulation (rising extractiveness and theater across the interval) and the waiver of exactly the committees that would hold the controller accountable. The contested founding-problem status routes through the R5 mismatch check rather than through a resolved flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    valuation_kernel_reading_position,
    'This constraint is the governance_skeptic reading of the valuation_legitimacy kernel; how would classification of the same underlying arrangement shift under the sibling readings (dcf_fundamentalist, real_options_technologist, musk_cult_believer)?',
    'Author each sibling as its own constraint story with its own epsilon over the same standing arrangement, then compare computed types across the family.',
    'Under dcf_fundamentalist the same structure may compute nearer rope (pricing discipline substituting for governance); under musk_cult_believer the extraction reading dissolves into earned-trust subsidy; the cross-family divergence is the measurement, not an error to reconcile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(valuation_kernel_reading_position, conceptual, 'Committer-frame routing: this story is one reading of a four-reading kernel; sibling readings are separate constraints.').

omega_variable(
    control_premium_composition,
    'What fraction of the $1.75T valuation is private-benefit-of-control premium versus fundamental option value for the underlying technology?',
    'Event studies around governance milestones (package ratifications, exemption changes, extended founder absences), calibrated against the dual-class discount literature and disinterested-shareholder pricing behavior.',
    'If the premium is largely fundamental, the authored extractiveness overstates and the structure drifts toward a cleaner coordination reading; if largely control rent, the extraction reading strengthens and payer-seat classifications harden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_premium_composition, empirical, 'Decomposition of the valuation premium between control rent and technology option value.').

omega_variable(
    insulation_accountability_separability,
    'Is founder insulation from short-term market pressure structurally separable from unaccountable control — could capped ratios, independent committees, and sunset clauses deliver the insulation without the extraction?',
    'Compare firms with time-decaying or capped dual-class structures that retained independent committees: did long-horizon capital allocation survive while accountability machinery operated?',
    'If separable, the current structure''s excess extraction is a design choice rather than a coordination cost, strengthening the extraction-dominant reading; if inseparable, part of the measured extraction is the genuine price of the insulation function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(insulation_accountability_separability, conceptual, 'Whether the coordination and extraction components of the dual-class architecture are structurally separable.').

omega_variable(
    cross_allocation_direction,
    'Does multi-company control systematically allocate opportunities, capital, and talent toward the privately held ventures or toward the public company?',
    'Audit related-party transactions, hiring flows, and capital commitments across the constellation; test whether the charter provision renouncing corporate opportunities for the founder mitigates the conflict or merely documents it.',
    'Systematic flow toward private vehicles would confirm the conflict as an active transfer channel and raise effective extraction on public holders; balanced or public-favoring flows would support the synergy framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_allocation_direction, empirical, 'Direction of cross-company resource allocation under unified control.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured quiescence of Class A holders structural (this stock is the only exposure channel to the upside; index mandates lock institutions in) or internalized (narrative fusion with the founder keeps retail holders voting with the control bloc and holding through governance controversies)?',
    'Trading and voting behavior around governance shocks: if holders who divest after structural fixes nonetheless continue endorsing the arrangement rhetorically, internalization is present; if behavior tracks available alternatives, the suppression is structural.',
    'If internalized, effective suppression exceeds the structural measure and persists after any governance fix — the constraint would travel with the holders; if structural, removing the vote-ratio machinery removes the suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression among minority holders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__governance_skeptic, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__governance_skeptic, theater_ratio, 0, 0.24).
narrative_ontology:measurement(valu_tr_t3, valuation_legitimacy__governance_skeptic, theater_ratio, 3, 0.29).
narrative_ontology:measurement(valu_tr_t6, valuation_legitimacy__governance_skeptic, theater_ratio, 6, 0.35).
narrative_ontology:measurement(valu_tr_t9, valuation_legitimacy__governance_skeptic, theater_ratio, 9, 0.41).
narrative_ontology:measurement(valu_tr_t12, valuation_legitimacy__governance_skeptic, theater_ratio, 12, 0.48).
narrative_ontology:measurement(valu_tr_t14, valuation_legitimacy__governance_skeptic, theater_ratio, 14, 0.54).
narrative_ontology:measurement(valu_tr_t16, valuation_legitimacy__governance_skeptic, theater_ratio, 16, 0.58).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__governance_skeptic, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(valu_be_t3, valuation_legitimacy__governance_skeptic, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(valu_be_t6, valuation_legitimacy__governance_skeptic, base_extractiveness, 6, 0.57).
narrative_ontology:measurement(valu_be_t9, valuation_legitimacy__governance_skeptic, base_extractiveness, 9, 0.61).
narrative_ontology:measurement(valu_be_t12, valuation_legitimacy__governance_skeptic, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(valu_be_t14, valuation_legitimacy__governance_skeptic, base_extractiveness, 14, 0.71).
narrative_ontology:measurement(valu_be_t16, valuation_legitimacy__governance_skeptic, base_extractiveness, 16, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t0, valuation_legitimacy__governance_skeptic, suppression_requirement, 0, 0.46).
narrative_ontology:measurement(valu_su_t3, valuation_legitimacy__governance_skeptic, suppression_requirement, 3, 0.5).
narrative_ontology:measurement(valu_su_t6, valuation_legitimacy__governance_skeptic, suppression_requirement, 6, 0.54).
narrative_ontology:measurement(valu_su_t9, valuation_legitimacy__governance_skeptic, suppression_requirement, 9, 0.58).
narrative_ontology:measurement(valu_su_t12, valuation_legitimacy__governance_skeptic, suppression_requirement, 12, 0.61).
narrative_ontology:measurement(valu_su_t14, valuation_legitimacy__governance_skeptic, suppression_requirement, 14, 0.64).
narrative_ontology:measurement(valu_su_t16, valuation_legitimacy__governance_skeptic, suppression_requirement, 16, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__governance_skeptic, enforcement_mechanism).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__musk_cult_believer).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'is the $1.75T valuation legitimate?' decomposes into four structurally distinct readings of one kernel, each with its own epsilon over the same standing arrangement. This member (governance_skeptic) authors epsilon for the dual-class arrangement as a governance failure; dcf_fundamentalist authors epsilon for the cash-flow basis; real_options_technologist for the option-space basis; musk_cult_believer for the founder-record basis. The family links run through the shared object of contest — the composition of the premium: governance findings feed DCF discounting, option-space arguments rebut control-rent claims, and the founder-record reading supplies the counter-narrative both analytic readings must answer. No member averages across readings (epsilon-invariance); each is a separate file linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(valuation_legitimacy__governance_skeptic, powerful, 0.06).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
