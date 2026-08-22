% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__extraction_snare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__extraction_snare_reading, []).

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
 *   constraint_id: statutory_debt_ceiling__extraction_snare_reading
 *   human_readable: Statutory Debt Ceiling as Weaponized Extraction Under Default Threat
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   The statutory debt ceiling, enacted in 1917 as a procedural convenience
 *   (Treasury flexibility in debt management), has been transformed over the
 *   past 30 years into a recurring hostage mechanism. A legislative minority
 *   faction strategically withholds support for raising the ceiling unless
 *   the majority agrees to policy concessions unrelated to debt or fiscal
 *   management. The threat of sovereign default—an existential crisis for
 *   financial markets and government operations—coerces the majority into
 *   bargaining it would not undertake under normal legislative process. This
 *   constraint story instantiates the extraction_snare_reading of the
 *   contested statutory_debt_ceiling kernel: the ceiling operates primarily
 *   as a weaponized boundary enabling extraction, not as a fiscal-discipline
 *   instrument or coordination mechanism. The sibling readings
 *   (constitutional_nullity_reading and coordination_scaffold_reading) are
 *   separate constraint stories with their own ε values and stakeholder
 *   structures.
 *
 * KEY AGENTS:
 *   - legislative_minority_faction: Strategic actor with blocking power; uses default threat to extract concessions
 *   - treasury_operations: Institutional payer; manages hard stops when ceiling is reached
 *   - mandatory_program_beneficiaries: Powerless victims; first-order targets for delay/cuts
 *   - financial_markets: Affected institutional actor; bears volatility and default risk
 *   - debt_holders: Institutional payers; face downgrades and confidence erosion
 *   - majority_coalition: Powerful actor forced into concessions; secondary beneficiary from ceiling existence
 *   - credit_rating_agencies: Observers; signal market impact but do not control outcome
 *   - president: Excluded from voting; forced to negotiate or threaten veto
 *   - international_governments: Excluded; spillover victims of default risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, 0.78).
domain_priors:suppression_score(statutory_debt_ceiling__extraction_snare_reading, 0.72).
domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__extraction_snare_reading, snare).
narrative_ontology:human_readable(statutory_debt_ceiling__extraction_snare_reading, "Statutory Debt Ceiling as Weaponized Extraction Under Default Threat").
narrative_ontology:topic_domain(statutory_debt_ceiling__extraction_snare_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__extraction_snare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__extraction_snare_reading, '1695648e-84d0-4203-a2fb-6803f05d9347').
narrative_ontology:cs_kernel_codification('1695648e-84d0-4203-a2fb-6803f05d9347', formalized).
narrative_ontology:cs_authority_grounding('1695648e-84d0-4203-a2fb-6803f05d9347', extraction).
narrative_ontology:cs_interpretation_layer_present('1695648e-84d0-4203-a2fb-6803f05d9347').
narrative_ontology:cs_reading_relation('1695648e-84d0-4203-a2fb-6803f05d9347', statutory_debt_ceiling__coordination_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('1695648e-84d0-4203-a2fb-6803f05d9347', statutory_debt_ceiling__constitutional_nullity_reading, coexists_with).
narrative_ontology:cs_axiom('1695648e-84d0-4203-a2fb-6803f05d9347', foundational, ceiling_weaponizable_as_veto_mechanism).
narrative_ontology:cs_axiom_status(ceiling_weaponizable_as_veto_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('1695648e-84d0-4203-a2fb-6803f05d9347', ceiling_weaponizable_as_veto_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('1695648e-84d0-4203-a2fb-6803f05d9347', foundational, default_threat_coercive_in_legislative_bargaining).
narrative_ontology:cs_axiom_status(default_threat_coercive_in_legislative_bargaining, holdable).
narrative_ontology:cs_axiom_grounding('1695648e-84d0-4203-a2fb-6803f05d9347', default_threat_coercive_in_legislative_bargaining, empirically_contingent).
narrative_ontology:cs_reference_frame('1695648e-84d0-4203-a2fb-6803f05d9347', ceiling_as_procedural_fiscal_device).
narrative_ontology:cs_drift_state('1695648e-84d0-4203-a2fb-6803f05d9347', contemporary_hostage_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('1695648e-84d0-4203-a2fb-6803f05d9347', '2026-06-11T14:23:45Z').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_faction).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, treasury_operations).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, mandatory_program_beneficiaries).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, financial_markets).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, debt_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, majority_coalition).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, majority_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls enough votes to block a clean debt ceiling raise but not enough to govern alone. Uses default threat (refusal to raise ceiling without concessions) to extract policy demands from the majority coalition, which must either capitulate or accept sovereign default. Derives power from the structural asymmetry: if the ceiling is not raised, the entire government defaults, not just the minority's preferred policies.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_faction, agenda_setter,
    organized, biographical, mobile, national).

% Cannot operate without authorization to borrow. Faces repeated hard stops when the ceiling is reached mid-fiscal-year. Each negotiation cycle forces a choice: halt benefit payments, miss payments on existing debt, or comply with minority demands. The Treasury cannot unilaterally raise the ceiling or navigate around it — the constraint is statutory and legislative.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, treasury_operations, payer,
    institutional, immediate, trapped, national).

% Social Security, Medicare, Veterans benefits, and other mandatory spending are first-order targets for delay or cut-backs when the ceiling binds. They bear the cost of the legislative standoff they had no vote in. Exit options are limited: they cannot opt out of benefits they depend on, and cannot compel Congress to act.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, mandatory_program_beneficiaries, payer,
    powerless, biographical, trapped, national).

% Experience uncertainty and volatility in the lead-up to ceiling deadlines. Treasury yields fluctuate; risk premiums rise. If default actually occurs, losses are severe. Markets can sell short-term Treasuries and shift to foreign assets (constrained exit), but the systemic damage from U.S. default affects global portfolio positioning. The market's ability to punish is real but bounded by time and information lags in policy resolution.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, financial_markets, payer,
    powerful, immediate, constrained, global).

% Hold U.S. Treasury securities as assets. Repeated near-default episodes degrade confidence in the covenant; downgrades follow. Holders (foreign central banks, pension funds, insurance companies) cannot exit U.S. debt entirely without massive portfolio restructuring, but they can reduce holdings and diversify into other currencies. The credibility damage accumulates across cycles.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, debt_holders, payer,
    organized, generational, constrained, global).

% Controls enough votes to pass spending bills but cannot raise the ceiling alone if the minority withholds support. They benefit from the ceiling's existence (it constrains spending they oppose) but also bear its cost: they must either negotiate with the minority or accept default. The standoff extracts policy concessions they would not grant in normal legislative process.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, majority_coalition, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__extraction_snare_reading, majority_coalition, payer).

% Assess U.S. credit risk. Multiple near-default episodes and one actual downgrade (2011) have established that the ceiling is now a material risk factor. They report on the constraint's operation but do not control it; they signal the market impact of default risk but cannot force political settlement.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, credit_rating_agencies, observer,
    institutional, generational, analytical, global).

% Cannot unilaterally raise the ceiling or borrowing authority. Has advanced legal theories (14th Amendment Section 4, emergency executive power) to work around the constraint, but none has been tested and all face severe constitutional contestation. Excluded from the voting process that determines the ceiling; can only negotiate or threaten veto.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, president, excluded,
    powerful, biographical, constrained, national).

% Have no vote in U.S. ceiling negotiations but face spillover from default risk: their own borrowing costs rise, their export markets weaken if U.S. demand contracts, and their dollar reserves face valuation risk. They cannot force the minority to concede but are affected by the extraction dynamic.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, international_governments, excluded,
    powerful, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_faction).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__extraction_snare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ceiling was originally framed as a check on executive borrowing authority—a legislative duty to approve debt issuance rather than an automatic right. Under this reading, coordination is minimal and vestigial: the constraint no longer solves a genuine fiscal-policy alignment problem but instead became a veto mechanism.
% TRANSFER_FUNCTION: The ceiling enables a legislative minority to extract policy concessions from the majority and the executive by threatening sovereign default. The transfer is not money but political leverage: the minority receives policy wins (budget cuts, regulatory rollbacks, spending restrictions) conditional on releasing its veto over debt service. Those who pay are Treasury operations (forced to manage under artificial constraint), mandatory-program beneficiaries (whose benefits face delays), and the financial system (which bears increased systemic risk and reputational damage).
% ABSENT_VOICES: The president (excluded from voting but forced to negotiate), international governments (excluded from voting but affected by default risk), and future generations (locked into debt obligations created during periods of brinkmanship). They would argue for removing or depoliticizing the ceiling; their absence from the voting coalition is what allows the minority's veto to function.
% DISAPPEARANCE_RATIONALE: If the ceiling disappeared, the Treasury would operate without hard stops, appropriations would remain the sole lever on fiscal policy, and the minority faction would lose its recurring hostage mechanism. The political dynamics of budget negotiation would revert to normal legislative process: majority rule, executive veto, and compromise. Bond markets would stabilize, credit ratings would normalize, and the Treasury would not need to manage contingency plans for payment prioritization. The constraint's removal would eliminate a source of systemic financial risk and policy uncertainty that benefits a recurrent minority veto-holder.
% FOUNDING_PROBLEM: The 1917 Liberty Bond Act created the first statutory debt ceiling to give the Treasury operational flexibility to manage debt issuance within a fixed aggregate authorization, rather than requiring Congress to approve each new bond issuance individually. The ceiling was a procedural convenience: it allowed Treasury to refinance maturing debt without repeated congressional votes on the same authority.
% FOUNDING_PROBLEM_CORROBORATION: Treasury officials, fiscal-policy economists (Lew, Summers, Blinder), and the Congressional Budget Office have testified and published that the original problem—Congress micromanaging each bond issuance—is not the lived problem anymore. The ceiling is no longer used for fine-tuning fiscal discipline; it is now a recurring hostage point. Even proponents of fiscal constraint (who would support a binding budget rule) acknowledge that the ceiling-as-currently-used does not enforce discipline but rather creates artificial crises. Republican deficit hawks during 1990s budget negotiations treated the ceiling as a negotiating point, not a fiscal principle. The shift from procedural convenience to political weapon is documented in Congressional Research Service reports and political-science analyses of legislative bargaining dynamics.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__extraction_snare_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__extraction_snare_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__extraction_snare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__extraction_snare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__extraction_snare_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness trajectory from 1917 to 2026 models the transformation from procedural tool to hostage mechanism. From 1917–1960, extractiveness was minimal (0.05–0.08): the ceiling existed but was raised routinely and was not weaponized. By 1995, extractiveness had risen to 0.35 as minority factions began using the ceiling strategically (Gingrich-led Republican Congress, 1995–96 showdown). The 2011 debt-ceiling crisis (Tea Party movement) saw extractiveness spike to 0.68; the Obama administration faced the first serious threat of default and extracted major spending concessions. By 2021–2026, extractiveness stabilized at 0.75–0.78: the minority faction treats the ceiling as a regular veto point, using it to extract defunding of regulatory agencies, tax concessions, or spending caps. Theater_ratio declined from 0.85 (1917: mostly procedural choreography) to 0.41 (2026: a substantial portion of enforcement activity is genuine hostage-taking, not procedural ritual). Suppression_requirement rose from 0.10 to 0.72: the constraint now requires high active enforcement—the minority must hold its coalition together, message aggressively to prevent backsliding, and credibly signal willingness to default (or at least to let the Treasury miss payments). The suppression of alternative political settlement (majority rule without veto) is structural to the constraint's function.
 *
 * PERSPECTIVAL GAP:
 *   From the legislative_minority_faction seat: the ceiling is a legitimate veto point in a supermajority system; it is not 'extraction' but rather the proper function of minority protection against fiscal adventurism. From the treasury_operations and mandatory_program_beneficiaries seats: the ceiling is an imposed hostage mechanism with no fiscal legitimacy—it prevents orderly government operations and forces pain on constituencies that had no voice in the standoff. From the financial_markets and debt_holders seats: the ceiling is a source of needless systemic risk that damages U.S. sovereign creditworthiness. From the president's seat (excluded): it is an unconstitutional constraint on executive authority to spend appropriated funds. The engine computes per-seat classifications from this heterogeneous structural data; the computed types will diverge across seats because the power atoms, exit options, and extraction profiles differ. The reading-level claim (snare) is the author's assessment that the constraint is structurally extractive; divergence between the claim and computed seat types is the measurement the corpus captures.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative_minority_faction is the clear beneficiary (d near 0.0): it gains policy concessions that would not survive normal legislative process and derives its power from control of the ceiling veto. The majority_coalition is a hybrid (secondary_role beneficiary because the ceiling constrains some spending it opposes, but also a payer forced into concessions). Treasury operations and mandatory_program_beneficiaries are targets (d near 1.0): they bear direct costs and have trapped exit. Financial_markets sit between (d ~0.6–0.7): they suffer volatility and default risk but are not the primary targets of extraction. The president is excluded: they cannot vote but are forced to negotiate. International_governments are similarly excluded and affected as spillover victims. This heterogeneity of directionality across seats is precisely what the engine computes from the structural data; the claim (snare) is independent of the metrics and reflects the reading's own assessment that the constraint functions primarily as extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (enabling Treasury flexibility without repeated congressional micromanagement) is dead: it is not the actual function the ceiling now serves. The majority of fiscal policy is carried through appropriations bills, not ceiling negotiations. The ceiling has become decoupled from fiscal discipline and functions instead as a recurring veto point for a minority faction. This is mandatrophy: the constraint persists because the legislative process itself has ossified around it (both parties now use it as a negotiating point), but the original justification is abandoned. The constraint could be eliminated without degrading the solution to the founding problem (Treasury already negotiates appropriations; adding repeated ceiling votes would be inefficient, not helpful). The persistence of the ceiling despite mandatrophy is explained by the extraction benefit to the minority faction and the political cost to any majority that unilaterally removes the minority's veto tool. The snare classification captures this: the constraint persists because it serves extraction interests, not because it solves the problem it was built for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_kernel_foreclosure,
    'Does the constitutional_nullity_reading logically foreclose this extraction_snare_reading within a single constitutional framework, or do the readings simply disagree on which framework is operative?',
    'Supreme Court decision on whether the 14th Amendment Section 4 self-executes and overrides the statutory ceiling, or requires legislative action to implement. If the Court rules Section 4 is self-executing and the ceiling is void, the nullity reading would foreclose the snare reading (no constraint = no extraction). If the Court rules the ceiling is constitutionally valid pending legislative repeal, the snare reading coexists with the nullity reading as competing claims about legislative intent.',
    'If foreclosure is established, the snare reading''s classification becomes moot (there is no constraint to classify). If coexistence is established, both readings are live and the corpus contains two distinct constraints instantiated from the same kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_foreclosure, empirical, 'Whether the constitutional nullity reading and extraction snare reading logically exclude each other or coexist as competing legal/political positions.').

omega_variable(
    minority_agency_and_coordination,
    'Is the minority faction''s use of the ceiling as a veto an exercise of legitimate supermajority-system coordination, or is it extraction that violates the system''s intended rules?',
    'Political-science and constitutional analysis of supermajority systems: do such systems depend on minority blocking power as a necessary check, or do they intend majorities to govern with some supermajority threshold on specific matters (like fiscal measures)? If minority blocking is structural (design-intentional), the snare classification may need to downgrade extractiveness. If minority blocking is a norm violation (abuse of a procedural tool), extractiveness remains high.',
    'If coordination, the snare classification might downgrade to tangled_rope (coordination with asymmetric extraction). If extraction, the snare classification holds and might upgrade severity. The fundamental question is whether the legislative system was designed to allow this use of the ceiling, or whether it represents rule-gaming by a minority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_agency_and_coordination, conceptual, 'Whether the minority''s veto use is structural or norm-violating within the constitutional/legislative design.').

omega_variable(
    default_threat_credibility,
    'Is the minority faction''s threat to allow default actually credible, or does the threat derive its power from majority fear of default even if the minority would not actually allow it?',
    'Historical counterfactual: if the threat were called (the ceiling reached and not raised), would the minority actually block payment on existing debt, or would they back down before triggering actual default? The 2011 and 2023 crises came very close; analyzing the actors'' statements and legislative behavior in extremis would test credibility.',
    'If the threat is not actually credible (the minority would back down), suppression is overstated because the actual mechanism is majority fear, not minority will. If the threat is credible, suppression is correctly measured and extractiveness reflects the force of a genuine doomsday commitment. This affects whether the constraint qualifies as snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(default_threat_credibility, empirical, 'Whether the default threat is actually credible or derived from majority perception of risk.').

omega_variable(
    kernel_reading_structural_distinction,
    'What structural facts distinguish the extraction_snare_reading from the coordination_scaffold_reading? Is the difference empirical (different measurement of the same constraint) or conceptual (disagreement on what the constraint''s function is)?',
    'If the readings assign different ε values to the same referent (the standing statutory constraint), the difference is empirical: one reading measures the constraint as extractive, the other as coordinative. If the readings assign different structural referents (one reading the constraint as a veto mechanism, the other as a procedural rule), the difference is conceptual and requires two separate constraint stories per the ε-invariance principle.',
    'If empirical divergence, both readings might measure the same constraint at different time points or might measure different aspects (extraction vs. coordination both present, weighted differently). If conceptual divergence, the readings describe materially different constraints and should be separate files. Current authoring assumes empirical divergence at a single time point (present day), with the snare reading emphasizing extraction and the scaffold reading emphasizing coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_distinction, conceptual, 'Whether the snare and scaffold readings differ empirically in measurement or conceptually in the constraint''s referent.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (legal rules preventing alternatives, treasury''s inability to work around the ceiling) or internalized (actors'' acceptance of the ceiling''s legitimacy, normalization of brinkmanship negotiation)?',
    'Post-removal suppression trajectory: if Congress were to repeal the ceiling, would the alternative settlement (majority rule on appropriations, no recurring hostage negotiations) persist, or would actors recreate the constraint through new rules or norms? If suppression persists post-removal, it is internalized. If alternatives are genuinely adopted, suppression was structural.',
    'If structural, the snare classification holds as stated. If internalized, the effective suppression experienced by victims may be higher than the metric suggests (they carry normalization of brinkmanship with them beyond the constraint itself). This would support reclassification toward severe snare or advocate for deeper institutional reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the measured suppression reflects legal/structural barriers or internalized normalization of the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__extraction_snare_reading, 1917, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1917, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1917, 0.85).
narrative_ontology:measurement(stat_tr_t1960, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1960, 0.75).
narrative_ontology:measurement(stat_tr_t1995, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1995, 0.58).
narrative_ontology:measurement(stat_tr_t2011, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2011, 0.42).
narrative_ontology:measurement(stat_tr_t2021, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2021, 0.4).
narrative_ontology:measurement(stat_tr_t2026, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(stat_be_t1917, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1917, 0.05).
narrative_ontology:measurement(stat_be_t1960, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1960, 0.08).
narrative_ontology:measurement(stat_be_t1995, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(stat_be_t2011, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2011, 0.68).
narrative_ontology:measurement(stat_be_t2021, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2021, 0.75).
narrative_ontology:measurement(stat_be_t2026, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2026, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1917, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1917, 0.1).
narrative_ontology:measurement(stat_su_t1960, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1960, 0.15).
narrative_ontology:measurement(stat_su_t1995, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement(stat_su_t2011, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2011, 0.68).
narrative_ontology:measurement(stat_su_t2021, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2021, 0.71).
narrative_ontology:measurement(stat_su_t2026, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__extraction_snare_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__extraction_snare_reading, 0.12).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__constitutional_nullity_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, treasury_payment_prioritization_protocol).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, us_credit_rating_downgrade_regime).

% DUAL FORMULATION NOTE:
% The statutory debt ceiling is a contested kernel with three instantiated readings: constitutional_nullity_reading (ceiling void under 14th Amendment Section 4; nearly zero extraction, mountain-type classification), coordination_scaffold_reading (ceiling is legitimate procedural coordination; low-moderate extraction, rope or tangled_rope classification), and extraction_snare_reading (this story: ceiling weaponized as hostage mechanism; high extraction, snare classification). The three readings share the same referent (the standing statutory constraint) but assign different ε values and different structural beneficiaries/victims. This story instantiates only the extraction_snare_reading. The sibling readings are authored separately with distinct ε-invariance, stakeholder structures, and classifications. All three are linked via network.affects_constraints to signal the kernel relationship. Readers consulting any one story should understand that the apparent classification divergence (nullity=mountain, scaffold=rope, snare=snare) reflects different readings of the same kernel, not measurement error or authoring inconsistency. The contest is structural and unresolved.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statutory_debt_ceiling__extraction_snare_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
