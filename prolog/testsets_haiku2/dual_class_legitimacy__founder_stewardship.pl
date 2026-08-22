% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__founder_stewardship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__founder_stewardship, []).

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
 *   constraint_id: dual_class_legitimacy__founder_stewardship
 *   human_readable: Dual-Class Share Structure: Founder Stewardship Reading
 *   domain: corporate_governance/securities_law
 *
 * SUMMARY:
 *   A company with dual-class share structure grants founder-stewards
 *   super-voting Class A shares (10–20x per-share votes) while public
 *   shareholders hold Class B shares (one vote per share). The founder
 *   reading frames this as necessary stewardship: concentrated control
 *   protects a multi-decade mission from quarterly pressure and activist
 *   intervention, benefiting all shareholders through sustained value
 *   creation over short-termism. This is one reading of a contested kernel —
 *   the same dual-class structure and founder authority are read by the
 *   minority_extraction reading as disproportionate founder appropriation at
 *   public shareholders' expense, and by the disclosure_consent reading as
 *   legitimate only to the degree informed consent is properly disclosed.
 *   This story instantiates the founder_stewardship reading: control is
 *   framed as coordination; the founder as fiduciary; the structure as
 *   mission protection; Class A holders benefit indirectly via mission
 *   success, not directly via extraction. The claim/metric gap is deliberate:
 *   the stewardship reading claims the constraint is rope (genuine
 *   coordination with beneficiary alignment), while the authored metrics
 *   reflect that substantial extraction is still present (extractiveness
 *   0.38) — the founder holds concentrated authority regardless of whether
 *   the stewardship premise is sound.
 *
 * KEY AGENTS:
 *   - founder_stewards: concentrated super-voting authority; identity-locked to the mission; generational time horizon
 *   - minority_class_b_shareholders: organized opposition; can exit via market sales; biographical time horizon
 *   - employees_and_mission_operators: constrained beneficiaries of mission focus; bear governance exclusion cost
 *   - activist_investors: structurally excluded from governance; immediate time horizon; would overturn the structure
 *   - securities_regulators: analytical observer seat; monitor disclosure adequacy and fiduciary compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, 0.38).
domain_priors:suppression_score(dual_class_legitimacy__founder_stewardship, 0.22).
domain_priors:theater_ratio(dual_class_legitimacy__founder_stewardship, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, extractiveness, 0.38).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Dual-Class Share Structure: Founder Stewardship Reading").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "corporate_governance/securities_law").

domain_priors:requires_active_enforcement(dual_class_legitimacy__founder_stewardship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, '6d9e19a8-86fe-476a-9001-0667b1caec68').
narrative_ontology:cs_kernel_codification('6d9e19a8-86fe-476a-9001-0667b1caec68', formalized).
narrative_ontology:cs_authority_grounding('6d9e19a8-86fe-476a-9001-0667b1caec68', extraction).
narrative_ontology:cs_interpretation_layer_present('6d9e19a8-86fe-476a-9001-0667b1caec68').
narrative_ontology:cs_reading_relation('6d9e19a8-86fe-476a-9001-0667b1caec68', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('6d9e19a8-86fe-476a-9001-0667b1caec68', dual_class_legitimacy__disclosure_consent, influences).
narrative_ontology:cs_axiom('6d9e19a8-86fe-476a-9001-0667b1caec68', foundational, fiduciary_founder_stewardship_constrains_extraction).
narrative_ontology:cs_axiom_status(fiduciary_founder_stewardship_constrains_extraction, holdable).
narrative_ontology:cs_axiom_grounding('6d9e19a8-86fe-476a-9001-0667b1caec68', fiduciary_founder_stewardship_constrains_extraction, deontological).
narrative_ontology:cs_axiom('6d9e19a8-86fe-476a-9001-0667b1caec68', foundational, long_horizon_mission_requires_control_insulation).
narrative_ontology:cs_axiom_status(long_horizon_mission_requires_control_insulation, holdable).
narrative_ontology:cs_axiom_grounding('6d9e19a8-86fe-476a-9001-0667b1caec68', long_horizon_mission_requires_control_insulation, empirically_contingent).
narrative_ontology:cs_reference_frame('6d9e19a8-86fe-476a-9001-0667b1caec68', founder_mission_protection_framework).
narrative_ontology:cs_drift_state('6d9e19a8-86fe-476a-9001-0667b1caec68', contemporary_activist_capitalism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6d9e19a8-86fe-476a-9001-0667b1caec68', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founder_stewards).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, all_shareholders_via_mission).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, employees_and_mission_operators).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, proxy_advisors_and_institutional_voters).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, debt_holders).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, minority_class_b_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, employees_and_mission_operators).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, long_horizon_mission_protection).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, fiduciary_founder_alignment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold super-voting Class A shares that grant 10x or 20x the vote per share relative to Class B (public) shares. Control the board and major decisions regardless of public share ownership levels. Justify this structure as necessary to execute a long-horizon mission that would be vulnerable to quarterly pressure or activist intervention. Cannot easily exit without dissolving the structure that defines their role as stewards.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founder_stewards, agenda_setter,
    institutional, generational, identity_locked, national).

% Own Class B (ordinary) shares with one vote per share. Have purchased into the company accepting the dual-class structure. Expect that founder mission focus will drive long-term value creation that compensates for governance dilution. Can exit by selling shares; alternatives exist (index funds, other equity). Bear the cost of governance exclusion in exchange for exposure to mission-driven returns.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, minority_class_b_shareholders, payer,
    organized, biographical, mobile, national).

% Benefit from founder commitment to long-horizon mission execution: stable strategy, reinvestment in mission over quarterly extraction, insulation from activist pressure to cut R&D or cut mission-aligned spending. Also bear the cost of potential founder overreach or inefficient capital allocation if the stewardship premise fails. Have limited ability to change the governance structure while employed.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, employees_and_mission_operators, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, employees_and_mission_operators, payer).

% Would contest control if they held supermajority Class B shares, seeking short-term value extraction (dividend hikes, asset sales, cost cutting). Are excluded from governance by the super-voting structure. Can buy Class B shares and propose alternatives; cannot force governance change. Would argue for proportional voting and direct board access.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, activist_investors_and_arbs, excluded,
    powerful, immediate, mobile, national).

% Oversee disclosure obligations and fiduciary duty rules. Examine whether Class A superiority and founder lockup are disclosed adequately and whether fiduciary duties of control are honored. Can issue no-action letters, prosecute fraud, or recommend legislative action. Remain neutral on governance structure legitimacy while monitoring disclosure adequacy.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, securities_regulators, observer,
    institutional, generational, analytical, national).

% Vote public shares held by their fund investors on governance proposals. Increasingly scrutinize dual-class structures and founder control; rely on founder performance record and disclosed mission to justify non-opposition. Benefit from sustained mission success that accrues to their portfolios; bear reputational risk if mission fails and they failed to challenge governance.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, proxy_advisors_and_institutional_voters, beneficiary,
    organized, biographical, constrained, national).

% Hold bonds or secured debt contingent on company solvency. Benefit from founder commitment to long-term value creation and mission stability over aggressive dividend extraction. Also bear covenant risk if founder stewardship fails or diverts resources inefficiently. Cannot change voting structure directly; rely on covenants and board representation for risk mitigation.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, debt_holders, beneficiary,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__founder_stewardship, founder_stewards).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__founder_stewardship, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns founder strategic vision with long-horizon execution: enables credible commitment to multi-decade mission (R&D cycles, infrastructure buildout, market development) that would be vulnerable to quarterly earnings pressure, activist proxy fights, or activist board takeover attempts. Solves the coordination problem of maintaining mission fidelity across generational time horizons despite public market pressure for immediate returns.
% TRANSFER_FUNCTION: Concentrates governance authority in founder hands. Public shareholders accept diminished voting power in exchange for exposure to mission-driven long-term value creation. The transfer is governance authority from pro-rata capital share to concentrated steward control; the offsetting benefit is mission stability and long-horizon value capture.
% ABSENT_VOICES: Activist investors and short-term arbitrageurs are structurally excluded from governance. They would argue for one-share-one-vote and quarterly performance optimization. They cannot force board seats or vetoes but can acquire Class B shares and propose governance changes (which typically fail under founder control).
% DISAPPEARANCE_RATIONALE: If the dual-class structure and founder super-voting evaporated overnight, immediate pressure would intensify: activist campaigns would proliferate, board composition would shift toward short-term-focused directors, quarterly earnings guidance would replace multi-year strategy disclosure, and mission-aligned capital spending would face cuts to boost shareholder distributions. The governance discipline that protected long-horizon execution would collapse.
% FOUNDING_PROBLEM: How can a founder-led company execute a multi-decade mission (scientific research, infrastructure, market transformation) in a public market environment where quarterly earnings pressure and activist intervention push toward short-term value extraction and mission compromise?
% FOUNDING_PROBLEM_CORROBORATION: The founder and long-term-focused institutional shareholders attest that quarterly pressure and activist campaigns remain credible threats to mission focus. Independent venture analysts and corporate governance scholars (outside the benefiting parties) document the historical pattern: venture-backed companies with founder exit pressures converge on short-term optimization once public; dual-class structures exist precisely as a structural response to this documented pressure. Activist campaigns against tech companies demonstrate the threat model actively.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__founder_stewardship, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__founder_stewardship, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dual_class_legitimacy__founder_stewardship, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__founder_stewardship, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__founder_stewardship_tests).
:- end_tests(dual_class_legitimacy__founder_stewardship_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate (0.38 at interval end) because the founder holds concentrated governance authority that could be used for extraction but under this reading is used for mission stewardship — the structural capacity for extraction is present, but the reading asserts it is constrained by founder fiduciary duty and identity-lock to mission. Suppression is low (0.22) because public shareholders retain liquid exit (sell Class B shares) and the structure is transparent and disclosed — no hidden coercion mechanism. Theater ratio is also low (0.18) because the mission-focus activity is substantively real: R&D spending, long-cycle project execution, and reinvestment are objectively documented. The measurement series shows extractiveness rising modestly from 0.25 to 0.38 over 40 years, consistent with founder aging, potential succession tension, and drift from initial mission clarity — but no collapse or acceleration. Suppression stays flat because the structural constraint (super-voting, board control) remains constant; no enforcement escalation occurs. Theater ratio rises only slightly, staying below 0.20, indicating the coordination/mission function stays primary.
 *
 * PERSPECTIVAL GAP:
 *   The founder's seat and the minority shareholders' seat compute differently. From the founder's position, the structure is coordination they designed to solve the generational-mission problem; from minority shareholders' positions, it is concentrated authority that could be abused and that denies them proportional voice. The engine should compute the founder's d near the beneficiary end (they control the arrangement and receive fiduciary discretion; they benefit from mission success and have constrained exit) and minority shareholders' d near symmetric (they can exit via sale, they benefit indirectly from mission success, but they bear governance exclusion). Activist investors' d would run high (trapped by exclusion, harmed by exclusion, high resistance). The divergence in d values is the seat-specific classification the engine produces from this structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Founder stewards: d~0.15 (low/beneficiary end). They hold concentrated authority, control the agenda, receive fiduciary discretion, and are identity-locked to mission. Exit is identity-dissolving. But they argue (and the stewardship reading endorses) that this authority is constrained by fiduciary duty to ALL shareholders and by the mission itself, not free appropriation. Class B shareholders: d~0.45 (near symmetric). They can exit via liquid markets, they benefit indirectly from mission success (long-term value), but they bear governance exclusion and have no board seat or veto. Employees: d~0.35 (beneficiary-leaning). They benefit from mission stability and constrained exit pressures; their constraints are employment-based, not capital-structure-based. Activists: d~0.75 (high/target end). They are structurally excluded, they bear opportunity cost (cannot influence decisions), and they have strong resistance. Debt holders: d~0.20 (beneficiary-leaning). They benefit from mission stability and long-term value; their constraints are covenants, not voting suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and attested: quarterly pressure and activist intervention ARE documented threats to mission-focused companies (Amazon's AWS reinvestment vs. activist pressure for shareholder distributions, Tesla's R&D spending vs. short-term EPS targeting). The dual-class structure solves this by making the founder immune to activist proxy fights. Whether the solution is legitimate or merely a vehicle for founder extraction is exactly where the kernel contest lives — the stewardship reading asserts that fiduciary duty and mission identity constrain founder appropriation; the minority_extraction reading asserts that concentrated authority permits unchecked appropriation regardless of fiduciary claims; the disclosure_consent reading asserts that legitimacy rests only on adequate disclosure and informed public choice, not on founder intent. None of these readings is mandatrophy in the sense of a dead founding problem being theatrically maintained — the problem is live. The classification divergence (rope vs. snare vs. tangled_rope, depending on reading) is the actual measurement the corpus takes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_fiduciary_constraint_reality,
    'Is founder fiduciary duty genuinely operative as a constraint on appropriation, or is it a nominal concept with no teeth once control is concentrated?',
    'Empirical tracking: (a) instances where founders declined value-extractive choices available to them in favor of mission alignment; (b) cases where minority shareholders successfully litigated breach of fiduciary duty claims against founder-controlled boards; (c) post-founder-exit successor founder behavior (does new leader continue mission focus or extract?); (d) independent board audit of capital allocation and founder compensation changes.',
    'If fiduciary constraint is real and operative: the stewardship reading holds, extractiveness is overstated by metrics that don''t distinguish constrained from unconstrained authority, and classification trends toward rope. If nominal: extractiveness metric is accurate, founders can extract at will within legal bounds, and classification trends toward tangled_rope or snare despite the stewardship framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_fiduciary_constraint_reality, empirical, 'Whether fiduciary duty meaningfully constrains founder extraction under concentrated control.').

omega_variable(
    mission_identity_vs_opportunistic_framing,
    'Is the founder''s commitment to mission genuine identity-fusion (they cannot imagine exiting the mission; stewardship is who they ARE) or strategic framing to justify concentrated authority (stewardship narrative is post-hoc justification for control)?',
    'Post-control analysis: founder behavior if control were removed or founder succession occurs; biographical/interview data on founder''s stated motivations; pattern analysis across multiple founder-controlled firms (do they show coherent long-term mission execution or variable behavior dependent on exit conditions?); successor founder behavior (if mission is what mattered, does successor maintain it?)',
    'If genuine identity-fusion: founder is genuinely locked in to mission focus, extraction is constrained by identity psychology, stewardship framing is structurally sound, and rope classification is justified. If opportunistic framing: founder will extract when exit approaches, succession brings value extraction, stewardship is theater, and snare/tangled_rope classification applies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mission_identity_vs_opportunistic_framing, conceptual, 'Whether founder stewardship is genuine identity commitment or strategic narrative.').

omega_variable(
    minority_shareholder_informed_consent,
    'Do Class B public shareholders purchase into the dual-class structure with genuine informed consent, or do many discover the governance cost only after purchase when activist action becomes relevant?',
    'Disclosure audit: prospectus language, complexity of dual-class terms, prominence in offering documents, investor survey data on comprehension at purchase time vs. post-hoc realization; market pricing analysis (do Class B shares trade at a consistent governance discount?); shareholder vote records on dual-class continuation proposals.',
    'If genuine informed consent and consistent pricing: the structure is legitimated by disclosure and choice, minority shareholders knowingly accepted the cost, and rope classification is defensible. If consent is low or investors discover governance cost post-purchase: classification shifts toward tangled_rope (coordination + extraction with suppressed alternatives) or snare (extraction with limited exit awareness).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minority_shareholder_informed_consent, empirical, 'Whether Class B shareholders purchase the dual-class structure with informed consent or post-hoc realization.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the founder_stewardship axiom (fiduciary founder stewardship protects mission, benefiting all shareholders) logically foreclose the minority_extraction reading''s axiom (founder uses concentrated authority to extract disproportionate value), or can both coexist across different parties'' frameworks?',
    'Logical analysis: the stewardship axiom asserts that fiduciary duty and identity-lock constrain extraction; the extraction axiom asserts that authority permits unconstrained appropriation. Both can coexist if fiduciary duty is real (stewards are genuinely constrained) OR if it is nominal (authority operates unconstrained). The readings do not logically foreclose each other — they inhabit different epistemic frames about whether the constraint on appropriation exists. No single framework can hold both IF fiduciary duty is definitively real or definitively nominal; but in the actual world, fiduciary duty''s operativeness is empirically contingent (omega_founder_fiduciary_constraint_reality), so coexistence is the structural state.',
    'Determines reading_relations type: if readings are truly incompatible in any coherent framework, use forecloses; if they can coexist because the constraining axiom is empirically contingent, use coexists_with; if one reading''s axiom creates pressure on the other''s plausibility, use influences. The answer here argues for coexists_with because the empirical contingency allows both to remain live positions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Structural relationship between the stewardship and extraction axioms across kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__founder_stewardship, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(dual_tr_t0, observed).
narrative_ontology:measurement(dual_tr_t5, dual_class_legitimacy__founder_stewardship, theater_ratio, 5, 0.13).
narrative_ontology:measurement_basis(dual_tr_t5, observed).
narrative_ontology:measurement(dual_tr_t10, dual_class_legitimacy__founder_stewardship, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(dual_tr_t10, observed).
narrative_ontology:measurement(dual_tr_t15, dual_class_legitimacy__founder_stewardship, theater_ratio, 15, 0.16).
narrative_ontology:measurement_basis(dual_tr_t15, observed).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__founder_stewardship, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(dual_tr_t20, observed).
narrative_ontology:measurement(dual_tr_t25, dual_class_legitimacy__founder_stewardship, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(dual_tr_t25, observed).
narrative_ontology:measurement(dual_tr_t30, dual_class_legitimacy__founder_stewardship, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(dual_tr_t30, observed).
narrative_ontology:measurement(dual_tr_t40, dual_class_legitimacy__founder_stewardship, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(dual_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__founder_stewardship, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(dual_be_t0, observed).
narrative_ontology:measurement(dual_be_t5, dual_class_legitimacy__founder_stewardship, base_extractiveness, 5, 0.28).
narrative_ontology:measurement_basis(dual_be_t5, observed).
narrative_ontology:measurement(dual_be_t10, dual_class_legitimacy__founder_stewardship, base_extractiveness, 10, 0.31).
narrative_ontology:measurement_basis(dual_be_t10, observed).
narrative_ontology:measurement(dual_be_t15, dual_class_legitimacy__founder_stewardship, base_extractiveness, 15, 0.34).
narrative_ontology:measurement_basis(dual_be_t15, observed).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__founder_stewardship, base_extractiveness, 20, 0.36).
narrative_ontology:measurement_basis(dual_be_t20, observed).
narrative_ontology:measurement(dual_be_t25, dual_class_legitimacy__founder_stewardship, base_extractiveness, 25, 0.37).
narrative_ontology:measurement_basis(dual_be_t25, observed).
narrative_ontology:measurement(dual_be_t30, dual_class_legitimacy__founder_stewardship, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(dual_be_t30, observed).
narrative_ontology:measurement(dual_be_t40, dual_class_legitimacy__founder_stewardship, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(dual_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__founder_stewardship, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(dual_su_t0, observed).
narrative_ontology:measurement(dual_su_t5, dual_class_legitimacy__founder_stewardship, suppression_requirement, 5, 0.19).
narrative_ontology:measurement_basis(dual_su_t5, observed).
narrative_ontology:measurement(dual_su_t10, dual_class_legitimacy__founder_stewardship, suppression_requirement, 10, 0.2).
narrative_ontology:measurement_basis(dual_su_t10, observed).
narrative_ontology:measurement(dual_su_t15, dual_class_legitimacy__founder_stewardship, suppression_requirement, 15, 0.21).
narrative_ontology:measurement_basis(dual_su_t15, observed).
narrative_ontology:measurement(dual_su_t20, dual_class_legitimacy__founder_stewardship, suppression_requirement, 20, 0.21).
narrative_ontology:measurement_basis(dual_su_t20, observed).
narrative_ontology:measurement(dual_su_t25, dual_class_legitimacy__founder_stewardship, suppression_requirement, 25, 0.22).
narrative_ontology:measurement_basis(dual_su_t25, observed).
narrative_ontology:measurement(dual_su_t30, dual_class_legitimacy__founder_stewardship, suppression_requirement, 30, 0.22).
narrative_ontology:measurement_basis(dual_su_t30, observed).
narrative_ontology:measurement(dual_su_t40, dual_class_legitimacy__founder_stewardship, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(dual_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__founder_stewardship, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__founder_stewardship, 0.1).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% The dual-class legitimacy kernel decomposes into three constraint stories sharing a fixed standing arrangement (super-voting Class A, ordinary Class B shares) but instantiating different readings with different ε values and beneficiary/victim structures. founder_stewardship (THIS READING) frames control as coordination with low extracted value (~0.38); minority_extraction frames the same structure as concentrated appropriation (~0.68+); disclosure_consent frames legitimacy as conditional on disclosed informed consent (~0.45, depending on disclosure quality). The readings are linked via network.affects_constraints; each story carries its own ε, its own beneficiary/victim set, and its own type classification. The kernel contest is the structural fact the corpus measures — disagreement across readings is not an error but the data the engine exists to collect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_class_legitimacy__founder_stewardship, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
