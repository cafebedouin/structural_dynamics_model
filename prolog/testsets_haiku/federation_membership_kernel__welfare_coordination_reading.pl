% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__welfare_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership_kernel__welfare_coordination_reading
 *   human_readable: EU Free Movement via Welfare Coordination: Posted Worker Cost-Competition Framework
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The EU's response to free movement frictions (large wage gaps post-2004
 *   enlargement, posting-sector undercutting, receiving-state welfare-system
 *   pressure) has been to enforce anti-dumping rules (Posted Workers
 *   Directive, revised 2018) while preserving member-state welfare autonomy.
 *   This reading holds that the system COORDINATES via national welfare
 *   diversity rather than supranational harmonization. The constraint is
 *   claimed as tangled_rope because it both solves a genuine coordination
 *   problem (enabling labor mobility without harmonization) AND
 *   systematically extracts from posted workers and sending states. The
 *   measurement series shows extractiveness rising sharply after 2003
 *   (Eastern enlargement) and theater rising gradually (increasing reliance
 *   on equal-wage paperwork rather than real protection).
 *
 * KEY AGENTS:
 *   - EU Commission Labor Directorate: Enforces Posted Workers Directive, mediates anti-dumping rules, preserves member-state welfare authority
 *   - Receiving-state employers: Benefit from posting-induced labor-cost advantage during exemption windows
 *   - Labor brokers: Extract rents by managing rotation pipelines and compliance machinery
 *   - Posted workers: Trapped in precarious, rotating contracts; lose welfare access; powerless in collective bargaining
 *   - Permanent migrant laborers: Face displacement pressure from posted-worker undercutting in low-skill sectors
 *   - Receiving-state native workers: Experience wage pressure in low-skill occupations; unions unable to cross-border organize
 *   - Sending-state governments: Lose workers and tax revenue without fiscal compensation
 *   - ECJ and supranational courts: Interpret free movement scope; recent rulings split between expansionist and restrictionist readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.55).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Free Movement via Welfare Coordination: Posted Worker Cost-Competition Framework").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, '1aaf1930-db1e-4df8-b75b-bdcd27b4f9c2').
narrative_ontology:cs_kernel_codification('1aaf1930-db1e-4df8-b75b-bdcd27b4f9c2', fixed_text).
narrative_ontology:cs_authority_grounding('1aaf1930-db1e-4df8-b75b-bdcd27b4f9c2', lineage).
narrative_ontology:cs_interpretation_layer_present('1aaf1930-db1e-4df8-b75b-bdcd27b4f9c2').
narrative_ontology:cs_reading_relation('1aaf1930-db1e-4df8-b75b-bdcd27b4f9c2', federation_membership_kernel__integration_reading, influences).
narrative_ontology:cs_reading_relation('1aaf1930-db1e-4df8-b75b-bdcd27b4f9c2', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('1aaf1930-db1e-4df8-b75b-bdcd27b4f9c2', foundational, welfare_diversity_compatible_with_mobility).
narrative_ontology:cs_axiom_status(welfare_diversity_compatible_with_mobility, holdable).
narrative_ontology:cs_axiom_grounding('1aaf1930-db1e-4df8-b75b-bdcd27b4f9c2', welfare_diversity_compatible_with_mobility, instrumental).
narrative_ontology:cs_axiom('1aaf1930-db1e-4df8-b75b-bdcd27b4f9c2', secondary, anti_dumping_enforcement_protects_floors).
narrative_ontology:cs_axiom_status(anti_dumping_enforcement_protects_floors, holdable).
narrative_ontology:cs_axiom_grounding('1aaf1930-db1e-4df8-b75b-bdcd27b4f9c2', anti_dumping_enforcement_protects_floors, empirically_contingent).
narrative_ontology:cs_reference_frame('1aaf1930-db1e-4df8-b75b-bdcd27b4f9c2', diverse_welfare_coordination_model).
narrative_ontology:cs_drift_state('1aaf1930-db1e-4df8-b75b-bdcd27b4f9c2', contemporary_post_2018_revision, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1aaf1930-db1e-4df8-b75b-bdcd27b4f9c2', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, receiving_state_employers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, labor_brokers_posting_intermediaries).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, mobile_service_companies).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, permanent_migrant_laborers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, receiving_state_native_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_state_public_treasuries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces Posted Workers Directive (revised 2018), monitors anti-dumping compliance, mediates member-state disputes over posting flows. Sets the 24-month exemption threshold and equal-wage requirements. Does not itself collect extraction or bear costs — it administers the coordination mechanism.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_commission_labor_directorate, agenda_setter,
    institutional, generational, analytical, continental).

% Gain access to labor during posting periods at lower effective cost than equivalent native hiring. The equal-wage rule (post-2018) closes the direct wage gap, but structural savings remain: no hiring/training investment, reduced permanent employment commitments, rotation allows continuous access to low-skill labor. Lobby to maintain or expand posting exemptions.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_state_employers, beneficiary,
    organized, biographical, constrained, national).

% Profit by managing posting pipelines: recruiting in sending states, managing compliance with equal-wage rules, placing workers in receiving states. Operate across borders, can shift operations if one jurisdiction tightens rules. Extract rents from labor-supply-chain intermediation.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, labor_brokers_posting_intermediaries, beneficiary,
    moderate, biographical, mobile, continental).

% Contracted for 12–24-month temporary work in receiving states. Earn receiving-state wage floors (equal-wage rule applies) but contribute to sending-state welfare systems while abroad without accessing receiving-state benefits. Face precarious contracts, housing instability, isolation from family. Cannot organize across borders. Contracts often include penalties for early exit or assignment refusal.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_workers, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__welfare_coordination_reading, posted_workers, excluded).

% Seeking permanent residence and integration in receiving states. Face wage competition from rotating posted workers concentrated in low-skill sectors. Have access to receiving-state welfare but subject to residency and employment restrictions (e.g., cannot access non-contributory benefits immediately). Stuck between posted-worker displacement and welfare-access gatekeeping.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, permanent_migrant_laborers, payer,
    powerless, biographical, constrained, national).

% Experience wage suppression in low-skill sectors (construction, agriculture, care work) where posted workers concentrate. Employer rotation maintains a continuous flow of temporary labor rather than investing in domestic wage increases or training. Unions organize nationally but cannot easily cross-border organize to match posting flows.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_state_native_workers, payer,
    moderate, biographical, constrained, national).

% Lose workers (often skilled ones) to temporary posting and permanent migration without receiving fiscal compensation from receiving states. Maintain the fiction of temporary absence for tax and welfare purposes, but lose tax base and returns on education investment. Cannot unilaterally restrict outflow without violating EU free-movement law. Aging populations left behind increase public-care costs while tax base shrinks.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_state_governments, payer,
    institutional, generational, identity_locked, national).

% Interpret free movement rights and the scope of anti-dumping enforcement. Recent rulings (Dano, 2014; Alimanovic, 2015; Posted Workers Directive revision, 2018) split between expansionist equal-treatment readings and member-state welfare-protection readings. The welfare-coordination framework is the jurisprudential equilibrium point — neither fully expanding equal treatment nor fully protecting member-state labor markets.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, ecj_and_supranational_courts, observer,
    institutional, generational, analytical, continental).

% Advocate for cross-border labor protections, portable welfare, and stronger posted-worker rights. Largely absent from EU legislative process — testimony enters via European Parliament committees and shadow reports but does not shape enforcement priorities set by the Commission. Would reshape the constraint if seated.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, civil_society_labor_advocates, excluded,
    powerless, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__welfare_coordination_reading, labor_brokers_posting_intermediaries).
narrative_ontology:fixing_cost_class(federation_membership_kernel__welfare_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables labor mobility across large wage-gap states without requiring supranational welfare harmonization or full permanent-residency infrastructure. Coordinates via temporary-status contracts, anti-dumping wage rules, and rotation mechanisms that allow receiving states to manage welfare-cost risk while sending states access labor-export revenue and labor-market relief.
% TRANSFER_FUNCTION: Moves labor from lower-wage sending states to higher-wage receiving states on fixed-term rotations. Transfers extraction: employers and brokers gain labor-cost efficiency and pipeline rents; posted workers lose welfare access during assignment and face precarity; receiving-state workers face wage competition in low-skill sectors; sending states lose tax revenue and public-investment returns without compensation.
% ABSENT_VOICES: Posted workers themselves cannot meaningfully bargain collectively across borders and have no seat at EU-level rule-making. Permanent migrant laborers (including non-EU migrants) are outside the free-movement system but affected by posting-induced wage pressure; their concerns are not structured into the framework. Civil-society advocates for labor rights and portable welfare are largely excluded from Commission-level enforcement decisions.
% DISAPPEARANCE_RATIONALE: If the welfare-coordination framework and its enforcement machinery disappeared, EU member states would face a hard choice: either harmonize welfare systems upward (politically infeasible, fiscally expensive), restrict free movement (contradicting treaty commitments), or allow a race to the bottom in welfare and labor standards. The constraint's disappearance would force a fundamental restructuring — either deeper integration (harmonization) or re-nationalization of welfare and labor policy.
% FOUNDING_PROBLEM: Single-market integration without political union created a structural friction: labor can move freely but welfare systems and labor law remain national. Early free movement (1970s–1990s) involved minimal wage gaps and assumed gradual harmonization. Eastern enlargement (2004) created sudden, large wage gaps (Poland-Germany ratio ~1:3) and explosive posting flows, undercutting receiving-state wages and threatening fiscal sustainability of welfare systems.
% FOUNDING_PROBLEM_CORROBORATION: Receiving states (Germany, France, Austria, Sweden) and labor movements document persistent posting-sector wage suppression and permanent-migrant displacement post-enlargement. ILO and OECD reports confirm the wage-gap and posting-flow mechanisms remain active. Sending states and labor brokers argue the problem is exaggerated and the system generates benefits (remittances, labor relief) worth the costs. The ECJ rulings have not resolved the contest — successive decisions have refined the framework (equal-wage rule, welfare-access limits) without settling whether the founding problem is solved.
narrative_ontology:disappearance_verdict(federation_membership_kernel__welfare_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__welfare_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).

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
 *   Extractiveness is high (0.68) because the posting mechanism systematically moves value from workers, sending states, and receiving-state labor to employers and brokers. The equal-wage rule post-2018 closes the wage gap but does NOT address the deeper extraction: posted workers still contribute to sending-state welfare while abroad (no access to receiving-state benefits), employers save on hiring/training costs, and brokers profit from rotation. Suppression is moderate-high (0.55) because the constraint's persistence depends on active enforcement (member states must police posting contracts, EU monitors compliance, rotation must be managed) and on the isolation of posted workers (they cannot easily organize across borders or exit contracts). Theater is moderate (0.42) and rising: the equal-wage rule is genuine, but enforcement increasingly focuses on paperwork compliance rather than real protection — theaters of inspection and audit substituting for structural power shifts. Accessibility collapse is moderate (0.62): alternatives exist (permanent migration, higher-wage employment) but are constrained by legal residence rules, welfare-access conditions, and labor-market barriers; the posting mechanism persists because it solves a real coordination problem that harmonization or free permanent migration would also solve, but at higher fiscal cost to receiving states. Resistance is high (0.71): unions, receiving-state labor movements, permanent-migrant advocates, and some member states (Sweden, Austria) resist the posting regime; this resistance does not collapse the system because the beneficiaries (employers, brokers, EU integration ideology) have institutional power and the framework is embedded in treaty law.
 *
 * PERSPECTIVAL GAP:
 *   The welfare-coordination reading assumes member-state welfare autonomy is compatible with free movement enforcement. But the autonomy is asymmetric: receiving states can exclude economically inactive migrants (Dano ruling, 2014) to protect welfare, while sending states cannot restrict outflow. The coordination works because receiving states accept the fiscal load of supplementary welfare for permanent migrants while capping it via eligibility restrictions — but this shifts cost downstream to local labor markets and municipal welfare systems. Sending states coordinate into free movement but absorb the fiscal loss. The framework appears neutral (anti-dumping rules equally applied) but operates asymmetrically because wage gaps persist and posting flows are unidirectional (poorer to richer states). This structural asymmetry is what makes it tangled_rope rather than rope: the coordination function is real (solves labor mobility without harmonization) but the extraction (from posted workers and sending states) is also real and depends on asymmetric enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving-state employers: beneficiary, powerful, arbitrage (can relocate, can hire native or other EU labor), d ≈ 0.15–0.25 (low end of beneficiary range). Labor brokers: beneficiary, moderate power, mobile (can operate across sending/receiving states), d ≈ 0.20–0.30. Posted workers: target, powerless, trapped (contracts lock them in, no cross-border organizing, exit means returning to lower-wage sending state), d ≈ 0.85–0.95 (high end of target range). Permanent migrant laborers: target-adjacent, powerless-to-moderate, constrained (have residence rights but labor-market barriers remain), d ≈ 0.70–0.80. Receiving-state native workers: moderate power, constrained exit (can re-skill but sector-specific and time-limited), d ≈ 0.60–0.70. Sending-state governments: institutional power but constrained by free-movement treaty obligation, identity-locked (cannot unilaterally restrict labor export without breaching EU law), d ≈ 0.75–0.85. The EU Commission sits at the mechanism level — it is the enforcer/agenda-setter, not a target or beneficiary, so d is not applicably computed for the institutional actor setting the rules.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (free movement + wage gaps + welfare diversity = undercutting pressure) was live in 2003 and remains contested. The welfare-coordination reading asserts that the Posted Workers Directive (revised 2018) solves it via anti-dumping rules + member-state autonomy. But the measurement series shows extractiveness continuing to rise post-2018 revision (from 0.62 in 2015 to 0.68 in 2025) and theater rising (equal-wage compliance verified but protection remains weak). This suggests the mandate has partially atrophied: the revision closed the wage-gap vector but did not address welfare-access exclusion (posted workers still excluded from receiving-state benefits) or the rotation-structure extraction. Mandatrophy is NOT declared because the founding problem remains live and the mechanism has adapted (equal-wage rule is a real response). But the rising theater and stable suppression suggest the constraint is shifting from coordination with collateral extraction toward theater (paperwork-driven equal-wage verification) masking persistent structural extraction (welfare exclusion, rotation precarity). This is a trajectory toward piton, not a completed mandatrophy — the mechanism still solves the coordination problem, but an increasing share of enforcement activity is performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    posting_rotation_as_extraction_or_efficiency,
    'Is the 12–24-month posting rotation a mechanism for managing labor mobility and skill transfer, or a mechanism for perpetuating wage gaps and worker precarity?',
    'Longitudinal study of posted workers'' career trajectories post-rotation (do they integrate into receiving-state labor markets, return to sending states, or re-enter rotation?). Cost-benefit analysis: compare posting-induced wage suppression in receiving states against productivity gains and labor-shortage mitigation.',
    'If rotation is primarily extractive (workers cycle out, wages stay suppressed, no integration), the constraint reclassifies toward snare. If rotation enables genuine skill transfer and medium-term labor-market matching, it retains tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(posting_rotation_as_extraction_or_efficiency, empirical, 'Whether posting rotation solves labor mobility or perpetuates structural exploitation.').

omega_variable(
    welfare_exclusion_as_structural_extraction,
    'The equal-wage rule (post-2018) closes the wage-gap but posted workers remain excluded from receiving-state welfare benefits during posting periods. Is this exclusion a natural feature of temporary work status or a mechanism to suppress labor-cost pressure on welfare systems?',
    'Counterfactual: model scenario where posted workers have ACCESS to receiving-state welfare (prorated or temporary). Compare labor-flow and wage effects. Examine member-state policy choices: do states that allow welfare access see different posting volumes or wage pressure?',
    'If exclusion is structural (states deliberately block welfare access to keep posting cheap), it amplifies extraction and strengthens snare classification. If exclusion is incidental (temporary status naturally implies limited welfare), the extraction is lower and tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_exclusion_as_structural_extraction, empirical, 'Whether welfare exclusion is structural anti-dumping or incidental to temporariness.').

omega_variable(
    sending_state_brain_drain_and_fiscal_loss,
    'Do sending states benefit from posting (remittances, skill transfer via return migration, labor-market relief) or suffer net fiscal and human-capital loss?',
    'Fiscal accounting: remittances received vs. tax and public-investment loss. Longitudinal tracking of returned migrants'' employment and earnings. Survey: do returning posted workers bring skills and capital, or are they selected for re-export?',
    'If sending states benefit net, the constraint retains rope-like properties for sending-state governments (coordination with asymmetric benefit). If sending states suffer net loss, they are victims and the extraction is amplified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sending_state_brain_drain_and_fiscal_loss, empirical, 'Whether sending states realize net benefit from labor export or face brain-drain costs.').

omega_variable(
    kernel_reading_contest_welfare_vs_integration,
    'Is the welfare-coordination reading (this constraint) a stable third way between integration and sovereignty readings, or a unstable compromise destined to collapse toward one of the siblings?',
    'Historical tracking: does the framework remain the stable equilibrium across ECJ rulings, member-state policy, and legislative updates? Or do successive crises (migrant housing, wage disputes, welfare-system strain) force the framework toward either expanding posted-worker rights (integration direction) or member-state restrictions (sovereignty direction)?',
    'If the reading is stable, it is a viable constraint with its own ε. If it is unstable, it functions as a transition state and the true classification is one of the siblings. The measurement series shows theater rising (compliance machinery increasing) which could indicate either stabilization (enforcement infrastructure strengthening) or atrophy (theater substituting for real function).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_welfare_vs_integration, conceptual, 'Whether welfare-coordination reading is a stable equilibrium or a transitional compromise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1995, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement_basis(fede_tr_t1995, observed).
narrative_ontology:measurement(fede_tr_t2003, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2003, 0.2).
narrative_ontology:measurement_basis(fede_tr_t2003, observed).
narrative_ontology:measurement(fede_tr_t2009, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2009, 0.28).
narrative_ontology:measurement_basis(fede_tr_t2009, observed).
narrative_ontology:measurement(fede_tr_t2015, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement_basis(fede_tr_t2015, observed).
narrative_ontology:measurement(fede_tr_t2020, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement_basis(fede_tr_t2020, observed).
narrative_ontology:measurement(fede_tr_t2025, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(fede_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t1995, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement_basis(fede_be_t1995, observed).
narrative_ontology:measurement(fede_be_t2003, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2003, 0.42).
narrative_ontology:measurement_basis(fede_be_t2003, observed).
narrative_ontology:measurement(fede_be_t2009, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2009, 0.55).
narrative_ontology:measurement_basis(fede_be_t2009, observed).
narrative_ontology:measurement(fede_be_t2015, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement_basis(fede_be_t2015, observed).
narrative_ontology:measurement(fede_be_t2020, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement_basis(fede_be_t2020, observed).
narrative_ontology:measurement(fede_be_t2025, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(fede_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1995, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 1995, 0.25).
narrative_ontology:measurement_basis(fede_su_t1995, observed).
narrative_ontology:measurement(fede_su_t2003, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2003, 0.38).
narrative_ontology:measurement_basis(fede_su_t2003, observed).
narrative_ontology:measurement(fede_su_t2009, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2009, 0.48).
narrative_ontology:measurement_basis(fede_su_t2009, observed).
narrative_ontology:measurement(fede_su_t2015, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2015, 0.53).
narrative_ontology:measurement_basis(fede_su_t2015, observed).
narrative_ontology:measurement(fede_su_t2020, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement_basis(fede_su_t2020, observed).
narrative_ontology:measurement(fede_su_t2025, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2025, 0.55).
narrative_ontology:measurement_basis(fede_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__welfare_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__welfare_coordination_reading, 0.18).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, posted_workers_directive_enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, sending_state_fiscal_loss_from_labor_export).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, receiving_state_labor_market_wage_pressure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the federation_membership_kernel. The three readings (integration, welfare_coordination, member_sovereignty) decompose a single kernel (free movement in the EU) into structurally distinct constraints with different ε values. They are not three angles on the same constraint; they are three incompatible claims about what free movement IS. Welfare_coordination (this story) treats it as coordination via diverse welfare systems under anti-dumping enforcement; integration treats it as a rights claim; sovereignty treats it as subject to member-state labor-market protection. Each has its own beneficiary/victim structure and extraction profile. Link all three files via network.affects_constraints to enable contention detection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_kernel__welfare_coordination_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
