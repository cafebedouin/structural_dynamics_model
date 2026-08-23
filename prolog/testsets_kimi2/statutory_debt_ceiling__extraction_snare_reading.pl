% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__extraction_snare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: statutory_debt_ceiling__extraction_snare_reading
 *   human_readable: Statutory Debt Ceiling: Extraction Snare Reading
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   The statutory debt ceiling is a formalized aggregate limit on federal
 *   borrowing, originally enacted to streamline Treasury operations. This
 *   constraint story instantiates the extraction_snare_reading of the
 *   statutory_debt_ceiling kernel: the ceiling operates not as a live
 *   coordination device but as a weaponized boundary that legislative
 *   minority factions deploy to extract policy concessions under threat of
 *   sovereign default. The sibling readingsâcoordination_scaffold_reading
 *   (procedural facilitation) and constitutional_nullity_reading (14th
 *   Amendment supersession)âare structurally distinct constraints linked
 *   through network.affects_constraints. The extraction reading authors high
 *   extractiveness (0.82) and suppression (0.79) because the constraint's
 *   persistence depends on actively maintaining the threat of catastrophic
 *   default and suppressing alternative fiscal coordination mechanisms; the
 *   theater ratio (0.55) reflects the performative brinkmanship that
 *   substitutes for genuine fiscal deliberation.
 *
 * KEY AGENTS:
 *   - legislative_minority_factions: Primary beneficiary (organized/mobile) â extracts policy concessions by threatening to withhold ceiling-increase votes
 *   - majority_party_leadership: Primary target (institutional/constrained) â must choose between default and concession
 *   - program_dependents: Diffuse target (powerless/trapped) â bear benefit cuts from extracted concessions
 *   - treasury_operations: Operational target (institutional/constrained) â manages extraordinary measures and default risk
 *   - federal_bond_market: Financial target (powerful/constrained) â absorbs credit downgrade and volatility
 *   - taxpayers: Diffuse target (moderate/constrained) â bear higher borrowing costs and austerity
 *   - credit_rating_agencies: Analytical observer (institutional/analytical) â documents systemic risk without preventing extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, 0.82).
domain_priors:suppression_score(statutory_debt_ceiling__extraction_snare_reading, 0.79).
domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__extraction_snare_reading, snare).
narrative_ontology:human_readable(statutory_debt_ceiling__extraction_snare_reading, "Statutory Debt Ceiling: Extraction Snare Reading").
narrative_ontology:topic_domain(statutory_debt_ceiling__extraction_snare_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__extraction_snare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__extraction_snare_reading, '2d2bb8c8-55ba-458a-9c1a-d8165df53332').
narrative_ontology:cs_kernel_codification('2d2bb8c8-55ba-458a-9c1a-d8165df53332', formalized).
narrative_ontology:cs_authority_grounding('2d2bb8c8-55ba-458a-9c1a-d8165df53332', extraction).
narrative_ontology:cs_interpretation_layer_present('2d2bb8c8-55ba-458a-9c1a-d8165df53332').
narrative_ontology:cs_reading_relation('2d2bb8c8-55ba-458a-9c1a-d8165df53332', statutory_debt_ceiling__coordination_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('2d2bb8c8-55ba-458a-9c1a-d8165df53332', statutory_debt_ceiling__constitutional_nullity_reading, coexists_with).
narrative_ontology:cs_axiom('2d2bb8c8-55ba-458a-9c1a-d8165df53332', foundational, statutory_ceiling_creates_exploitable_hostage_structure).
narrative_ontology:cs_axiom_status(statutory_ceiling_creates_exploitable_hostage_structure, holdable).
narrative_ontology:cs_axiom_grounding('2d2bb8c8-55ba-458a-9c1a-d8165df53332', statutory_ceiling_creates_exploitable_hostage_structure, empirically_contingent).
narrative_ontology:cs_axiom('2d2bb8c8-55ba-458a-9c1a-d8165df53332', foundational, default_threat_is_primary_operative_logic).
narrative_ontology:cs_axiom_status(default_threat_is_primary_operative_logic, holdable).
narrative_ontology:cs_axiom_grounding('2d2bb8c8-55ba-458a-9c1a-d8165df53332', default_threat_is_primary_operative_logic, empirically_contingent).
narrative_ontology:cs_reference_frame('2d2bb8c8-55ba-458a-9c1a-d8165df53332', exceptional_default_threat_mechanism).
narrative_ontology:cs_drift_state('2d2bb8c8-55ba-458a-9c1a-d8165df53332', post_2011_standoff_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('2d2bb8c8-55ba-458a-9c1a-d8165df53332', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_factions).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, program_dependents).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, treasury_operations).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, taxpayers).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_bond_market).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, majority_party_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Withhold votes needed to raise the debt ceiling unless policy concessions are granted by the majority coalition. They gain legislative leverage and policy wins from the default threat that the statutory ceiling creates. Their exit is simpleâthey could support a clean increaseâbut doing so would surrender their leverage.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_factions, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_factions, agenda_setter).

% Depend on federal programs that become bargaining chips during debt ceiling standoffs; face benefit delays, cuts, or uncertainty when minority factions use the ceiling to secure spending concessions. They cannot easily exit the federal programs they rely on for subsistence.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, program_dependents, payer,
    powerless, immediate, trapped, national).

% Must implement extraordinary measures and payment prioritization when the ceiling is threatened, bearing operational risk and legal uncertainty. If the limit is reached, they manage the operational fallout of disrupted cash flows. Exit is constrained by statutory obligation and the lack of a clear legal alternative to borrowing.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, treasury_operations, payer,
    institutional, immediate, constrained, national).

% Bear increased federal borrowing costs after credit rating downgrades triggered by ceiling brinkmanship, and face future austerity or tax increases resulting from concessions made during standoffs. They cannot opt out of the tax system or the sovereign debt market.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Holders of US sovereign debt face price volatility, rating downgrades, and default risk premium spikes during ceiling standoffs; the global safe-asset status of Treasuries is periodically leveraged to secure domestic policy concessions. Exiting the market entirely would cause systemic disruption to their own portfolios and to global finance.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_bond_market, payer,
    powerful, immediate, constrained, global).

% Must choose between allowing sovereign default or conceding policy priorities to minority factions; the ceiling turns routine fiscal management into periodic high-stakes negotiation. Their exit is constrained by the statutory requirement and by the political impossibility of allowing default.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, majority_party_leadership, payer,
    institutional, biographical, constrained, national).

% Assess and downgrade US sovereign credit during standoffs, documenting the fiscal risk but lacking authority to prevent the political brinkmanship. They observe and rate, neither paying nor benefiting from the constraint.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, credit_rating_agencies, observer,
    institutional, immediate, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_factions).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__extraction_snare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The surface claim is that the ceiling coordinates aggregate borrowing authority with congressional fiscal oversight; under this reading, no genuine coordination problem is solved by the arrangement in its current operation. The procedural coordination of Treasury borrowing is already handled by appropriations and revenue measures, while the ceiling itself functions as a bargaining chip rather than a fiscal coordination device.
% TRANSFER_FUNCTION: Moves policy concessions, spending cuts, and regulatory changes from the majority coalition and program dependents to legislative minority factions under threat of sovereign default, credit downgrade, and Treasury operational disruption.
% ABSENT_VOICES: Future taxpayers and non-citizen holders of US debt are not represented in ceiling negotiations. Beneficiaries of programs that are cut in extracted concessions have no seat at the bargaining table. The original legislative architects who conceived the ceiling as a procedural convenience are absent from the contemporary discourse, which has reconstituted the mechanism as a weapon.
% DISAPPEARANCE_RATIONALE: If the statutory debt ceiling disappeared, the minority faction's primary leverage point for extracting policy concessions would vanish; fiscal negotiations would revert to the normal appropriations and revenue process without the periodic threat of catastrophic default. Treasury operations would normalize without extraordinary measures, and the sovereign debt market would lose a recurring source of artificial volatility.
% FOUNDING_PROBLEM: Originally created to streamline Treasury borrowing authority by replacing discrete bond-issue authorizations with a single aggregate limit, reducing congressional micromanagement of individual debt instruments during World War I and the New Deal.
% FOUNDING_PROBLEM_CORROBORATION: The original 1917 and 1939 statutory frameworks were procedural innovations to facilitate war and recovery financing. Contemporary fiscal historians and budget scholars (outside the benefiting minority factions) attest that the aggregate limit long ago lost its operational coordination function with the rise of modern appropriations and revenue procedures; the Government Accountability Office and Congressional Research Service document that the ceiling does not constrain spending decisions, which are made separately through appropriations.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__extraction_snare_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__extraction_snare_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__extraction_snare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statutory_debt_ceiling__extraction_snare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__extraction_snare_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored at 0.82 because the ceiling systematically transfers policy control from majority coalitions to minority factions without corresponding compensation; suppression is 0.79 because the constraint's power depends on legally and politically suppressing executive bypass options (14th Amendment, platinum coin, clean repeal) and enforcing the norm that default is unthinkable. Theater ratio is 0.55 because a substantial share of ceiling activity is performative brinkmanship designed to force concessions rather than to decide fiscal policy on merits. Accessibility collapse is 0.68: theoretical alternatives exist but are institutionally blocked by statutory text and political taboo. Resistance is 0.45 because the targeted majority resists concessions but repeatedly capitulates to default threats, limiting sustained opposition.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seat (minority factions) experiences the constraint as a legitimate source of bargaining leverage within constitutional separation of powers; the payer seats (majority leadership, program dependents, bond market) experience it as coercive extraction backed by catastrophic risk. The engine computes this divergence from the structural data: same constraint, opposite directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority factions are declared beneficiaries (d near 0.0) because they capture the extracted policy concessions and leverage. Majority party leadership, program dependents, treasury operations, federal bond market, and taxpayers are declared victims/payers (d near 1.0) because they bear the costs of concessions, operational disruption, credit downgrades, and higher borrowing costs. The asymmetry is structural: the minority gains policy wins without bearing default risk; the diffuse public and institutional operators bear the risk without gaining leverage.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as snare (rather than tangled rope) is warranted because the coordination storyâaggregate borrowing oversightâis not the operative function under this reading. The constraint does not solve a live collective-action problem that would revert to chaos if removed; appropriations and revenue bills already coordinate fiscal policy. The ceiling adds no coordination value not achievable by the existing budget process. The mandatrophy check confirms this is not a degraded coordination mechanism (piton) because the extraction is active, concentrated, and intentional, not inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is the debt ceiling best understood as a live coordination mechanism, a constitutionally nullified relic, or an active extraction device?',
    'Comparative institutional analysis tracking whether ceiling standoffs produce net coordination benefits, null outcomes, or asymmetric policy transfers; constitutional adjudication on the 14th Amendment theory.',
    'If the coordination_scaffold reading is correct, classification shifts toward rope/scaffold; if constitutional_nullity is correct, the constraint dissolves; if this extraction reading holds, snare classification persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural ambiguity between coordination, nullity, and extraction readings of the same statutory kernel.').

omega_variable(
    minority_beneficiary_concentration,
    'Do the extracted policy concessions systematically accrue to the legislative minority factions, or do they diffuse across the chamber?',
    'Roll-call analysis and policy outcome tracing in post-ceiling negotiation packages.',
    'If gains are concentrated in the threatening minority, the extraction is targeted and directional; if diffuse, the constraint operates more like general fiscal chaos.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_beneficiary_concentration, empirical, 'Whether extraction concentrates in minority factions or diffuses.').

omega_variable(
    executive_override_alternative,
    'Would executive invocation of the 14th Amendment to bypass the ceiling constitute a viable exit option or merely trigger a constitutional crisis?',
    'Observed executive behavior during future standoffs; judicial review if invoked.',
    'If viable, accessibility_collapse is lower than measured and the snare weakens; if not, the suppression metric is validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(executive_override_alternative, empirical, 'Viability of constitutional bypass as an exit from the extraction dynamic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__extraction_snare_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sdcesr_tr_t0, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sdcesr_tr_t4, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 4, 0.4).
narrative_ontology:measurement(sdcesr_tr_t8, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 8, 0.5).
narrative_ontology:measurement(sdcesr_tr_t12, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 12, 0.55).
narrative_ontology:measurement(sdcesr_tr_t16, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 16, 0.6).
narrative_ontology:measurement(sdcesr_tr_t20, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(sdcesr_tr_t24, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 24, 0.55).

% Extraction over time
narrative_ontology:measurement(sdcesr_be_t0, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sdcesr_be_t4, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(sdcesr_be_t8, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(sdcesr_be_t12, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(sdcesr_be_t16, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 16, 0.78).
narrative_ontology:measurement(sdcesr_be_t20, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(sdcesr_be_t24, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 24, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(sdcesr_su_t0, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sdcesr_su_t4, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(sdcesr_su_t8, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(sdcesr_su_t12, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(sdcesr_su_t16, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 16, 0.78).
narrative_ontology:measurement(sdcesr_su_t20, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(sdcesr_su_t24, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 24, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, constitutional_nullity_reading).

% DUAL FORMULATION NOTE:
% The statutory debt ceiling label conflates three structurally distinct constraints: a procedural coordination mechanism (scaffold), a constitutionally superseded limit (nullity), and an active extraction device (snare). Each reading has distinct epsilon values, beneficiary/victim structures, and empirical signatures. They are linked as a constraint family because they compete to explain the same statutory text's operative force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
