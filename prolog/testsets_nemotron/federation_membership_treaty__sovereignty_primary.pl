% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__sovereignty_primary, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Federation Membership Treaty: Sovereignty-Primary Reading of Free Movement
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint story captures the sovereignty-primary reading of the
 *   federation membership treaty's free movement provisions. The treaty
 *   establishes a federal union with nominal free movement of persons, but
 *   this reading holds that movement is conditional on member state consent —
 *   each state retains authority to restrict inflow to protect its national
 *   labor market and welfare system. The constraint operates as a
 *   coordination mechanism among states (mutual recognition of labor market
 *   sovereignty) while simultaneously extracting from mobile workers who face
 *   restricted access, delayed rights recognition, and contributory
 *   exclusion. The coordination function is real: states avoid a
 *   race-to-the-bottom in labor standards and welfare generosity by
 *   preserving national policy space. The extraction function is also real:
 *   mobile workers pay into systems they cannot fully access, accept wage
 *   discounts in protected sectors, and bear the cost of regulatory
 *   fragmentation. The engine will compute per-seat classifications from the
 *   structural data below.
 *
 * KEY AGENTS:
 *   - member_state_governments: Primary agenda_setter (institutional/arbitrage) — sets restriction policy, controls welfare access rules
 *   - national_labor_unions: Beneficiary (organized/constrained) — protects domestic wage floors and sectoral agreements from competitive pressure
 *   - domestic_welfare_administrators: Beneficiary (institutional/mobile) — preserves fiscal sustainability of national systems by controlling access
 *   - mobile_workers: Primary payer (moderate/constrained) — bears restricted access, delayed portability, sectoral exclusion
 *   - cross_border_commuters: Payer (moderate/constrained) — faces daily regulatory friction, double taxation risk, social security gaps
 *   - migrant_families: Payer (powerless/trapped) — experiences compounded exclusion from housing, education, healthcare systems
 *   - federal_commission: Observer (institutional/analytical) — monitors treaty compliance, initiates infringement proceedings, proposes harmonization
 *   - employers_in_shortage_sectors: Excluded (powerful/constrained) — would benefit from open access but lacks voice in restriction decisions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.68).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.72).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Federation Membership Treaty: Sovereignty-Primary Reading of Free Movement").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, '281e39ec-95c9-4c6b-8810-1191f09b4492').
narrative_ontology:cs_kernel_codification('281e39ec-95c9-4c6b-8810-1191f09b4492', formalized).
narrative_ontology:cs_authority_grounding('281e39ec-95c9-4c6b-8810-1191f09b4492', lineage).
narrative_ontology:cs_interpretation_layer_present('281e39ec-95c9-4c6b-8810-1191f09b4492').
narrative_ontology:cs_reading_relation('281e39ec-95c9-4c6b-8810-1191f09b4492', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('281e39ec-95c9-4c6b-8810-1191f09b4492', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('281e39ec-95c9-4c6b-8810-1191f09b4492', foundational, national_labor_market_sovereignty_inviolable).
narrative_ontology:cs_axiom_status(national_labor_market_sovereignty_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('281e39ec-95c9-4c6b-8810-1191f09b4492', national_labor_market_sovereignty_inviolable, deontological).
narrative_ontology:cs_axiom('281e39ec-95c9-4c6b-8810-1191f09b4492', foundational, welfare_state_autonomy_precedes_mobility_rights).
narrative_ontology:cs_axiom_status(welfare_state_autonomy_precedes_mobility_rights, holdable).
narrative_ontology:cs_axiom_grounding('281e39ec-95c9-4c6b-8810-1191f09b4492', welfare_state_autonomy_precedes_mobility_rights, conventional).
narrative_ontology:cs_reference_frame('281e39ec-95c9-4c6b-8810-1191f09b4492', founding_treaty_sovereignty_equilibrium).
narrative_ontology:cs_drift_state('281e39ec-95c9-4c6b-8810-1191f09b4492', contemporary_enlargement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('281e39ec-95c9-4c6b-8810-1191f09b4492', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, member_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_labor_unions).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, domestic_welfare_administrators).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, mobile_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, cross_border_commuters).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, migrant_families).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, national_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, labor_market_protection_principle).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, welfare_state_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Each member state government controls its labor market restriction policy, welfare access rules, and professional recognition procedures. They negotiate mutual recognition agreements bilaterally and invoke treaty safeguard clauses unilaterally. They collect the policy autonomy rent: the ability to set wages, benefits, and standards without federal harmonization pressure. Exit is arbitrage-grade: they can threaten treaty opt-out or non-compliance with minimal domestic cost.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, member_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Domestic unions in protected sectors (construction, public services, regulated professions) gain wage floors and bargaining leverage from restricted labor supply. They lobby for restriction invocation and defend sectoral agreements against liberalization. Their exit is constrained: organized at national level, they cannot easily shift to federal-level bargaining without losing institutional recognition and membership base.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_labor_unions, beneficiary,
    organized, biographical, constrained, national).

% National social security agencies, health insurance funds, and employment services preserve fiscal control by managing access rules for mobile workers: waiting periods, contribution portability limits, benefit export restrictions. They coordinate through federal technical committees but retain decision authority. Exit is mobile: they could harmonize systems but choose not to, preserving national administrative autonomy.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, domestic_welfare_administrators, beneficiary,
    institutional, generational, mobile, national).

% Workers who move across member state borders for employment face: restricted access to regulated professions (recognition delays of 12-36 months), sectoral employment bans in protected occupations, welfare waiting periods (3-5 years for full access), and wage discounts in exposed sectors (15-30% below domestic equivalents). They contribute to social systems immediately but access benefits delayed. Exit is constrained: moving to another member state triggers a new restriction cycle; returning home means career disruption.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, mobile_workers, payer,
    moderate, biographical, constrained, continental).

% Workers living in one member state and employed in an adjacent state face daily regulatory friction: double social security liability risk, tax coordination gaps, healthcare access fragmentation, and pension accrual discontinuities. They are geographically trapped — their housing, family, and community ties fix them in the cross-border zone. Exit is constrained: changing employer or residence breaks the commuting arrangement; full relocation to the employment state triggers new restriction cycles.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, cross_border_commuters, payer,
    moderate, biographical, constrained, regional).

% Families with mixed migration statuses (one spouse mobile worker, children born in host state, elderly dependents) experience compounded exclusion: children face education tracking into vocational streams, spouses face labor market access barriers, elderly dependents face healthcare access denials. Their identity is fused to the host society — children are educated there, families put down roots — making exit identity-locked rather than merely constrained. They bear the intergenerational cost of the restriction regime.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, migrant_families, payer,
    powerless, generational, identity_locked, national).

% The federal executive monitors treaty compliance, initiates infringement proceedings against disproportionate restrictions, and proposes harmonization directives. It takes testimony from all seats, commissions economic analyses, and can impose remedies — but its enforcement is politically constrained by member state governments in the council. It occupies the analytical seat: it sees the full structure but cannot unilaterally change it.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, federal_commission, observer,
    institutional, generational, analytical, continental).

% Employers in healthcare, construction, hospitality, and tech sectors face documented labor shortages but cannot access mobile workers due to professional recognition barriers and sectoral restrictions. They would benefit from open access and lobby for liberalization, but their voice is excluded from the restriction invocation process — which is controlled by member state governments responding to domestic union pressure. Their exit is constrained: they cannot relocate production easily (care work is local, construction is site-bound) and face domestic political costs for advocating foreign labor.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, employers_in_shortage_sectors, excluded,
    powerful, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a race-to-the-bottom in labor standards and welfare generosity among member states by preserving national policy autonomy: each state can maintain higher wages, stronger protections, and more generous welfare without losing its workforce to lower-standard competitors or attracting only net-benefit migrants.
% TRANSFER_FUNCTION: Moves labor market access, welfare benefits, and professional recognition rights from mobile workers/cross-border commuters/migrant families to member state governments/national unions/domestic welfare administrators — mobile workers pay contributions immediately but access benefits delayed; they accept wage discounts in protected sectors; they bear deskilling from recognition delays.
% ABSENT_VOICES: Mobile workers and migrant families are structurally present but politically muted — they lack voting rights in host states, their unions are weak or non-existent, and their advocacy is fragmented. Cross-border commuters have no dedicated representation in any national parliament. Employers in shortage sectors are excluded from the restriction decision process despite bearing documented economic costs. The federal parliament has consultative but not legislative authority over restriction invocations.
% DISAPPEARANCE_RATIONALE: If the sovereignty-primary restriction regime vanished overnight: member states would face immediate competitive pressure to harmonize labor standards downward or raise welfare contributions; mobile workers would gain immediate access to regulated professions and full welfare portability; cross-border commuters would see regulatory friction dissolve; national unions would lose wage protection leverage; welfare administrators would face fiscal pressure from expanded access; federal harmonization directives would accelerate. The federal labor market and welfare architecture would reorganize around either integration_primary or subsidiarity_balance logics.
% FOUNDING_PROBLEM: At federation founding, member states feared that unrestricted free movement would trigger a race-to-the-bottom: capital and high-skill workers would concentrate in low-tax, low-regulation states, while low-skill workers and net-benefit migrants would concentrate in high-welfare states — destroying the fiscal basis of national welfare systems and undermining collectively bargained labor standards.
% FOUNDING_PROBLEM_CORROBORATION: Member state governments and national unions attest the founding problem remains live, citing persistent wage and welfare differentials. The federal commission, independent economic institutes, and mobile worker advocacy organizations attest the problem is substantially mitigated by existing coordination (minimum wage directives, social security coordination regulations, posting of workers rules) and that current restrictions exceed what the founding problem justifies — the arrangement persists as protectionism masked as coordination.
narrative_ontology:disappearance_verdict(federation_membership_treaty__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(federation_membership_treaty__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__sovereignty_primary, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the substantial transfer from mobile workers to protected domestic arrangements: restricted labor market access depresses wages in exposed sectors; welfare contribution/benefit asymmetries persist for 5-10 years; professional qualification recognition delays create deskilling. Suppression (0.72) is high because the constraint actively prevents exit from national regulatory regimes — workers cannot choose a more favorable member state's labor law or welfare system without physical relocation, and even relocation triggers new restriction cycles. Theater ratio (0.28) is moderate: the labor market protection and welfare sustainability justifications are genuine coordination rationales, but a growing share of restriction invocations target sectors with documented shortages rather than genuine displacement risks. Accessibility collapse (0.58) reflects that alternatives exist (irregular migration, remote work, sectoral niche strategies) but are costly and incomplete. Resistance (0.52) is significant: mobile workers litigate, commuters organize, employers lobby — but the constraint's federal treaty foundation gives it deep legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the member state government seat, this is a rope: a genuine coordination mechanism that prevents destructive competition among welfare states and labor markets. From the mobile worker seat, this is a snare: the coordination story is cover for extracting labor and contributions while denying access. From the national union seat, this is a scaffold (they would say rope): temporary protection while domestic systems adjust — but the sunset never arrives. From the federal commission seat, this is a tangled rope: the coordination function is real but the extraction asymmetry has widened beyond what proportionality allows. The engine computes this divergence from the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state governments are structural beneficiaries (d ~ 0.15): they control the restriction regime, collect the policy autonomy rents, and face minimal exit costs (arbitrage-grade exit via treaty opt-outs). National labor unions are beneficiaries (d ~ 0.25): they gain wage protection and bargaining leverage, with constrained exit (organized at national level). Domestic welfare administrators are beneficiaries (d ~ 0.20): they preserve fiscal control, with mobile exit options. Mobile workers are primary targets (d ~ 0.85): they bear the access restrictions, contribution/benefit gaps, and deskilling, with constrained exit (relocation triggers new restrictions). Cross-border commuters are targets (d ~ 0.75): daily regulatory friction, trapped by geography. Migrant families are deep targets (d ~ 0.90): compounded exclusion, identity-locked through settlement and family ties. Employers in shortage sectors are excluded (d ~ 0.70): they would benefit from openness but lack agenda-setting power. Federal commission is observer (d ~ 0.50): symmetric analytical position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing a race-to-the-bottom in labor standards and welfare generosity during federal integration) remains partially live — but the coordination solution has accumulated extraction layers: sectoral restrictions now cover documented shortage occupations; welfare waiting periods exceed integration timelines; qualification recognition delays serve protectionist rather than quality-assurance functions. The constraint has not resolved its mandatrophy; it has drifted from coordination into extraction while retaining the original justification. The declared claimed_type (tangled_rope) captures this hybrid state: genuine coordination function + asymmetric extraction + active enforcement required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'This constraint is one reading (sovereignty_primary) of the contested kernel federation_membership_treaty. What structural changes would the integration_primary or subsidiarity_balance readings produce?',
    'Compare the three readings'' beneficiary/victim sets, enforcement structures, and ε values. The sovereignty_primary reading places local labor markets as beneficiaries and mobile workers as victims; integration_primary would invert this structure. Subsidiarity_balance would produce an intermediate structure with proportionality-based restrictions.',
    'If the sibling readings produce materially different ε values and structural relationships, the kernel label ''free movement'' conflates multiple constraints — the decomposition is warranted. If they converge, the label describes a single constraint with interpretive variance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Kernel decomposition: whether sovereignty_primary, integration_primary, and subsidiarity_balance are structurally distinct constraints').

omega_variable(
    national_consent_mechanism_ambiguity,
    'Does ''member state consent'' operate as a genuine coordination mechanism (states mutually recognize each other''s labor market protections) or as a unilateral veto power (each state blocks mobility that threatens its domestic arrangement)?',
    'Analyze treaty invocation patterns: count mutual recognition agreements vs. unilateral restriction notifications. Track whether restrictions are reciprocated or asymmetrically imposed.',
    'If mutual recognition dominates, the constraint has stronger rope characteristics (coordination). If unilateral veto dominates, extraction is higher and the constraint leans toward snare for mobile workers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_consent_mechanism_ambiguity, empirical, 'Whether the consent mechanism coordinates or extracts').

omega_variable(
    welfare_protection_vs_exclusion_boundary,
    'Where is the boundary between legitimate welfare system protection and exclusionary extraction? At what point does protecting national welfare become extracting from mobile workers who contribute but cannot access?',
    'Longitudinal fiscal incidence analysis: compare mobile workers'' tax/social contributions against benefit access over 5-10 year horizons across member states.',
    'If mobile workers are net contributors with restricted access, the welfare protection framing masks extraction. If they are net beneficiaries, the protection claim has stronger coordination grounding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_protection_vs_exclusion_boundary, empirical, 'Whether welfare protection framing covers net extraction from mobile workers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__sovereignty_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fede_tr_t5, federation_membership_treaty__sovereignty_primary, theater_ratio, 5, 0.15).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__sovereignty_primary, theater_ratio, 10, 0.19).
narrative_ontology:measurement(fede_tr_t15, federation_membership_treaty__sovereignty_primary, theater_ratio, 15, 0.23).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__sovereignty_primary, theater_ratio, 20, 0.26).
narrative_ontology:measurement(fede_tr_t25, federation_membership_treaty__sovereignty_primary, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__sovereignty_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fede_be_t5, federation_membership_treaty__sovereignty_primary, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__sovereignty_primary, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(fede_be_t15, federation_membership_treaty__sovereignty_primary, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__sovereignty_primary, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(fede_be_t25, federation_membership_treaty__sovereignty_primary, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__sovereignty_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fede_su_t5, federation_membership_treaty__sovereignty_primary, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__sovereignty_primary, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(fede_su_t15, federation_membership_treaty__sovereignty_primary, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__sovereignty_primary, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(fede_su_t25, federation_membership_treaty__sovereignty_primary, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__sovereignty_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__sovereignty_primary, 0.18).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__subsidiarity_balance).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federal_labor_law_harmonization).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, cross_border_social_security_coordination).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, professional_qualification_recognition_directive).

% DUAL FORMULATION NOTE:
% This story is one member of the federation_membership_treaty constraint family. The three readings (sovereignty_primary, integration_primary, subsidiarity_balance) decompose the colloquial label 'free movement provisions' into structurally distinct constraints with different ε values, different beneficiary/victim structures, and different enforcement logics. This reading (sovereignty_primary) has the highest ε (0.68) and strongest preservation of national autonomy. The integration_primary reading would have lower ε (~0.25) and mobile workers as beneficiaries. The subsidiarity_balance reading would sit intermediately (~0.45) with proportionality-based restrictions. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__sovereignty_primary, institutional, 0.15).
constraint_indexing:directionality_override(federation_membership_treaty__sovereignty_primary, organized, 0.25).
constraint_indexing:directionality_override(federation_membership_treaty__sovereignty_primary, moderate, 0.8).
constraint_indexing:directionality_override(federation_membership_treaty__sovereignty_primary, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
