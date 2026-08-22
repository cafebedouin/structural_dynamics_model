% ============================================================================
% CONSTRAINT STORY: federation_membership__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__integration_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: federation_membership__integration_reading
 *   human_readable: Federation Membership as Irreversible Integration — Free Movement as Constitutional Right
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the integration_reading of the
 *   federation_membership kernel: membership is framed as irreversible
 *   integration, supranational authority is treated as legitimate, and free
 *   movement is elevated to a constitutional right. The standing arrangement
 *   under contest is a federal union where border controls between members
 *   are prohibited and labor mobility is legally guaranteed. The
 *   integration_reading assesses ε against this standing arrangement — high
 *   extractiveness (0.62) reflects the reading's own structural view that the
 *   arrangement transfers costs from mobile citizens and cross-border
 *   employers onto local labor markets and municipal authorities. The
 *   sovereignty_reading (a sibling constraint) would assess the same standing
 *   arrangement differently, authoring low ε and a different
 *   beneficiary/victim structure. Per Rule 1, this story generates ONLY the
 *   integration_reading as a clean ε-invariant constraint.
 *
 * KEY AGENTS:
 *   - mobile_citizens: Primary beneficiaries (powerful/arbitrage) — exercise free movement rights across internal borders
 *   - supranational_institutions: Agenda setters (institutional/analytical) — adjudicate and enforce the integration mandate
 *   - cross_border_employers: Beneficiaries (organized/mobile) — access expanded labor pool without relocation costs
 *   - local_labor_markets: Primary victims (organized/constrained) — absorb displacement effects without compensation
 *   - non_mobile_workers: Victims (moderate/trapped) — bear wage/employment pressure with limited exit
 *   - municipal_fiscal_authorities: Victims (organized/constrained) — fund integration costs without revenue authority
 *   - national_governments: Dual role (institutional/constrained) — both enforce and resist supranational mandates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__integration_reading, 0.62).
domain_priors:suppression_score(federation_membership__integration_reading, 0.48).
domain_priors:theater_ratio(federation_membership__integration_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership__integration_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(federation_membership__integration_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(federation_membership__integration_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__integration_reading, "Federation Membership as Irreversible Integration — Free Movement as Constitutional Right").
narrative_ontology:topic_domain(federation_membership__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__integration_reading, '4f4b35ed-1c5c-4092-bebd-c3034e4ed98f').
narrative_ontology:cs_kernel_codification('4f4b35ed-1c5c-4092-bebd-c3034e4ed98f', formalized).
narrative_ontology:cs_authority_grounding('4f4b35ed-1c5c-4092-bebd-c3034e4ed98f', lineage).
narrative_ontology:cs_interpretation_layer_present('4f4b35ed-1c5c-4092-bebd-c3034e4ed98f').
narrative_ontology:cs_reading_relation('4f4b35ed-1c5c-4092-bebd-c3034e4ed98f', federation_membership__sovereignty_reading, influences).
narrative_ontology:cs_axiom('4f4b35ed-1c5c-4092-bebd-c3034e4ed98f', foundational, free_movement_as_fundamental_right).
narrative_ontology:cs_axiom_status(free_movement_as_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('4f4b35ed-1c5c-4092-bebd-c3034e4ed98f', free_movement_as_fundamental_right, deontological).
narrative_ontology:cs_axiom('4f4b35ed-1c5c-4092-bebd-c3034e4ed98f', foundational, integration_irreversibility).
narrative_ontology:cs_axiom_status(integration_irreversibility, holdable).
narrative_ontology:cs_axiom_grounding('4f4b35ed-1c5c-4092-bebd-c3034e4ed98f', integration_irreversibility, conventional).
narrative_ontology:cs_reference_frame('4f4b35ed-1c5c-4092-bebd-c3034e4ed98f', postwar_reconstruction_integration).
narrative_ontology:cs_drift_state('4f4b35ed-1c5c-4092-bebd-c3034e4ed98f', contemporary_constitutional_order, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4f4b35ed-1c5c-4092-bebd-c3034e4ed98f', '').
narrative_ontology:cs_kernel_id(federation_membership__integration_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, mobile_citizens).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, supranational_institutions).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, cross_border_employers).
narrative_ontology:constraint_victim(federation_membership__integration_reading, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership__integration_reading, non_mobile_workers).
narrative_ontology:constraint_victim(federation_membership__integration_reading, municipal_fiscal_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership__integration_reading, cross_border_employers).
narrative_ontology:constraint_victim(federation_membership__integration_reading, national_governments).
narrative_ontology:constraint_vindicates(federation_membership__integration_reading, free_movement_as_constitutional_right).
narrative_ontology:constraint_vindicates(federation_membership__integration_reading, supranational_authority_legitimacy).
narrative_ontology:constraint_vindicates(federation_membership__integration_reading, irreversible_integration_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise constitutionally guaranteed free movement across internal borders for work, study, retirement. Capture wage arbitrage and lifestyle gains. Exit is trivial — they can move to where conditions are best. The constraint subsidizes their mobility; they bear no enforcement costs.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, mobile_citizens, beneficiary,
    powerful, biographical, arbitrage, continental).

% Adjudicate and enforce the integration mandate through court rulings, treaty interpretation, and legislative initiatives. Legitimacy derives from the constitutional framing of free movement as a fundamental right. They set the agenda but do not directly extract — their budget comes from member state contributions. Exit is analytical: they observe the structure from outside the distributional conflict.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, supranational_institutions, agenda_setter,
    institutional, generational, analytical, continental).

% Access an integrated labor pool across member states without relocation costs or visa barriers. Benefit from wage arbitrage and labor supply elasticity. Secondarily pay into the system through employer contributions and compliance costs, but net beneficiaries. Exit is mobile: can relocate operations but face sunk costs.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, cross_border_employers, beneficiary,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership__integration_reading, cross_border_employers, payer).

% Absorb displacement effects: wage pressure in tradable sectors, skill mismatch, demographic churn. Organized through unions and local politics but constrained by supranational legal supremacy — cannot restrict entry. Exit is constrained: municipal boundaries are porous, regional coordination is weak, and compensation mechanisms are politically blocked by the constitutional framing.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, local_labor_markets, payer,
    organized, biographical, constrained, regional).

% Bear concentrated wage and employment effects in exposed sectors without the option to move. Skills are location-specific; social networks, housing, and family ties bind them. The constitutional right framing delegitimizes their claims for protection. Exit is trapped: they cannot practically leave and have no voice in the supranational arena.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, non_mobile_workers, payer,
    moderate, biographical, trapped, local).

% Fund integration costs (housing, language training, social services, infrastructure) without corresponding revenue authority — tax base is mobile, expenditure mandates are not. Organized through municipal associations but constrained by national/federal fiscal rules. Exit is constrained: cannot opt out of mandate, cannot raise revenue from mobile base.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, municipal_fiscal_authorities, payer,
    organized, biographical, constrained, local).

% Dual position: formally enforce supranational mandates through domestic law, but bear political costs from displaced constituents. Some governments benefit from outflow (remittances, reduced unemployment); others bear inflow costs. Exit is constrained: treaty withdrawal is legally possible but politically and economically prohibitive. The directionality override (d=0.55) captures this split — neither full beneficiary nor full target.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, national_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__integration_reading, national_governments, payer).

% Political actors and movements that would challenge the constitutional framing of free movement as absolute. Structurally excluded from the supranational adjudication process — their arguments are treated as illegitimate by the integration_reading's own logic. Exit is constrained: they operate within national democracies but the constraint's authority sits above that level.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, sovereignty_advocates, excluded,
    organized, generational, constrained, national).

% Analyze the constraint from outside the distributional conflict. No material stake in the arrangement's persistence or dissolution. Provide the analytical seat for the engine's per-seat classification.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, comparative_federalism_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a single labor market across member states: eliminates transaction costs of cross-border hiring, enables skill matching at continental scale, prevents beggar-thy-neighbor labor policies, and provides automatic adjustment mechanism for asymmetric shocks.
% TRANSFER_FUNCTION: Moves adjustment costs from mobile capital and mobile labor onto immobile labor and local public finances: wage pressure, fiscal strain, and political dislocation flow to non-mobile workers and municipal authorities; wage gains, lifestyle arbitrage, and profit flow to mobile citizens and cross-border employers.
% ABSENT_VOICES: Non-mobile workers in exposed sectors and municipal fiscal authorities would object to the distributional structure if they had standing in the supranational arena. They are present in national politics but excluded from the constitutional adjudication that makes border restriction illegitimate. Their absence is structural: the integration_reading's framing treats their claims as category errors.
% DISAPPEARANCE_RATIONALE: If the constitutional right to free movement vanished overnight, member states would reimpose border controls within months, labor markets would re-segment, wage differentials would re-emerge, and fiscal pressures on high-inflow municipalities would shift to national budgets. The single market would fracture; the supranational authority would lose its core legitimacy anchor. The world rearranges because the constraint is constitutive of the current arrangement.
% FOUNDING_PROBLEM: Post-war reconstruction required preventing interstate conflict and enabling factor mobility for economic recovery. Free movement was originally a functional coordination mechanism for a war-torn continent with labor shortages and reconstruction needs.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (e.g., Eichengreen, Crafts) document that the original coordination problem (post-war reconstruction, interstate conflict prevention) was resolved by the 1970s. The integration_reading's own beneficiaries (supranational institutions) acknowledge the founding context but argue the arrangement has evolved into a constitutional order. No corroborating source outside the beneficiary set attests the founding problem remains live in its original form.
narrative_ontology:disappearance_verdict(federation_membership__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__integration_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__integration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(federation_membership__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__integration_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is high because the integration_reading structurally views the standing arrangement as transferring significant costs onto non-mobile populations: wage depression in exposed sectors, fiscal strain on municipalities, and political dislocation — while mobile citizens and capital capture gains. Suppression (0.48) is moderate: the constraint does not primarily coerce through violence but through legal supremacy doctrines that make border restriction illegitimate; resistance comes from political movements challenging the constitutional framing. Theater ratio (0.38) reflects that solidarity rhetoric and integration ceremonies increasingly mask the distributional reality. Accessibility collapse (0.42) is moderate: alternatives (conditional movement, compensatory transfers) exist conceptually but are politically delegitimized by the constitutional framing. Resistance (0.55) is substantial: political contestation over free movement has intensified across the interval.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from declared beneficiaries and victims. Mobile citizens (beneficiary, exit=arbitrage) → d near 0.0 (full beneficiary). Supranational institutions (agenda_setter, exit=analytical) → d near 0.0. Cross-border employers (beneficiary, exit=mobile) → d ~ 0.15. Local labor markets (victim, exit=constrained) → d ~ 0.85. Non-mobile workers (victim, exit=trapped) → d ~ 0.95. Municipal authorities (victim, exit=constrained) → d ~ 0.80. National governments (dual, exit=constrained) → d ~ 0.55 (split between enforcing and bearing costs). Spatial scope is continental for most agents, amplifying effective extraction via the engine's scope modifier.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy: the founding coordination problem (preventing interstate war, enabling post-war reconstruction) is largely dead (status=dead), yet the arrangement persists and has expanded in scope. The integration_reading treats the original mandate as superseded by a new constitutional order, but the founding problem's resolution is not acknowledged by the authority structure — the constitutional right framing prevents sunset. This creates a tangled_rope: genuine coordination (single market, labor mobility) coexists with asymmetric extraction (non-mobile workers bear costs without voice). The coordination function is real but the extraction is not incidental — it is sustained by the constitutional framing that makes compensation mechanisms politically illegitimate (they would imply the right is negotiable).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the federation membership constraint a single kernel with multiple readings, or are these structurally distinct constraints?',
    'Track whether the integration_reading and sovereignty_reading produce divergent ε values and beneficiary/victim structures when evaluated against the same observable standing arrangement. If they diverge, they are separate constraints linked by network.affects_constraints.',
    'If confirmed as separate constraints, the integration_reading authors its own ε (high, from labor displacement) while the sovereignty_reading would author low ε (coordination function). The kernel_id federation_membership then becomes a family label, not a single constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the contested kernel decomposes into multiple ε-invariant constraints per the ε-invariance principle').

omega_variable(
    integration_extraction_naturalness,
    'Is the extraction from local labor markets a necessary cost of the coordination function (free movement enabling labor market integration), or is it asymmetric extraction masquerading as coordination?',
    'Compare labor market outcomes in integration-committed federations vs. those with conditional free movement; measure whether wage/employment effects on non-mobile workers are proportional to aggregate gains and whether compensation mechanisms exist.',
    'If necessary cost, ε is partly coordination overhead and the constraint leans rope/scaffold. If asymmetric extraction, ε is substantially extractive and the constraint is tangled_rope or snare depending on enforcement structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_extraction_naturalness, empirical, 'Whether the constraint''s extraction is coordination cost or asymmetric transfer').

omega_variable(
    irreversibility_enforcement,
    'Does the ''irreversible'' claim require active enforcement against exit attempts (secession, border reimposition), or is it self-stabilizing through network effects?',
    'Observe historical episodes where member states attempted to restrict free movement or withdraw: was supranational enforcement invoked, or did political/economic pressure suffice?',
    'Active enforcement → requires_active_enforcement = true (tangled_rope gate satisfied). Self-stabilizing → enforcement requirement lower, may shift classification toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irreversibility_enforcement, empirical, 'Whether the integration claim''s irreversibility is enforced or emergent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__integration_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t5, federation_membership__integration_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(fede_tr_t10, federation_membership__integration_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(fede_tr_t15, federation_membership__integration_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement(fede_tr_t20, federation_membership__integration_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(fede_tr_t25, federation_membership__integration_reading, theater_ratio, 25, 0.38).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__integration_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fede_be_t5, federation_membership__integration_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(fede_be_t10, federation_membership__integration_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(fede_be_t15, federation_membership__integration_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(fede_be_t20, federation_membership__integration_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(fede_be_t25, federation_membership__integration_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__integration_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(fede_su_t5, federation_membership__integration_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(fede_su_t10, federation_membership__integration_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(fede_su_t15, federation_membership__integration_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(fede_su_t20, federation_membership__integration_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(fede_su_t25, federation_membership__integration_reading, suppression_requirement, 25, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__integration_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(federation_membership__integration_reading, 0.18).
narrative_ontology:affects_constraint(federation_membership__integration_reading, federation_membership__sovereignty_reading).

% DUAL FORMULATION NOTE:
% The federation_membership kernel decomposes into two ε-invariant constraints: integration_reading (this story) and sovereignty_reading (sibling). The integration_reading authors high ε (0.62) from labor displacement and constitutional illegitimacy of border restriction. The sovereignty_reading would author low ε treating free movement as conditional coordination. Both evaluate the same standing arrangement but from structurally different commitments. The integration_reading influences the sovereignty_reading: the constitutional framing creates downstream pressure on national border legitimacy, altering the sovereignty_reading's operating environment without logically foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership__integration_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
