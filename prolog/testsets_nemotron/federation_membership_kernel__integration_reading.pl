% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__integration_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: federation_membership_kernel__integration_reading
 *   human_readable: EU Free Movement as Fundamental Citizenship Right (Integration Reading)
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story captures the 'integration reading' of the EU free
 *   movement kernel: the European Court of Justice interprets Treaty
 *   provisions on free movement of workers and EU citizenship expansively,
 *   treating mobility as a fundamental right that overrides national welfare
 *   boundaries and labor market protections. The constraint coordinates
 *   continental labor mobility (rope function) but simultaneously extracts
 *   fiscal and regulatory costs from receiving-state welfare systems,
 *   sending-state human capital, and national labor protections — without
 *   fiscal compensation mechanisms — while empowering the ECJ as the
 *   agenda-setting interpreter. This is the structural delta declared in the
 *   kernel context: displaced local labor enters the victim set; receiving
 *   state welfare systems bear costs without compensation; sending state
 *   brain drain is externalized; ECJ rulings override national protections.
 *   The claimed type is tangled_rope because the coordination function
 *   (single market completion) is genuine but the extraction is asymmetric
 *   and requires active enforcement (ECJ rulings, Commission infringement
 *   proceedings, Treaty obligation supremacy).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.62).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "EU Free Movement as Fundamental Citizenship Right (Integration Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, '59b4c851-908f-44a7-b96b-378010c7fc12').
narrative_ontology:cs_kernel_codification('59b4c851-908f-44a7-b96b-378010c7fc12', formalized).
narrative_ontology:cs_authority_grounding('59b4c851-908f-44a7-b96b-378010c7fc12', lineage).
narrative_ontology:cs_interpretation_layer_present('59b4c851-908f-44a7-b96b-378010c7fc12').
narrative_ontology:cs_reading_relation('59b4c851-908f-44a7-b96b-378010c7fc12', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('59b4c851-908f-44a7-b96b-378010c7fc12', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('59b4c851-908f-44a7-b96b-378010c7fc12', foundational, free_movement_as_fundamental_citizenship_right).
narrative_ontology:cs_axiom_status(free_movement_as_fundamental_citizenship_right, holdable).
narrative_ontology:cs_axiom_grounding('59b4c851-908f-44a7-b96b-378010c7fc12', free_movement_as_fundamental_citizenship_right, deontological).
narrative_ontology:cs_axiom('59b4c851-908f-44a7-b96b-378010c7fc12', foundational, ecj_as_supranational_authority_on_mobility_scope).
narrative_ontology:cs_axiom_status(ecj_as_supranational_authority_on_mobility_scope, holdable).
narrative_ontology:cs_axiom_grounding('59b4c851-908f-44a7-b96b-378010c7fc12', ecj_as_supranational_authority_on_mobility_scope, conventional).
narrative_ontology:cs_reference_frame('59b4c851-908f-44a7-b96b-378010c7fc12', maastricht_citizenship_framework).
narrative_ontology:cs_drift_state('59b4c851-908f-44a7-b96b-378010c7fc12', post_dano_alimanovic_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('59b4c851-908f-44a7-b96b-378010c7fc12', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, ecj_institutional_authority).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, receiving_state_employers).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_human_capital).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, national_labor_market_protections).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, member_state_governments).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, eu_citizenship_fundamental_right).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, single_market_completion_through_mobility).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, supranational_judicial_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise free movement to seek better wages and conditions across member states. Gain direct access to labor markets without national barriers. Their mobility is the constraint's declared purpose; they collect the wage differential and career advancement. Exit is high — they can move again or return home.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, mobile_eu_workers, beneficiary,
    organized, biographical, mobile, continental).

% Interprets Treaty provisions on free movement expansively through case law (e.g., Martinez Sala, Baumbast, Dano, Alimanovic). Each ruling extends the scope of who counts as a worker, what benefits are portable, and how far equal treatment reaches. The Court's authority grows with each expansive interpretation; it faces no higher appeal. Exit is analytical — it does not exit, it adjudicates.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, ecj_institutional_authority, agenda_setter,
    institutional, generational, analytical, continental).

% Access a larger, more flexible labor pool at competitive wages. Benefit from wage suppression in low-skill sectors and skill supplementation in high-skill sectors. Can recruit across borders without sponsorship costs. Exit is arbitrage-grade — they hire where labor is cheapest.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_employers, beneficiary,
    powerful, biographical, arbitrage, national).

% Face wage pressure and displacement in sectors with high migrant inflows (construction, hospitality, care, logistics). National collective bargaining and minimum wage floors are eroded by posted-worker rules and cross-border service provision. Exit is constrained — retraining is costly, geographic mobility within the country is limited by housing and family ties, and the constraint applies across the whole continental labor market.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, displaced_local_labor, payer,
    moderate, biographical, constrained, national).

% Bear fiscal costs of healthcare, housing assistance, child benefits, and social assistance for mobile EU citizens without corresponding fiscal transfers from sending states or EU budget. The Dano/Alimanovic line allows limited exclusion of economically inactive migrants, but the Court's equal-treatment logic pulls toward full portability. Exit is constrained — welfare systems cannot opt out of Treaty obligations; reform requires Treaty change.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_welfare_systems, payer,
    institutional, generational, constrained, national).

% Lose educated and skilled workers (especially healthcare, STEM) trained at public expense to higher-wage member states. Remittances partially offset but do not replace lost human capital and tax base. Brain drain weakens domestic service provision and growth potential. Exit is trapped — the constraint is the open border itself; sending states cannot restrict emigration and receive no compensation for the fiscal externality.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_state_human_capital, payer,
    moderate, generational, trapped, national).

% Collective agreements, sectoral minimum wages, posting directives, and social partner autonomy are overridden by ECJ rulings that prioritize cross-border service freedom (e.g., Laval, Viking, Rüffert). The constraint treats labor protections as barriers to the single market rather than as social rights. Exit is trapped — these protections exist only at national level and cannot be 'exited' to a supranational equivalent.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, national_labor_market_protections, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(federation_membership_kernel__integration_reading, national_labor_market_protections).

% Negotiate Treaty changes and secondary legislation (e.g., Posted Workers Directive revision, Social Security Coordination reform) but are bound by ECJ interpretations they cannot unilaterally reverse. Bear political costs of welfare chauvinism accusations when they resist expansion. Exit is constrained — leaving the EU (Art. 50) is the only full exit, politically catastrophic for core members.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, member_state_governments, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__integration_reading, member_state_governments, payer).

% Proposes legislation to manage free movement (e.g., coordination regulations, posted workers enforcement) but also acts as 'guardian of the Treaties' pushing expansive interpretation. Its institutional interest aligns with deepening integration. Exit is analytical — it does not exit the system it administers.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, eu_commission, agenda_setter,
    institutional, generational, analytical, continental).

% National trade unions and employer associations lose bargaining autonomy when ECJ subordinates collective action to economic freedoms. They are not formal parties to Treaty interpretation but bear the consequences. Their cross-border coordination (ETUC, BusinessEurope) is weak relative to national structures. Exit is constrained — they operate within national systems that are being reshaped from above.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, social_partners_unions_employers, excluded,
    organized, biographical, constrained, national).

% Analyze the constraint's evolution across law, economics, and political science. No material stake in outcomes; provide the analytical seat for the engine.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, academic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates labor supply across a continental single market by removing national barriers to mobility, enabling workers to move to where their productivity is highest and employers to recruit from a larger pool.
% TRANSFER_FUNCTION: Transfers fiscal costs (welfare, public services, human capital investment) from mobile workers and their employers to receiving-state welfare systems and sending-state publics; transfers wage-setting power from national collective bargaining to cross-border market forces; transfers interpretive authority from national courts/legislatures to the ECJ.
% ABSENT_VOICES: Third-country nationals legally resident in member states (excluded from EU citizenship rights but subject to same labor market pressures); future generations in sending states who inherit depleted human capital; economically inactive EU citizens (retirees, students, caregivers) whose mobility rights are contested in the Dano/Alimanovic line but who lack organized representation at EU level.
% DISAPPEARANCE_RATIONALE: If the expansive ECJ interpretation vanished overnight, member states would reassert control over welfare access for mobile citizens, posted-worker rules would revert to national standards, and sending states would demand fiscal compensation for brain drain. The single market would continue but with nationally segmented labor markets and welfare systems — a fundamental rearrangement of the EU's social dimension.
% FOUNDING_PROBLEM: Post-1992 completion of the single market required removing barriers to factor mobility (labor, capital, services). The founding problem was economic: national labor market fragmentation prevented efficient allocation of human resources across the Community, and citizens were denied the practical benefits of a common market.
% FOUNDING_PROBLEM_CORROBORATION: The Commission and ECJ attest the problem remains live — barriers persist, enforcement gaps remain, and 'Social Europe' is incomplete. Member state governments (especially Austria, Germany, Netherlands, Denmark) and national social partners attest the economic integration problem is substantially solved and the arrangement now functions as supranational social policy without democratic mandate. The 2014 UK renegotiation demand for 'emergency brake' on in-work benefits and the 2016 Brexit referendum outcome corroborate the shifted-function reading from outside the benefiting parties.
narrative_ontology:disappearance_verdict(federation_membership_kernel__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__integration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(federation_membership_kernel__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__integration_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the accumulated fiscal and regulatory externalities: receiving states fund welfare for mobile citizens without sending-state contributions; sending states lose human capital investment; national wage floors are eroded by cross-border competition. Suppression (0.62) captures the active enforcement required: ECJ rulings displace national law, infringement proceedings discipline non-compliant states, and the Treaty framework prevents unilateral exit from the regime. Theater ratio (0.28) is moderate — the coordination function (labor market integration) is real and valued, but a growing share of the Court's expansive interpretation serves institutional expansion rather than market efficiency. Accessibility collapse (0.45) is moderate: alternatives (national labor market regulation, welfare conditionality) are partially available but constrained by Treaty supremacy. Resistance (0.58) is significant: member states have pushed back through secondary legislation (Posted Workers Directive revision 2018, Social Security Coordination reform attempts), national constitutional court challenges (German PSPP, Polish rule-of-law), and political mobilization (Brexit, welfare chauvinism narratives).
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent per-seat classifications from the structural data. From the ECJ's seat, the constraint is a rope (genuine coordination, minimal extraction). From mobile workers' seat, it is a rope with subsidy (they gain). From receiving-state employers, a rope with subsidy. From displaced local labor, receiving-state welfare, and sending-state human capital, it computes as snare (high extraction, constrained exit, active suppression). From national labor market protections, it computes as snare (overridden by supranational authority). From member state governments, it computes as tangled_rope (they negotiate but are bound). This seat divergence is the measurement — the constraint is not one type but a field of types.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile EU workers are structural beneficiaries (d ~ 0.15): they collect wage differentials and career gains, with high exit mobility. ECJ institutional authority is the primary agenda-setter (d ~ 0.10): it gains interpretive power and institutional centrality from each expansive ruling. Receiving-state employers are beneficiaries (d ~ 0.20): they access cheaper/more flexible labor with arbitrage-grade exit. Displaced local labor are payers (d ~ 0.85): they bear wage pressure with constrained exit. Receiving-state welfare systems are payers (d ~ 0.80): they bear fiscal costs with constrained exit (Treaty-bound). Sending-state human capital is a payer (d ~ 0.90): brain drain is trapped — emigration cannot be restricted, no compensation received. National labor market protections are payers (d ~ 0.95): they are structurally overridden by ECJ case law with no exit. Member state governments are dual-positioned: agenda-setters in Council negotiations but payers of political/fiscal costs with constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (single market completion through labor mobility) was economically live in 1992. By 2024, the economic integration function is substantially achieved — barriers are low, mobility is high. Yet the expansive interpretation continues, now extending to welfare portability for economically inactive citizens and overriding national collective bargaining. The mandate has atrophied: the coordination function is mature but the extraction function expands. This is not a piton (no theatrical maintenance — the Court actively extends the frontier) and not a scaffold (no sunset clause, no declared transition). It is a tangled_rope where the coordination justification persists but the marginal coordination gain is near zero while marginal extraction rises. The mandatrophy is unresolved: the arrangement persists because the benefiting seats (ECJ, Commission, mobile workers, employers) have institutional power to block reversal, while the paying seats lack coordinated exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'At what point does the ECJ''s expansive interpretation of free movement cease to serve single market completion and become pure supranational social policy without democratic mandate?',
    'Counterfactual analysis: if the Court reverted to a ''market access'' standard (worker status tied to genuine economic activity) rather than ''citizenship rights'' standard (residence-based equal treatment), how much labor mobility would be lost vs. how much fiscal/regulatory extraction would be reduced?',
    'If the boundary is crossed, the constraint reclassifies from tangled_rope to snare for receiving-state welfare and national labor protection seats. The coordination function becomes a cover for extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the integration reading''s coordination function is still operative or has been displaced by extraction').

omega_variable(
    fiscal_compensation_feasibility,
    'Could a fiscal compensation mechanism (e.g., EU-level mobility fund, sending-state contributions to receiving-state welfare) be designed that would internalize the externalities without destroying the coordination function?',
    'Economic modeling of fiscal flows under different compensation designs; political feasibility assessment in Council and European Parliament.',
    'If feasible, the tangled_rope could be restructured toward rope (coordination with internalized costs). If infeasible, the extraction is structural to the kernel and the snare classification for payer seats is permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_compensation_feasibility, empirical, 'Whether the asymmetric extraction can be corrected while preserving the coordination function').

omega_variable(
    kernel_framing_ambiguity,
    'Does the federation_membership_kernel admit a single coherent commitment, or is it inherently a contested bundle of incompatible readings that cannot be stabilized?',
    'Compare the three declared readings on the six-questions battery: if their founding_problem, coordination_function, and transfer_function are mutually contradictory rather than complementary, the kernel is not a single commitment but a site of permanent contestation.',
    'If the kernel is inherently fragmented, each reading is not a ''view'' of one constraint but a distinct constraint. The ε-invariance principle would require treating them as separate constraint stories with no shared referent — which this story already does, but the kernel_id linkage would be misleading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the kernel itself is a coherent commitment or a contested label for distinct constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmk_ir_tr_t1992, federation_membership_kernel__integration_reading, theater_ratio, 1992, 0.12).
narrative_ontology:measurement(fmk_ir_tr_t1999, federation_membership_kernel__integration_reading, theater_ratio, 1999, 0.15).
narrative_ontology:measurement(fmk_ir_tr_t2004, federation_membership_kernel__integration_reading, theater_ratio, 2004, 0.18).
narrative_ontology:measurement(fmk_ir_tr_t2008, federation_membership_kernel__integration_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement(fmk_ir_tr_t2014, federation_membership_kernel__integration_reading, theater_ratio, 2014, 0.25).
narrative_ontology:measurement(fmk_ir_tr_t2018, federation_membership_kernel__integration_reading, theater_ratio, 2018, 0.27).
narrative_ontology:measurement(fmk_ir_tr_t2024, federation_membership_kernel__integration_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fmk_ir_be_t1992, federation_membership_kernel__integration_reading, base_extractiveness, 1992, 0.25).
narrative_ontology:measurement(fmk_ir_be_t1999, federation_membership_kernel__integration_reading, base_extractiveness, 1999, 0.32).
narrative_ontology:measurement(fmk_ir_be_t2004, federation_membership_kernel__integration_reading, base_extractiveness, 2004, 0.41).
narrative_ontology:measurement(fmk_ir_be_t2008, federation_membership_kernel__integration_reading, base_extractiveness, 2008, 0.48).
narrative_ontology:measurement(fmk_ir_be_t2014, federation_membership_kernel__integration_reading, base_extractiveness, 2014, 0.56).
narrative_ontology:measurement(fmk_ir_be_t2018, federation_membership_kernel__integration_reading, base_extractiveness, 2018, 0.62).
narrative_ontology:measurement(fmk_ir_be_t2024, federation_membership_kernel__integration_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fmk_ir_su_t1992, federation_membership_kernel__integration_reading, suppression_requirement, 1992, 0.35).
narrative_ontology:measurement(fmk_ir_su_t1999, federation_membership_kernel__integration_reading, suppression_requirement, 1999, 0.42).
narrative_ontology:measurement(fmk_ir_su_t2004, federation_membership_kernel__integration_reading, suppression_requirement, 2004, 0.51).
narrative_ontology:measurement(fmk_ir_su_t2008, federation_membership_kernel__integration_reading, suppression_requirement, 2008, 0.55).
narrative_ontology:measurement(fmk_ir_su_t2014, federation_membership_kernel__integration_reading, suppression_requirement, 2014, 0.59).
narrative_ontology:measurement(fmk_ir_su_t2018, federation_membership_kernel__integration_reading, suppression_requirement, 2018, 0.61).
narrative_ontology:measurement(fmk_ir_su_t2024, federation_membership_kernel__integration_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__integration_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__integration_reading, 0.15).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__welfare_coordination_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, eu_posted_workers_directive).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, eu_social_security_coordination).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, eu_citizenship_directive_2004_38).

% DUAL FORMULATION NOTE:
% This constraint family (federation_membership_kernel) decomposes the single label 'EU free movement' into three structurally distinct readings with different ε values, beneficiary/victim structures, and claimed types. The integration_reading (this story) has ε=0.68 and claimed tangled_rope. The member_sovereignty_reading would have lower ε (coordination bounded by national welfare) and claimed rope or scaffold. The welfare_coordination_reading would have moderate ε (coordination with anti-dumping enforcement) and claimed rope. They are linked via network.affects_constraints because the integration_reading's expansive interpretation structurally pressures the other two — it is the upstream driver in the kernel's drift.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_kernel__integration_reading, institutional, 0.15).
constraint_indexing:directionality_override(federation_membership_kernel__integration_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
