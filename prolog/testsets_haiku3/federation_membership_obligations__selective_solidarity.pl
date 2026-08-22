% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__selective_solidarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__selective_solidarity, []).

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
 *   constraint_id: federation_membership_obligations__selective_solidarity
 *   human_readable: Federation Membership Obligations: Selective Solidarity Reading
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The selective solidarity reading instantiates a specific claim about
 *   federation membership obligations: free movement is preserved, but access
 *   to welfare and certain rights is tiered by employment contribution
 *   history. This is NOT integration_primary (which treats free movement as
 *   unconditional and foundational to federation identity) nor
 *   member_sovereignty_primary (which treats federation commitments as
 *   subordinate to national welfare closure). The selective solidarity
 *   reading splits the difference—acknowledging both the federation's free
 *   movement commitment AND member states' fiscal constraints—by tiering
 *   rather than closing. This constraint is CLAIMED as tangled_rope because
 *   it coordinates the labor market incentive (job-seekers remain mobile)
 *   while extracting from the inactive (access denial). The measurement
 *   series show extractiveness rising modestly (0.54 → 0.68) as enforcement
 *   infrastructure matures; theater ratio rising (0.28 → 0.41) as the
 *   fairness and sustainability justifications become less tied to actual
 *   cost reduction and more theatrical maintenance of the tiers themselves.
 *
 * KEY AGENTS:
 *   - welfare_state_protectionists: agenda-setters controlling eligibility gates (institutional power, arbitrage exit)
 *   - employment_protected_citizens: beneficiaries with full rights secured (organized power, mobile exit)
 *   - economically_inactive_migrants: payersWithing restricted rights (powerless, trapped exit)
 *   - job_seeking_non_citizens: excluded and doubly restricted (powerless, trapped exit)
 *   - integration_advocates: excluded from rule-setting (institutional power, constrained exit)
 *   - regional_welfare_administrations: implementers and observers (institutional power, analytical exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.68).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.72).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "Federation Membership Obligations: Selective Solidarity Reading").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, 'b5659731-1f3c-47b2-bf78-a38d41d23a55').
narrative_ontology:cs_kernel_codification('b5659731-1f3c-47b2-bf78-a38d41d23a55', formalized).
narrative_ontology:cs_authority_grounding('b5659731-1f3c-47b2-bf78-a38d41d23a55', lineage).
narrative_ontology:cs_interpretation_layer_present('b5659731-1f3c-47b2-bf78-a38d41d23a55').
narrative_ontology:cs_reading_relation('b5659731-1f3c-47b2-bf78-a38d41d23a55', federation_membership_obligations__integration_primary, influences).
narrative_ontology:cs_reading_relation('b5659731-1f3c-47b2-bf78-a38d41d23a55', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('b5659731-1f3c-47b2-bf78-a38d41d23a55', foundational, contribution_basis_welfare_legitimacy).
narrative_ontology:cs_axiom_status(contribution_basis_welfare_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b5659731-1f3c-47b2-bf78-a38d41d23a55', contribution_basis_welfare_legitimacy, conventional).
narrative_ontology:cs_axiom('b5659731-1f3c-47b2-bf78-a38d41d23a55', secondary, tiered_membership_operationalizable).
narrative_ontology:cs_axiom_status(tiered_membership_operationalizable, holdable).
narrative_ontology:cs_axiom_grounding('b5659731-1f3c-47b2-bf78-a38d41d23a55', tiered_membership_operationalizable, empirically_contingent).
narrative_ontology:cs_reference_frame('b5659731-1f3c-47b2-bf78-a38d41d23a55', contribution_earned_rights_framework).
narrative_ontology:cs_drift_state('b5659731-1f3c-47b2-bf78-a38d41d23a55', contemporary_migration_pressure_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b5659731-1f3c-47b2-bf78-a38d41d23a55', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, welfare_state_protectionists).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, employment_protected_citizens).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, job_seeking_non_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National governments and welfare administrators who set eligibility gates based on contribution history. They justify tiered access as protecting fiscal sustainability and ensuring fairness to long-term contributors. They administer the means testing and contribution-tracking infrastructure and benefit from reduced welfare rolls for non-contributors.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, welfare_state_protectionists, agenda_setter,
    institutional, generational, arbitrage, national).

% Established workers with documented employment histories in their home federation member states. They retain full free movement rights and welfare access as long as employment status is maintained. Their competitive position improves when job-seeking migrants face restricted access to welfare.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, employment_protected_citizens, beneficiary,
    organized, biographical, mobile, national).

% Mobile EU citizens who are seeking employment, between jobs, caring for dependents, or in education without employment. They face restricted free movement rights—permission to stay in a member state is conditional on demonstrating economic self-sufficiency. Welfare access is explicitly tied to contribution history; they are ineligible for most social benefits even in acute need.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_inactive_migrants, payer,
    powerless, biographical, trapped, regional).

% Third-country nationals seeking employment in federation member states. They are formally outside the free movement regime; tiered contribution-based access applies only to federation members. Their exclusion from the tiering system itself means they face the most restrictive combination: no free movement rights and no welfare access.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, job_seeking_non_citizens, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__selective_solidarity, job_seeking_non_citizens, excluded).

% EU institutions and member states advocating for unconditional free movement as a core federation constitutional value. They would argue that tiering by contribution status violates the founding premise of federation citizenship and creates a two-tier class of members. They are largely excluded from setting the contribution gates themselves.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, integration_advocates, excluded,
    institutional, generational, constrained, continental).

% Subnational authorities responsible for welfare delivery and eligibility determination. They observe the tension between federal free movement rights and national welfare closure, implementing the tiers on the ground and reporting compliance data on contribution verification and access restrictions.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, regional_welfare_administrations, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__selective_solidarity, welfare_state_protectionists).
narrative_ontology:fixing_cost_class(federation_membership_obligations__selective_solidarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates federation-wide labor market functioning by tying welfare eligibility to employment contribution, creating incentives for rapid job-seeking by mobile workers and reducing pressure on social insurance systems. Simultaneously enables member states to maintain welfare sustainability by targeting benefits to contributors rather than all comers.
% TRANSFER_FUNCTION: Redistributes welfare eligibility (access to income support, health care, housing assistance) from economically inactive mobile persons to employed persons with documented contribution histories. Moves the enforcement burden of welfare gatekeeping from employment status (who is working?) to contribution history (how long and where have you worked?).
% ABSENT_VOICES: Economically inactive migrants and job-seeking non-citizens lack formal representation in the framework-setting bodies; their principal objection—that contribution-based tiering violates the founding premise of federation citizenship and creates a subordinate class of members within the federation—is not authored into the arrangement. Integration advocates are excluded from setting the contribution gates themselves.
% DISAPPEARANCE_RATIONALE: If the tiering were eliminated overnight, welfare costs in receiving member states would increase; the employment incentive structure for mobile job-seekers would shift (they would retain access to social benefits while searching rather than having to prove self-sufficiency); and member states would face renewed pressure to either harmonize welfare eligibility upward or deploy tighter enforcement on mobility rights themselves. The federation's coherence as a labor market would rearrange around a different conception of membership obligations.
% FOUNDING_PROBLEM: Early federation mobility created pressure on member state welfare systems when economically inactive persons moved to generous-benefit jurisdictions. The founding problem: how to preserve free movement for workers while protecting member state welfare sustainability from unlimited non-working migration.
% FOUNDING_PROBLEM_CORROBORATION: Member state welfare administrations and budget authorities attest the fiscal sustainability problem is live and worsened by recent migration flows. EU integration advocates and migration scholars attest the problem is overstated and that selective solidarity tiering contradicts founding federation values; independent welfare-mobility studies from outside the member-state system show mixed evidence on whether contribution-based tiering materially affects welfare costs vs. simply redistributing access within the same cohorts.
narrative_ontology:disappearance_verdict(federation_membership_obligations__selective_solidarity, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__selective_solidarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_obligations__selective_solidarity, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__selective_solidarity, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__selective_solidarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__selective_solidarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures how much the constraint deviates from equal treatment and how asymmetrically it distributes free movement rights by contribution status. At 0.68, this reflects a substantial departure from the federation's nominal principle of equal citizenship: mobility is permitted but welfare access is gated, creating a real cost for inactive persons trying to exercise free movement. Suppression at 0.72 reflects the enforcement requirement: member states must continuously verify contribution history, police welfare access, and refuse entry or benefits to those who fail the test. The enforcement infrastructure (means testing, contribution tracking, eligibility audits) is substantial and must be maintained actively—without it the tiers collapse. Theater at 0.41 (moderate) reflects that the fairness and sustainability framing is partly genuine (contribution-based welfare is a coherent principle) but increasingly performative (the enforcement energy is not proportional to actual cost reduction; the same fiscal problem persists even with the tiers, and the principal effect is exclusion rather than cost control).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute different constraint types from identical structural data: from the payer position this is extractive (mobility restricted), while from the agenda-setter position it is coordinating (incentive alignment). The engine computes this by directionality: payers get high d (targets), agenda-setters get low d (beneficiaries), which feeds into effective extraction. The claim/metric gap is deliberate and intentional: the selective solidarity reading is CLAIMED as tangled_rope (has both coordination—labor incentives—and extraction—welfare denial), and the metrics describe substantially extractive operation with active enforcement. This is the framework's way of detecting when a reading's own coherence masks a bifurcated effect.
 *
 * DIRECTIONALITY LOGIC:
 *   Welfare_state_protectionists are beneficiaries by construction: they set the rules, control the gates, and benefit from reduced welfare expenditure (though the savings are modest). Their directionality is near the beneficiary end (d ≈ 0.1). Employment_protected_citizens are also beneficiaries: they retain full free movement and welfare access while the inactive persons' access is restricted, improving their competitive position. Their d ≈ 0.2. Economically_inactive_migrants and job_seeking_non_citizens are targets: they are explicitly restricted by the constraint, their welfare access is denied, and they bear the adjustment cost of the tiers. Their d ≈ 0.9. Integration_advocates are excluded rather than beneficiary or payer—they have no formal role in rule-setting and no direct cost/benefit, but their preferred alternative (unconditional free movement) is foreclosed. This complex structure of asymmetric exit options and beneficiary/victim assignments is what drives the tangled_rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The selective solidarity reading instantiates a mandate that has partially outlived its original function. The founding problem was high-cost welfare access by economically inactive migrants; the selective solidarity solution was to tier access by contribution. However, the measurement series show that extractiveness is rising even as enforcement intensity increases, and theater ratio is climbing faster than actual cost savings accumulate. This suggests the constraint is becoming increasingly performative: the fairness and sustainability justifications persist, but the actual fiscal problem (member state welfare costs from migration) is not materially solved by the tiers. A mandatrophy flag would trigger on the mismatch between stated function (protect welfare sustainability) and observed outcome (exclusion intensifies, costs persist, enforcement rises). This is not full mandatrophy yet—the constraint still performs a coordination function (job-seekers remain mobile, incentive structure is coherent)—but the trajectory suggests degradation. The analysis supports the tangled_rope classification: the coordination component is real but attenuating, while the extraction component is structural and persistent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_sustainability_vs_exclusion_mechanism,
    'Is the selective solidarity constraint''s primary function to reduce member state welfare costs, or is its primary effect to exclude non-contributing mobile persons for political legitimacy reasons independent of cost?',
    'Empirical comparison of welfare costs in member states with tiered contribution gates vs. those with universal welfare access holding other variables constant (e.g., via natural experiments from member states that have eliminated or reinstated contribution gates). Post-elimination accounting of actual savings.',
    'If selective solidarity materially reduces costs, the extraction component (access denial) is justified as coordination cost. If costs persist despite tiers, the constraint is primarily extractive (political class maintenance) wearing coordination language. This determines whether the reading''s core justification (fiscal sustainability) is genuine or performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_vs_exclusion_mechanism, empirical, 'Whether the constraint solves the fiscal problem it claims to solve.').

omega_variable(
    contribution_history_operationalization,
    'What counts as ''contribution'' for purposes of welfare eligibility—employment only, or also caregiving, education, military service, disability work? How are cross-member-state contributions tracked and credited? Do member states converge on the same definition or operate different systems?',
    'Audit of member state welfare eligibility codes and actual implementation practices; analysis of appeal cases where contribution history is disputed; interview with welfare administrators on de facto interpretations.',
    'A narrow definition (employment only) is more extractive (excludes more people) and more enforceable (verifiable). A broad definition (any socially valued work) is less extractive and harder to enforce. Non-convergence (different member states count contributions differently) creates mobile persons who are eligible in one state and ineligible in another, compounding extraction. Operational fragmentation is a signal of underlying lack of agreement on what the contribution principle actually means.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contribution_history_operationalization, empirical, 'The definition and operationalization of ''contribution'' across the federation.').

omega_variable(
    reading_foreclosure_premise,
    'Is the selective solidarity reading genuinely distinct from member_sovereignty_primary, or is it a rationalized version of the same member-state gatekeeping impulse?',
    'Examine whether selective solidarity can coherently hold if a member state unilaterally abolished its contribution-based tiers while remaining in the federation. If the reading''s logic permits this (federal obligation to accept unconditional free movement even if states prefer tiering), it forecloses member_sovereignty_primary. If the reading''s logic requires member state consent to the tiers, it coexists_with member_sovereignty_primary.',
    'Foreclosure would establish selective solidarity as a genuine federation-level commitment, not just a member-state option. Coexistence would mean the reading is merely one state''s preferred implementation within a member-sovereignty framework. This determines whether selective solidarity is an architecture of the federation or a negotiated outcome.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_premise, conceptual, 'Whether selective solidarity is a federation-level commitment or a member-state bargain.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) structural—enforced by border controls, eligibility databases, and welfare denials—or is part of it internalized, where economically inactive migrants internalize the message that they ''shouldn''t'' use welfare or move without savings?',
    'Survey of mobile workers in economically inactive status: do those who face structural barriers (denied entry, welfare rejection) report lower mobility and access attempts than those who face internalized barriers (believing they shouldn''t seek benefits)? Post-entry or post-exit interviews with those who withdrew from welfare access despite eligibility.',
    'If suppression is primarily structural, removing the gates would restore access. If substantial part is internalized, the suppression persists after the gates are removed—the constraint carries its enforcement mechanism inside the targets themselves. This would indicate the constraint operates as identity discipline (you are not a ''real'' federation member unless you contribute) rather than merely administrative gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized mechanisms of suppression in the tiered regime.').

omega_variable(
    federation_vs_kernel_framing,
    'Is the reading generated here (selective_solidarity) a reading of a federation-level kernel (''membership obligations''), or is it a reading of a national-welfare-system kernel (''eligibility for benefits'')?',
    'Examine whether the constraint''s legitimacy is claimed at federation level (EU law, inter-member-state agreements, federation constitutional texts) or at member-state level (national welfare codes, national labor law). If federation-level claims are invoked, the kernel is federation-level and the reading is genuine; if only national-level claims exist, the constraint is a national welfare policy and the ''federation'' framing is a reinterpretation.',
    'Federation-level kernel framing means the constraint is about federation identity and membership terms—a fundamental question. National-level kernel framing means the constraint is about national welfare administration, less fundamental. This affects which readings are genuinely in contest and what the structural stakes are.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federation_vs_kernel_framing, conceptual, 'Whether this constraint is read off a federation kernel or a national welfare kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__selective_solidarity, theater_ratio, 0, 0.28).
narrative_ontology:measurement(fede_tr_t5, federation_membership_obligations__selective_solidarity, theater_ratio, 5, 0.32).
narrative_ontology:measurement(fede_tr_t10, federation_membership_obligations__selective_solidarity, theater_ratio, 10, 0.36).
narrative_ontology:measurement(fede_tr_t15, federation_membership_obligations__selective_solidarity, theater_ratio, 15, 0.39).
narrative_ontology:measurement(fede_tr_t20, federation_membership_obligations__selective_solidarity, theater_ratio, 20, 0.4).
narrative_ontology:measurement(fede_tr_t25, federation_membership_obligations__selective_solidarity, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__selective_solidarity, base_extractiveness, 0, 0.54).
narrative_ontology:measurement(fede_be_t5, federation_membership_obligations__selective_solidarity, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(fede_be_t10, federation_membership_obligations__selective_solidarity, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(fede_be_t15, federation_membership_obligations__selective_solidarity, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(fede_be_t20, federation_membership_obligations__selective_solidarity, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(fede_be_t25, federation_membership_obligations__selective_solidarity, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__selective_solidarity, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(fede_su_t5, federation_membership_obligations__selective_solidarity, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(fede_su_t10, federation_membership_obligations__selective_solidarity, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(fede_su_t15, federation_membership_obligations__selective_solidarity, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(fede_su_t20, federation_membership_obligations__selective_solidarity, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(fede_su_t25, federation_membership_obligations__selective_solidarity, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__selective_solidarity, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__selective_solidarity, 0.18).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, member_sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, welfare_state_fiscal_sustainability).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, labor_market_bidding_dynamics).

% DUAL FORMULATION NOTE:
% This constraint is part of the federation_membership_obligations kernel family. Selective_solidarity is one of three live readings: integration_primary (unconditional mobility), member_sovereignty_primary (national welfare closure), and selective_solidarity (tiered by contribution). Each reading instantiates different structural arrangements (different beneficiary/victim sets, different directionality atoms). They are linked as coexisting but influential readings, not as a single constraint viewed from multiple angles. The affects_constraints edges point to downstream constraints that each reading shapes: selective_solidarity particularly influences labor_market_bidding_dynamics (by creating variable entry costs for job-seekers) and welfare_state_fiscal_sustainability (by making welfare exposure dependent on prior contribution).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__selective_solidarity, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
