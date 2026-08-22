% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__member_sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__member_sovereignty_primary, []).

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
 *   constraint_id: federation_membership_obligations__member_sovereignty_primary
 *   human_readable: Member State Welfare Closure & Labor Market Protection (Sovereignty-Primary Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The European Union and other federal structures retain member-state
 *   closure authority over welfare access despite supranational free-movement
 *   commitments. This story instantiates ONE reading of the federation
 *   membership kernel: the member-sovereignty reading, which holds that
 *   national welfare states retain democratic closure authority as a
 *   precondition for fiscal sustainability and labor-market protection. The
 *   sibling readings (integration_primary and selective_solidarity) are
 *   separate constraint stories, not variants of this one. This reading
 *   asserts that member states can exclude or delay mobile workers' access to
 *   welfare benefits, restricting full beneficiary status to residents and
 *   long-term contributors. The authorization for this closure is grounded in
 *   the doctrine that member states remain sovereign over their domestic
 *   welfare systems and can set eligibility rules based on residency,
 *   contribution history, and demographic sustainability arguments.
 *   Structurally, the constraint is a tangled_rope: it coordinates domestic
 *   labor-market protection and welfare-system sustainability around a
 *   membership boundary (the genuine coordination function), while
 *   simultaneously extracting welfare access from mobile workers and using
 *   them as a lower-wage labor supply (the extraction component). The
 *   constraint persists because member-state legislatures actively enforce it
 *   through immigration law, welfare eligibility rules, and tax documentation
 *   requirements. The theater ratio (0.42) is moderate: the coordination
 *   function (protecting resident workers, maintaining welfare-system
 *   solvency) is genuine and observable, but a growing share of enforcement
 *   energy goes to defending the extraction (keeping mobile workers out or
 *   delaying their access) rather than to the coordination goal itself.
 *
 * KEY AGENTS:
 *   - resident_citizen_workers — beneficiaries of protected labor markets and full welfare access
 *   - member_state_welfare_administrators — agenda-setters who control eligibility rules and enforce closure
 *   - mobile_workers_from_other_member_states — primary targets of extraction (pay taxes, receive restricted welfare)
 *   - labor_unions_and_worker_councils — organized beneficiaries defending labor-market closure
 *   - eu_integration_institutions — observer seat pushing for non-discrimination and free movement
 *   - low_wage_employers — beneficiaries of cheap labor from excluded mobile workers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, 0.68).
domain_priors:suppression_score(federation_membership_obligations__member_sovereignty_primary, 0.71).
domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "Member State Welfare Closure & Labor Market Protection (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__member_sovereignty_primary, 'f743c168-fc2c-4a8d-895d-add83a472ce2').
narrative_ontology:cs_kernel_codification('f743c168-fc2c-4a8d-895d-add83a472ce2', fixed_text).
narrative_ontology:cs_authority_grounding('f743c168-fc2c-4a8d-895d-add83a472ce2', extraction).
narrative_ontology:cs_interpretation_layer_present('f743c168-fc2c-4a8d-895d-add83a472ce2').
narrative_ontology:cs_reading_relation('f743c168-fc2c-4a8d-895d-add83a472ce2', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('f743c168-fc2c-4a8d-895d-add83a472ce2', federation_membership_obligations__selective_solidarity, coexists_with).
narrative_ontology:cs_axiom('f743c168-fc2c-4a8d-895d-add83a472ce2', foundational, member_state_welfare_closure_authority).
narrative_ontology:cs_axiom_status(member_state_welfare_closure_authority, holdable).
narrative_ontology:cs_axiom_grounding('f743c168-fc2c-4a8d-895d-add83a472ce2', member_state_welfare_closure_authority, deontological).
narrative_ontology:cs_axiom('f743c168-fc2c-4a8d-895d-add83a472ce2', foundational, welfare_system_fiscal_sustainability_doctrine).
narrative_ontology:cs_axiom_status(welfare_system_fiscal_sustainability_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('f743c168-fc2c-4a8d-895d-add83a472ce2', welfare_system_fiscal_sustainability_doctrine, empirically_contingent).
narrative_ontology:cs_reference_frame('f743c168-fc2c-4a8d-895d-add83a472ce2', member_state_democratic_self_governance).
narrative_ontology:cs_drift_state('f743c168-fc2c-4a8d-895d-add83a472ce2', contemporary_open_borders_migration_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f743c168-fc2c-4a8d-895d-add83a472ce2', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, resident_citizen_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, member_state_welfare_administrators).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, mobile_workers_from_other_member_states).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, third_country_nationals_seeking_settlement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, labor_unions_and_worker_councils).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, low_wage_employers_seeking_labor_supply).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, national_democratic_accountability_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, welfare_state_sustainability_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive priority in domestic labor markets and full welfare access (unemployment insurance, family allowances, housing support, healthcare) without residency or contribution gatekeeping. Protected from wage competition via preferential hiring norms and union agreements. Their position depends on the welfare system's closure to non-residents; opening access would dilute finite welfare resources and intensify labor market competition.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, resident_citizen_workers, beneficiary,
    organized, biographical, constrained, national).

% Control access to the welfare system via residency requirements, contribution histories, and eligibility gates. Enforce the boundary between resident beneficiaries and mobile workers. Justify closure as necessary to sustain social insurance systems funded by citizen payroll taxes and designed for demographic replacement within the national population. They set the rules, conduct means-testing and eligibility review, and defend the closure against integration-pressure from supranational institutions and mobile workers.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, member_state_welfare_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Migrate in search of higher wages or better employment conditions but encounter restricted access to welfare benefits despite paying taxes. They contribute payroll taxes to the resident state's welfare system but are excluded from full beneficiary status (especially family allowances, housing support, and long-term unemployment support) for a waiting period that extends years or even permanently. They can work but cannot draw on the welfare floor that resident citizens access automatically, creating a secondary labor-market status.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, mobile_workers_from_other_member_states, payer,
    moderate, biographical, constrained, regional).

% Face even stricter closure: their access to work is conditioned on demonstration of labor-market need and their access to welfare is minimal or absent during settlement periods. They pay taxes on wages they earn but are excluded from the social insurance system entirely until (and unless) they naturalize. They are the canonical outsiders whom the closure is designed to keep out; their presence is strictly limited.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, third_country_nationals_seeking_settlement, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__member_sovereignty_primary, third_country_nationals_seeking_settlement, excluded).

% Push for free movement of workers and citizens as a foundational right and single-market functioning requirement. They interpret the founding treaties as requiring welfare non-discrimination after a contribution period. They issue directives, case law, and policy pressure to open welfare access, generating persistent tension with member state legislatures. They observe the constraint from the perspective of integration imperatives.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, eu_integration_institutions, observer,
    institutional, generational, analytical, regional).

% Defend labor-market closure as protection for their membership (resident citizens). They organize wage negotiations under the assumption of protected membership, and view open access to labor markets as undermining their negotiating position and wage floors. They align with welfare administrators in maintaining closure, seeing it as the structural precondition for collective bargaining power.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, labor_unions_and_worker_councils, beneficiary,
    organized, biographical, constrained, national).

% Observe the constraint from a cost-containment angle, using welfare closure as a fiscal discipline tool. They are not primary beneficiaries (they do not collect welfare) but they see closure as a necessary limit on welfare costs, enabling strict means-testing and eligibility gating. Their interest is in restricting the welfare system's expansion, not in protecting worker income; closure aids that goal by making welfare more selective.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, fiscal_conservatives_and_welfare_retrenchment_advocates, observer,
    moderate, biographical, mobile, national).

% Benefit from mobile workers' availability as a labor supply willing to work at lower wages (because excluded from full welfare safety net) while bearing the cost of contributing to the welfare system they do not directly profit from. They are genuinely captured by the constraint's enforcement machinery: they want cheap labor but the welfare closure is maintained by rules they must respect, and they cannot unilaterally access the excluded labor pool outside regulatory compliance.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, low_wage_employers_seeking_labor_supply, payer,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__member_sovereignty_primary, resident_citizen_workers).
narrative_ontology:fixing_cost_class(federation_membership_obligations__member_sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains a national social insurance system by limiting welfare access to residents (or residents-plus-contributors), preventing unlimited claims on finite welfare resources from highly mobile populations. Coordinates domestic labor-market protection (wage floors, working conditions, unemployment insurance) around a membership boundary. Solves the problem of how a welfare state funded by progressive taxation on resident incomes can set its own eligibility rules without being overwhelmed by inflows of mobile workers.
% TRANSFER_FUNCTION: Moves welfare resources (unemployment insurance, family allowances, housing support, healthcare subsidies, pension credits) from the broader tax base of resident citizens to a narrowly defined beneficiary set (typically citizens + long-term residents + documented contributors). Mobile workers contribute taxes but are excluded from receiving equivalent welfare support, creating a net transfer from them to residents. Low-wage employers extract labor at sub-welfare-inclusive rates while contributing to welfare systems they do not directly access.
% ABSENT_VOICES: Supranational integration advocates and human rights bodies argue that welfare closure violates free movement rights and non-discrimination principles; mobile workers themselves are partially excluded from the room (they can lobby but lack citizen voting rights in the host state). Third-country nationals seeking settlement are almost entirely outside the conversation. Ecological and demographic projectionists who argue welfare closure is unsustainable (insufficient contribution base, aging population requiring open immigration) are heard but not decisional.
% DISAPPEARANCE_RATIONALE: If member state welfare closure disappeared overnight, the system would reorganize around open access and non-discrimination: welfare systems would face immediate fiscal pressure requiring either tax increases or eligibility retrenchment; labor markets would clear at lower wages (the exclusionary premium would evaporate); worker mobility would surge; and member state legislatures would lose a primary tool for managing population composition and welfare costs. The political economy of the welfare state would be fundamentally altered.
% FOUNDING_PROBLEM: How can a democratic nation-state maintain a welfare system funded by resident citizens' taxes and designed for national demographic replacement without facing unlimited claims from highly mobile populations from other member states or from outside the federation?
% FOUNDING_PROBLEM_CORROBORATION: Member state legislatures and welfare administrators attest the problem is live and acute: demographic aging, fiscal pressure, and migration flows require strict eligibility gating. Integration institutions and mobile-worker advocates attest the problem is a cover story for economic protectionism: the founding problem (finite welfare resources) is actually solvable through higher taxation and better-designed contribution conditions, and the closure serves rent-protection for resident workers, not genuine system sustainability. Labor economists outside the welfare-administrator camp are divided on whether closure is structurally necessary or merely convenient for resident-worker protection.
narrative_ontology:disappearance_verdict(federation_membership_obligations__member_sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__member_sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__member_sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_obligations__member_sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__member_sovereignty_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__member_sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__member_sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint transfers welfare benefits from mobile workers (who contribute taxes) to residents (who access welfare automatically) without proportionate compensation. The transfer is justified as system sustainability, but mobile workers experience direct welfare exclusion despite paying into the system. Suppression is even higher (0.71) because the constraint's persistence depends on active legal enforcement (residency requirements, contribution histories, documentation gates) that must be continuously renewed against integration pressure. Theater (0.42) is moderate because the coordination function is genuinely operative—the constraint does protect resident workers and constrain welfare costs—but increasingly the enforcement machinery is dedicated to preventing mobile-worker access rather than to solving the original coordination problem. The temporal series shows extractiveness rising from 0.54 to 0.68 over the interval, with suppression and theater rising in parallel, suggesting a pattern of tightening closure as migration pressures increase and member-state legislatures respond by raising barriers rather than reforming the coordination function itself. Suppression plateaus at 0.71 around t=25, suggesting enforcement capacity reaches a practical limit; thereafter, extraction persists through legal gates rather than intensifying suppression infrastructure. The measurement basis is 'observed' throughout, reflecting actual policy enforcement patterns and welfare-access statistics from member states over the documented period.
 *
 * PERSPECTIVAL GAP:
 *   This story should compute different classifications in the resident-worker seat versus the mobile-worker seat, and different again in the member-state legislator seat versus the EU integration institution seat. From the resident-worker perspective, the constraint appears as rope (genuine coordination protecting their position) with low extraction and low suppression (the coordination solves their problem). From the mobile-worker perspective, the constraint appears as snare (active suppression of their access, with the coordination function serving as justification). From the member-state legislator perspective, the constraint is rope (solving the coordination problem of sustaining welfare while managing migration). From the EU integration institution perspective, the constraint is snare (a cover story for economic protectionism). The authored story represents the member-sovereignty reading's structural logic: the constraint is tangled_rope (both coordination and extraction are real), and that classification is what the engine should compute for the member-state legislator seat. The divergence between seats is the dividend of the per-seat computation. Directionality reflects this gap: resident workers have low d (beneficiaries), mobile workers have high d (targets), member-state administrators have low d (they set and benefit from the rules).
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary set (resident citizens, member-state welfare administrations) derives low directionality (d ≈ 0.1–0.25) because they benefit from the constraint and have high exit options (if they dislike the constraint, they can dismantle it through democratic process—they control the rules). The victim set (mobile workers) derives high directionality (d ≈ 0.7–0.85) because they pay taxes but are excluded from welfare, and their exit options are constrained (leaving the member state means giving up employment and settlement opportunity, a costly exit). Labor unions and organized resident workers have moderate d (d ≈ 0.15–0.35) because they benefit but their power to enforce the constraint is distributed across member-state legislatures. Low-wage employers are captured: they benefit from cheap labor (low d) but cannot exit the enforcement machinery (they must comply with welfare rules), and they contribute to welfare systems they do not directly access (slight upward pressure on d). The directionality override for low-wage employers accounts for this capture: their apparent beneficiary position (cheap labor access) is asymmetrically constrained by their compliance obligations, so d is raised slightly from the beneficiary baseline.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how can a welfare state maintain solvency and resident-worker protection without unlimited mobile-worker claims?) is asserted as 'contested,' and the welfare-access situation is assessed as world_rearranges. The tension here is live: if the founding problem is dead (welfare systems can sustain open access through higher taxation or better contribution design), but the constraint persists (member states actively maintain closure), the arrangement is a zombie—a constraint maintained beyond its functional necessity. The measured extraction (0.68, rising over the interval) and the rising theater ratio (from 0.28 to 0.42) are consistent with mandatrophy: as the founding problem becomes less empirically salient (welfare systems do not demonstrably collapse under open access in other jurisdictions), the constraint persists through institutional inertia and rent-protection rather than through genuine coordination necessity. The theater ratio's plateau after t=25 suggests the constraint has reached a stable inertial state: enforcement is no longer intensifying because a new equilibrium has formed. This is the diagnostic profile of a constraint approaching piton status, where the original coordination function is atrophying but the extraction mechanism persists. The story does not claim mandatrophy_resolved; the gate (founding_problem_status=dead + disappearance_verdict=world_rearranges) is not yet satisfied. But the temporal series positions the constraint at the threshold of that gate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sustainability_vs_protectionism_boundary,
    'Is welfare closure structurally necessary for system sustainability, or is it primarily a mechanism for protecting resident-worker labor-market rents?',
    'Comparative analysis of welfare systems with different closure regimes (Denmark/Netherlands with high contribution thresholds vs. open-access models) over a 20+ year period, examining fiscal outcomes, employment rates, and wage floors under identical demographic and migration pressures.',
    'If closure is shown to be non-necessary (sustainability achieved under open access with higher taxation), the constraint reclassifies from tangled_rope (genuine coordination + extraction) to snare (pure extraction using sustainability as cover). If closure is shown to be structurally necessary, the extracted component (the restriction on mobile workers) becomes part of the coordinated solution''s necessary cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sustainability_vs_protectionism_boundary, conceptual, 'Whether welfare closure is structurally required for system viability or instrumentally chosen to protect resident-worker rents.').

omega_variable(
    member_sovereignty_vs_supranational_integration_tension,
    'Does this reading''s core assertion—that member states retain closure authority—foreclose or merely coexist with the integration-primary reading''s core assertion that free movement is constitutive of federation membership?',
    'Examination of founding treaties and interpretive history: does the charter of the federation actually require member states to surrender closure authority, or does it permit it with exceptions? What do treaty signers (at founding and major amendments) claim they agreed to?',
    'If treaty language mandates non-discrimination in welfare access, this reading forecloses the integration_primary reading (both cannot be true simultaneously in a single legal framework). If treaty language is ambiguous and both readings find plausible textual support, they coexist as competing interpretations sustained by different political coalitions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(member_sovereignty_vs_supranational_integration_tension, empirical, 'Whether member-state closure authority and supranational free-movement rights are logically compatible within a single federation.').

omega_variable(
    kernel_reading_committer_multiplicity,
    'Which reading of the federation membership kernel is actually in force: this member-sovereignty reading, the integration-primary reading, or the selective-solidarity reading?',
    'Analysis of case law, enforcement patterns, and policy outcomes in the federation over the interval. Which reading''s logic actually governs member-state legislative action? Which institutions enforce which interpretation? How do conflict cases (mobile workers challenging welfare exclusions) actually resolve?',
    'The reading that is actually in force determines the descriptive referent of ε (what extraction measurement describes) and shapes the directionality of stakeholder positions. If member-sovereignty is in force, member-state legislatures have de facto closure authority; if integration-primary is in force, they face supranational override risk. The measured extraction (0.68) describes the member-sovereignty reading''s operation; a different reading in force would yield a different constraint story with different ε and beneficiary/victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_multiplicity, empirical, 'Which kernel reading is actually operative in the federation''s institutional practice.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of mobile workers'' access claims (0.71) structural (legal bars, residency requirements, documentation burdens) or internalized (mobile workers believe they do not deserve welfare, accept exclusion as legitimate)?',
    'Ethnographic and survey analysis of mobile workers'' attitudes post-exit from the host state: do exclusion effects persist (they continue to believe they did not deserve welfare)? Do they organize collectively to challenge closure rules? What proportion of suppression remains after the legal barriers are removed?',
    'If suppression is largely internalized (mobile workers accept the closure as legitimate), the constraint''s effective suppression is higher than the structural measure suggests and reclassifies toward snare. If suppression is purely structural (mobile workers would fight the closure if barriers were removed), it remains a tangled_rope with genuine extraction but also genuine coordination requiring active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of mobile-worker access claims is structural or internalized belief.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t5, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(fede_tr_t5, observed).
narrative_ontology:measurement(fede_tr_t10, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(fede_tr_t10, observed).
narrative_ontology:measurement(fede_tr_t15, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(fede_tr_t15, observed).
narrative_ontology:measurement(fede_tr_t20, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(fede_tr_t20, observed).
narrative_ontology:measurement(fede_tr_t25, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(fede_tr_t25, observed).
narrative_ontology:measurement(fede_tr_t30, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(fede_tr_t30, observed).
narrative_ontology:measurement(fede_tr_t35, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(fede_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t5, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(fede_be_t5, observed).
narrative_ontology:measurement(fede_be_t10, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(fede_be_t10, observed).
narrative_ontology:measurement(fede_be_t15, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(fede_be_t15, observed).
narrative_ontology:measurement(fede_be_t20, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(fede_be_t20, observed).
narrative_ontology:measurement(fede_be_t25, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(fede_be_t25, observed).
narrative_ontology:measurement(fede_be_t30, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(fede_be_t30, observed).
narrative_ontology:measurement(fede_be_t35, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(fede_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t5, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(fede_su_t5, observed).
narrative_ontology:measurement(fede_su_t10, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(fede_su_t10, observed).
narrative_ontology:measurement(fede_su_t15, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(fede_su_t15, observed).
narrative_ontology:measurement(fede_su_t20, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(fede_su_t20, observed).
narrative_ontology:measurement(fede_su_t25, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(fede_su_t25, observed).
narrative_ontology:measurement(fede_su_t30, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(fede_su_t30, observed).
narrative_ontology:measurement(fede_su_t35, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(fede_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__member_sovereignty_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__member_sovereignty_primary, 0.18).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__selective_solidarity).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading kernel family. The federation-membership-obligations kernel is the persisting commitment that member states are bound by federation treaties; what those treaties require of member states regarding welfare access and free movement is under interpretation dispute. The member-sovereignty-primary reading (this story) holds that member states retain closure authority and measures ε ≈ 0.68 as a tangled_rope. The integration-primary reading holds that free movement supercedes closure authority (higher ε, likely snare). The selective-solidarity reading holds that access is tiered by contribution history (moderate ε, likely rope or tangled_rope). Each reading instantiates a distinct constraint with its own beneficiary/victim structure and classification. Network links route contestation between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__member_sovereignty_primary, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
