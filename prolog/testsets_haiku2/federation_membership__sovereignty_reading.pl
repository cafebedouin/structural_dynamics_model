% ============================================================================
% CONSTRAINT STORY: federation_membership__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__sovereignty_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Federation Membership as Conditional Treaty (Sovereignty Reading)
 *   domain: political_economy/federalism
 *
 * SUMMARY:
 *   Federation membership under the sovereignty reading operates as a
 *   conditional treaty framework. Member states retain unilateral authority
 *   over borders and labor mobility, treating federation membership not as
 *   irreversible integration but as a revocable compact contingent on
 *   national interest. National governments and labor-market constituents
 *   benefit from controlled labor supply; mobile citizens bear the costs of
 *   mobility restrictions. The constraint actively enforces borders through
 *   visa gates, work permits, and internal checks. The reading competes with
 *   an integration reading that positions federation membership as
 *   irreversible and free movement as a constitutional right. The kernel —
 *   federation membership itself — is fixed; the readings diverge on whether
 *   membership is reversible (sovereignty) or irreversible (integration), and
 *   whether free movement is negotiable or constitutional.
 *
 * KEY AGENTS:
 *   - National labor-market protectors (governments, unions): retain border authority, benefit from labor scarcity management
 *   - Border-control authorities (immigration, customs): administer conditional-membership gates, derive legitimacy from border control
 *   - Mobile federation citizens: bear movement costs, lack automatic access to cross-border employment and residency
 *   - Regional labor markets: benefit from controlled labor supply and wage protection
 *   - Supranational institutions: excluded from border-setting, advocate for irreversible integration
 *   - Federation observer institutions: monitor compliance, document rights restrictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.68).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.72).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership as Conditional Treaty (Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political_economy/federalism").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, '9fdb1a7f-fee1-4d78-a016-c8da9710f08e').
narrative_ontology:cs_kernel_codification('9fdb1a7f-fee1-4d78-a016-c8da9710f08e', formalized).
narrative_ontology:cs_authority_grounding('9fdb1a7f-fee1-4d78-a016-c8da9710f08e', lineage).
narrative_ontology:cs_interpretation_layer_present('9fdb1a7f-fee1-4d78-a016-c8da9710f08e').
narrative_ontology:cs_reading_relation('9fdb1a7f-fee1-4d78-a016-c8da9710f08e', federation_membership__integration_reading, coexists_with).
narrative_ontology:cs_axiom('9fdb1a7f-fee1-4d78-a016-c8da9710f08e', foundational, federation_membership_conditionally_revocable).
narrative_ontology:cs_axiom_status(federation_membership_conditionally_revocable, holdable).
narrative_ontology:cs_axiom_grounding('9fdb1a7f-fee1-4d78-a016-c8da9710f08e', federation_membership_conditionally_revocable, conventional).
narrative_ontology:cs_axiom('9fdb1a7f-fee1-4d78-a016-c8da9710f08e', foundational, national_border_authority_legitimate).
narrative_ontology:cs_axiom_status(national_border_authority_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('9fdb1a7f-fee1-4d78-a016-c8da9710f08e', national_border_authority_legitimate, deontological).
narrative_ontology:cs_reference_frame('9fdb1a7f-fee1-4d78-a016-c8da9710f08e', conditional_treaty_framework).
narrative_ontology:cs_drift_state('9fdb1a7f-fee1-4d78-a016-c8da9710f08e', contemporary_integration_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9fdb1a7f-fee1-4d78-a016-c8da9710f08e', '').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, national_labor_market_protectors).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, border_control_authorities).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_federation_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, regional_labor_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National governments and their constituent labor interests (trade unions, regional employers, social welfare administrations). They benefit from the ability to control labor supply across borders and manage domestic welfare provision through immigration gates. They set the conditions of federation membership and enforce border closure selectively. Their exit option is unilateral federation withdrawal, which is formally available but politically and economically costly.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, national_labor_market_protectors, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__sovereignty_reading, national_labor_market_protectors, agenda_setter).

% Immigration, customs, and internal-security agencies. They administer the treaty's conditional-membership enforcement: visa procedures, work-permit gates, internal mobility checks, border surveillance. They derive operational budget and institutional legitimacy from the constraint. Their exit option is institutional reabsorption into supranational structures, which would eliminate their regulatory function entirely.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, border_control_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Individuals and families seeking to move between member states for employment, education, or family reunification. Formally they hold federation-membership guarantees; practically, work permits, residency rights, and welfare access are contingent on continuous negotiation with national authorities. Their options are acceptance of gate restrictions, expensive legal challenge, non-registration/informal migration, or emigration entirely outside the federation. The constraint actively restricts their movement by requiring state approval at borders and for employment.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_federation_citizens, payer,
    moderate, biographical, constrained, global).

% Federation-level administrative bodies, courts, and legislatures. The sovereignty reading assigns them limited authority over border policy — they are excluded from setting labor mobility rules and are actively undermined when member states exercise unilateral border closure. If present in negotiation, they would advocate for irreversible integration and constitutionalized free movement; their exclusion is maintained by the treaty's sovereignty-preserving architecture.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, supranational_institutions, excluded,
    institutional, generational, trapped, global).

% Employers and labor associations within member states who benefit from controlled labor supply. They can hire across federation borders when convenient but are protected from wage competition during downturns by restrictive immigration policy. Their exit option is to emigrate operations outside the federation, which they can do but prefer not to.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, regional_labor_markets, beneficiary,
    organized, biographical, constrained, regional).

% International bodies (UN human rights, ILO, comparative-federalism research institutions) observing the constraint's operation and recording non-compliance with international mobility norms. They have no enforcement power within the federation but produce reports and recommendations that feed sovereignty-reading contestation.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, federation_observer_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__sovereignty_reading, national_labor_market_protectors).
narrative_ontology:fixing_cost_class(federation_membership__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for member-state labor-market coordination: states commit to common commercial and investment rules while retaining unilateral border authority. This solves the coordination problem of cross-border trade and capital movement without requiring labor mobility; labor mobility is treated as a separate, negotiable policy question, not a corollary of federation.
% TRANSFER_FUNCTION: Moves authority over labor-market entry from supranational (where federation-integration logic would place it) back to member states. National governments gain the ability to restrict citizen movement and to calibrate welfare provision to citizen labor supply. Mobile citizens lose frictionless movement and bear transaction costs (permits, delays, contingency) on cross-border labor flows.
% ABSENT_VOICES: Supranational federation institutions and human-rights monitoring bodies are formally excluded from border-policy setting. They would argue that member citizenship entails a constitutional right to free movement and that labor restrictions violate federation identity. Their absence from rule-setting is structurally maintained by the sovereignty reading's premise that federation membership is conditional and revocable, not irreversible integration.
% DISAPPEARANCE_RATIONALE: If the constraint vanished — if free movement became unilaterally guaranteed and border gates were removed — member states would immediately lose labor-supply control, welfare systems would recalibrate, and the federation would reorganize on irreversible-integration grounds. The architecture of national labor-market protection would collapse within months.
% FOUNDING_PROBLEM: Cross-border capital and goods movement creates efficiency gains, but uncontrolled labor mobility destabilizes domestic labor markets and welfare spending during economic downturns. Member states cannot independently manage labor supply; they need a framework that permits trade while reserving migration policy to national discretion.
% FOUNDING_PROBLEM_CORROBORATION: Member-state labor ministries and trade unions attest the problem is live and justify border controls by pointing to wage-depression evidence and welfare-cost studies. Supranational institutions and human-rights organizations attest the problem is overstated and masked a redistribution of authority away from workers; they point to pre-federation labor analyses and post-frontier-closure wage data that show mixed or negligible effects. Independent labor economists produce divided testimony.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint transfers labor-market authority from mobile citizens and supranational bodies to member states, enabling rent capture through labor scarcity. Suppression is high (0.72) because the constraint's persistence depends on actively maintaining border gates and work-permit bureaucracy — the institutional machinery exists to keep the restriction in place. Theater ratio is moderate (0.41) because border enforcement includes genuine security and welfare-protection functions alongside labor-supply control; a substantial share of enforcement effort defends labor exclusivity, but a real coordination function (trade, capital movement, security) underlies the broader federation. The measurement series show extraction and suppression rising steeply in the first 15 points (interval establishment phase) and plateauing thereafter (mature enforcement phase), suggesting the constraint's institutional consolidation occurs early and then stabilizes. Theater ratio rises throughout, indicating growing proportional investment in public-framing (constitutional sovereignty, legitimate border control) relative to functional labor-gating.
 *
 * PERSPECTIVAL GAP:
 *   The sovereignty-reading beneficiaries (national governments, labor protectors) experience the constraint as legitimate treaty autonomy and rational labor-market management; they perceive low extraction because they frame border control as coordination, not rent-seeking. The mobile-citizen payers experience high extraction and active suppression — their movement is restricted, their access to employment across borders is contingent, and border authorities actively prevent their informal migration and work-around strategies. The engine computes directionality from beneficiary/victim declarations: the beneficiaries get low d (full-beneficiary end), the victims get high d (full-target end), which produces divergent classification seats. From the payer perspective this is a snare (pure extraction, high suppression, restricted alternatives); from the beneficiary perspective it is a rope (coordination benefit from unified trade framework, extraction is incidental to legitimate national labor policy). The gap arises from the sovereignty reading's foundational claim that national authority over borders IS legitimate — a claim the integration reading contests.
 *
 * DIRECTIONALITY LOGIC:
 *   National labor-market protectors are structural beneficiaries: they capture labor-scarcity rents, set the federation's membership conditions, and maintain unilateral border authority. Their directionality is near the beneficiary end (d ≈ 0.2). Border-control authorities are quasi-beneficiaries with institutional-inertia interests: they derive budget and functional legitimacy from border administration, but border closure also imposes enforcement costs. Their directionality is lower-middle (d ≈ 0.35). Mobile citizens are the clear targets: they bear transaction costs, face contingent labor access, and lack automatic residency or work rights. Their directionality is near the target end (d ≈ 0.85). Regional labor markets sit near symmetric (d ≈ 0.50): they benefit from labor scarcity during normal times but lose flexibility during boom periods when labor is undersupply-constrained. Supranational institutions are excluded rather than classified: their directionality would be 0.0 (pure beneficiary) if they controlled the constraint, but they are barred from setting border policy, so no d value applies — they are outside the constraint's operative surface.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — labor-market destabilization from uncontrolled cross-border movement — was live at federation inception. The sovereignty reading treats this problem as persistently live and justifies continuous border control as the solution. However, empirical wage-depression and welfare-cost studies from both inside and outside member states show mixed effects, suggesting the founding problem has either weakened substantially or was overstated to begin with. This is a mandatrophy candidate: the founding problem's status has shifted from live to contested/dead, yet the border-enforcement machinery persists and grows (theater ratio rising). The constraint persists not because the founding problem demands it but because the institutional beneficiaries (border authorities, labor-protectionist constituencies) derive rents from it and because the sovereignty reading's framing naturalizes border control as legitimate national autonomy. The classification remains tangled_rope (coordination + extraction) rather than degrading to piton because a real coordination function (trade and capital movement) still requires federation membership, and that membership is still negotiated through the treaty framework. But the mismatch between founding-problem-status (contested) and extraction-trajectory (rising) is a flag for institutional drift toward rent-collection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_market_effect_empirical_contestation,
    'What is the actual effect of labor mobility restriction on domestic wage levels and welfare spending across federation member states?',
    'Controlled comparison of wage and employment outcomes in member states with different enforcement intensity of work permits and residency restrictions; longitudinal analysis of welfare costs and labor-market participation before and after border tightening or loosening; meta-analysis of existing labor-economic studies.',
    'If effects are negligible or negative (welfare costs exceed labor-scarcity benefits), the sovereignty reading''s founding problem dissolves and the constraint reclassifies as pure extraction (snare) with no coordination defense. If effects are substantial and positive (significant wage protection, welfare savings), the tangled_rope classification holds and extraction is defensible as a price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_effect_empirical_contestation, empirical, 'Whether labor mobility restriction achieves its stated founding objective of protecting domestic labor markets.').

omega_variable(
    federation_membership_reversibility_axiom,
    'Is federation membership in fact revocable, or have member states become locked in by path dependency and institutional coupling?',
    'Historical analysis of withdrawal costs and political feasibility; test case: a member state attempts formal withdrawal and the economic, legal, and political costs are revealed.',
    'If membership is demonstrably revocable (low cost, formal process available, precedent), the sovereignty reading''s core axiom holds and border autonomy is legitimate. If membership is functionally irreversible (withdrawal costs prohibitive, institutional coupling deep, no precedent), the sovereignty reading''s framing becomes a fiction and the constraint reclassifies as a snare using integration-reading referents (high extraction from apparent-freedom-but-actual-lock).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federation_membership_reversibility_axiom, conceptual, 'Whether the sovereignty reading''s axiom (federation membership is conditionally revocable) corresponds to structural reality or masks path-dependent lock-in.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) a product of external institutional barriers (border gates, bureaucracy, legal sanctions) or have mobile citizens internalized the belief that movement is contingent and deserve restriction?',
    'Post-exit analysis: if a mobile citizen emigrates outside the federation and then reflects on their prior movement-seeking behavior, do they continue to believe movement restriction is legitimate, or does departure from the constraint reveal suppression was internalized? Survey data on how mobile citizens describe their own movement behavior and beliefs about legitimate border control.',
    'If suppression is purely structural (external barriers), removal of gates would immediately liberalize movement. If suppression is substantially internalized (citizens believe they should ask permission, deserve scrutiny, owe reciprocal obligation to national labor protectors), movement liberalization would take longer because citizens carry internalized suppression with them. High internalization suggests the constraint operates through identity fusion — mobile citizens have absorbed the sovereignty reading''s framing as common-sense legitimacy — and would require ideological as well as institutional change to dislodge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is an external institutional property or an internalized identity belief.').

omega_variable(
    kernel_contest_coexistence_vs_foreclosure,
    'Can the sovereignty and integration readings coexist as live positions within a single federation, or does strengthening one reading eventually foreclose the other at the framework level?',
    'Longitudinal institutional analysis: track the rise and fall of sovereignty-reading and integration-reading political strength across federation history; test case: a supranational court or legislature definitively adjudicates the reading (e.g., rules that free movement is constitutionally entailed by federation membership) and observe whether the sovereignty reading loses institutional support or persists despite the adjudication.',
    'If readings are genuinely coexistent (both live, neither forecloses), the federation operates as a contested-kernel system and both constraints remain valid structures. If one reading forecloses the other upon institutional dominance, the winning reading''s constraint becomes the true operative one and the loser''s constraint reclassifies as theater/piton (institutionally obsolete framing of what is actually a resolved kernel).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_coexistence_vs_foreclosure, conceptual, 'Whether sovereignty and integration readings are logically coexistent or whether the kernel permits only one stable reading at the framework level.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__sovereignty_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t5, federation_membership__sovereignty_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(fede_tr_t5, observed).
narrative_ontology:measurement(fede_tr_t10, federation_membership__sovereignty_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(fede_tr_t10, observed).
narrative_ontology:measurement(fede_tr_t15, federation_membership__sovereignty_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement_basis(fede_tr_t15, observed).
narrative_ontology:measurement(fede_tr_t20, federation_membership__sovereignty_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(fede_tr_t20, observed).
narrative_ontology:measurement(fede_tr_t25, federation_membership__sovereignty_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement_basis(fede_tr_t25, observed).
narrative_ontology:measurement(fede_tr_t30, federation_membership__sovereignty_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(fede_tr_t30, observed).
narrative_ontology:measurement(fede_tr_t40, federation_membership__sovereignty_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(fede_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__sovereignty_reading, base_extractiveness, 0, 0.51).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t5, federation_membership__sovereignty_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(fede_be_t5, observed).
narrative_ontology:measurement(fede_be_t10, federation_membership__sovereignty_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(fede_be_t10, observed).
narrative_ontology:measurement(fede_be_t15, federation_membership__sovereignty_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(fede_be_t15, observed).
narrative_ontology:measurement(fede_be_t20, federation_membership__sovereignty_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(fede_be_t20, observed).
narrative_ontology:measurement(fede_be_t25, federation_membership__sovereignty_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(fede_be_t25, observed).
narrative_ontology:measurement(fede_be_t30, federation_membership__sovereignty_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(fede_be_t30, observed).
narrative_ontology:measurement(fede_be_t40, federation_membership__sovereignty_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(fede_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__sovereignty_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t5, federation_membership__sovereignty_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(fede_su_t5, observed).
narrative_ontology:measurement(fede_su_t10, federation_membership__sovereignty_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(fede_su_t10, observed).
narrative_ontology:measurement(fede_su_t15, federation_membership__sovereignty_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(fede_su_t15, observed).
narrative_ontology:measurement(fede_su_t20, federation_membership__sovereignty_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(fede_su_t20, observed).
narrative_ontology:measurement(fede_su_t25, federation_membership__sovereignty_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(fede_su_t25, observed).
narrative_ontology:measurement(fede_su_t30, federation_membership__sovereignty_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(fede_su_t30, observed).
narrative_ontology:measurement(fede_su_t40, federation_membership__sovereignty_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(fede_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership__sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, federation_membership__integration_reading).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, supranational_authority_legitimacy).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, labor_mobility_as_human_right).

% DUAL FORMULATION NOTE:
% This constraint (sovereignty_reading) and federation_membership__integration_reading form a kernel pair. Both describe federation membership but from opposed reading positions: sovereignty treats it as conditional treaty with revocable member state authority over borders; integration treats it as irreversible commitment with supranational authority over labor mobility. The ε values differ because the readings instantiate different extraction referents. Sovereignty-reading ε measures extraction from the constraint as sovereignty framers see it (labor protection, high extraction). Integration-reading ε measures extraction from the constraint as integration advocates see it (wrongful mobility restriction, potentially higher ε). The readings coexist as live political positions but influence each other's institutional context. Link via network.affects_constraints in both files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership__sovereignty_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
