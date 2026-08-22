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
    narrative_ontology:constraint_stakeholder/7,
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
 *   domain: political/economic
 *
 * SUMMARY:
 *   Under the sovereignty reading of federation membership, each member state
 *   retains constitutional authority over borders and labor-market access.
 *   Federation membership is framed as a conditional treaty among sovereign
 *   equals, not as an irreversible political integration. Free movement for
 *   federation citizens is negotiable policy—each state may restrict it on
 *   grounds of labor-market protection, welfare sustainability, and national
 *   security. This reading legitimates border control, credential
 *   gatekeeping, and differential treatment of citizens and non-citizens as
 *   expressions of national democracy and territorial authority. The reading
 *   instantiates a high-extraction constraint because mobility restrictions
 *   capture substantial value for incumbent resident populations and domestic
 *   labor markets, while mobile citizens and prospective migrants bear the
 *   cost. Extraction rises modestly over the interval as member states layer
 *   new credential-recognition bureaucracy and labor-market-impact
 *   assessments, theatricalizing labor protection while the underlying
 *   mobility restriction persists.
 *
 * KEY AGENTS:
 *   - Member state governments: set and enforce border rules; frame national sovereignty as legitimate justification
 *   - Incumbent resident populations: gain employment priority and welfare preference; experience free movement as optional
 *   - Domestic labor markets: protected from labor-supply shocks; coordinate wage floors through national policy
 *   - Mobile citizens: face substantive barriers (bureaucracy, credential delays, welfare-eligibility gaps) despite nominal freedom to move
 *   - Prospective migrants: entirely excluded; no claim to federation entry
 *   - Supranational federation authority: interprets free movement as negotiable, not constitutional; defers to member-state discretion
 *   - Regional labor unions and civil society: excluded from border-rule legitimacy despite advocacy for mobility-as-right
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
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership as Conditional Treaty (Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political/economic").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, '0aae7c60-aae1-4b37-b7d5-2c2f87d44577').
narrative_ontology:cs_kernel_codification('0aae7c60-aae1-4b37-b7d5-2c2f87d44577', formalized).
narrative_ontology:cs_authority_grounding('0aae7c60-aae1-4b37-b7d5-2c2f87d44577', lineage).
narrative_ontology:cs_interpretation_layer_present('0aae7c60-aae1-4b37-b7d5-2c2f87d44577').
narrative_ontology:cs_reading_relation('0aae7c60-aae1-4b37-b7d5-2c2f87d44577', federation_membership__integration_reading, forecloses).
narrative_ontology:cs_axiom('0aae7c60-aae1-4b37-b7d5-2c2f87d44577', foundational, national_sovereignty_over_labor_markets).
narrative_ontology:cs_axiom_status(national_sovereignty_over_labor_markets, holdable).
narrative_ontology:cs_axiom_grounding('0aae7c60-aae1-4b37-b7d5-2c2f87d44577', national_sovereignty_over_labor_markets, deontological).
narrative_ontology:cs_axiom('0aae7c60-aae1-4b37-b7d5-2c2f87d44577', foundational, free_movement_as_negotiable_policy).
narrative_ontology:cs_axiom_status(free_movement_as_negotiable_policy, holdable).
narrative_ontology:cs_axiom_grounding('0aae7c60-aae1-4b37-b7d5-2c2f87d44577', free_movement_as_negotiable_policy, conventional).
narrative_ontology:cs_reference_frame('0aae7c60-aae1-4b37-b7d5-2c2f87d44577', treaty_sovereignty_framework).
narrative_ontology:cs_drift_state('0aae7c60-aae1-4b37-b7d5-2c2f87d44577', contemporary_labor_mobility_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0aae7c60-aae1-4b37-b7d5-2c2f87d44577', '').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, domestic_labor_markets).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, incumbent_resident_populations).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_citizens).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, prospective_migrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain from border controls that limit labor supply shocks and protect wage floors for incumbent workers. Union organizations, employer associations, and workforce development agencies in each member state benefit from controlling the speed and composition of labor-market entry. They argue border management preserves employment stability and funding for social protection.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, domestic_labor_markets, beneficiary,
    organized, generational, constrained, national).

% Benefit from prioritized access to employment, housing, social services, and public resources within their home member state. Citizenship confers preferential treatment in welfare distribution and labor-market access. They experience free movement as optional (they can leave, but they are not pressured to) while non-citizens face mandatory gatekeeping.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, incumbent_resident_populations, beneficiary,
    moderate, biographical, mobile, national).

% Federation citizens who wish to work, study, or reside outside their home member state must navigate discretionary national admission rules, labor-market access restrictions, social-benefit eligibility delays, and housing discrimination. They are nominally free to move but face substantive barriers—border bureaucracy, professional credential recognition delays, family reunification restrictions. Their formal status changes the instant they cross a border; they lose incumbent protections and enter a secondary status in the receiving state.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_citizens, payer,
    moderate, biographical, constrained, continental).

% Non-citizen migrants from outside the federation are entirely excluded by border controls. The sovereignty reading treats federation membership as a property right of the member state itself, not as a gateway open to migration pressure. They have no formal claim to entry and face the full apparatus of national immigration law with no supranational appeal.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, prospective_migrants, payer,
    powerless, biographical, trapped, global).

% Retain constitutional and treaty authority over border policy. Each government sets and enforces admission rules, labor-market protections, welfare eligibility criteria, and credential recognition standards. They frame these as legitimate expressions of national sovereignty and democratic will. The federation is a treaty among sovereign states; member states use border control to preserve electoral accountability and the link between citizenship and social benefit.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, member_state_governments, agenda_setter,
    institutional, generational, mobile, national).

% Administers federation rules but interprets them through the sovereignty lens: they enforce that free movement is a negotiable policy right, not a constitutional entitlement, and that national security and labor-market protection justify restrictive border rules. They mediate disputes but defer to member-state authority when conflict arises over admission, credential recognition, or welfare portability.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, supranational_federation_authority, observer,
    institutional, generational, analytical, continental).

% Advocate for open labor-market mobility to strengthen cross-border worker solidarity and wage bargaining power. Under the sovereignty reading, they are excluded from border-rule setting; national labor federations have voice, but transnational labor interest is not seated at the federation negotiation table. They would argue free movement strengthens worker power; they are kept out of the legitimacy conversation.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, regional_labor_unions, excluded,
    organized, biographical, constrained, continental).

% Advocacy groups, human-rights organizations, and transnational NGOs argue that mobility restrictions violate dignity and opportunity equality. Under the sovereignty reading, civil society voices are advisory only; member-state governments control the binding rules. They are systematically excluded from the legitimacy frame that treats national authority as dispositive.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, civil_society_networks, excluded,
    moderate, biographical, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__sovereignty_reading, member_state_governments).
narrative_ontology:fixing_cost_class(federation_membership__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Member states coordinate border and labor-market policy to prevent a race-to-the-bottom in wage floors and social protection. Each state retains the right to set admission rules; the federation provides a framework for negotiating mutual recognition of credentials, temporary work permits, and limited welfare portability. The coordination solves the collective-action problem of preventing one state's permissive labor policy from undercutting another's wage floor.
% TRANSFER_FUNCTION: Moves employment opportunity, welfare benefits, housing access, and social services from mobile and prospective non-citizens to domestic incumbent populations and local labor markets. Restricts the free choice of where to live and work in exchange for a promise of national labor-market protection and welfare-benefit prioritization.
% ABSENT_VOICES: Prospective migrants from outside the federation have no seat; transnational labor unions and human-rights advocates are structurally excluded from border-rule legitimacy. Regional civil society networks that would argue for mobility-as-right are present but only in an advisory capacity, not as co-authors of the rules.
% DISAPPEARANCE_RATIONALE: If national border authority and labor-market gatekeeping vanished overnight, labor and capital would redistribute across member states at the pace preferred by individuals and markets rather than national governments. Wage competition would intensify, incumbent worker protections would erode in some states, and federal-level coordination on welfare and taxation would become urgent. Governments would lose the ability to link citizenship to social benefit and electoral representation.
% FOUNDING_PROBLEM: Post-war federation was built to prevent labor-market dumping and wage collapse during economic integration. Each member state feared uncontrolled migration from lower-wage neighbors would undercut domestic labor standards and welfare sustainability. Border controls were the price of federation: pooled markets required protected local labor markets.
% FOUNDING_PROBLEM_CORROBORATION: Member-state governments attest the founding problem is live, citing wage-pressure evidence and welfare-sustainability concerns. Labor economists in the integration_reading tradition attest the problem is substantially solved by sectoral labor agreements and wage enforcement, and that border controls now protect rents rather than floors. Neither corroboration is from the agenda-setter's own position; both have published economic analysis and legislative testimony.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is 0.68 at the interval end, reflecting substantial value transfer from mobile and non-citizen populations to incumbents and domestic labor markets. Suppression is 0.72 because the constraint's persistence depends on active enforcement: border bureaucracy, credential-recognition delays, family-reunification restrictions, and differential welfare eligibility must be maintained by member-state apparatus. Theater rises from 0.28 to 0.41 over the interval as member states add labor-market-impact assessments and skills-matching reviews, appearing to protect worker interests while the underlying mobility restriction remains in place. Accessibility collapse is moderate (0.58) because alternatives exist in theory—work remotely, seek asylum, petition for exceptions—but are costly and unreliable in practice. Resistance is moderate-high (0.63) because mobile citizens and human-rights organizations actively contest the restriction, litigation is recurring, and political pressure for integration persists. The measurement series tracks enforcement intensification: suppression requirement rises early (t=0 to t=20) as new border infrastructure is built, then plateaus (t=20 onward) as the apparatus becomes routinized. Theater rises throughout as bureaucratic performance increases relative to functional filtering. Extraction growth slows after t=25, suggesting the constraint has reached a stable extortion level where further tightening faces diminishing returns.
 *
 * PERSPECTIVAL GAP:
 *   From the member-state seat (agenda_setter), the constraint is legitimate coordination protecting labor standards and democratic accountability. From the mobile-citizen seat, it is enforced restriction on opportunity. From the incumbent-resident seat, it is protective benefit. From the prospective-migrant seat, it is total exclusion. The engine computes these divergences from the structural data: the same rules produce different directionalities for agents at different power levels with different exit options. The divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Member states are beneficiaries with high exit options (they can alter the rules unilaterally via treaty renegotiation, d near 0.1–0.2); incumbent residents are beneficiaries with constrained exit (they benefit but cannot easily leave without losing the benefit, d near 0.25–0.35); mobile citizens are targets with constrained exit (they bear the cost and cannot easily exit the federation without sacrificing opportunity, d near 0.75–0.85); prospective migrants are targets with trapped exit (total exclusion, d approaching 1.0). The sovereignty reading's structural data—high suppression, active enforcement, dual beneficiary/victim sets—drive these directionalities without authorial intervention. Overrides are not needed because the derivation chain correctly captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled_rope: it has a genuine coordination function (protecting member-state labor markets from undercutting), active enforcement apparatus (border controls, credential gatekeeping, welfare-eligibility rules), identified beneficiaries (domestic labor markets, incumbents), and identified victims (mobile citizens, prospective migrants). The coordination is real—without border controls, member states would face wage pressure and welfare sustainability crises—but the extraction is also real: the protection is maintained at the cost of mobility rights. Neither pure coordination (rope) nor pure extraction (snare) captures the structure. Mandatrophy (the founding problem becoming obsolete while the constraint persists) is a live hypothesis: member states attest wage pressure is live, but economic evidence suggests sectoral labor agreements and wage enforcement have substantially solved the founding problem, and the constraint now protects rents (incumbent employment advantage, higher wages) rather than floors (basic wage floors, welfare sustainability). This mismatch is recorded in the founding_problem_status as 'contested' and in the omega variables as the empirical question about whether founding conditions persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Has wage-floor pressure from cross-border labor migration substantially diminished due to sectoral labor agreements, wage enforcement, and economic convergence—making the original labor-protection justification for border controls obsolete?',
    'Comparative wage analysis across member states controlling for sectoral composition; econometric studies of wage trends pre- and post-labor-standard harmonization; testimony from labor economists and labor unions outside the member-state government apparatus.',
    'If founding problem is dead (wage pressure largely solved by other means), the constraint reclassifies toward snare—pure extraction maintained theatrically as labor protection. If founding problem is live (wage pressure remains substantive), classification holds as tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the original labor-protection rationale for border controls remains valid or has been superseded by other wage-protection mechanisms.').

omega_variable(
    kernel_reading_alternative_framing,
    'If the integration_reading''s premise—that federation membership is irreversible integration grounding supranational free-movement rights—were adopted as the legitimate framing instead of the sovereignty reading, would the measured constraint properties change?',
    'Hypothetical analysis: under integration framing, the same border controls and mobility restrictions would be classified as illegitimate constraint (snare), not legitimate coordination (tangled_rope). The structural data (extraction, suppression, beneficiaries, victims) would not change; only the legitimacy interpretation would shift.',
    'This is a conceptual omega documenting the kernel contest itself. Resolution is not data-dependent but frame-dependent. The engine does not resolve it; it measures both readings'' classifications and records the divergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framing, conceptual, 'The sovereignty and integration readings of federation membership are incommensurable frameworks. No empirical fact will resolve which is correct; the choice is political and philosophical. This omega documents that incommensurability.').

omega_variable(
    suppression_mechanism_internalization,
    'To what extent is the measured suppression (0.72) structural (legal barriers, bureaucratic costs, credential gatekeeping) versus internalized (mobile citizens have come to accept restricted movement as normal, or have fused their identity with their home-state citizenship)?',
    'Post-removal suppression trajectory: if suppression persists after formal removal of border restrictions (e.g., in historical cases where mobility restrictions were eliminated and attitudes shifted slowly), the component is partially internalized. Cross-national surveys of citizenship attachment and mobility aspiration pre- and post-policy-change provide evidence.',
    'If substantial internalization is present, effective suppression is higher than the measured structural 0.72; the constraint carries a psychological component beyond its formal apparatus. Classification stability is unaffected, but the extraction''s true depth is underestimated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether measured suppression is structural enforcement or partially internalized identity-fusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__sovereignty_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t5, federation_membership__sovereignty_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(fede_tr_t5, observed).
narrative_ontology:measurement(fede_tr_t10, federation_membership__sovereignty_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(fede_tr_t10, observed).
narrative_ontology:measurement(fede_tr_t15, federation_membership__sovereignty_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(fede_tr_t15, observed).
narrative_ontology:measurement(fede_tr_t20, federation_membership__sovereignty_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(fede_tr_t20, observed).
narrative_ontology:measurement(fede_tr_t25, federation_membership__sovereignty_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(fede_tr_t25, observed).
narrative_ontology:measurement(fede_tr_t30, federation_membership__sovereignty_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(fede_tr_t30, observed).
narrative_ontology:measurement(fede_tr_t35, federation_membership__sovereignty_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(fede_tr_t35, observed).
narrative_ontology:measurement(fede_tr_t40, federation_membership__sovereignty_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(fede_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__sovereignty_reading, base_extractiveness, 0, 0.51).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t5, federation_membership__sovereignty_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(fede_be_t5, observed).
narrative_ontology:measurement(fede_be_t10, federation_membership__sovereignty_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(fede_be_t10, observed).
narrative_ontology:measurement(fede_be_t15, federation_membership__sovereignty_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(fede_be_t15, observed).
narrative_ontology:measurement(fede_be_t20, federation_membership__sovereignty_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(fede_be_t20, observed).
narrative_ontology:measurement(fede_be_t25, federation_membership__sovereignty_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(fede_be_t25, observed).
narrative_ontology:measurement(fede_be_t30, federation_membership__sovereignty_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(fede_be_t30, observed).
narrative_ontology:measurement(fede_be_t35, federation_membership__sovereignty_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(fede_be_t35, observed).
narrative_ontology:measurement(fede_be_t40, federation_membership__sovereignty_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(fede_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__sovereignty_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t5, federation_membership__sovereignty_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement_basis(fede_su_t5, observed).
narrative_ontology:measurement(fede_su_t10, federation_membership__sovereignty_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(fede_su_t10, observed).
narrative_ontology:measurement(fede_su_t15, federation_membership__sovereignty_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(fede_su_t15, observed).
narrative_ontology:measurement(fede_su_t20, federation_membership__sovereignty_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(fede_su_t20, observed).
narrative_ontology:measurement(fede_su_t25, federation_membership__sovereignty_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(fede_su_t25, observed).
narrative_ontology:measurement(fede_su_t30, federation_membership__sovereignty_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(fede_su_t30, observed).
narrative_ontology:measurement(fede_su_t35, federation_membership__sovereignty_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(fede_su_t35, observed).
narrative_ontology:measurement(fede_su_t40, federation_membership__sovereignty_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(fede_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership__sovereignty_reading, 0.18).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, federation_membership__integration_reading).

% DUAL FORMULATION NOTE:
% The federation_membership kernel admits two readings: sovereignty_reading (this constraint) and integration_reading. They share the same standing arrangement (national border controls, labor-market gatekeeping, differential treatment of citizens and non-citizens) but assign it different ε values and legitimacy frames. Sovereignty reading: ε=0.68, classified as tangled_rope, treats borders as legitimate policy tool. Integration reading: ε would be higher (0.75–0.82), classified as snare, treats borders as illegitimate constraint on supranational right. The readings affect each other: integration_reading's success in reframing federation membership would delegitimize sovereignty_reading's authority grounding (national sovereignty over labor markets would be overridden by supranational free-movement rights). The ε-invariance principle requires separate constraint stories per reading; the readings are not observables of one constraint but alternative constraint definitions sharing a kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
