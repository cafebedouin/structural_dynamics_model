% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__member_sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: federation_membership_obligations__member_sovereignty_primary
 *   human_readable: Member State Welfare Closure Authority (Sovereignty-Primary Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint instantiates the member-sovereignty-primary reading of
 *   the federation-membership-obligations kernel. The reading posits that
 *   member state welfare systems, funded through national tax systems and
 *   sustained by democratic consent within member states, retain closure
 *   authority over welfare access. Free movement of labor is permitted for
 *   employment but is conditional on member states' capacity to protect
 *   domestic labor markets and maintain welfare system fiscal sustainability.
 *   Mobile workers are permitted to work across borders but are structurally
 *   excluded from full welfare entitlements in receiving states unless they
 *   acquire permanent residency or work-long prerequisites. This reading is
 *   contested by the integration-primary reading (which claims free movement
 *   is constitutive of federation citizenship and welfare boundaries must
 *   yield) and the selective-solidarity reading (which proposes tiered
 *   mobility rights based on contribution history rather than citizenship).
 *   The authoring seat is the member-sovereignty reading's own framework; the
 *   constraint describes what that reading sees.
 *
 * KEY AGENTS:
 *   - Member state legislatures: institutional stewards of welfare closure authority; they set and enforce eligibility rules but do not directly capture extraction
 *   - Protected domestic workforces: beneficiaries of labor-supply constraint and preferential welfare access
 *   - Mobile workers from other member states: excluded from full welfare entitlements despite working and paying taxes; face bureaucratic barriers and reduced safety-net access
 *   - Cross-border jobseekers: completely excluded from receiving-state labor markets until employment is secured, creating trap dynamics
 *   - Supranational institutions: analytical observers; can issue non-binding rulings but lack enforcement authority against non-compliant member states
 *   - Origin countries: excluded from receiving-state decision-making; their citizens bear the constraint's costs
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
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "Member State Welfare Closure Authority (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__member_sovereignty_primary, '1c886a2b-3eb6-439a-8b7e-d701d823785c').
narrative_ontology:cs_kernel_codification('1c886a2b-3eb6-439a-8b7e-d701d823785c', formalized).
narrative_ontology:cs_authority_grounding('1c886a2b-3eb6-439a-8b7e-d701d823785c', extraction).
narrative_ontology:cs_interpretation_layer_present('1c886a2b-3eb6-439a-8b7e-d701d823785c').
narrative_ontology:cs_reading_relation('1c886a2b-3eb6-439a-8b7e-d701d823785c', federation_membership_obligations__integration_primary, forecloses).
narrative_ontology:cs_reading_relation('1c886a2b-3eb6-439a-8b7e-d701d823785c', federation_membership_obligations__selective_solidarity, coexists_with).
narrative_ontology:cs_axiom('1c886a2b-3eb6-439a-8b7e-d701d823785c', foundational, national_welfare_legitimacy).
narrative_ontology:cs_axiom_status(national_welfare_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('1c886a2b-3eb6-439a-8b7e-d701d823785c', national_welfare_legitimacy, deontological).
narrative_ontology:cs_axiom('1c886a2b-3eb6-439a-8b7e-d701d823785c', foundational, fiscal_sustainability_closure_requirement).
narrative_ontology:cs_axiom_status(fiscal_sustainability_closure_requirement, holdable).
narrative_ontology:cs_axiom_grounding('1c886a2b-3eb6-439a-8b7e-d701d823785c', fiscal_sustainability_closure_requirement, empirically_contingent).
narrative_ontology:cs_reference_frame('1c886a2b-3eb6-439a-8b7e-d701d823785c', national_welfare_democracy_framework).
narrative_ontology:cs_drift_state('1c886a2b-3eb6-439a-8b7e-d701d823785c', contemporary_federation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1c886a2b-3eb6-439a-8b7e-d701d823785c', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, protected_domestic_workforces).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, mobile_workers_from_other_members).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, cross_border_jobseekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain formal authority to set welfare eligibility criteria, residency requirements, and labor market protections. They claim this authority is necessary to preserve fiscal sustainability and democratic accountability for redistribution. They enforce borders through residence-based eligibility, work-permit requirements, and differential benefit structures. The legislative bodies themselves do not collect material benefit but act as stewards of the welfare state's fiscal envelope. They face pressure from supranational institutions to open access, which they resist via refined bureaucratic architecture.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Receive priority in hiring, wage-setting, and access to welfare benefits. Protected through union agreements, minimum-wage enforcement, and welfare eligibility rules that prioritize citizens or long-term residents. They benefit from the constraint because it reduces labor supply competition and ensures welfare resources flow preferentially to them. Their exit options are limited by lack of portability of welfare claims across borders, making them structurally dependent on the constraint's persistence.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, protected_domestic_workforces, beneficiary,
    organized, biographical, constrained, national).

% Face restrictions on welfare access even while working in the receiving state, even when formally permitted to be present for employment. They pay taxes and social contributions but are typically excluded from full welfare entitlements (child allowances, housing assistance, unemployment insurance at citizen tier). They can remain and work, but their exit from the constraint requires either acquiring permanent residency (a discretionary grant controlled by the state) or returning to their home country where welfare claims may not be portable. Their work is legally permitted but their security is structurally precarious.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, mobile_workers_from_other_members, payer,
    moderate, biographical, constrained, regional).

% Attempting to relocate for employment face bureaucratic barriers, residence permit conditions that require pre-arranged employment before entry, and exclusion from unemployment insurance and job-search assistance if they cannot secure work immediately. They are formally excluded from the legislative conversation about welfare boundaries; their exclusion is the enforcement mechanism itself. Their only real exit is to remain in their origin country and accept structural unemployment or lower-wage domestic opportunities.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, cross_border_jobseekers, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__member_sovereignty_primary, cross_border_jobseekers, excluded).

% Monitor the constraint's administration and interpret the federation's foundational texts regarding freedom of movement and non-discrimination. They can issue rulings that narrow member state closure authority, but enforcement depends on political will and the economic leverage of the member state in question. They are not party to the welfare transfer but sit as arbiters of the boundary between member sovereignty and supranational rights. Their rulings have repeatedly required member states to narrow explicit closure while achieving equivalent exclusion through refined bureaucratic means.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, supranational_institutions, observer,
    institutional, generational, analytical, continental).

% Would prefer fewer restrictions on their citizens' access to welfare in receiving states, both because remittance income depends on worker stability and because they bear the fiscal burden of non-portable welfare claims (citizens returning without accumulated benefits). They are institutionally excluded from the receiving state's legislative process; their voice is heard only through collective supranational mechanisms or through retaliatory trade/migration restrictions. Their citizens are the targets of the constraint but their governments have limited leverage to challenge it.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, origin_countries_of_mobile_workers, excluded,
    powerful, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures).
narrative_ontology:fixing_cost_class(federation_membership_obligations__member_sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables the federation to function as a partial economic union (labor mobility, trade) while preserving member state fiscal autonomy over redistributive welfare systems. The constraint coordinates two conflicting goals: allowing labor markets to clear across borders while preventing fiscal competition where member states undercut each other's welfare provisions or face overwhelming redistribution burdens.
% TRANSFER_FUNCTION: Channels welfare entitlements and labor market protections from mobile workers (and their receiving-state employers) to protected domestic workforces. The transfer occurs via: (1) reduced labor supply competition for citizens, (2) preferential access to public-sector jobs, (3) differential welfare eligibility that reserves full benefits for citizens and long-term residents, and (4) administrative overhead that cross-border jobseekers must bear.
% ABSENT_VOICES: Mobile workers have no voting representation in the receiving state legislatures that set welfare eligibility. Cross-border jobseekers are structurally excluded from labor market participation itself, creating a Catch-22 (must have a job to immigrate, but cannot immigrate to search for a job). Origin countries' legislatures would advocate for open welfare access to their citizens but are absent from the receiving state's decision-making. Supranational courts can overturn specific exclusions but lack enforcement power against non-compliant member states.
% DISAPPEARANCE_RATIONALE: If member state welfare closure authority vanished overnight, mobile workers would gain immediate access to full welfare benefits, labor market protections would lose their effectiveness (wage compression from unlimited labor supply would occur rapidly), member states would face fiscal crises as welfare expenditure rose without corresponding revenue increases, and the federation itself might fracture as high-redistribution states saw unsustainable migration and low-redistribution states faced exit pressure. The constraint's disappearance would force a renegotiation of the federation's basic structure — either toward supranational welfare provision or toward restricted mobility.
% FOUNDING_PROBLEM: Early federation-building faced a coordination dilemma: member states' welfare systems are funded by tax revenues and political consent tied to national constituencies. Without closure authority, high-welfare states face incentives to reduce benefits (competitive downward pressure) or face fiscal insolvency. Without cross-border labor mobility, single markets cannot clear, and labor productivity gains are lost. The founding problem was how to open labor markets while preserving member states' fiscal autonomy over redistribution.
% FOUNDING_PROBLEM_CORROBORATION: Member state legislatures and labor unions attest the founding problem remains live: welfare sustainability requires closure authority. Supranational institutions and integration-primary advocates attest the problem is solved at the federation level by open mobility and market integration. Independent economic analyses support both readings: some show welfare-state viability under closure, others show efficiency gains from open mobility that could sustain welfare if redistributed. No single external corroborator settles the dispute.
narrative_ontology:disappearance_verdict(federation_membership_obligations__member_sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__member_sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__member_sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is measured at 0.68 (interval end) because the constraint transfers substantive welfare benefits and labor-market position from mobile workers to protected domestic workforces, despite the mobility rights being formally permitted. The transfer is not coercive in the classical sense (mobile workers can legally work) but is enforced through bureaucratic exclusion from welfare entitlements and administrative complexity that mobile workers must bear. Suppression is high (0.71) because the constraint's persistence depends on actively maintaining barriers to cross-border jobseeking and welfare access, not on participant voluntary coordination. The suppression requirement rose monotonically from 0.48 to 0.71 over the interval: as migration pressure increased (EU eastern enlargement 2004, economic crises 2008-2015), member states implemented more complex bureaucratic machinery to maintain closure (residence-duration requirements, employability tests, work-permit screening). Theater is moderate (0.42) and rising: the constraint is justified via sustainability rhetoric and labor-market protection language, but supranational court rulings have repeatedly required member states to narrow their closure authority, forcing refinement of the bureaucratic architecture to achieve the same exclusionary effect while appearing to comply with non-discrimination rules. The measurement series share one time grid (interval start 1995, end 2025) with all three metrics authored at the same time points.
 *
 * PERSPECTIVAL GAP:
 *   The member-state-legislature seat and the mobile-worker seat should compute very differently. From the legislature's perspective, the constraint solves a genuine problem (welfare sustainability under open migration) and is justified as defensive coordination. From the mobile-worker's perspective, the same structure is enforced extraction: they are permitted to contribute labor and taxes but excluded from the risk-pooling that gives the constraint its legitimacy. The supranational institutional seat sits in between, able to see both logics but unable to enforce either. The engine's per-seat computation will reflect this: the legislature will likely compute the constraint as rope-like (genuine coordination with selective enforcement), while mobile workers will compute it as snare-like (enforcement without offsetting benefit).
 *
 * DIRECTIONALITY LOGIC:
 *   Member state legislatures (institutional, constrained exit) are the agenda-setters: they unilaterally set welfare eligibility and labor rules, and they claim justification in fiscal sustainability. Their directionality is near beneficiary (d ≈ 0.2): they do not personally collect the extraction, but they administer the system that produces it and maintain it against supranational pressure to open. Protected domestic workforces (organized, constrained exit) are clear beneficiaries (d ≈ 0.1): they receive reduced labor-supply competition and preferential welfare access. Mobile workers (moderate power, constrained exit) are targets (d ≈ 0.75): they bear the costs of bureaucratic exclusion and reduced welfare access despite working and paying taxes. Cross-border jobseekers (powerless, trapped exit) are the deepest targets (d ≈ 0.95): they are completely excluded from the labor market itself unless they pre-secure employment, a barrier that is structurally impenetrable. Supranational institutions (institutional, analytical exit) sit near symmetric (d ≈ 0.5): they have formal authority to review the constraint but lack enforcement power, placing them in an ambiguous structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint faces mandatrophy pressure because the founding problem is contested. The member-sovereignty reading was built to solve the problem of welfare sustainability under open migration; that problem was genuine in 1995-2005 when member states faced fiscal stress from low-income migration. However, by 2015-2025, the founding problem's status shifted: economic development in origin countries reduced migration pressure, EU growth strategies shifted toward labor-shortage narratives, and demographic decline in receiving states created labor-demand conditions that contradicted the labor-supply-protection justification. The constraint persists not because the founding problem is live but because the institutional machinery (welfare bureaucracy, immigration control apparatus) has become self-perpetuating and because political constituencies (protected workforces, right-wing nationalist movements) have made welfare closure a symbol of national sovereignty independent of its original fiscal function. The theater-ratio rise (0.22 to 0.42 over the interval) signals this drift: the constraint increasingly operates through performative compliance (member states appearing to obey supranational rulings while achieving equivalent exclusion through refined bureaucratic architecture) rather than through direct labor-market protection. A strong mandatrophy signal would be: founding problem (welfare sustainability under open migration) status=dead; disappearance verdict=world_rearranges; yet constraint persists without amendment. This story declares that tension via the contested founding_problem_status and the high theater_ratio trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_sustainability_empirical_premise,
    'Can member state welfare systems sustain themselves under unrestricted free movement, if coupled with fiscal redistribution mechanisms at the federation level?',
    'Comparative case study of federation-level welfare provision (Canada, Australia, US) vs. pure member-state provision (EU current model); economic modeling of hypothetical federation-level welfare floor.',
    'If member states can sustain welfare without closure authority (through federation-level redistribution), the constraint''s justification collapses and the member-sovereignty reading becomes exposed as rent-seeking. If closure is empirically necessary, the reading''s core claim is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_sustainability_empirical_premise, empirical, 'Whether welfare sustainability requires member-state closure authority or whether federation-level redistribution could substitute.').

omega_variable(
    democratic_accountability_vs_supranational_governance,
    'Is welfare redistribution legitimated by democratic consent within member-state electorates, or can it be legitimated through supranational democratic processes (federation-wide voting on welfare access)?',
    'Political-philosophy arguments from democratic theory; empirical observation of whether supranational welfare provision (when it exists) generates equivalent legitimacy to member-state provision.',
    'If member-state democracy is the only legitimate basis, the constraint is justified and integration_primary''s claims are illegitimate. If supranational democracy can provide equivalent legitimacy, the constraint becomes a choice for closure rather than a structural necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_accountability_vs_supranational_governance, conceptual, 'The locus of democratic legitimacy for redistribution (member-state vs. supranational).').

omega_variable(
    labor_market_closure_effectiveness,
    'Does welfare-closure actually protect domestic labor market positions, or do protected workforces lose those gains through other mechanisms (capital flight, wage stagnation, unemployment)?',
    'Empirical labor-market analysis comparing protected and non-protected periods; natural experiments from member states that opened welfare access and measured employment/wage effects on domestic workers.',
    'If closure effectively protects domestic workforces, the constraint''s coordination rationale is validated. If gains are lost to other mechanisms, the constraint becomes theater protecting only symbolic national control without real economic benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_closure_effectiveness, empirical, 'Whether welfare closure delivers measurable labor-market protection to domestic workforces.').

omega_variable(
    structural_vs_internalized_suppression,
    'Is the measured suppression of cross-border jobseeking structural (bureaucratic barriers, legal exclusion) or internalized (jobseekers have internalized the belief that they cannot immigrate and no longer attempt)?',
    'Post-policy change tracking: if member states removed bureaucratic barriers and jobseekers rapidly attempted to immigrate, suppression was structural; if removal had little effect, suppression was partially internalized.',
    'If suppression is mainly structural, it is removable by legislative action and the constraint is more extractive than it appears (barriers create artificial scarcity). If partly internalized, the constraint persists even if formal barriers fall, and the effective suppression is higher than structural measures alone show.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Whether suppression of cross-border jobseeking is structural (external barriers) or internalized (psychological/cultural adaptation).').

omega_variable(
    kernel_reading_foreclosure_test,
    'Does the member-sovereignty reading''s core claim (welfare closure is necessary for sustainability and democracy) logically foreclose the integration-primary reading''s core claim (free movement is constitutive of federation citizenship)?',
    'Philosophical analysis: can a single federation simultaneously hold that closure is necessary for sustainability AND that unrestricted access is constitutive? If yes, the readings coexist; if no, foreclosure exists.',
    'If foreclosure exists, the member-sovereignty reading and integration-primary reading cannot coexist in a single framework, and the federation must choose. If they coexist, both readings remain live policy options held by different coalitions, and the constraint''s classification may differ by political seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_test, conceptual, 'Whether the member-sovereignty and integration-primary readings are logically incompatible (foreclosure) or can coexist as competing policy positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1995, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(fede_tr_t2000, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(fede_tr_t2005, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2005, 0.33).
narrative_ontology:measurement(fede_tr_t2010, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(fede_tr_t2015, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(fede_tr_t2020, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(fede_tr_t2025, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(fede_be_t1995, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(fede_be_t2000, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(fede_be_t2005, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement(fede_be_t2010, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(fede_be_t2015, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(fede_be_t2020, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(fede_be_t2025, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1995, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement(fede_su_t2000, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2000, 0.56).
narrative_ontology:measurement(fede_su_t2005, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2005, 0.63).
narrative_ontology:measurement(fede_su_t2010, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2010, 0.67).
narrative_ontology:measurement(fede_su_t2015, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(fede_su_t2020, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(fede_su_t2025, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__member_sovereignty_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__member_sovereignty_primary, 0.18).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__selective_solidarity).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, domestic_labor_market_wage_compression).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, welfare_state_fiscal_crisis).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, supranational_regulatory_capture).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the federation-membership-obligations kernel. The integration_primary and selective_solidarity readings instantiate the same kernel from different policy stances. The three stories should be read as a constraint family: all three describe aspects of the same institutional tension (free labor mobility vs. welfare system closure), but each reading produces a different structural classification and different beneficiary/victim set. The member-sovereignty reading forecloses integration_primary's claim that unrestricted access is constitutive of federation citizenship; both readings coexist as live policy positions held by different political coalitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__member_sovereignty_primary, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
