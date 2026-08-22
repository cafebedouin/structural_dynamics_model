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
 *   human_readable: Member-State Welfare Closure Authority Over Free Movement
 *   domain: political_economy/federalism/migration
 *
 * SUMMARY:
 *   This story instantiates the member_sovereignty_primary reading of the
 *   federation_membership_obligations kernel: national welfare states retain
 *   closure authority over benefit access, and free movement rights are
 *   treated as conditional on labor market protection and the fiscal
 *   sustainability of the receiving state's welfare system. This is NOT a
 *   story about free movement generally — it is the specific claim that
 *   closure authority sits with the member state legislature and
 *   administration, not with EU-level integration norms. The sibling readings
 *   (integration_primary: mobility rights constitute citizenship and override
 *   welfare boundaries; selective_solidarity: access follows contribution
 *   history rather than citizenship) are different constraints with different
 *   beneficiary/victim structures and are not evaluated here.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, 0.58).
domain_priors:suppression_score(federation_membership_obligations__member_sovereignty_primary, 0.52).
domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "Member-State Welfare Closure Authority Over Free Movement").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political_economy/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__member_sovereignty_primary, '7a0cab0a-77e8-4f72-ba76-d32ba8c3d4ef').
narrative_ontology:cs_kernel_codification('7a0cab0a-77e8-4f72-ba76-d32ba8c3d4ef', distributed).
narrative_ontology:cs_authority_grounding('7a0cab0a-77e8-4f72-ba76-d32ba8c3d4ef', distributed).
narrative_ontology:cs_reading_relation('7a0cab0a-77e8-4f72-ba76-d32ba8c3d4ef', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('7a0cab0a-77e8-4f72-ba76-d32ba8c3d4ef', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('7a0cab0a-77e8-4f72-ba76-d32ba8c3d4ef', foundational, national_fiscal_closure_precedes_mobility_right).
narrative_ontology:cs_axiom_status(national_fiscal_closure_precedes_mobility_right, holdable).
narrative_ontology:cs_axiom_grounding('7a0cab0a-77e8-4f72-ba76-d32ba8c3d4ef', national_fiscal_closure_precedes_mobility_right, conventional).
narrative_ontology:cs_axiom('7a0cab0a-77e8-4f72-ba76-d32ba8c3d4ef', secondary, welfare_system_sustainability_justifies_qualifying_periods).
narrative_ontology:cs_axiom_status(welfare_system_sustainability_justifies_qualifying_periods, holdable).
narrative_ontology:cs_axiom_grounding('7a0cab0a-77e8-4f72-ba76-d32ba8c3d4ef', welfare_system_sustainability_justifies_qualifying_periods, instrumental).
narrative_ontology:cs_reference_frame('7a0cab0a-77e8-4f72-ba76-d32ba8c3d4ef', post_maastricht_national_welfare_closure).
narrative_ontology:cs_drift_state('7a0cab0a-77e8-4f72-ba76-d32ba8c3d4ef', post_dano_jurisprudence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7a0cab0a-77e8-4f72-ba76-d32ba8c3d4ef', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, receiving_state_labor_forces).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, national_welfare_administrations).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, mobile_eu_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, long_term_resident_non_contributors).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, cross_border_jobseekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain and exercise statutory veto authority over the terms of welfare access for incoming EU nationals — residence tests, habitual-residence requirements, contribution thresholds, benefit-export rules. They set the closure conditions and can tighten or loosen them through domestic legislation without EU-level pre-clearance, subject only to eventual CJEU review.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures, agenda_setter,
    institutional, generational, arbitrage, national).

% Administer eligibility tests case by case, absorbing fiscal pressure from an aging domestic population while being tasked with excluding claimants whose contribution history or residence duration falls short. They benefit from the closure authority in that it lets them ration a fixed benefit pool, but they also bear the administrative cost of contesting individual claims and litigation risk.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, national_welfare_administrations, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__member_sovereignty_primary, national_welfare_administrations, beneficiary).

% Domestic workers and their unions benefit from labor-market protection clauses that slow or condition the entry of mobile workers into local wage-setting and benefit competition. Their exit option is bounded by their own national labor market; they are not mobile in the way the constraint's targets are, and they experience the closure authority as protective rather than restrictive.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, receiving_state_labor_forces, beneficiary,
    organized, biographical, constrained, national).

% Move across borders under nominal free-movement rights but find welfare access conditioned on habitual residence, minimum contribution periods, or activity status tests they frequently cannot yet satisfy on arrival. Their formal right to move is intact; their substantive right to the receiving state's welfare floor is deferred, means-tested, or denied outright during the qualifying period. Returning home forfeits accrued host-state entitlements in many cases, making exit itself costly.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, mobile_eu_workers, payer,
    moderate, biographical, constrained, continental).

% Have resided for years but fall outside the contributory categories the closure regime privileges — part-time, informal, or care-work histories that do not generate qualifying contribution records. They are structurally excluded from the full welfare beneficiary set even though residence duration would, under an integration-primary reading, qualify them. Domestic reclassification (deportation risk, benefit clawback) makes exit from the host state itself a loss, not a relief.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, long_term_resident_non_contributors, payer,
    powerless, biographical, trapped, national).

% Arrive seeking work under free-movement rights but are excluded from most welfare access until they establish worker status, and can be required to leave if they do not find work within a defined window. They bear the full downside of the sovereignty reading's central premise — that movement rights and welfare rights are decoupled — with essentially no institutional voice in how the qualifying period is set.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, cross_border_jobseekers, payer,
    powerless, immediate, trapped, continental).

% Would press for a more integration-primary reading in which welfare access tracks residence and citizenship rather than national contribution tests, but has no direct authority to override member-state closure decisions absent treaty infringement litigation. Its preferred framing is structurally present in EU law but subordinated in practice to national administrative discretion under this reading.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, european_commission, excluded,
    institutional, generational, analytical, continental).

% Reviews individual challenges to national closure rules on a case-by-case basis (Dano, Alimanovic, Brey line of cases) and can find specific applications disproportionate, but has generally deferred to member states' authority to protect welfare system sustainability in the aggregate — its jurisprudence is itself a site of contest between the readings rather than a settled resolution.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, cjeu, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__member_sovereignty_primary, national_welfare_administrations).
narrative_ontology:fixing_cost_class(federation_membership_obligations__member_sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the fiscal sustainability of national contributory welfare systems against the risk that unconditional free movement would allow claimants to access benefit pools they have not funded, protecting the actuarial basis on which domestic welfare states are built.
% TRANSFER_FUNCTION: Withholds welfare access (means-tested benefits, family allowances, housing support) from mobile workers and long-resident non-contributors during qualifying periods, effectively transferring the fiscal burden of their support back onto themselves, their families, or their state of origin, while preserving the existing benefit pool for domestically-contributing residents.
% ABSENT_VOICES: Mobile workers and long-term non-contributing residents have no seat at the national legislatures that set the closure terms — they are non-voters in the receiving state and structurally underrepresented in the sending state once they have left. The European Commission's integration-primary framing is present in treaty text but subordinated in enforcement practice.
% DISAPPEARANCE_RATIONALE: If member-state closure authority were abolished overnight, welfare eligibility would default to residence or citizenship-based entitlement across the federation; national welfare administrations would lose their principal rationing mechanism, receiving-state labor forces would face increased benefit-pool competition, and several national systems would need emergency fiscal recalibration or EU-level fiscal transfers to remain solvent under the changed eligibility rules.
% FOUNDING_PROBLEM: Free movement was extended across states with radically different welfare generosity and contribution structures without harmonizing the underlying fiscal base; closure authority was retained to prevent the wealthier or more generous welfare states from becoming default destinations for benefit-seeking rather than work-seeking mobility, and to preserve domestic political consent for continued participation in the free-movement regime.
% FOUNDING_PROBLEM_CORROBORATION: National welfare ministries and domestic labor unions attest the fiscal sustainability problem remains live, citing continued asymmetries in benefit generosity and contribution bases across member states. Independent EU-level empirical studies (e.g., Commission-commissioned mobility impact assessments) and migration researchers outside the beneficiary set report that actual benefit-tourism rates are low relative to the scale of the restrictions imposed, suggesting the closure regime now does more protective and political work than the fiscal-sustainability problem alone would justify.
narrative_ontology:disappearance_verdict(federation_membership_obligations__member_sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__member_sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__member_sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_obligations__member_sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__member_sovereignty_primary, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects a genuine coordination function — protecting contributory welfare systems from unfunded claims — combined with asymmetric cost imposition on mobile workers and long-resident non-contributors who bear the qualifying-period exclusion without a proportionate say in setting its terms. Suppression (0.52) is moderate: the mechanism is legal and procedural (residence tests, contribution thresholds) rather than coercive, but it does actively foreclose welfare access that formal free-movement rights would otherwise suggest. Resistance (0.61) is comparatively high because mobile workers, migrant advocacy groups, and the Commission actively litigate and lobby against specific closure applications (the Dano/Alimanovic case line). Accessibility collapse (0.45) is only moderate because CJEU review and treaty-based free movement rights remain a partially live alternative channel, unlike a genuine mountain where no such channel exists.
 *
 * PERSPECTIVAL GAP:
 *   From the national legislature's seat, this is a coordination mechanism protecting a shared, actuarially-bounded welfare pool — closer to a rope. From the mobile worker's seat, the same structure functions as enforced exclusion riding on a formally universal mobility right — closer to a tangled rope or snare, depending on how binding the qualifying period proves in practice. The engine computes these divergent seat-level readings from the structural data; the claimed_type (tangled_rope) reflects the authoring judgment that both a genuine coordination function and asymmetric extraction are simultaneously present.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state legislatures and welfare administrations sit at the beneficiary/agenda-setter end: they design and enforce the qualifying-period rules and capture the fiscal benefit of exclusion. Receiving-state labor forces benefit incidentally from reduced benefit-pool competition. Mobile EU workers, cross-border jobseekers, and long-term non-contributing residents sit at the target end: they bear the transfer (deferred or denied welfare access) with constrained-to-trapped exit, since returning to the sending state often forfeits accrued entitlements and remaining in the receiving state means continued exclusion during the qualifying window.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting contribution-funded welfare systems from unfunded benefit claims under free movement — remains partially live (asymmetric welfare generosity across member states persists), but the empirical scale of actual benefit-tourism is contested and, per independent studies, appears smaller than the scope of restriction imposed. This divergence between founding_problem_status (contested, trending toward diminished) and the persistence of the closure machinery is exactly the tangled_rope signature: enough live coordination function to resist a pure-snare reading, enough asymmetric extraction and inertial persistence to resist a pure-rope reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_reading_kernel_locus,
    'Is the federation_membership_obligations kernel genuinely under-specified across the member_sovereignty_primary, integration_primary, and selective_solidarity readings, or has CJEU case law (Dano, Alimanovic, Brey) already resolved the contest in favor of one reading in practice even though treaty text remains ambiguous?',
    'Systematic review of CJEU jurisprudential trend lines over a longer interval, plus tracking of Commission infringement actions against member states that tighten closure rules — a rising rate of successful infringement actions would indicate integration_primary is displacing member_sovereignty_primary in practice.',
    'If jurisprudence has effectively settled the contest toward integration_primary, this reading''s persistence would itself be evidence of piton-like inertia (administrative practice lagging judicial doctrine) rather than a live, contested reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_reading_kernel_locus, conceptual, 'Whether the kernel contest is genuinely open or already resolved in practice by case law.').

omega_variable(
    benefit_tourism_empirical_scale,
    'What is the actual empirical scale of benefit-motivated (as opposed to work-motivated) intra-EU mobility, relative to the scope of the closure restrictions imposed on all mobile workers?',
    'Longitudinal comparison of mobile-worker benefit claim rates against domestic claim rates, controlling for employment status and residence duration, across multiple member states.',
    'If benefit-motivated mobility is empirically small, the fiscal-sustainability coordination function this reading claims is substantially narrower than the population it restricts, strengthening the case that measured extractiveness undercounts the asymmetry; if benefit-motivated mobility is empirically significant, the coordination function is closer to the scale of restriction and the tangled_rope classification is on firmer ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_tourism_empirical_scale, empirical, 'Whether the fiscal-sustainability rationale is proportionate to the restriction''s actual scope.').

omega_variable(
    reading_selection_signal,
    'The kernel could equally be framed around the treaty text (Article 45 TFEU free movement) or around the political-legitimacy narrative (domestic electorates'' consent to continued EU membership depending on perceived welfare-system protection). Framing around the treaty text favors integration_primary as the textual default with member_sovereignty_primary as a derogation; framing around political legitimacy favors member_sovereignty_primary as the operative constraint regardless of textual default.',
    'This story adopted the political-legitimacy framing because it matches observed administrative practice (qualifying periods are actively enforced, not dormant derogations) rather than textual formalism; a corpus reviewer should check whether textual-default framing would change the coordination_function characterization.',
    'Under textual framing, this reading would appear as an exception requiring justification each time; under political-legitimacy framing (adopted here), it appears as the operative baseline that integration_primary must overcome. This changes which reading bears the burden of proof in commentary, though it does not change the authored ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_signal, conceptual, 'Alternative framings (textual-default vs. political-legitimacy) of which reading is the operative baseline within the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fede_tr_t5, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 5, 0.23).
narrative_ontology:measurement(fede_tr_t10, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 10, 0.26).
narrative_ontology:measurement(fede_tr_t15, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 15, 0.29).
narrative_ontology:measurement(fede_tr_t20, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 20, 0.31).
narrative_ontology:measurement(fede_tr_t25, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 25, 0.32).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fede_be_t5, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(fede_be_t10, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(fede_be_t15, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(fede_be_t20, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(fede_be_t25, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(fede_su_t5, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(fede_su_t10, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(fede_su_t15, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 15, 0.49).
narrative_ontology:measurement(fede_su_t20, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(fede_su_t25, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 25, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__member_sovereignty_primary, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__selective_solidarity).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the federation_membership_obligations kernel. member_sovereignty_primary (this story) authors ε=0.58 with beneficiaries in national welfare administrations and receiving-state labor forces, and victims among mobile/non-contributing workers. integration_primary would author a different ε and an inverted beneficiary/victim set (mobile workers as beneficiaries of an enforced mobility right; receiving-state fiscal systems as the party bearing cost). selective_solidarity would author yet a third structure keyed to contribution history rather than citizenship or national closure authority. All three are linked here per the ε-invariance decomposition principle — they are not the same constraint measured three ways, but three constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
