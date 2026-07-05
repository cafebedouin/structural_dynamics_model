% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Sovereignty-Primary Reading of Free Movement: Member State Consent as Gatekeeper
 *   domain: political economy / federalism / migration policy
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-primary reading of the federation
 *   membership treaty's free-movement kernel: member state consent is the
 *   operative principle, and restrictions on labor mobility are presumptively
 *   legitimate exercises of retained sovereign authority over national labor
 *   markets and welfare systems, subject only to treaty-level oversight that
 *   itself depends on state cooperation. This is one of three readings of the
 *   same kernel (federation_membership_treaty) — the integration_primary
 *   reading treats restrictions as presumptively illegitimate exceptions to a
 *   constitutive mobility right, and the subsidiarity_balance reading treats
 *   mobility as a right bounded by proportionality review rather than
 *   unilateral consent. Each reading is authored as its own constraint with
 *   its own epsilon; this file does not average across them or describe the
 *   contest internally. Under sovereignty_primary, national labor markets and
 *   welfare administrators structurally enter the beneficiary set (they
 *   retain and exercise gatekeeping authority) and mobile workers/jobseekers
 *   structurally enter the victim set (nominal rights gated by discretionary
 *   state action) — this is the expected structural delta for this reading
 *   and is reflected directly in the beneficiary/victim declarations above.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.58).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.52).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Sovereignty-Primary Reading of Free Movement: Member State Consent as Gatekeeper").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political economy / federalism / migration policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, 'a1e64a6f-e9fe-4824-b6bb-ec336331fe3a').
narrative_ontology:cs_kernel_codification('a1e64a6f-e9fe-4824-b6bb-ec336331fe3a', fixed_text).
narrative_ontology:cs_authority_grounding('a1e64a6f-e9fe-4824-b6bb-ec336331fe3a', lineage).
narrative_ontology:cs_interpretation_layer_present('a1e64a6f-e9fe-4824-b6bb-ec336331fe3a').
narrative_ontology:cs_reading_relation('a1e64a6f-e9fe-4824-b6bb-ec336331fe3a', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('a1e64a6f-e9fe-4824-b6bb-ec336331fe3a', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('a1e64a6f-e9fe-4824-b6bb-ec336331fe3a', foundational, member_state_consent_is_constitutive_of_mobility_right).
narrative_ontology:cs_axiom_status(member_state_consent_is_constitutive_of_mobility_right, holdable).
narrative_ontology:cs_axiom_grounding('a1e64a6f-e9fe-4824-b6bb-ec336331fe3a', member_state_consent_is_constitutive_of_mobility_right, conventional).
narrative_ontology:cs_axiom('a1e64a6f-e9fe-4824-b6bb-ec336331fe3a', secondary, national_welfare_solvency_takes_precedence_over_uniform_mobility).
narrative_ontology:cs_axiom_status(national_welfare_solvency_takes_precedence_over_uniform_mobility, holdable).
narrative_ontology:cs_axiom_grounding('a1e64a6f-e9fe-4824-b6bb-ec336331fe3a', national_welfare_solvency_takes_precedence_over_uniform_mobility, instrumental).
narrative_ontology:cs_reference_frame('a1e64a6f-e9fe-4824-b6bb-ec336331fe3a', consent_based_accession_bargain).
narrative_ontology:cs_drift_state('a1e64a6f-e9fe-4824-b6bb-ec336331fe3a', post_enlargement_labor_mobility_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a1e64a6f-e9fe-4824-b6bb-ec336331fe3a', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_labor_market_incumbents).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_welfare_system_administrators).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, domestic_low_wage_workforce).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, mobile_eu_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, cross_border_jobseekers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, labor_short_receiving_sectors).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, member_state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, consent_based_federation_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer transitional restrictions, work-permit quotas, and emergency-brake mechanisms that condition inbound labor mobility on domestic market conditions. They invoke treaty safeguard clauses to slow or block free movement when domestic unemployment or wage pressure is politically salient, and they control the timeline for lifting restrictions.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_labor_ministries, agenda_setter,
    institutional, generational, arbitrage, national).

% Domestic workers and unions in protected sectors benefit from reduced wage competition and slower dilution of collective bargaining leverage. They lobby actively to maintain safeguard clauses and treat restricted mobility as protection of hard-won labor standards.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_labor_market_incumbents, beneficiary,
    organized, biographical, mobile, national).

% Administer eligibility rules that condition access to unemployment benefits, healthcare, and housing support on residency duration or habitual-residence tests. They design these tests explicitly to prevent what they characterize as benefit-driven mobility, preserving the actuarial integrity of nationally-financed welfare pools.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_welfare_system_administrators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__sovereignty_primary, national_welfare_system_administrators, agenda_setter).

% Workers in sectors most exposed to cross-border competition (construction, agriculture, hospitality) experience reduced downward wage pressure when restrictions bind. They have little individual leverage but benefit collectively and diffusely from the constraint's operation.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, domestic_low_wage_workforce, beneficiary,
    moderate, biographical, constrained, national).

% Workers seeking employment across the federation face permit delays, quota caps, and residency thresholds that make relocation slower and riskier than treaty free-movement language promises. Their formal right to move exists but is gated by consent mechanisms they cannot invoke or waive; exit to a non-restricting member state is possible but requires abandoning accumulated local ties, pension credits, or family arrangements.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, mobile_eu_workers, payer,
    powerless, biographical, constrained, continental).

% Unemployed workers attempting to search for work in another member state encounter habitual-residence and benefit-export restrictions that leave them without income support during the job search window, effectively trapping many in their state of origin despite nominal mobility rights.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, cross_border_jobseekers, payer,
    powerless, immediate, trapped, continental).

% Employers in sectors with genuine domestic labor shortages (elder care, seasonal agriculture, specialized trades) are denied timely access to willing cross-border labor because national safeguard mechanisms are calibrated to aggregate political concern rather than sector-specific shortage data. They have no direct voice in triggering or lifting restrictions.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, labor_short_receiving_sectors, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__sovereignty_primary, labor_short_receiving_sectors, excluded).

% The supranational body charged with monitoring treaty compliance can issue opinions and infringement proceedings against restrictive member states but cannot compel free movement over sustained sovereign objection; its enforcement power is itself conditioned on the consent this reading privileges.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, federation_commission_secretariat, observer,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__sovereignty_primary, federation_commission_secretariat, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__sovereignty_primary, national_welfare_system_administrators).
narrative_ontology:fixing_cost_class(federation_membership_treaty__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows member states to join a shared labor mobility framework while retaining a credible mechanism to protect domestic labor markets and welfare systems from sudden demand shocks, making broader federation membership politically sustainable for states with weaker or more exposed economies.
% TRANSFER_FUNCTION: Moves bargaining protection and welfare-system solvency from mobile workers and labor-short sectors to national labor market incumbents and welfare administrators, via delay, quota, and eligibility-gating mechanisms applied at the state's discretion.
% ABSENT_VOICES: Mobile workers and cross-border jobseekers have no institutional seat at the point where safeguard clauses are triggered or renewed — those decisions are made in national ministries responding to domestic political pressure. Labor-short receiving sectors are also absent from the triggering process despite bearing direct costs.
% DISAPPEARANCE_RATIONALE: National labor ministries and welfare administrators would say the world rearranges catastrophically — sudden unmanaged labor flows overwhelming welfare systems and destabilizing domestic labor markets. Mobile workers and the federation secretariat would say the underlying single-market coordination problem persists largely unchanged, since actual displacement effects from free movement are empirically modest in most sectoral studies; what would vanish is the political cover states use to manage domestic anxiety, not a genuine coordination necessity.
% FOUNDING_PROBLEM: Founding member states needed a credible commitment device to accept binding free-movement obligations without exposing weaker welfare systems and labor markets to shocks they could not absorb during accession or downturns — the safeguard/consent architecture was built to make integration domestically sellable.
% FOUNDING_PROBLEM_CORROBORATION: National governments and their labor ministries attest the founding problem remains live, citing periodic surges in intra-federation migration during economic divergence. Independent labor economists and the federation secretariat's own compliance reports attest that empirical displacement and welfare-burden effects have been consistently smaller than safeguard invocations suggest, and that the mechanism now functions more as domestic political insurance than as a response to a live economic threat — this corroboration comes from outside the beneficiary set (secretariat reports and independent academic labor-mobility studies, not from labor ministries themselves).
narrative_ontology:disappearance_verdict(federation_membership_treaty__sovereignty_primary, contested).
narrative_ontology:founding_problem_status(federation_membership_treaty__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_treaty__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__sovereignty_primary, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects a substantial but not overwhelming transfer: safeguard mechanisms genuinely solve a real coordination problem (making integration politically sustainable for exposed states) while also functioning as durable protection for domestic incumbents against mobile competition. Suppression (0.52) is moderate — restrictions are formally bounded by treaty oversight and infringement procedure, but in practice the consent-based architecture means enforcement against a resistant state is slow and politically costly, giving the constraint real coercive bite against mobile workers even though it is not absolute. Theater ratio (0.28) is low-moderate: most safeguard invocation is genuinely operative (permits are actually delayed, quotas actually bind) rather than purely symbolic, though a growing share of invocation is politically performative relative to measured economic need, which the rising trajectory captures. Accessibility collapse (0.40) is moderate — mobile workers retain formal exit to other member states or non-restricting sectors, so alternatives are not fully foreclosed, distinguishing this from a harder snare. Resistance (0.62) is elevated because mobile-worker advocacy groups, receiving-sector employers, and the federation secretariat actively contest safeguard invocations through litigation and political pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   National labor ministries and welfare administrators sit at the beneficiary/agenda-setter end: they hold institutional power, arbitrage-grade exit (they can choose when to invoke or lift restrictions), and directly collect the protective benefit. Domestic labor market incumbents and low-wage workforce are secondary beneficiaries — organized or moderate power, real but indirect benefit, no administrative control. Mobile EU workers and cross-border jobseekers sit at the target end: powerless, constrained-to-trapped exit, and bear the cost of gated access through delay and eligibility exclusion — directionality here is high (near full-target) despite the formal treaty right to move, because the right is conditioned on a consent mechanism they cannot invoke. Labor-short receiving sectors are payers without direct victim status in the classic sense — they bear an efficiency cost rather than a rights-denial cost, reflected in moderate power and constrained rather than trapped exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making binding free-movement obligations domestically sellable to states with exposed labor markets — was live at accession and remains partially live during genuine economic divergence shocks, which prevents a clean 'dead mandate' classification. But the corroboration split (ministries say live; secretariat and independent economists say largely dead) combined with rising extractiveness and theater_ratio over the measured interval suggests the mechanism is drifting from crisis-response tool toward standing political insurance — a classic pattern the tangled_rope classification is built to hold without collapsing into either pure coordination (rope) or pure extraction (snare): the coordination function is real but shrinking relative to the extraction it now sustains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_integration_kernel_location,
    'Is the disagreement between this reading and its integration_primary sibling located in a factual dispute about displacement effects, or in an irreducible normative disagreement about whether consent or constitutive right is the correct foundation for federation membership?',
    'Track whether empirical convergence on displacement-effect magnitude (via long-run panel studies) narrows the political distance between the readings. If ministries continue invoking safeguards at similar rates despite empirical consensus on modest displacement, the disagreement is normative/foundational, not evidentiary.',
    'If the disagreement is purely empirical, the sovereignty_primary reading''s beneficiary/victim structure could shift substantially as evidence accumulates against safeguard necessity, moving this constraint''s classification toward snare. If foundational, the tangled_rope classification is stable regardless of evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_integration_kernel_location, conceptual, 'Whether the sovereignty/integration kernel split is empirically resolvable or normatively irreducible.').

omega_variable(
    safeguard_necessity_measurement,
    'Do current safeguard-clause invocations correspond to genuine, measurable labor-market or welfare-system stress, or have they become decoupled from the economic conditions that originally justified the mechanism?',
    'Compare safeguard invocation timing and duration against independent labor-market stress indicators (unemployment differentials, welfare caseload growth, wage compression data) across multiple invoking states.',
    'If invocations are decoupled from underlying stress, extractiveness is understated and the theater_ratio trajectory should be read as evidence of driftinf toward pure political insurance rather than coordination — supporting eventual reclassification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safeguard_necessity_measurement, empirical, 'Whether safeguard invocation tracks genuine economic necessity or has become politically decoupled.').

omega_variable(
    consent_mechanism_capture,
    'Is the consent-based veto mechanism itself vulnerable to capture by incumbent-favoring domestic political coalitions in ways that were not anticipated at treaty founding?',
    'Trace the political coalitions lobbying for safeguard renewal in each invoking state and compare against the coalition structure present at original treaty negotiation.',
    'If capture has occurred, the beneficiary set should be narrowed from broad national labor-market interest to specific organized incumbent groups, which would push the classification closer to snare for the general public even while remaining tangled_rope for the organized beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_mechanism_capture, empirical, 'Whether the sovereignty-consent mechanism has been captured by narrower interests than its founding justification names.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__sovereignty_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t4, federation_membership_treaty__sovereignty_primary, theater_ratio, 4, 0.17).
narrative_ontology:measurement(fede_tr_t8, federation_membership_treaty__sovereignty_primary, theater_ratio, 8, 0.19).
narrative_ontology:measurement(fede_tr_t12, federation_membership_treaty__sovereignty_primary, theater_ratio, 12, 0.22).
narrative_ontology:measurement(fede_tr_t16, federation_membership_treaty__sovereignty_primary, theater_ratio, 16, 0.24).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__sovereignty_primary, theater_ratio, 20, 0.26).
narrative_ontology:measurement(fede_tr_t24, federation_membership_treaty__sovereignty_primary, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__sovereignty_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fede_be_t4, federation_membership_treaty__sovereignty_primary, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(fede_be_t8, federation_membership_treaty__sovereignty_primary, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(fede_be_t12, federation_membership_treaty__sovereignty_primary, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(fede_be_t16, federation_membership_treaty__sovereignty_primary, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__sovereignty_primary, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(fede_be_t24, federation_membership_treaty__sovereignty_primary, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__sovereignty_primary, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fede_su_t4, federation_membership_treaty__sovereignty_primary, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(fede_su_t8, federation_membership_treaty__sovereignty_primary, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(fede_su_t12, federation_membership_treaty__sovereignty_primary, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(fede_su_t16, federation_membership_treaty__sovereignty_primary, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__sovereignty_primary, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(fede_su_t24, federation_membership_treaty__sovereignty_primary, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__sovereignty_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__sovereignty_primary, 0.12).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the federation_membership_treaty kernel. integration_primary treats free movement as constitutive and restrictions as presumptively illegitimate (likely closer to rope/mountain framing with narrower beneficiary sets); subsidiarity_balance treats mobility as bounded by proportionality (likely a scaffold or milder tangled_rope with more balanced beneficiary/victim symmetry). This sovereignty_primary reading has the widest beneficiary set among the three (national administrators plus domestic incumbents) and the most concentrated victim set (mobile workers with formal-but-gated rights), which is why it computes as the most extraction-heavy of the three siblings under identical treaty text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
