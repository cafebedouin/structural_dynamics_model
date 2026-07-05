% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__developmental_state_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__developmental_state_reading, []).

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
 *   constraint_id: flexible_employment_legitimacy__developmental_state_reading
 *   human_readable: Flexible Employment as State-Managed Transition to Formalization (Developmental State Reading)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This story instantiates the developmental_state_reading of the
 *   flexible_employment_legitimacy kernel: the claim that flexible platform
 *   employment is a deliberate, state-managed transitional stage on the way
 *   to formal employment, legitimated by a phased 12-point plan targeting
 *   full standardization by 2027. Under this reading, current worker
 *   precarity is the acceptable cost of a sequenced institutional build-out,
 *   not the terminus of the arrangement. This is distinct from the
 *   market_efficiency_reading (which denies any need for transition because
 *   flexibility is already the efficient endpoint) and the
 *   precarity_extraction_reading (which denies the transition is real and
 *   reads the same facts as durable extraction dressed in developmental
 *   language). Each reading is authored as its own constraint with its own ε;
 *   this file does not average across them or describe the contest internally
 *   — only this reading's structure is modeled here, per the ε-invariance
 *   principle.
 *
 * KEY AGENTS:
 *   - state_labor_ministry: architect and enforcer of the roadmap — institutional/analytical exit
 *   - gig_workers_awaiting_formalization: bear interim costs of transitional status — powerless/constrained
 *   - platform_workers_transitioning_to_formal_status: early beneficiaries of completed tranches — moderate/constrained
 *   - domestic_platform_firms_compliant_with_roadmap: invest early for competitive and regulatory advantage — organized/constrained
 *   - small_platform_operators_facing_compliance_costs: absorb disproportionate compliance burden — moderate/trapped
 *   - national_social_insurance_funds: long-run fiscal beneficiary of formalization — institutional/analytical
 *   - international_labor_standards_bodies: analytical observer comparing this roadmap to prior transitions elsewhere
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, 0.48).
domain_priors:suppression_score(flexible_employment_legitimacy__developmental_state_reading, 0.42).
domain_priors:theater_ratio(flexible_employment_legitimacy__developmental_state_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__developmental_state_reading, scaffold).
narrative_ontology:human_readable(flexible_employment_legitimacy__developmental_state_reading, "Flexible Employment as State-Managed Transition to Formalization (Developmental State Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__developmental_state_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__developmental_state_reading).
narrative_ontology:has_sunset_clause(flexible_employment_legitimacy__developmental_state_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__developmental_state_reading, '8a14cf27-1fe8-4f82-8c46-15a9cc78df05').
narrative_ontology:cs_kernel_codification('8a14cf27-1fe8-4f82-8c46-15a9cc78df05', formalized).
narrative_ontology:cs_authority_grounding('8a14cf27-1fe8-4f82-8c46-15a9cc78df05', extraction).
narrative_ontology:cs_interpretation_layer_present('8a14cf27-1fe8-4f82-8c46-15a9cc78df05').
narrative_ontology:cs_reading_relation('8a14cf27-1fe8-4f82-8c46-15a9cc78df05', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a14cf27-1fe8-4f82-8c46-15a9cc78df05', flexible_employment_legitimacy__precarity_extraction_reading, influences).
narrative_ontology:cs_axiom('8a14cf27-1fe8-4f82-8c46-15a9cc78df05', foundational, state_sequencing_capacity_legitimates_interim_burden).
narrative_ontology:cs_axiom_status(state_sequencing_capacity_legitimates_interim_burden, holdable).
narrative_ontology:cs_axiom_grounding('8a14cf27-1fe8-4f82-8c46-15a9cc78df05', state_sequencing_capacity_legitimates_interim_burden, instrumental).
narrative_ontology:cs_axiom('8a14cf27-1fe8-4f82-8c46-15a9cc78df05', foundational, formalization_is_the_stable_endpoint_not_flexibility).
narrative_ontology:cs_axiom_status(formalization_is_the_stable_endpoint_not_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('8a14cf27-1fe8-4f82-8c46-15a9cc78df05', formalization_is_the_stable_endpoint_not_flexibility, empirically_contingent).
narrative_ontology:cs_reference_frame('8a14cf27-1fe8-4f82-8c46-15a9cc78df05', developmental_state_capacity_framework).
narrative_ontology:cs_drift_state('8a14cf27-1fe8-4f82-8c46-15a9cc78df05', pre_2027_target_year, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('8a14cf27-1fe8-4f82-8c46-15a9cc78df05', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, platform_workers_transitioning_to_formal_status).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, state_labor_ministries).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, domestic_platform_firms_compliant_with_roadmap).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, national_social_insurance_funds).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, gig_workers_awaiting_formalization).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, small_platform_operators_facing_compliance_costs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, gig_workers_awaiting_formalization).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, domestic_platform_firms_compliant_with_roadmap).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__developmental_state_reading, developmental_state_capacity_doctrine).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__developmental_state_reading, phased_formalization_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the 12-point roadmap toward the 2027 standardization target, sets phased milestones for converting flexible arrangements into recognized employment categories with benefits, and negotiates compliance timelines with platform firms. Frames the current flexible regime explicitly as transitional, not terminal, and stakes its own institutional credibility on hitting the 2027 target.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, state_labor_ministry, agenda_setter,
    institutional, generational, analytical, national).

% Work under current flexible arrangements without full benefits or job security while the state's phased plan matures. Bear the cost of the interim period — no employer-provided insurance, unpredictable income, no severance protection — on the state's promise that formalization is coming on a fixed schedule. Cannot individually accelerate their own transition; must wait on the roadmap's timeline.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, gig_workers_awaiting_formalization, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__developmental_state_reading, gig_workers_awaiting_formalization, beneficiary).

% Workers in sectors or platforms already moved onto the early tranches of the roadmap, now receiving portable benefits, minimum earnings guarantees, or reclassified status ahead of the 2027 deadline. Their situation is the proof-of-concept the ministry points to when defending the transition framing.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, platform_workers_transitioning_to_formal_status, beneficiary,
    moderate, biographical, constrained, national).

% Invest in compliance infrastructure — payroll systems, benefit administration, reporting — ahead of competitors, betting that early alignment with the roadmap yields regulatory goodwill, procurement preference, and reduced future penalty exposure. Bear near-term cost but gain a seat at the table shaping how the 12-point plan's remaining milestones are defined.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, domestic_platform_firms_compliant_with_roadmap, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__developmental_state_reading, domestic_platform_firms_compliant_with_roadmap, payer).

% Lack the administrative capacity of larger platforms to absorb phased compliance costs. Face closure or forced consolidation as the roadmap's milestones tighten, since they cannot spread fixed compliance overhead across large worker pools the way dominant platforms can.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, small_platform_operators_facing_compliance_costs, payer,
    moderate, biographical, trapped, national).

% Stand to gain a broadened contributor base once flexible workers are formalized and begin paying into pension and health insurance systems. Their long-run solvency projections are built into the state's justification for the 2027 target.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, national_social_insurance_funds, beneficiary,
    institutional, civilizational, analytical, national).

% Monitor whether the state's phased roadmap represents genuine developmental sequencing comparable to prior successful formalization drives, or whether the transitional framing has become a standing justification for postponing worker protections indefinitely. Publish comparative assessments cited by both the ministry and its critics.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, international_labor_standards_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__developmental_state_reading, diffuse).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__developmental_state_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sequences an economy-wide shift from informal, flexible platform work to formal employment status in phases the state and firms can administratively absorb, avoiding a disruptive one-shot mandate that could collapse platform employment overnight.
% TRANSFER_FUNCTION: In the interim period, moves benefit and security costs from platforms and the state onto workers currently classified as flexible; as tranches convert, moves contribution revenue toward social insurance funds and compliance costs toward platforms.
% ABSENT_VOICES: Gig workers in later-scheduled tranches have no seat in setting the roadmap's sequencing — the order in which sectors formalize is negotiated between the ministry and organized platform firms, not workers awaiting their turn.
% DISAPPEARANCE_RATIONALE: The ministry and social insurance funds would say the world rearranges sharply — years of phased infrastructure-building collapse and formalization reverts to ad hoc negotiation. Workers still awaiting formalization, and critics who read the roadmap as a stalling mechanism, would say little changes for them either way since the promised transition has not yet reached their tranche.
% FOUNDING_PROBLEM: A large and growing share of the workforce operates outside formal employment categories, generating labor income the state cannot easily tax, protect, or count, while workers in these arrangements lack benefits, and firms face regulatory uncertainty over classification.
% FOUNDING_PROBLEM_CORROBORATION: The ministry attests the problem is live and the roadmap is progressing on schedule. Independent labor economists and the international labor standards bodies note that similar transitional roadmaps in comparable economies have repeatedly slipped their sunset dates, and that some tranches show the classification problem persisting a decade after initial 'transitional' framing was adopted — corroboration from outside the ministry's own progress reporting is mixed rather than confirming.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__developmental_state_reading, contested).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__developmental_state_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__developmental_state_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__developmental_state_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__developmental_state_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__developmental_state_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__developmental_state_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.48, declining slightly over the interval as tranches convert — reflecting the developmental_state_reading's core claim that the current burden is diminishing on a schedule, not accumulating. Theater ratio is authored rising from 0.30 to 0.40: as the 2027 deadline approaches without full completion, an increasing share of the ministry's visible activity (progress reports, milestone ceremonies, roadmap revisions) functions to reaffirm the transition narrative rather than to move workers into formal status. Suppression is modest and rising slowly (0.38 to 0.42), consistent with a state increasingly relying on administrative and legal enforcement of the phased sequence — including restrictions on which sectors or firms can 'jump the queue' — as pressure to accelerate mounts. Accessibility collapse is moderate (0.35): the roadmap does not foreclose alternative formalization paths, but it does channel political and administrative attention toward its own sequencing, crowding out competing reform proposals.
 *
 * PERSPECTIVAL GAP:
 *   From the ministry's analytical seat, the arrangement is a coordinated, time-bound institutional build sequenced for administrative feasibility. From the seat of gig workers awaiting a later tranche, the same structure presents as an open-ended deferral with no individual leverage to accelerate it. The engine should compute these seats differently given their divergent power, exit options, and time horizons — the developmental_state_reading's legitimacy rests on whether the ministry's declared schedule is honored, which is a fact about the future this story cannot resolve, only flag via omega.
 *
 * DIRECTIONALITY LOGIC:
 *   The state labor ministry sits at the clear agenda-setting position with the lowest directionality toward extraction — it authors and controls the sequencing that defines what counts as 'on schedule.' Workers awaiting formalization bear the highest directionality: they pay the interim cost (foregone benefits, income volatility) with constrained exit, since leaving flexible work into a genuinely formal job depends on tranche timing they do not control. Compliant platform firms occupy a dual position — payers of near-term compliance cost, beneficiaries of first-mover regulatory standing — which is why they carry both beneficiary and payer roles. Small operators are trapped rather than merely constrained: they lack the scale to spread compliance costs and face exit only through closure or acquisition, which the roadmap's schedule does not accommodate.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification is conditioned entirely on the sunset clause being real: the 2027 standardization target must actually terminate the transitional arrangement, not merely restate it. The founding_problem_status is authored as contested precisely because this is unresolved — the ministry's own reporting cannot corroborate its own sunset credibly. If 2027 arrives and the roadmap is extended rather than completed (the pattern the international labor standards bodies flag as common in comparable economies), the classification would need to be re-examined: a scaffold whose sunset repeatedly rolls forward is functioning as a tangled_rope or piton in practice, regardless of its developmental_state framing. This is exactly the divergence the classification exists to surface, not to pre-empt.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sunset_credibility_ambiguity,
    'Will the 2027 standardization target actually terminate the transitional flexible-employment regime, or will it be extended/restated as prior comparable roadmaps have been?',
    'Track whether the 2027 deadline passes with completed formalization of the scheduled tranches, or whether the ministry issues a revised roadmap/extended timeline; compare against the historical base rate of sunset-clause extension in comparable developmental labor-formalization programs cited by international labor standards bodies.',
    'If the target is honored, the scaffold classification is vindicated as a genuine transitional structure. If extended or restated, the constraint functions as a tangled_rope (coordination narrative persisting alongside continued extraction) or piton (transition machinery maintained theatrically after its function has stalled) rather than a true scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_credibility_ambiguity, empirical, 'Whether the 2027 sunset clause will actually terminate the transitional regime or be extended.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the developmental_state_reading the correct framing of flexible employment''s legitimacy claim, or do the market_efficiency_reading and precarity_extraction_reading better capture the same observed facts (declining but nonzero extraction, rising theater ratio, rising enforcement)?',
    'Compare this story''s classification and metric trajectory against the sibling readings'' independently-authored stories; examine whether independent labor-market outcomes (wage growth, benefit coverage rates, platform firm profitability) track the developmental_state_reading''s predicted managed-transition pattern rather than a market-clearing pattern or a stable-extraction pattern.',
    'If the precarity_extraction_reading''s metrics better fit observed wage and benefit stagnation despite the roadmap''s stated progress, the developmental framing functions as legitimating cover for continued extraction rather than an accurate account of what is occurring.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which of the three kernel readings best fits the observed labor-market trajectory; this story commits to the developmental_state framing but the fit is contestable.').

omega_variable(
    administrative_capacity_ambiguity,
    'Does the state labor ministry possess the genuine administrative capacity to execute the 12-point plan on schedule, or is the plan''s detail a performance of capacity the ministry does not actually have?',
    'Audit staffing, budget allocation, and inter-agency coordination mechanisms behind each of the 12 points against comparable state capacity benchmarks for successful formalization programs elsewhere.',
    'If capacity is genuinely present, the rising theater_ratio reflects legitimate communication of real progress; if capacity is largely absent, the rising theater_ratio reflects a widening gap between roadmap rhetoric and administrative reality, supporting a downgrade toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_capacity_ambiguity, empirical, 'Whether the ministry''s administrative capacity matches the ambition of the 12-point plan.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__developmental_state_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(flex_tr_t2, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 2, 0.32).
narrative_ontology:measurement(flex_tr_t4, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(flex_tr_t6, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 6, 0.37).
narrative_ontology:measurement(flex_tr_t8, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 8, 0.39).
narrative_ontology:measurement(flex_tr_t10, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(flex_be_t2, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 2, 0.53).
narrative_ontology:measurement(flex_be_t4, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(flex_be_t6, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 6, 0.49).
narrative_ontology:measurement(flex_be_t8, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 8, 0.485).
narrative_ontology:measurement(flex_be_t10, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(flex_su_t2, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 2, 0.39).
narrative_ontology:measurement(flex_su_t4, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(flex_su_t6, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 6, 0.41).
narrative_ontology:measurement(flex_su_t8, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 8, 0.415).
narrative_ontology:measurement(flex_su_t10, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__developmental_state_reading, resource_allocation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__precarity_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the flexible_employment_legitimacy kernel, decomposed per the ε-invariance principle because the three readings assign structurally different extraction values and beneficiary/victim sets to what is colloquially called 'the flexibility debate.' developmental_state_reading (this file) authors moderate, declining extraction under a scaffold claim with a real sunset condition. market_efficiency_reading would author low extraction under a rope or mountain-adjacent claim (flexibility as efficient market clearing, no transition needed). precarity_extraction_reading would author high, non-declining extraction under a snare or tangled_rope claim (flexibility as durable extraction mechanism, transition language as cover). All three should link to each other via affects_constraints; none should be treated as more 'true' than the others at the schema level — the divergence between their computed classifications is itself the analytical product.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
