% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__principled_intervention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__principled_intervention_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: constitutional_secularism__principled_intervention_reading
 *   human_readable: Constitutional Secularism — Principled State Intervention in Religious Affairs
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This story instantiates the 'principled intervention' reading of the
 *   constitutional-secularism kernel: the state may reach into religious
 *   affairs where doing so advances social reform and protects weaker
 *   sections within a religious community, distinguishing 'essential'
 *   religious practice (protected) from 'social' or 'secular' accretions
 *   (reformable). This is a genuine coordination function — it corrects
 *   entrenched intra-community harms that internal governance would not
 *   remedy — but it operates through active judicial and legislative
 *   enforcement, names identifiable payers (denominations and their
 *   leadership who lose institutional authority over reclassified practices),
 *   and creates a standing risk that the essential-practices line is drawn
 *   along majoritarian lines. This is distinct from the strict-neutrality
 *   reading, which forecloses state interference altogether, and from the
 *   reformist reading, which grants the state a stronger affirmative duty
 *   overriding religious autonomy outright. Do not read this story as
 *   covering those readings; each is a separate constraint with its own ε.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, 0.42).
domain_priors:suppression_score(constitutional_secularism__principled_intervention_reading, 0.38).
domain_priors:theater_ratio(constitutional_secularism__principled_intervention_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__principled_intervention_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__principled_intervention_reading, "Constitutional Secularism — Principled State Intervention in Religious Affairs").
narrative_ontology:topic_domain(constitutional_secularism__principled_intervention_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__principled_intervention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__principled_intervention_reading, 'dd25e51d-e962-497c-a8c7-308c31cafc5a').
narrative_ontology:cs_kernel_codification('dd25e51d-e962-497c-a8c7-308c31cafc5a', formalized).
narrative_ontology:cs_authority_grounding('dd25e51d-e962-497c-a8c7-308c31cafc5a', lineage).
narrative_ontology:cs_interpretation_layer_present('dd25e51d-e962-497c-a8c7-308c31cafc5a').
narrative_ontology:cs_reading_relation('dd25e51d-e962-497c-a8c7-308c31cafc5a', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('dd25e51d-e962-497c-a8c7-308c31cafc5a', constitutional_secularism__reformist_reading, influences).
narrative_ontology:cs_axiom('dd25e51d-e962-497c-a8c7-308c31cafc5a', foundational, state_may_condition_religious_autonomy_on_reform_justification).
narrative_ontology:cs_axiom_status(state_may_condition_religious_autonomy_on_reform_justification, holdable).
narrative_ontology:cs_axiom_grounding('dd25e51d-e962-497c-a8c7-308c31cafc5a', state_may_condition_religious_autonomy_on_reform_justification, conventional).
narrative_ontology:cs_axiom('dd25e51d-e962-497c-a8c7-308c31cafc5a', foundational, essential_practices_carve_out_preserves_core_religious_autonomy).
narrative_ontology:cs_axiom_status(essential_practices_carve_out_preserves_core_religious_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('dd25e51d-e962-497c-a8c7-308c31cafc5a', essential_practices_carve_out_preserves_core_religious_autonomy, instrumental).
narrative_ontology:cs_reference_frame('dd25e51d-e962-497c-a8c7-308c31cafc5a', constitutionally_bounded_religious_autonomy).
narrative_ontology:cs_drift_state('dd25e51d-e962-497c-a8c7-308c31cafc5a', contemporary_social_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dd25e51d-e962-497c-a8c7-308c31cafc5a', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__principled_intervention_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, intra_community_reform_beneficiaries).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, state_judicial_and_legislative_authorities).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, religious_denominations_subject_to_intervention).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, orthodox_religious_leadership).
narrative_ontology:constraint_vindicates(constitutional_secularism__principled_intervention_reading, essential_practices_doctrine).
narrative_ontology:constraint_vindicates(constitutional_secularism__principled_intervention_reading, social_reform_supremacy_over_religious_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts and legislatures determine which religious practices count as 'essential' versus 'reformable,' and enact statutes overriding religious governance structures (temple entry, personal law reform, trust administration) in the name of social welfare. They administer the intervention power and set its boundaries case by case, which gives them ongoing discretion over the line between religion and reform.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, state_judicial_and_legislative_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Historically excluded or oppressed members within a religious community — lower-caste worshippers barred from temple entry, women excluded from certain rites, children subject to practices redefined as harmful — who gain legal standing and protection through state intervention that their own religious authority structure would not grant them. Their exit from the religious community without state protection was often costly or impossible; the intervention substitutes for an exit they could not otherwise take.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, intra_community_reform_beneficiaries, beneficiary,
    powerless, biographical, constrained, national).

% Religious institutions and their governing bodies lose autonomous control over practices the state reclassifies as social rather than essentially religious. They cannot opt out of the reviewing jurisdiction; their only recourse is litigating the essential-practices boundary itself, which is adjudicated by the same state authority doing the intervening.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, religious_denominations_subject_to_intervention, payer,
    organized, generational, constrained, national).

% Leaders whose authority rests on maintaining contested practices as core doctrine bear direct loss of institutional power when courts rule those practices non-essential and reformable. They are structurally distrusted as interested parties in essential-practices litigation, so their account of what is doctrinally central carries less weight than the state's own historical and textual analysis.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, orthodox_religious_leadership, payer,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__principled_intervention_reading, orthodox_religious_leadership, excluded).

% Smaller or numerically minority religious groups fear that a state empowered to define 'essential practice' for social-reform purposes can be captured by majoritarian preferences and turned selectively against minority customs deemed backward by majority standards, while majority-community practices escape equivalent scrutiny. They are rarely the test cases that set precedent, so the doctrine's contours are set without their direct participation.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, minority_religious_communities, excluded,
    moderate, generational, constrained, national).

% Analyze the doctrine's application across cases, tracking whether intervention is applied even-handedly across majority and minority religions or whether it correlates with which communities lack the political capital to resist judicial reclassification of their practices.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for correcting internally entrenched harms within religious communities — caste exclusion, discriminatory personal law, coerced practices — that the community's own governance structure has no internal remedy for and that pure non-interference would leave permanently unaddressed.
% TRANSFER_FUNCTION: Moves authority over defining and enforcing 'legitimate' religious practice from religious governance bodies to state courts and legislatures, and moves protection and standing from previously excluded intra-community members toward state-backed guarantees; correspondingly moves institutional control away from religious leadership.
% ABSENT_VOICES: Minority religious communities and, within larger communities, the orthodox leadership's own account of doctrinal centrality are structurally discounted — courts favor historical-textual analysis over community self-definition, and minority faiths rarely litigate the precedent-setting cases that later get applied to them.
% DISAPPEARANCE_RATIONALE: Without the intervention doctrine, temple-entry rights, personal-law reforms, and welfare-based restrictions on religious trusts would lose their constitutional grounding; excluded intra-community members would revert to depending entirely on internal community reform or legislative action requiring the community's own consent, and several landmark reforms (temple entry acts, abolition of specific discriminatory customs) would become constitutionally vulnerable to religious-freedom challenges they currently survive.
% FOUNDING_PROBLEM: Post-independence constitution-framers confronted religious practices (untouchability in temple access, certain caste and gender exclusions) that were harmful, entrenched, and unlikely to be reformed from within given the internal power structures benefiting from them, while a purely hands-off religious-freedom guarantee would have constitutionally entrenched those harms indefinitely.
% FOUNDING_PROBLEM_CORROBORATION: Social reform movements and some independent legal historians attest the underlying problem (caste and gender exclusion within religious governance) remains substantially live and the doctrine is still doing real work. Religious institutional bodies and a body of comparative constitutional scholars outside both camps note that the doctrine's application has drifted toward broader social-policy review well beyond the founding cases, with the essential-practices test increasingly criticized by scholars as judicially indeterminate rather than principled — corroboration exists on both the live-problem and the doctrine-drift readings, from sources outside the direct beneficiary and payer seats.
narrative_ontology:disappearance_verdict(constitutional_secularism__principled_intervention_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__principled_intervention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__principled_intervention_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__principled_intervention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__principled_intervention_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__principled_intervention_reading_tests).
:- end_tests(constitutional_secularism__principled_intervention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rising slowly: the doctrine began narrowly (temple entry, specific caste exclusions) and has gradually been invoked across a widening range of practices, which is real but incremental accumulation, not runaway extraction. Suppression is moderate (0.38) — reclassification is imposed on religious bodies through binding judicial and legislative action, but denominations retain litigation and political-mobilization channels, so it falls well short of the near-total suppression a snare would show. Theater ratio is low-moderate (0.22): the doctrine does perform genuine adjudicative work (essential-practices litigation is substantive, not symbolic), though some interventions function more as declaratory gestures than enforced reform. Accessibility collapse is moderate (0.35): religious governance retains substantial autonomy over undisputed essential practices; only contested zones are subject to collapse. Resistance is fairly high (0.58) because organized religious bodies actively litigate and mobilize against reclassification.
 *
 * DIRECTIONALITY LOGIC:
 *   State authorities sit at the agenda-setting end — they define and apply the essential-practices boundary and bear no direct cost from doing so. Intra-community reform beneficiaries are structural beneficiaries with low d: the doctrine substitutes for an exit option they otherwise lacked. Religious denominations and orthodox leadership are targets with high d: they lose institutional control they cannot bargain around, and their exit (relocating to unregulated jurisdiction, or simply not being subject to Indian constitutional jurisdiction) is not realistically available. Minority religious communities are excluded rather than coordinated: precedent is set in cases they are not party to, then applied to them later.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (entrenched intra-community harm with no internal remedy) remains partially live for some practices but is contested as a general justification: critics observe the doctrine now reaches well beyond its founding cases into areas with weaker claims to protecting the powerless, which is exactly the drift the tangled-rope classification is built to catch — coordination function and extraction risk are structurally simultaneous here, not sequential, so no single 'coordination phase' can be cleanly separated from a later 'extraction phase.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    essential_practices_test_indeterminacy,
    'Is the essential-practices test a principled doctrinal boundary or a discretionary instrument that lets courts reach whatever result social-reform sentiment favors in a given era?',
    'Longitudinal case analysis: track whether essential-practices rulings correlate with independently verifiable theological/historical evidence of centrality, or instead track contemporaneous social-reform political sentiment and case-specific outcome preferences.',
    'If the test tracks doctrine, this reading functions closer to a rope with defined coordination limits. If it tracks sentiment, the reading is closer to a snare wearing constitutional-doctrine cover, with courts substituting their own social preferences for religious self-governance under a legitimating label.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(essential_practices_test_indeterminacy, empirical, 'Whether the essential-practices boundary is principled or outcome-driven.').

omega_variable(
    majoritarian_capture_asymmetry,
    'Does this reading''s intervention power get applied more readily against minority-religion practices than against numerically dominant religion practices with comparable claims to reform justification?',
    'Comparative frequency and outcome analysis of intervention litigation across majority versus minority religious communities, controlling for severity of underlying harm alleged.',
    'Confirmed asymmetry would support the expected structural delta (higher risk of majoritarian capture) and push the effective classification toward a more extractive reading for minority-community stakeholders specifically, even while the doctrine remains genuinely reformist for majority-community intra-group victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_capture_asymmetry, empirical, 'Whether intervention power is applied asymmetrically against minority faiths.').

omega_variable(
    kernel_reading_boundary_stability,
    'Is the line between this reading (bounded intervention preserving an essential-practices carve-out) and the reformist reading (affirmative duty overriding religious autonomy outright) a stable doctrinal boundary, or does litigation pressure erode it over time toward the reformist pole?',
    'Track whether later cases increasingly dispense with essential-practices analysis altogether in favor of direct social-welfare balancing, which would indicate drift from this reading toward the reformist reading.',
    'If the boundary erodes, this constraint''s ε trajectory should be read as converging toward the reformist reading''s higher extraction profile over time rather than remaining a stable, independent reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_stability, conceptual, 'Structural stability of the boundary between this reading and the reformist sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__principled_intervention_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1950, constitutional_secularism__principled_intervention_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(cons_tr_t1965, constitutional_secularism__principled_intervention_reading, theater_ratio, 1965, 0.13).
narrative_ontology:measurement(cons_tr_t1980, constitutional_secularism__principled_intervention_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(cons_tr_t1995, constitutional_secularism__principled_intervention_reading, theater_ratio, 1995, 0.17).
narrative_ontology:measurement(cons_tr_t2010, constitutional_secularism__principled_intervention_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(cons_tr_t2025, constitutional_secularism__principled_intervention_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(cons_be_t1950, constitutional_secularism__principled_intervention_reading, base_extractiveness, 1950, 0.28).
narrative_ontology:measurement(cons_be_t1965, constitutional_secularism__principled_intervention_reading, base_extractiveness, 1965, 0.32).
narrative_ontology:measurement(cons_be_t1980, constitutional_secularism__principled_intervention_reading, base_extractiveness, 1980, 0.36).
narrative_ontology:measurement(cons_be_t1995, constitutional_secularism__principled_intervention_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(cons_be_t2010, constitutional_secularism__principled_intervention_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(cons_be_t2025, constitutional_secularism__principled_intervention_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1950, constitutional_secularism__principled_intervention_reading, suppression_requirement, 1950, 0.25).
narrative_ontology:measurement(cons_su_t1965, constitutional_secularism__principled_intervention_reading, suppression_requirement, 1965, 0.28).
narrative_ontology:measurement(cons_su_t1980, constitutional_secularism__principled_intervention_reading, suppression_requirement, 1980, 0.31).
narrative_ontology:measurement(cons_su_t1995, constitutional_secularism__principled_intervention_reading, suppression_requirement, 1995, 0.33).
narrative_ontology:measurement(cons_su_t2010, constitutional_secularism__principled_intervention_reading, suppression_requirement, 2010, 0.36).
narrative_ontology:measurement(cons_su_t2025, constitutional_secularism__principled_intervention_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
