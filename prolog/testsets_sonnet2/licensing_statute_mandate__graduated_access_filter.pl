% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__graduated_access_filter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__graduated_access_filter, []).

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
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Occupational Licensing Statute as Class-Sorted Access Filter
 *   domain: labor_economics/regulatory_policy
 *
 * SUMMARY:
 *   This story instantiates the graduated_access_filter reading of the
 *   licensing_statute_mandate kernel: statutory credentialing requirements —
 *   hours of mandated training, exam fees, apprenticeship time — are read
 *   here as a structural sorting mechanism that tracks prior resource access
 *   rather than competence. Under this reading, the same statutory text that
 *   sibling readings interpret as a safety-coordination device or a
 *   rent-extraction device for incumbents is interpreted as a
 *   class-stratifying access filter: those with prior capital, time, and
 *   geographic proximity to accredited programs clear the bar; those without
 *   are excluded regardless of actual skill. The ε authored here is for the
 *   standing licensing arrangement as this reading sees it — high extraction,
 *   not the reformed competence-testing alternative this reading would
 *   prefer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.72).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.68).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.72).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Occupational Licensing Statute as Class-Sorted Access Filter").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor_economics/regulatory_policy").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, '8ca5813a-157f-448e-9d90-5f1e9fb13209').
narrative_ontology:cs_kernel_codification('8ca5813a-157f-448e-9d90-5f1e9fb13209', formalized).
narrative_ontology:cs_authority_grounding('8ca5813a-157f-448e-9d90-5f1e9fb13209', extraction).
narrative_ontology:cs_interpretation_layer_present('8ca5813a-157f-448e-9d90-5f1e9fb13209').
narrative_ontology:cs_reading_relation('8ca5813a-157f-448e-9d90-5f1e9fb13209', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('8ca5813a-157f-448e-9d90-5f1e9fb13209', licensing_statute_mandate__rent_seeking_suppression, influences).
narrative_ontology:cs_axiom('8ca5813a-157f-448e-9d90-5f1e9fb13209', foundational, differential_resource_access_determines_credential_attainment).
narrative_ontology:cs_axiom_status(differential_resource_access_determines_credential_attainment, holdable).
narrative_ontology:cs_axiom_grounding('8ca5813a-157f-448e-9d90-5f1e9fb13209', differential_resource_access_determines_credential_attainment, empirically_contingent).
narrative_ontology:cs_axiom('8ca5813a-157f-448e-9d90-5f1e9fb13209', secondary, requirement_stringency_tracks_class_barrier_not_competence).
narrative_ontology:cs_axiom_status(requirement_stringency_tracks_class_barrier_not_competence, holdable).
narrative_ontology:cs_axiom_grounding('8ca5813a-157f-448e-9d90-5f1e9fb13209', requirement_stringency_tracks_class_barrier_not_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('8ca5813a-157f-448e-9d90-5f1e9fb13209', legislatively_enacted_minimum_competence_standard).
narrative_ontology:cs_drift_state('8ca5813a-157f-448e-9d90-5f1e9fb13209', contemporary_licensing_stringency_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8ca5813a-157f-448e-9d90-5f1e9fb13209', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credentialed_incumbent_practitioners).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, accredited_training_institutions).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, licensing_board_administrators).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, marginalized_aspiring_workers).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, informal_sector_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, rural_low_income_applicants).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__graduated_access_filter, minimum_competence_standard_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the license already; the statute forecloses new entrants who cannot afford the same path, holding down competition for their services and their wages. They sit on licensing boards and lobby to keep requirements at or above the level they themselves cleared.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credentialed_incumbent_practitioners, beneficiary,
    organized, biographical, arbitrage, regional).

% Sell the tuition, exam prep, and clock-hours the statute requires as the only lawful path to licensure. Revenue scales directly with the number of hours and dollars the statute mandates; they have no incentive to see requirements reduced.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, accredited_training_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Write and enforce the specific hour, fee, exam, and background-check requirements; deny or approve applications; can raise or lower the bar administratively without new legislation in many jurisdictions. Their board seats are frequently occupied by incumbent practitioners.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, licensing_board_administrators, agenda_setter,
    institutional, generational, analytical, regional).

% Cannot front the tuition, unpaid apprenticeship hours, or exam fees the statute requires, even though they may already possess the practical skill. Working informally to survive while saving toward licensure is often itself illegal under the statute, closing the on-ramp entirely. Exit means abandoning the occupation.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, marginalized_aspiring_workers, payer,
    powerless, biographical, trapped, local).

% Already perform the work — braiding hair, home repair, basic care work — through informal networks, but statutory enforcement criminalizes or fines unlicensed practice, cutting off their income unless they can clear the same barrier the incumbents cleared under more favorable circumstances.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, informal_sector_practitioners, payer,
    powerless, immediate, trapped, local).

% Face the added cost of travel to distant accredited programs and testing centers, compounding the tuition barrier with geographic distance urban applicants do not face. Relocation for licensure is often not financially possible.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, rural_low_income_applicants, payer,
    powerless, biographical, constrained, regional).

% Would argue for competence testing decoupled from expensive credential-hour mandates, but are largely absent from board rulemaking, which is dominated by incumbent practitioner associations who control the technical specification of what counts as adequate training.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, consumer_safety_advocates, excluded,
    moderate, generational, analytical, national).

% Study licensing's wage and entry effects; produce evidence that requirement stringency correlates with incumbent wage premiums and reduced entry by low-income applicants, largely independent of any measurable safety improvement.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, labor_economists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__graduated_access_filter, diffuse).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__graduated_access_filter, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The statute is presented as coordinating a minimum competence signal so consumers do not have to individually vet every practitioner — a genuine information problem in principle.
% TRANSFER_FUNCTION: Moves labor-market entry opportunity and wage premium from would-be entrants who lack up-front capital toward incumbents already licensed and toward institutions selling the mandated training pathway; entry cost is paid disproportionately by the resource-poor.
% ABSENT_VOICES: Consumer safety advocates and prospective low-income entrants are structurally absent from the board rulemaking process that sets the specific hour and fee requirements; the people bearing the exclusion cost do not sit on the body that calibrates the barrier.
% DISAPPEARANCE_RATIONALE: If the statute vanished overnight, informal-sector and marginalized workers currently locked out would enter the market immediately, incumbent wage premiums would compress, and training institutions dependent on mandated-hour tuition revenue would lose their captive market — the labor market for this occupation would visibly reorganize.
% FOUNDING_PROBLEM: Historically framed as protecting consumers from unqualified or fraudulent practitioners in occupations where harm from incompetence was plausible.
% FOUNDING_PROBLEM_CORROBORATION: Licensing boards and incumbent associations attest the safety problem remains live. Independent labor economists studying entry and wage effects, and consumer advocates outside the credentialed class, attest that requirement stringency tracks incumbent wage protection far more tightly than it tracks any measurable safety outcome — corroboration for the exclusion reading comes from outside the beneficiary set.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__graduated_access_filter, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__graduated_access_filter, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(licensing_statute_mandate__graduated_access_filter, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__graduated_access_filter, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__graduated_access_filter_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__graduated_access_filter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.72 at interval end) reflects the persistent wage/entry-opportunity transfer from excluded workers to credentialed incumbents and training institutions, rising over the interval as requirement stringency has historically ratcheted upward (hours requirements added, rarely removed). Suppression (0.68) captures active enforcement — fines, cease-and-desist actions, criminal penalties for unlicensed practice — that closes the informal on-ramp entirely rather than merely disadvantaging it. Theater (0.40) reflects that some genuine competence-testing function persists (the exam itself measures something) even as the hour/fee architecture around it does most of the exclusionary work.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed incumbents and training institutions are declared beneficiaries: the statute suppresses their competition and monetizes the mandated pathway, so directionality sits near the full-beneficiary end. Marginalized aspiring workers, informal-sector practitioners, and rural low-income applicants are declared victims: trapped exit options and local/regional scope combine with victim status to push directionality toward the full-target end — they cannot arbitrage around the requirement the way an institutional actor could.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (consumer protection from incompetence) may once have been substantially live; this reading holds it is now largely serving a different function — protecting incumbent wages — while the safety rationale is retained rhetorically. The mismatch between founding_problem_status (contested, trending dead-as-safety-problem) and disappearance_verdict (world_rearranges) is exactly the zombie-mandate signature: the arrangement persists and still moves real resources, but increasingly for a different beneficiary than its stated purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    safety_function_residual_magnitude,
    'How much of the statute''s current operation still performs a genuine, measurable safety-coordination function versus how much is class-sorting overhead unrelated to competence?',
    'Compare harm/complaint rates in jurisdictions with substantially lower hour/fee requirements or alternative competence-testing regimes against jurisdictions with high-stringency requirements, controlling for occupation type.',
    'If harm rates are statistically indistinguishable across stringency levels, the safety-coordination reading loses empirical support and the exclusion reading strengthens; if harm rates rise sharply with reduced stringency, the coordination component is real and this reading''s ε may be overstated relative to the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_function_residual_magnitude, empirical, 'How much genuine safety coordination survives inside the graduated_access_filter reading''s extraction estimate.').

omega_variable(
    kernel_reading_disaggregation_boundary,
    'Is the statutory text genuinely ONE kernel with three readings, or does the class-sorting effect described here actually arise from a separable sub-provision (e.g., specific hour-count minimums) that could be isolated from the core competence-testing requirement?',
    'Legislative and regulatory history analysis: trace which specific provisions (hour minimums vs. exam content vs. background checks) were added and by which interest group, and whether removing only the resource-intensive provisions would eliminate the class-sorting effect while preserving competence testing.',
    'If the exclusionary effect is traceable to a severable sub-provision, this reading may itself be decomposable into a further pair of constraints (a Mountain-adjacent exam-competence core and a Snare-like hour/fee overlay), which would refine rather than replace this story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disaggregation_boundary, conceptual, 'Whether this reading itself hides a further ε-invariance decomposition within the statutory text.').

omega_variable(
    board_capture_degree,
    'To what degree are licensing board administrators independent decision-makers versus captured proxies for incumbent practitioner associations?',
    'Board composition audits (fraction of seats held by practicing incumbents vs. public/consumer representatives) and analysis of rulemaking history for correlation with incumbent association lobbying positions.',
    'High capture would support treating licensing_board_administrators'' agenda_setter role as substantially aligned with the beneficiary class rather than a neutral administrative seat, strengthening the snare classification''s coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(board_capture_degree, empirical, 'Whether the nominally neutral administrative seat is functionally captured by the beneficiary class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__graduated_access_filter, theater_ratio, 0, 0.22).
narrative_ontology:measurement(lice_tr_t4, licensing_statute_mandate__graduated_access_filter, theater_ratio, 4, 0.26).
narrative_ontology:measurement(lice_tr_t8, licensing_statute_mandate__graduated_access_filter, theater_ratio, 8, 0.3).
narrative_ontology:measurement(lice_tr_t12, licensing_statute_mandate__graduated_access_filter, theater_ratio, 12, 0.33).
narrative_ontology:measurement(lice_tr_t16, licensing_statute_mandate__graduated_access_filter, theater_ratio, 16, 0.36).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__graduated_access_filter, theater_ratio, 20, 0.38).
narrative_ontology:measurement(lice_tr_t24, licensing_statute_mandate__graduated_access_filter, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(lice_be_t4, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 4, 0.56).
narrative_ontology:measurement(lice_be_t8, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(lice_be_t12, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(lice_be_t16, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(lice_be_t24, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 24, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(lice_su_t4, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(lice_su_t8, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 8, 0.59).
narrative_ontology:measurement(lice_su_t12, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(lice_su_t16, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(lice_su_t20, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(lice_su_t24, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__graduated_access_filter, enforcement_mechanism).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, rent_seeking_suppression).

% DUAL FORMULATION NOTE:
% Three constraints share the licensing_statute_mandate kernel and the same statutory text. graduated_access_filter (this story) authors high ε centered on class/resource-access sorting, with marginalized/informal/rural workers as victims and credentialed incumbents/training institutions/board administrators as beneficiaries — classified snare. public_safety_coordination authors low ε for the same text read as a genuine competence-signaling coordination device. rent_seeking_suppression authors substantial ε centered on incumbent wage-premium extraction via labor-supply restriction, a distinct beneficiary emphasis from this reading's class-sorting emphasis even though beneficiary sets overlap. Each story carries its own stable ε per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
