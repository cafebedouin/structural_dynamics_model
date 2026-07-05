% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__graduated_access_filter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Occupational Licensing Statute as Graduated Access Filter
 *   domain: labor_economics/regulatory_policy
 *
 * SUMMARY:
 *   This story instantiates the 'graduated_access_filter' reading of the
 *   licensing_statute_mandate kernel: the same statutory credential
 *   requirement, read as a mechanism that sorts market access by pre-existing
 *   class position and resource access rather than by measured competence.
 *   Two sibling readings of the same kernel text exist as separate
 *   constraints: public_safety_coordination (the requirement genuinely solves
 *   an information-asymmetry problem for consumers) and
 *   rent_seeking_suppression (the requirement exists to restrict labor supply
 *   and extract incumbent rents). This reading does not deny that a safety
 *   function or a rent-extraction function may also be present — it isolates
 *   the specific structural claim that the barrier's HEIGHT and SHAPE (unpaid
 *   apprenticeship hours, fee schedules, geographic testing-center access)
 *   track applicants' prior capital and time-flexibility rather than their
 *   competence, and that this tracking reproduces class stratification in
 *   market access. The victim set here is specifically those excluded by
 *   resource constraints rather than by demonstrated incompetence.
 *
 * KEY AGENTS:
 *   - credentialed_incumbent_practitioners: beneficiary (organized/arbitrage) — collects wage premium from restricted supply
 *   - accredited_training_institutions: beneficiary (institutional/arbitrage) — collects mandated enrollment revenue
 *   - licensing_board_administrators: agenda_setter (institutional/analytical) — sets and enforces the specific barrier parameters
 *   - marginalized_aspirant_workers: primary target (powerless/trapped) — priced and time-gated out regardless of competence
 *   - informal_sector_practitioners: primary target (powerless/trapped) — criminalized despite demonstrated skill
 *   - rural_low_income_applicants: secondary target (powerless/constrained) — bears added geographic cost layer
 *   - state_legislators: excluded from meaningful input despite nominal authorship
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.71).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.68).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.71).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Occupational Licensing Statute as Graduated Access Filter").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor_economics/regulatory_policy").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, 'c38dbf17-cc43-43b9-b419-b1c66efd3032').
narrative_ontology:cs_kernel_codification('c38dbf17-cc43-43b9-b419-b1c66efd3032', formalized).
narrative_ontology:cs_authority_grounding('c38dbf17-cc43-43b9-b419-b1c66efd3032', extraction).
narrative_ontology:cs_interpretation_layer_present('c38dbf17-cc43-43b9-b419-b1c66efd3032').
narrative_ontology:cs_reading_relation('c38dbf17-cc43-43b9-b419-b1c66efd3032', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('c38dbf17-cc43-43b9-b419-b1c66efd3032', licensing_statute_mandate__rent_seeking_suppression, influences).
narrative_ontology:cs_axiom('c38dbf17-cc43-43b9-b419-b1c66efd3032', foundational, barrier_design_reproduces_class_position).
narrative_ontology:cs_axiom_status(barrier_design_reproduces_class_position, holdable).
narrative_ontology:cs_axiom_grounding('c38dbf17-cc43-43b9-b419-b1c66efd3032', barrier_design_reproduces_class_position, empirically_contingent).
narrative_ontology:cs_axiom('c38dbf17-cc43-43b9-b419-b1c66efd3032', secondary, resource_gated_pathways_are_not_competence_neutral).
narrative_ontology:cs_axiom_status(resource_gated_pathways_are_not_competence_neutral, holdable).
narrative_ontology:cs_axiom_grounding('c38dbf17-cc43-43b9-b419-b1c66efd3032', resource_gated_pathways_are_not_competence_neutral, empirically_contingent).
narrative_ontology:cs_reference_frame('c38dbf17-cc43-43b9-b419-b1c66efd3032', competence_based_gatekeeping_ideal).
narrative_ontology:cs_drift_state('c38dbf17-cc43-43b9-b419-b1c66efd3032', contemporary_licensing_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c38dbf17-cc43-43b9-b419-b1c66efd3032', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credentialed_incumbent_practitioners).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, accredited_training_institutions).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, licensing_board_administrators).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, marginalized_aspirant_workers).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, informal_sector_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, rural_low_income_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, consumers_of_licensed_services).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, consumers_of_licensed_services).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Already hold the license and practice under reduced competition. They shaped or lobbied for the statute's requirements (exam fees, apprenticeship hours, prior degree prerequisites) and benefit from wage premiums the barrier sustains. Their exit is arbitrage-grade: they can move between jurisdictions or specialties while newcomers cannot enter at all.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credentialed_incumbent_practitioners, beneficiary,
    organized, biographical, arbitrage, national).

% Sell the courses, exam prep, and certification pathways the statute mandates. Revenue scales directly with the number of people forced through the credentialing pipeline; they have no incentive to shorten or cheapen the pathway and lobby to preserve or extend it.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, accredited_training_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Write and enforce the specific requirements — hours, fees, exam content, reciprocity rules — and adjudicate exceptions. Their institutional survival depends on the statute remaining in force; they control the knobs (fee levels, hour requirements, waiver criteria) that determine how steep the barrier actually is for any given applicant.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, licensing_board_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Cannot afford the unpaid apprenticeship hours, exam fees, or the training institution's tuition, and often cannot take unpaid leave from existing work to accumulate required hours. The statute does not ban them from the trade outright — it prices and time-gates them out. Their only path in requires resources (savings, family support, employer sponsorship) that track existing class position, so the barrier reproduces rather than tests competence.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, marginalized_aspirant_workers, payer,
    powerless, biographical, trapped, regional).

% Already perform the work competently outside the formal system (unlicensed contractors, community health workers, informal caregivers) but are criminalized or priced out of formal recognition. Enforcement actions target them directly — cease-and-desist orders, fines — while their actual skill is never assessed.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, informal_sector_practitioners, payer,
    powerless, biographical, trapped, local).

% Face the same fee and hour requirements as urban applicants but must also travel long distances to accredited testing centers or apprenticeship sites, adding transportation and lodging costs the statute does not account for. Reciprocity gaps between states compound the burden if they relocate.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, rural_low_income_applicants, payer,
    powerless, biographical, constrained, regional).

% Receive some genuine assurance of minimum competence for high-stakes services, but pay elevated prices from the reduced labor supply and see wait times increase where credentialed practitioners are scarce, particularly in underserved areas the excluded practitioners would otherwise have served.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, consumers_of_licensed_services, beneficiary,
    moderate, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__graduated_access_filter, consumers_of_licensed_services, payer).

% Nominally set the statutory framework but in practice defer heavily to board and incumbent-association drafting language; economists and labor advocates who could testify to the class-sorting effect are rarely called before the relevant committees, which hear primarily from credentialed professional associations.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, state_legislators, excluded,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__graduated_access_filter, diffuse).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__graduated_access_filter, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared minimum-competence signal so consumers and employers do not need to individually vet every practitioner's skill before transacting.
% TRANSFER_FUNCTION: Moves market access and the associated wage premium from applicants without prior capital, time flexibility, or educational credentials toward applicants who already possess those resources, and moves tuition/fee revenue from all applicants toward training institutions and board administration.
% ABSENT_VOICES: Informal-sector practitioners with demonstrated competence and low-income aspirants who cannot afford the credentialing pathway rarely testify at rulemaking hearings; labor economists studying the barrier's distributional effects are seldom invited into board proceedings that set the specific hour and fee thresholds.
% DISAPPEARANCE_RATIONALE: If the statute vanished overnight, informal and marginalized practitioners would enter formal service delivery immediately, incumbent wage premiums would compress, training institutions would lose mandated enrollment, and licensing boards would lose their reason to exist — the labor market for the credentialed occupation would reorganize substantially within a single hiring cycle.
% FOUNDING_PROBLEM: Historically framed as preventing unqualified practitioners from causing consumer harm in occupations with real information asymmetry (e.g., a patient cannot easily verify a practitioner's competence in advance).
% FOUNDING_PROBLEM_CORROBORATION: Licensing boards and incumbent associations attest the safety problem remains live and justifies current requirements. Independent labor economists (outside the beneficiary set) and antitrust regulators studying occupational licensing report that the same safety outcomes are achievable with substantially lower hour/fee thresholds or with narrower scope-of-practice tiers, and that the specific barrier heights track incumbent lobbying rather than measured harm rates — indicating the founding problem's magnitude does not match the current barrier design.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__graduated_access_filter, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__graduated_access_filter, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(licensing_statute_mandate__graduated_access_filter, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__graduated_access_filter, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.71 at interval end) reflects that the statute's specific design parameters — unpaid hour requirements, fee schedules, testing-center geography — are set by parties who benefit from a higher barrier, and that the barrier height does not track measured harm rates in comparable jurisdictions with lower requirements. Suppression (0.68) is substantial because enforcement (cease-and-desist actions, fines against informal practitioners) actively closes the informal-practice exit rather than merely failing to open a formal one. Theater ratio (0.4) is moderate: continuing-education and re-certification requirements increasingly serve institutional revenue rather than measurable competence maintenance, a share that has grown across the interval as the theater_ratio series shows.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed incumbents and training institutions sit near the full-beneficiary end: they collect wage premiums or tuition revenue and hold arbitrage-grade exit (they can relocate or diversify while newcomers cannot enter). Marginalized aspirants and informal practitioners sit near the full-target end: trapped exit options, no resources to absorb the barrier's cost, and enforcement actively directed against their existing informal practice. Rural low-income applicants are constrained rather than fully trapped — geographic mobility exists in principle but at a cost that reproduces the same class sort. Consumers occupy a genuinely mixed position: real benefit from competence assurance, real cost from reduced supply and higher prices — reflecting that this reading does not deny a safety function exists, only that the specific barrier design is not calibrated to that function alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem status is authored as contested rather than dead: the underlying information-asymmetry problem the statute nominally addresses has not disappeared, so this is not a simple obsolescence case. What this reading isolates is that the CURRENT SHAPE of the requirement is not calibrated to the safety problem's actual magnitude — independent economic analysis finds barrier height tracks incumbent lobbying more than harm data. Classifying this as a graduated_access_filter snare (rather than collapsing it into either sibling reading) prevents two mislabeling errors: treating the whole statute as pure public-safety coordination (which would erase the documented class-sorting effect) and treating it as pure incumbent rent-seeking (which would erase the genuine, if miscalibrated, safety function the sibling readings separately capture).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    barrier_calibration_vs_safety_need,
    'Do the specific credentialing thresholds (hours, fees, exam content) track the actual measured harm rate from unlicensed practice, or do they track incumbent-lobbied barrier height independent of harm data?',
    'Cross-jurisdictional comparison of harm/complaint rates against licensing stringency in occupations with wide state-to-state variation in requirements (e.g., cosmetology, interior design, teeth-whitening) controlling for other confounds.',
    'If harm rates are flat across widely varying stringency levels, the graduated_access_filter reading is strongly corroborated as the dominant structural fact and the public_safety_coordination reading''s ε should be revised toward negligible; if harm rates track stringency closely, this reading''s extraction claim should be weighted lower relative to the safety reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(barrier_calibration_vs_safety_need, empirical, 'Whether barrier height is calibrated to safety need or to class-sorting/incumbent interest.').

omega_variable(
    class_sorting_vs_incumbent_capture_boundary,
    'Is the class-sorting effect (this reading) structurally distinct from incumbent rent extraction (the rent_seeking_suppression sibling), or are they the same mechanism described at different levels of abstraction?',
    'Trace whether barrier parameters that most burden low-resource applicants (unpaid apprenticeship hours, geographic testing requirements) are the SAME parameters incumbents lobby hardest to preserve, versus parameters that burden resource-poor applicants disproportionately for reasons unrelated to incumbent lobbying (e.g., simple fixed fee structures set decades ago without adjustment).',
    'If the two mechanisms are structurally identical, this reading and rent_seeking_suppression should be merged rather than kept as siblings; if distinct causal pathways exist (some barriers class-sort without being incumbent-lobbied, e.g. inherited colonial-era fee structures), the decomposition into separate readings is warranted and should be preserved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(class_sorting_vs_incumbent_capture_boundary, conceptual, 'Whether the class-sorting reading is analytically separable from the rent-seeking reading of the same statute.').

omega_variable(
    competence_assessment_alternative_availability,
    'Do lower-cost, less resource-dependent competence-assessment alternatives (e.g., direct practical examination without mandated hour requirements) exist and perform comparably, or is the current credentialing pathway the only viable competence signal?',
    'Examine occupations that have piloted alternative certification pathways (portfolio review, direct examination, apprenticeship-equivalent work experience credit) and compare outcomes and access rates to traditional pathway states.',
    'If viable low-cost alternatives exist and produce comparable competence outcomes, the current pathway''s resource-intensiveness is not functionally necessary, strengthening the graduated_access_filter classification; if no viable alternative exists, some of the measured extraction may be irreducible coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_assessment_alternative_availability, empirical, 'Whether the resource-intensive credentialing pathway is functionally necessary or one of several viable competence-signaling designs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__graduated_access_filter, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lice_tr_t8, licensing_statute_mandate__graduated_access_filter, theater_ratio, 8, 0.25).
narrative_ontology:measurement(lice_tr_t16, licensing_statute_mandate__graduated_access_filter, theater_ratio, 16, 0.29).
narrative_ontology:measurement(lice_tr_t24, licensing_statute_mandate__graduated_access_filter, theater_ratio, 24, 0.33).
narrative_ontology:measurement(lice_tr_t32, licensing_statute_mandate__graduated_access_filter, theater_ratio, 32, 0.37).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__graduated_access_filter, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lice_be_t8, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(lice_be_t16, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(lice_be_t24, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(lice_be_t32, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(lice_su_t8, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(lice_su_t16, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(lice_su_t24, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(lice_su_t32, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 32, 0.65).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__graduated_access_filter, enforcement_mechanism).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, rent_seeking_suppression).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the natural-language concept 'occupational licensing statute' per the ε-invariance principle. public_safety_coordination models the competence-signaling claim (lower ε, closer to rope/mountain). rent_seeking_suppression models the incumbent-extraction claim (high ε concentrated on a narrower victim set — market entrants generally, not class-stratified specifically). This file (graduated_access_filter) models the class/resource-stratification claim, with a victim set specifically defined by lack of prior capital/time-flexibility rather than lack of competence or mere market-entrant status. All three are linked bidirectionally in commentary and network.affects_constraints; none subsumes the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(licensing_statute_mandate__graduated_access_filter, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
