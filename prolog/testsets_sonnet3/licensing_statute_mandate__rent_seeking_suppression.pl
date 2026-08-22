% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__rent_seeking_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__rent_seeking_suppression, []).

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
 *   constraint_id: licensing_statute_mandate__rent_seeking_suppression
 *   human_readable: Occupational Licensing Statute as Incumbent Rent Extraction
 *   domain: labor_economics/regulatory_policy
 *
 * SUMMARY:
 *   A state licensing board, staffed largely by incumbent practitioners, sets
 *   and periodically raises the training-hour, examination, and
 *   continuing-education requirements that gate legal entry into the
 *   occupation. The statute is framed publicly as consumer protection. This
 *   reading holds that framing functions as legitimating cover: requirement
 *   stringency has drifted upward over decades in a pattern that tracks
 *   incumbent lobbying and training-institution revenue interests more
 *   closely than documented harm rates, restricting labor supply and
 *   sustaining a wage premium for those already licensed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, 0.78).
domain_priors:suppression_score(licensing_statute_mandate__rent_seeking_suppression, 0.72).
domain_priors:theater_ratio(licensing_statute_mandate__rent_seeking_suppression, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, extractiveness, 0.78).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__rent_seeking_suppression, snare).
narrative_ontology:human_readable(licensing_statute_mandate__rent_seeking_suppression, "Occupational Licensing Statute as Incumbent Rent Extraction").
narrative_ontology:topic_domain(licensing_statute_mandate__rent_seeking_suppression, "labor_economics/regulatory_policy").

domain_priors:requires_active_enforcement(licensing_statute_mandate__rent_seeking_suppression).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__rent_seeking_suppression, '057f929e-7408-4bd7-b9de-d3b693e62a40').
narrative_ontology:cs_kernel_codification('057f929e-7408-4bd7-b9de-d3b693e62a40', formalized).
narrative_ontology:cs_authority_grounding('057f929e-7408-4bd7-b9de-d3b693e62a40', extraction).
narrative_ontology:cs_interpretation_layer_present('057f929e-7408-4bd7-b9de-d3b693e62a40').
narrative_ontology:cs_reading_relation('057f929e-7408-4bd7-b9de-d3b693e62a40', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('057f929e-7408-4bd7-b9de-d3b693e62a40', licensing_statute_mandate__graduated_access_filter, coexists_with).
narrative_ontology:cs_axiom('057f929e-7408-4bd7-b9de-d3b693e62a40', foundational, requirement_stringency_tracks_incumbent_interest_not_harm).
narrative_ontology:cs_axiom_status(requirement_stringency_tracks_incumbent_interest_not_harm, holdable).
narrative_ontology:cs_axiom_grounding('057f929e-7408-4bd7-b9de-d3b693e62a40', requirement_stringency_tracks_incumbent_interest_not_harm, empirically_contingent).
narrative_ontology:cs_axiom('057f929e-7408-4bd7-b9de-d3b693e62a40', secondary, board_self_governance_produces_capture_not_expertise_calibration).
narrative_ontology:cs_axiom_status(board_self_governance_produces_capture_not_expertise_calibration, holdable).
narrative_ontology:cs_axiom_grounding('057f929e-7408-4bd7-b9de-d3b693e62a40', board_self_governance_produces_capture_not_expertise_calibration, empirically_contingent).
narrative_ontology:cs_reference_frame('057f929e-7408-4bd7-b9de-d3b693e62a40', competence_verification_founding_rationale).
narrative_ontology:cs_drift_state('057f929e-7408-4bd7-b9de-d3b693e62a40', contemporary_requirement_stringency, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('057f929e-7408-4bd7-b9de-d3b693e62a40', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, incumbent_licensed_practitioners).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, professional_licensing_boards).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, accredited_training_institutions).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, aspiring_entrants).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, consumers_paying_higher_prices).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, unlicensed_practice_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the license already, sit on or lobby the licensing board that sets entry requirements, and collect the wage premium that flows from restricted supply. Face no cost from raising entry barriers further; every increase in required hours, exams, or continuing-education fees protects their existing position and raises the price floor they can charge.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, incumbent_licensed_practitioners, beneficiary,
    organized, biographical, arbitrage, national).

% Statutorily empowered to set and enforce the credential requirements, staffed predominantly by incumbent practitioners themselves. Justify each requirement in the language of consumer protection while board composition and funding structure make them structurally responsive to incumbent preferences rather than consumer or entrant testimony.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, professional_licensing_boards, agenda_setter,
    institutional, generational, arbitrage, national).

% Capture tuition and fee revenue from mandatory training hours that exceed what competence actually requires; lobby to maintain or extend program-length and accreditation requirements because their revenue model depends on the mandate's continuation.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, accredited_training_institutions, beneficiary,
    organized, generational, arbitrage, national).

% Must complete costly, time-consuming credentialing before earning any income in the occupation, often taking on debt for training whose content exceeds practical competence needs. Cannot practice the trade to demonstrate competence without first clearing the licensing gate; exit means abandoning the occupation entirely or relocating to a jurisdiction with lower barriers, which is itself costly.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, aspiring_entrants, payer,
    powerless, biographical, constrained, national).

% Pay the wage premium licensing creates, passed through as higher service prices, with no visibility into how much of the price reflects genuine competence assurance versus artificial scarcity. Have essentially no individual leverage over licensing requirements set by boards they do not sit on.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, consumers_paying_higher_prices, payer,
    powerless, immediate, constrained, national).

% Practice the underlying skill without the credential — sometimes competently, sometimes as a matter of economic necessity — and face fines, injunctions, or criminal penalties for unlicensed practice. Enforcement actions target this group directly as the visible face of the boundary the licensing statute defends.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, unlicensed_practice_workers, payer,
    powerless, biographical, trapped, national).

% Enact and periodically review the enabling statute, largely on the basis of testimony from the licensing board and incumbent professional associations. Rarely hear from unlicensed workers or entrants who are priced out before they have standing to testify, and rarely commission independent cost-benefit review of the requirements they renew.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, state_legislators, observer,
    institutional, biographical, analytical, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__rent_seeking_suppression, state_legislators, agenda_setter).

% Would have entered the occupation but were deterred by the cost or time of credentialing and moved into lower-wage alternative work instead. Their foregone entry is invisible in legislative review — no one testifies on behalf of people who never showed up.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, displaced_entrants_alternate_occupations, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__rent_seeking_suppression, incumbent_licensed_practitioners).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__rent_seeking_suppression, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, credentialing coordinates a genuine information problem: consumers cannot easily verify practitioner competence pre-transaction, so a credible third-party signal reduces search and harm-avoidance costs. This reading holds that this coordination story is present but functions primarily as legitimating cover for the requirement's actual operation.
% TRANSFER_FUNCTION: Moves income from aspiring entrants (who pay training costs and foregone wages during the credentialing period) and from consumers (who pay the wage premium as higher service prices) to incumbent practitioners (wage premium) and accredited training institutions (tuition capture).
% ABSENT_VOICES: Aspiring entrants who have not yet entered have no standing in legislative hearings on renewal; unlicensed practitioners who work informally are visible only as enforcement targets, never as witnesses on the requirement's cost; consumers as a class rarely organize to testify on diffuse price effects.
% DISAPPEARANCE_RATIONALE: If the statute vanished overnight, entry into the occupation would open substantially: prices would likely fall as supply expanded, incumbent wage premiums would compress, training institutions tied to the mandate would lose a captive revenue stream, and enforcement actions against unlicensed practice would cease. The occupation would not disappear — competence signaling would shift to voluntary certification, reputation, or insurance-backed guarantees, but the current rent structure would collapse.
% FOUNDING_PROBLEM: The statute was enacted to address a genuine information asymmetry: consumers could not verify practitioner competence before purchase, and incompetent practice caused real, sometimes irreversible harm.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent practitioners and licensing boards attest the problem remains fully live and the requirements are calibrated to current risk. Independent labor economists (e.g. sunset-review commission studies and cross-state comparative analyses of requirement stringency versus harm incidence) and state legislative auditors in several jurisdictions have found requirement levels substantially exceed what harm data justify, and that requirement stringency correlates more strongly with incumbent lobbying activity than with documented harm rates — corroboration from outside the beneficiary set supports the reading that the founding problem has been substantially decoupled from the current requirement level.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__rent_seeking_suppression, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__rent_seeking_suppression, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__rent_seeking_suppression, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(licensing_statute_mandate__rent_seeking_suppression, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__rent_seeking_suppression, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78) and rising over the interval because the gap between required credentialing burden and demonstrated harm-reduction value has widened, consistent with a rent-seeking trajectory rather than a stable coordination equilibrium. Suppression (0.72) reflects the active enforcement machinery — fines, injunctions, unauthorized-practice prosecutions — required to keep unlicensed practice from competing away the premium; this is a raw structural fact about the constraint's enforcement, not scaled by scope or power. Theater ratio is authored as moderate-high and rising (0.55 at T=40) because an increasing share of continuing-education and re-certification requirements function as revenue and gatekeeping activity with declining marginal connection to competence verification. Accessibility collapse (0.62) is moderate rather than near-total because informal and out-of-state practice routes exist but are costly and risky. Resistance (0.58) reflects active pushback from entrants, sunset-review advocates, and some legislators, though this resistance rarely succeeds against organized incumbent lobbying.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent practitioners and licensing boards sit near the full-beneficiary end: they set the requirements, bear none of the entry cost, and collect the wage premium the scarcity produces — d near 0. Accredited training institutions similarly benefit through captured tuition revenue tied directly to mandate persistence. Aspiring entrants and unlicensed practice workers sit near the full-target end: they bear the training cost, the foregone-income cost, and in the unlicensed case, direct enforcement risk, with limited exit (constrained or trapped) because relocating or abandoning the occupation is itself costly. Consumers sit as diffuse, powerless payers who bear the price pass-through without organized voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (genuine information asymmetry about competence) was real at statute enactment and gives this reading its coordination veneer. This reading holds the mandate has drifted from that founding function: the mismatch between founding_problem_status (contested, with independent corroboration favoring 'substantially decoupled') and disappearance_verdict (world_rearranges — because incumbents' wage premiums and institutions' tuition capture depend on it) is exactly the signature of arrangement-outlived-function extraction rather than a neutral, still-necessary mountain. Classifying this as snare rather than mountain or rope prevents the coordination story from being taken as the whole story; the sibling public_safety_coordination reading exists precisely so the genuine competence-signaling function is not erased by this reading's extraction finding, and vice versa.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Is the licensing_statute_mandate kernel''s core function competence assurance (public_safety_coordination), class-sorting by resource access (graduated_access_filter), or incumbent rent extraction (this reading)? The disagreement is located in whether requirement stringency tracks documented harm rates, prior-resource-based sorting effects, or incumbent lobbying intensity.',
    'Cross-jurisdictional comparison of requirement stringency against (a) harm incidence data, (b) entrant socioeconomic composition pre/post requirement changes, and (c) incumbent association lobbying expenditure and board composition, tested for which variable requirement changes track most tightly over time.',
    'If requirement stringency tracks harm data closely, the public_safety_coordination reading dominates and this reading''s high ε is overstated for that jurisdiction. If it tracks lobbying intensity and board capture most tightly, this reading''s snare classification is the structurally correct one for that jurisdiction''s arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Where the three sibling readings'' core premises actually diverge empirically.').

omega_variable(
    incumbent_board_capture_degree,
    'Is the licensing board''s incumbent-dominated composition itself an artifact of a rational information-asymmetry-driven design (only practitioners have the expertise to set competence standards) or a captured governance structure that entrenches extraction?',
    'Comparative analysis of licensing boards with mandated non-practitioner majority representation (where they exist) versus incumbent-majority boards, measuring requirement stringency trajectories and entrant pass rates over comparable periods.',
    'If non-practitioner-majority boards produce materially lower requirement stringency for comparable harm profiles, this corroborates the rent-seeking reading; if stringency converges regardless of board composition, the expertise-necessity justification is stronger and the requirement may reflect genuine competence needs rather than capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_board_capture_degree, empirical, 'Whether board composition is a rational expertise requirement or a captured extraction mechanism.').

omega_variable(
    training_content_competence_gap,
    'What proportion of the mandated training curriculum and hours is demonstrably connected to harm-reduction competence versus credential-gating volume with no demonstrated safety return?',
    'Curriculum audit comparing required training content against documented harm-causing errors and near-misses in the occupation, identifying which curriculum elements map to actual failure modes.',
    'A large gap between curriculum volume and harm-relevant content would strongly support the theater_ratio trajectory authored here and the rent-seeking reading generally; a tight mapping would weaken this reading relative to public_safety_coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(training_content_competence_gap, empirical, 'Whether training requirements are calibrated to safety or inflated as entry cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__rent_seeking_suppression, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0, 0.25).
narrative_ontology:measurement(lice_tr_t8, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 8, 0.32).
narrative_ontology:measurement(lice_tr_t16, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 16, 0.38).
narrative_ontology:measurement(lice_tr_t24, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 24, 0.44).
narrative_ontology:measurement(lice_tr_t32, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 32, 0.5).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(lice_be_t8, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(lice_be_t16, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(lice_be_t24, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(lice_be_t32, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(lice_su_t8, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(lice_su_t16, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(lice_su_t24, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(lice_su_t32, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__rent_seeking_suppression, identity_coordination).
narrative_ontology:boltzmann_floor_override(licensing_statute_mandate__rent_seeking_suppression, 0.08).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, graduated_access_filter).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the licensing_statute_mandate kernel. public_safety_coordination reads the same statutory text as genuine minimum-competence coordination (low ε, rope-shaped, negligible victim set). graduated_access_filter reads it as a class-sorting mechanism where differential barriers sort by prior resource access rather than by simple incumbent-capture (tangled_rope-shaped, distinct victim composition weighted toward under-resourced entrants specifically). This reading (rent_seeking_suppression) reads it as artificial scarcity extraction transferring rents to incumbents and training institutions (snare-shaped, high ε). All three share the same statutory kernel but instantiate structurally distinct constraints with distinct ε values, beneficiary/victim sets, and classifications — they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(licensing_statute_mandate__rent_seeking_suppression, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
