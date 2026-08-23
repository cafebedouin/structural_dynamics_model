% ============================================================================
% CONSTRAINT STORY: us_constitution_text__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__originalist_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: us_constitution_text__originalist_reading
 *   human_readable: Originalist Constitutional Interpretation Constraint
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   The originalist reading of the US Constitution instantiates a constraint
 *   requiring judges to recover and apply the original public meaning of the
 *   constitutional text at ratification. Originalists present this as a
 *   natural constraint of legal interpretation — a mountain emerging from the
 *   nature of written constitutions and democratic legitimacy. The authored
 *   metrics describe a different structure: high and rising extraction (0.75)
 *   from rights claimants whose claims lack historical grounding, high
 *   suppression (0.85) of adaptive interpretation enforced through judicial
 *   appointments and professional gatekeeping, and moderate theater (0.40) as
 *   originalist methodology increasingly serves as a vehicle for conservative
 *   policy outcomes rather than pure historical recovery. The constraint has
 *   identifiable beneficiaries (conservative legal movement, originalist
 *   judges) and victims (non-originalist rights claimants, living
 *   constitutionalist judges), making it a false summit mountain candidate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.75).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.85).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, mountain).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "Originalist Constitutional Interpretation Constraint").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).
domain_priors:emerges_naturally(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, '246b176c-9c13-4d34-bafa-de9bb4191659').
narrative_ontology:cs_kernel_codification('246b176c-9c13-4d34-bafa-de9bb4191659', fixed_text).
narrative_ontology:cs_authority_grounding('246b176c-9c13-4d34-bafa-de9bb4191659', lineage).
narrative_ontology:cs_interpretation_layer_present('246b176c-9c13-4d34-bafa-de9bb4191659').
narrative_ontology:cs_reading_relation('246b176c-9c13-4d34-bafa-de9bb4191659', us_constitution_text__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('246b176c-9c13-4d34-bafa-de9bb4191659', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('246b176c-9c13-4d34-bafa-de9bb4191659', foundational, original_public_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(original_public_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('246b176c-9c13-4d34-bafa-de9bb4191659', original_public_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('246b176c-9c13-4d34-bafa-de9bb4191659', secondary, judicial_restraint_requires_historical_constraint).
narrative_ontology:cs_axiom_status(judicial_restraint_requires_historical_constraint, holdable).
narrative_ontology:cs_axiom_grounding('246b176c-9c13-4d34-bafa-de9bb4191659', judicial_restraint_requires_historical_constraint, instrumental).
narrative_ontology:cs_reference_frame('246b176c-9c13-4d34-bafa-de9bb4191659', founding_era_original_understanding).
narrative_ontology:cs_drift_state('246b176c-9c13-4d34-bafa-de9bb4191659', contemporary_originalist_dominance, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('246b176c-9c13-4d34-bafa-de9bb4191659', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, originalist_judges).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, non_originalist_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, living_constitutionalist_judges).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, general_citizenry).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, general_citizenry).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, original_public_meaning_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, judicial_restraint_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Built and maintains the institutional architecture (Federalist Society, judicial appointment pipeline, legal academia positions) that enforces originalist methodology. Gains institutional dominance and policy outcomes aligned with conservative preferences. Can shift resources across legal and political venues.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, conservative_legal_movement, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive career advancement, intellectual coherence, and institutional legitimacy from adhering to originalism. Their judicial identity is fused with the methodology; exit means abandoning the professional community that elevated them. Constrained by lifetime appointment and professional identity.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, originalist_judges, beneficiary,
    powerful, biographical, constrained, national).

% Bear the cost of having rights claims (reproductive autonomy, LGBTQ+ protections, digital privacy, etc.) rejected because they lack 18th/19th century historical grounding. Must litigate in hostile forums or seek legislative remedies in polarized environments. Exit from constitutional adjudication is practically unavailable.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, non_originalist_rights_claimants, payer,
    moderate, biographical, constrained, national).

% Find their interpretive methodology structurally suppressed; opinions adopting adaptive interpretation are treated as illegitimate by the dominant originalist bloc. Professional identity is bound to living constitutionalism; exit means abandoning their judicial philosophy. Constrained by institutional minority status.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, living_constitutionalist_judges, payer,
    powerful, biographical, identity_locked, national).

% Receive legal stability and democratic legitimacy benefits from constrained judicial discretion. Simultaneously lose adaptive constitutional protections for modern conditions. Exit from the constitutional order is effectively impossible; constitutional amendment is prohibitively difficult.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, general_citizenry, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__originalist_reading, general_citizenry, payer).

% Produces scholarly critique of originalism but is structurally excluded from judicial appointment pipelines and institutional power centers. Would object to originalism's empirical claims and normative conclusions but lacks levers to affect the constraint's operation. Mobile across academic institutions but not into judicial power.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, liberal_legal_academy, excluded,
    organized, generational, mobile, national).

% Observes the interpretive contest from outside the partisan-legal fray. Sees the full structure: originalism as a methodology that claims natural-law status while distributing interpretive authority and policy outcomes along ideological lines. No stake in the outcome.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Constrains judicial discretion by tethering interpretation to fixed historical meaning, providing legal stability and democratic legitimacy by preventing unelected judges from imposing contemporary values.
% TRANSFER_FUNCTION: Moves interpretive authority from contemporary majorities and judges to the historical ratifiers of 1787-1791 and 1865-1870; suppresses rights claims and governmental powers not grounded in original public understanding.
% ABSENT_VOICES: Future generations whose constitutional claims would require adaptive interpretation; marginalized groups whose protections were unrecognized at ratification (enslaved persons, women, indigenous nations, LGBTQ+ persons); the global south whose constitutional models are shaped by US doctrine but have no voice in its interpretation.
% DISAPPEARANCE_RATIONALE: If the originalist constraint vanished overnight, the Supreme Court would immediately adopt living constitutionalist or pluralist methodologies; rights jurisprudence would expand rapidly to cover reproductive autonomy, voting rights, digital privacy, and economic regulation; the conservative legal movement's half-century institutional project would lose its coordinating methodology; democratic politics would shift as constitutional amendment pressure diminishes.
% FOUNDING_PROBLEM: Judicial activism and democratic illegitimacy of unelected judges imposing contemporary values under the guise of constitutional interpretation, undermining the separation of powers and the amendment process.
% FOUNDING_PROBLEM_CORROBORATION: The conservative legal movement attests the founding problem remains live, citing Dobbs and Bruen as evidence of continuing judicial overreach. Liberal legal scholars and living constitutionalist judges attest the problem is inverted: originalism now enables judicial activism by conservative judges (e.g., striking down gun regulations, voting rights protections). No neutral corroboration exists outside the benefiting parties; the dispute is the structure of the field.
narrative_ontology:disappearance_verdict(us_constitution_text__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_text__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__originalist_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(us_constitution_text__originalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(us_constitution_text__originalist_reading),
    narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(us_constitution_text__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high because the constraint transfers interpretive authority and policy outcomes from contemporary democratic majorities and affected groups to a fixed historical moment, systematically disadvantaging claims unrecognized in the 18th/19th centuries. Suppression is very high because the constraint's persistence depends on active enforcement: judicial appointment litmus tests, Federalist Society gatekeeping, law school hiring pressures, and the treatment of non-originalist opinions as illegitimate. Theater ratio is moderate because originalist historical analysis is genuine in many cases, but a growing share of opinions use originalist rhetoric to reach results aligned with conservative policy preferences rather than historical evidence. Accessibility collapse is high because accepting the originalist frame makes alternative interpretive methodologies appear lawless; resistance is moderate because living constitutionalism persists in academia, lower courts, and dissenting opinions.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the constraint is genuine coordination: it solves the democratic legitimacy problem of judicial review. From the payer seats, the same structure operates as enforced extraction: their constitutional claims are foreclosed by a methodology they reject, maintained by appointments they cannot influence. The engine computes this divergence from the structural data; the claimed mountain type reflects the originalist self-presentation, while the metrics reflect the observed operation.
 *
 * DIRECTIONALITY LOGIC:
 *   The conservative legal movement is the primary agenda setter (d ≈ 0.05): it built the institutional machinery and collects the policy payoff. Originalist judges are beneficiaries (d ≈ 0.15): they gain legitimacy and career advancement within the dominant paradigm. Non-originalist rights claimants are full targets (d ≈ 0.95): they bear the full cost of suppressed claims with no exit. Living constitutionalist judges are near-targets (d ≈ 0.85): their methodology is suppressed and their professional identity is locked to the losing side. General citizenry sits near symmetric (d ≈ 0.5): genuine stability benefit vs. diffuse adaptive-rights cost. The analytical observer sits at d = 0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (judicial activism) is contested: originalists say it persists; critics say originalism has become the vehicle for conservative judicial activism. The constraint persists not because the founding problem is solved, but because the institutional architecture (Federalist Society, appointment pipeline) now depends on originalism for its coherence and power. This is mandatrophy: the constraint's mandate (constraining judges) has been inverted (enabling a particular judicial faction), but the arrangement persists through institutional inertia and identity lock.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_methodology,
    'Is originalism a genuine natural constraint of legal interpretation (emerging from the nature of written constitutions) or a constructed methodology that benefits the conservative legal movement?',
    'Comparative analysis of constitutional interpretation across democratic systems: if fixed-meaning interpretation emerges universally without ideological coordination, it supports natural-law status; if it correlates with conservative legal movements, it supports constructed methodology.',
    'If constructed, the mountain claim fails and FSM reclassifies to tangled_rope; if natural, the beneficiary structure is incidental and mountain certification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_methodology, conceptual, 'Whether the originalist constraint is a natural feature of legal interpretation or an ideological project.').

omega_variable(
    historical_meaning_recoverability,
    'Can original public meaning be reliably recovered for modern controversies (digital privacy, reproductive rights, administrative state) that did not exist at ratification?',
    'Empirical study of originalist opinions: measure inter-judge agreement on original meaning for novel issues; track divergence between originalist methodology and conservative policy preferences.',
    'If meaning cannot be reliably recovered, the constraint''s coordination function is illusory and extraction is the dominant operation; if recoverable, the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_meaning_recoverability, empirical, 'Whether the constraint''s stated coordination function (historical recovery) is operationally realizable.').

omega_variable(
    suppression_mechanism_originalism,
    'Is the suppression of adaptive interpretation structural (appointment power, professional gatekeeping) or internalized (professional identity fusion, belief in originalism''s legitimacy)?',
    'Track post-exit suppression trajectory: if living constitutionalist judges who leave the bench continue to self-censor, or if law students internalize originalist frames without external pressure, internalized component is significant.',
    'If internalized, effective suppression exceeds the structural measure; the constraint persists even if appointment politics shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_originalism, empirical, 'Structural vs. internalized suppression in the originalist interpretive regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1971, us_constitution_text__originalist_reading, theater_ratio, 1971, 0.1).
narrative_ontology:measurement(us_c_tr_t1982, us_constitution_text__originalist_reading, theater_ratio, 1982, 0.15).
narrative_ontology:measurement(us_c_tr_t1993, us_constitution_text__originalist_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(us_c_tr_t2005, us_constitution_text__originalist_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(us_c_tr_t2016, us_constitution_text__originalist_reading, theater_ratio, 2016, 0.35).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_text__originalist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1971, us_constitution_text__originalist_reading, base_extractiveness, 1971, 0.15).
narrative_ontology:measurement(us_c_be_t1982, us_constitution_text__originalist_reading, base_extractiveness, 1982, 0.25).
narrative_ontology:measurement(us_c_be_t1993, us_constitution_text__originalist_reading, base_extractiveness, 1993, 0.35).
narrative_ontology:measurement(us_c_be_t2005, us_constitution_text__originalist_reading, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(us_c_be_t2016, us_constitution_text__originalist_reading, base_extractiveness, 2016, 0.65).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_text__originalist_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1971, us_constitution_text__originalist_reading, suppression_requirement, 1971, 0.2).
narrative_ontology:measurement(us_c_su_t1982, us_constitution_text__originalist_reading, suppression_requirement, 1982, 0.35).
narrative_ontology:measurement(us_c_su_t1993, us_constitution_text__originalist_reading, suppression_requirement, 1993, 0.5).
narrative_ontology:measurement(us_c_su_t2005, us_constitution_text__originalist_reading, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement(us_c_su_t2016, us_constitution_text__originalist_reading, suppression_requirement, 2016, 0.75).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_text__originalist_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__originalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(us_constitution_text__originalist_reading, 0.08).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__positivist_reading).

% DUAL FORMULATION NOTE:
% Originalist reading forecloses living constitutionalist reading (mutually exclusive core premises) but coexists with positivist reading (can hold both fixed meaning and enactment-based validity). The three readings form a constraint family around the US Constitution text kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_text__originalist_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
