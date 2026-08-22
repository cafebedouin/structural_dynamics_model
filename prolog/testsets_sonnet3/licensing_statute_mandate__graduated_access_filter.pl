% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__graduated_access_filter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Occupational Licensing Statute as Graduated Class-Access Filter
 *   domain: labor_economics/regulatory_policy/public_administration
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the licensing_statute_mandate
 *   kernel: the graduated_access_filter reading, which holds that statutory
 *   credential requirements — regardless of their stated public-safety
 *   rationale — operate structurally as a class and resource sorting
 *   mechanism. Under this reading, the requirement's binding constraint on
 *   entry is not documented competence but the applicant's prior possession
 *   of money, time, geographic proximity to training infrastructure, and a
 *   clean legal record. The beneficiary set (incumbent practitioners who
 *   cleared the barrier and now enjoy restricted competition, and training
 *   institutions that monetize the mandated pathway) and victim set (workers
 *   who possess the underlying skill or trainability but lack the resource
 *   base to clear the statutory hurdle) are drawn from this reading's own
 *   account of who wins and loses. This is a distinct constraint from the
 *   public_safety_coordination reading (which would locate ε near the
 *   coordination-benefit pole, with negligible extraction, because it takes
 *   the competence-signaling function at face value) and from the
 *   rent_seeking_suppression reading (which shares this reading's low opinion
 *   of the safety rationale but locates the mechanism purely in labor-supply
 *   restriction for incumbents, without this reading's specific emphasis on
 *   class/resource sorting as the operative filter variable). All three
 *   readings describe the same statutory apparatus but assign it structurally
 *   different ε, beneficiary sets, and victim sets — they are linked via
 *   network.affects_constraints, not merged into one story.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.71).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.68).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.71).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Occupational Licensing Statute as Graduated Class-Access Filter").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor_economics/regulatory_policy/public_administration").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, '8fbffbda-d9af-4925-a645-0b0d57eda9cd').
narrative_ontology:cs_kernel_codification('8fbffbda-d9af-4925-a645-0b0d57eda9cd', formalized).
narrative_ontology:cs_authority_grounding('8fbffbda-d9af-4925-a645-0b0d57eda9cd', extraction).
narrative_ontology:cs_interpretation_layer_present('8fbffbda-d9af-4925-a645-0b0d57eda9cd').
narrative_ontology:cs_reading_relation('8fbffbda-d9af-4925-a645-0b0d57eda9cd', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('8fbffbda-d9af-4925-a645-0b0d57eda9cd', licensing_statute_mandate__rent_seeking_suppression, influences).
narrative_ontology:cs_axiom('8fbffbda-d9af-4925-a645-0b0d57eda9cd', foundational, requirement_stringency_tracks_resource_access_not_competence).
narrative_ontology:cs_axiom_status(requirement_stringency_tracks_resource_access_not_competence, holdable).
narrative_ontology:cs_axiom_grounding('8fbffbda-d9af-4925-a645-0b0d57eda9cd', requirement_stringency_tracks_resource_access_not_competence, empirically_contingent).
narrative_ontology:cs_axiom('8fbffbda-d9af-4925-a645-0b0d57eda9cd', secondary, credential_pathway_cost_functions_as_de_facto_means_test).
narrative_ontology:cs_axiom_status(credential_pathway_cost_functions_as_de_facto_means_test, holdable).
narrative_ontology:cs_axiom_grounding('8fbffbda-d9af-4925-a645-0b0d57eda9cd', credential_pathway_cost_functions_as_de_facto_means_test, empirically_contingent).
narrative_ontology:cs_reference_frame('8fbffbda-d9af-4925-a645-0b0d57eda9cd', verified_competence_gatekeeping).
narrative_ontology:cs_drift_state('8fbffbda-d9af-4925-a645-0b0d57eda9cd', contemporary_licensing_proliferation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8fbffbda-d9af-4925-a645-0b0d57eda9cd', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credentialed_incumbent_practitioners).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, accredited_training_institutions).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, licensing_board_administrators).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, low_income_aspiring_workers).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, immigrant_workers_with_foreign_credentials).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, rural_workers_without_local_training_access).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, formerly_incarcerated_workers).
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

% Already hold the license, having typically had the family income, geographic proximity to training programs, or time flexibility needed to complete the credentialing pathway. Benefit directly from restricted labor supply — the statute shields their wages and market position from competitors who cannot clear the same barrier. Lobby to maintain or raise requirements through professional associations.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credentialed_incumbent_practitioners, beneficiary,
    organized, biographical, mobile, national).

% Collect tuition and fee revenue directly tied to the statutory requirement — the mandate is their customer acquisition mechanism. Sit on or advise the boards that set curriculum hour requirements, giving them influence over how large the mandated pathway becomes.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, accredited_training_institutions, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__graduated_access_filter, accredited_training_institutions, agenda_setter).

% Write and enforce the statutory requirements, set required hours and fees, adjudicate reciprocity and waiver requests, and administer the exam and disciplinary apparatus. Their institutional survival is tied to the requirement's continued existence and complexity.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, licensing_board_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Cannot afford the tuition, unpaid apprenticeship hours, or opportunity cost of the required training pathway while also supporting themselves or dependents. The requirement does not test for the skill differential it claims to certify but rather for the capacity to absorb months or years of unpaid or underpaid preparation — a resource test disguised as a competence test. Practicing without the license risks fines or criminal exposure, foreclosing informal-market entry.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, low_income_aspiring_workers, payer,
    powerless, biographical, trapped, regional).

% Hold demonstrated competence and often formal credentials from their country of origin but face non-recognition, forcing a full repeat of domestic training regardless of documented skill. The re-certification cost functions as a second barrier layered on top of the first, sorting by resources to redo training rather than by actual competence.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, immigrant_workers_with_foreign_credentials, payer,
    powerless, biographical, trapped, national).

% Live far from the concentrated urban locations of accredited programs, making completion require relocation costs, lost local wages, or long commutes that urban applicants do not bear. Distance to the credentialing pathway functions as an unstated but real geographic wealth test.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, rural_workers_without_local_training_access, payer,
    powerless, biographical, constrained, regional).

% Face categorical exclusion or discretionary denial from licensing boards on character-and-fitness grounds regardless of completed training, closing off entire occupational categories from re-entry into stable work after conviction — compounding an existing structural disadvantage rather than testing present competence.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, formerly_incarcerated_workers, payer,
    powerless, biographical, trapped, national).

% Receive some assurance of a practitioner floor but also pay higher prices from restricted supply and face reduced access in underserved areas where the barrier has excluded potential local providers entirely.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, consumers_of_licensed_services, beneficiary,
    moderate, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__graduated_access_filter, consumers_of_licensed_services, payer).

% Nominally set the statutory framework but in practice defer to incumbent professional associations and board recommendations when drafting requirement levels; rarely hear directly from excluded workers who lack lobbying capacity, so the class-sorting effect is largely invisible in the legislative record.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, state_legislators, excluded,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__graduated_access_filter, credentialed_incumbent_practitioners).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__graduated_access_filter, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides consumers a legible signal that a licensed practitioner has completed some minimum standardized preparation, reducing search costs for buyers who cannot otherwise verify quality before purchase.
% TRANSFER_FUNCTION: Moves market access from anyone willing and able to work at a competitive wage to a subset defined by prior possession of money, time, geographic proximity, and clean legal record — and moves rents from excluded workers and consumers toward incumbent practitioners and training institutions through restricted supply and mandated tuition spending.
% ABSENT_VOICES: Excluded workers — the low-income, rural, immigrant, and formerly incarcerated populations who cannot clear the barrier — are almost never represented in the legislative or board rulemaking process that sets requirement levels; they lack the standing, funding, or organizational capacity that incumbent professional associations bring to hearings.
% DISAPPEARANCE_RATIONALE: If the statutory requirement vanished overnight, currently excluded workers would enter the occupation at market wages, incumbent wage premiums tied to restricted supply would compress, training institutions dependent on mandated enrollment would lose a captive revenue stream, and consumers in underserved areas would gain access to providers who are currently locked out — a substantial reallocation of who works and who profits.
% FOUNDING_PROBLEM: Historically framed as preventing incompetent or fraudulent practitioners from harming consumers who cannot verify quality before purchase.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent practitioner associations and licensing boards attest the safety problem remains live and requires the current barrier level. Independent economic research (labor economists studying occupational licensing across states, and legislative sunset-review commissions in several states) attests that required hours and fees in many licensed occupations bear little relationship to documented harm rates and instead track political influence of incumbent associations — corroboration from outside the beneficiary set exists but is contested by those same beneficiaries.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__graduated_access_filter, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__graduated_access_filter, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.71 at interval end) and rising because the requirement's practical effect — sorting by capacity to absorb unpaid preparation time and tuition cost — has, on this reading, intensified as required training hours and fees have grown over successive statutory revisions in many licensed occupations, a pattern documented in sunset-review literature. Suppression (0.68) reflects the criminal and civil enforcement apparatus against unlicensed practice, which forecloses the informal-market exit that would otherwise let excluded workers compete on price. Theater ratio (0.42) reflects that some genuine competence-testing function persists (the exam component, in particular) even as the coursework-hour and reciprocity-denial components function increasingly as pure gatekeeping, on this reading's assessment. Accessibility collapse (0.62) is moderate-high but not mountain-grade: informal, unlicensed, or under-the-table practice remains a real if criminalized alternative, distinguishing this from a fully collapsed mountain-type barrier. Resistance (0.55) captures active organizing by excluded-worker advocacy groups and some legislative reform efforts, which the licensing board and incumbent associations resist.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed incumbents and training institutions sit near the full-beneficiary end: the statute subsidizes their market position and revenue directly, and their exit options are mobile or arbitrage-grade (they can relocate practice, expand into adjacent credentialing markets, or lobby for favorable rule changes). The four victim groups are powerless with trapped or constrained exit — none can afford to simply leave the labor market, and none has a viable path around the statute except absorbing its cost, which is exactly the resource test this reading identifies as the actual filter. Consumers occupy a genuinely mixed position (moderate power, some benefit from quality assurance, some cost from restricted supply and reduced rural access) and are authored as beneficiary/payer dual-role accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination story (public-safety signaling) may have had real force at the statute's founding when practitioner quality genuinely could not be verified by consumers. This reading's central claim is that even if that founding problem was once live, the specific form the requirement has taken — hour counts, fee levels, non-portable state-specific credentials, categorical exclusion of formerly incarcerated applicants regardless of trade-relevant conduct — has drifted well past what any documented safety-outcome differential would justify, per the sunset-review corroboration cited in six_questions. Classifying this as snare rather than tangled_rope reflects this reading's assessment that the coordination function, while not entirely absent, is thin enough relative to the extraction that 'the coordination story is cover' is the more accurate structural description; a tangled_rope classification would be more apt for the public_safety_coordination sibling reading, which weights the competence-signaling function more heavily.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    class_sorting_vs_safety_signal_weight,
    'Does the statutory requirement''s actual barrier height track documented harm-prevention value, or does it track the resource threshold needed to exclude lower-class applicants regardless of trade competence?',
    'Compare required training hours and fees across states/occupations against documented consumer-harm incident rates; states with materially lower requirements and no corresponding harm increase would support the graduated_access_filter reading over the public_safety_coordination reading.',
    'If harm rates track requirement stringency closely, the public_safety_coordination reading gains support and this reading''s high ε would be an overstatement; if harm rates are flat across wide variation in requirement stringency, this reading''s structural claim is strongly corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(class_sorting_vs_safety_signal_weight, empirical, 'Whether barrier height tracks safety outcomes or class-sorting capacity.').

omega_variable(
    kernel_reading_selection_basis,
    'Which of the three sibling readings (graduated_access_filter, public_safety_coordination, rent_seeking_suppression) best characterizes the DOMINANT operative mechanism of any given specific licensing statute, and is this a single fact or does it vary by occupation?',
    'Occupation-by-occupation empirical audit: exam pass-rate differentials by prior income/geography, reciprocity denial rates, and reduced-form wage-premium estimates for incumbents would triangulate which mechanism dominates for a given license.',
    'The three readings are not mutually exclusive at the level of any single real statute — a given license may exhibit genuine safety-signal function, incumbent rent extraction, AND class-sorting simultaneously in different proportions. This story deliberately isolates the class-sorting component as its own constraint per the ε-invariance principle rather than averaging across mechanisms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'How the three kernel readings relate when applied to any single concrete licensing statute.').

omega_variable(
    informal_market_exit_viability,
    'How viable is unlicensed informal-market practice as a genuine exit option for excluded workers, given enforcement intensity and consumer willingness to hire unlicensed providers?',
    'Survey enforcement action frequency and informal-market wage/volume data in occupations with active unlicensed practice (e.g., informal home care, unlicensed contracting) to assess whether this functions as a real safety valve or a marginal, high-risk activity.',
    'High informal-market viability would lower the effective accessibility_collapse score; near-zero viability (due to enforcement intensity or consumer risk-aversion) would push this constraint closer to mountain-grade collapse despite the formally lower authored value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_market_exit_viability, empirical, 'Whether unlicensed practice is a real exit option or a criminalized dead end.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__graduated_access_filter, theater_ratio, 0, 0.22).
narrative_ontology:measurement(lice_tr_t8, licensing_statute_mandate__graduated_access_filter, theater_ratio, 8, 0.27).
narrative_ontology:measurement(lice_tr_t16, licensing_statute_mandate__graduated_access_filter, theater_ratio, 16, 0.31).
narrative_ontology:measurement(lice_tr_t24, licensing_statute_mandate__graduated_access_filter, theater_ratio, 24, 0.35).
narrative_ontology:measurement(lice_tr_t32, licensing_statute_mandate__graduated_access_filter, theater_ratio, 32, 0.39).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__graduated_access_filter, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(lice_be_t8, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(lice_be_t16, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(lice_be_t24, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(lice_be_t32, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(lice_su_t8, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(lice_su_t16, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 16, 0.59).
narrative_ontology:measurement(lice_su_t24, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(lice_su_t32, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 32, 0.66).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__graduated_access_filter, identity_coordination).
narrative_ontology:boltzmann_floor_override(licensing_statute_mandate__graduated_access_filter, 0.08).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, rent_seeking_suppression).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the licensing_statute_mandate kernel. graduated_access_filter (this file) authors ε=0.71 (snare) centered on class/resource sorting as the operative mechanism. public_safety_coordination authors a substantially lower ε centered on genuine competence-verification value. rent_seeking_suppression authors a comparably high ε but centers the mechanism on incumbent labor-supply restriction rather than class sorting specifically. All three share the same underlying statutory text but diverge in beneficiary/victim structure and mechanism — per the ε-invariance principle they are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
