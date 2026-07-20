% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__user_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__user_centric_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: fair_use_four_factor_test__user_centric_reading
 *   human_readable: Fair Use Four-Factor Test (User-Centric Reading)
 *   domain: legal/intellectual_property
 *
 * SUMMARY:
 *   This constraint is the user-centric reading of the
 *   fair_use_four_factor_test kernel, which treats fair use as an affirmative
 *   user right and interprets the four statutory factors to prioritize public
 *   access and cultural production over proprietary control. It extracts from
 *   rights holders by limiting their licensing monopoly, but coordinates a
 *   genuine collective-action problem: the prohibitive transaction costs of
 *   clearing rights for every educational, critical, or transformative use.
 *   The structural asymmetry between diffuse public beneficiaries and
 *   concentrated rights-holder victims makes it a tangled rope.
 *
 * KEY AGENTS:
 *   - public_users: Primary beneficiary (moderate/constrained) â gains access without payment
 *   - educational_institutions: Primary beneficiary (institutional/constrained) â gains pedagogical and preservation freedom
 *   - rights_holders: Primary payer (powerful/constrained) â bears lost licensing revenue and exclusivity
 *   - federal_judiciary: Agenda setter (institutional/analytical) â administers the four-factor balancing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, 0.28).
domain_priors:suppression_score(fair_use_four_factor_test__user_centric_reading, 0.45).
domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__user_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__user_centric_reading, "Fair Use Four-Factor Test (User-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__user_centric_reading, "legal/intellectual_property").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__user_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, '71dbbc6b-1a2e-405b-95d4-fa3a33d2b4d7').
narrative_ontology:cs_kernel_codification('71dbbc6b-1a2e-405b-95d4-fa3a33d2b4d7', formalized).
narrative_ontology:cs_authority_grounding('71dbbc6b-1a2e-405b-95d4-fa3a33d2b4d7', lineage).
narrative_ontology:cs_interpretation_layer_present('71dbbc6b-1a2e-405b-95d4-fa3a33d2b4d7').
narrative_ontology:cs_reading_relation('71dbbc6b-1a2e-405b-95d4-fa3a33d2b4d7', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('71dbbc6b-1a2e-405b-95d4-fa3a33d2b4d7', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_axiom('71dbbc6b-1a2e-405b-95d4-fa3a33d2b4d7', foundational, public_access_as_primary_copyright_purpose).
narrative_ontology:cs_axiom_status(public_access_as_primary_copyright_purpose, holdable).
narrative_ontology:cs_axiom_grounding('71dbbc6b-1a2e-405b-95d4-fa3a33d2b4d7', public_access_as_primary_copyright_purpose, deontological).
narrative_ontology:cs_reference_frame('71dbbc6b-1a2e-405b-95d4-fa3a33d2b4d7', copyright_as_public_bargain).
narrative_ontology:cs_drift_state('71dbbc6b-1a2e-405b-95d4-fa3a33d2b4d7', contemporary_digital_copyright_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('71dbbc6b-1a2e-405b-95d4-fa3a33d2b4d7', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, public_users).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, educational_institutions).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, rights_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Access copyrighted works for commentary, criticism, education, and cultural participation without individualized licensing or payment, relying on the four-factor test as a shield against infringement claims.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, public_users, beneficiary,
    moderate, biographical, constrained, national).

% Rely on fair use to teach, research, preserve, and digitize materials, avoiding prohibitive transaction costs that would come with clearing rights for every classroom or library use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, educational_institutions, beneficiary,
    institutional, generational, constrained, national).

% Bear reduced licensing revenue and loss of exclusivity when courts find that public or educational uses are fair; they fund litigation to narrow the doctrine and resist its expansion.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, rights_holders, payer,
    powerful, biographical, constrained, national).

% Administers the four-factor test in infringement litigation, weighing purpose, nature, amount, and market effect; under this reading, the weighing is calibrated to preserve public access and cultural production.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__user_centric_reading, diffuse).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__user_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables public access to knowledge and culture by permitting certain unauthorized uses of copyrighted works without the transaction costs of individualized licensing.
% TRANSFER_FUNCTION: Transfers the ability to control and monetize specific uses from rights holders to the public and educational institutions.
% ABSENT_VOICES: Individual creators who lack collective bargaining power and would prefer direct licensing; also foreign rights holders operating under civil-law traditions without fair use.
% DISAPPEARANCE_RATIONALE: Without the user-centric fair use shield, educational institutions would face massive clearance costs, documentary and critical speech would chill, and the public would lose a primary legal pathway for unauthorized but socially valuable uses.
% FOUNDING_PROBLEM: Copyright's grant of exclusive rights could block access to knowledge, commentary, and cultural participation if every secondary use required permission from the rights holder.
% FOUNDING_PROBLEM_CORROBORATION: Historical legislative history of the 1976 Copyright Act and early Supreme Court precedent (e.g., Sony Corp. of America v. Universal City Studios) corroborate the access problem from outside the immediate beneficiary set; rights holders and their trade associations contest its current severity, arguing that modern licensing markets have obviated the need.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__user_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__user_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__user_centric_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_four_factor_test__user_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__user_centric_reading, 0.28, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__user_centric_reading_tests).
:- end_tests(fair_use_four_factor_test__user_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the user-centric reading is designed to permit only socially valuable uses, not to maximize extraction; suppression is moderate (0.45) because rights holders actively litigate to constrain the doctrine's reach; theater is low (0.18) because the coordination function is real and substantial. Resistance is significant (0.60) because major rights-holder industries consistently challenge expansive fair use rulings. Accessibility collapse is moderate-high (0.65) because, once the user-centric reading is accepted, the alternative of licensing every use becomes prohibitively costly and culturally impractical.
 *
 * PERSPECTIVAL GAP:
 *   The rights-holder seat experiences the constraint as extraction: a forced waiver of property rights that transfers value to users. The public-user and educational-institution seats experience it as a necessary coordination mechanism that prevents copyright from blocking speech and education. The judiciary sits in between, performing the balancing. The engine will compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Public users and educational institutions are declared beneficiaries (low d, subsidized by the constraint). Rights holders are declared victims (high d, targeted by the constraint). The federal judiciary is the agenda setter with analytical exit (d near symmetric but institutionally biased toward maintaining the doctrine). Because rights holders are powerful but constrained within the copyright system, their directionality is high; because users are moderate and constrained by the law's boundaries, their directionality is low but not zero.
 *
 * MANDATROPHY ANALYSIS:
 *   The user-centric reading prevents mislabeling by keeping the coordination function in view: fair use genuinely solves a clearance-cost problem that would otherwise paralyze education and commentary. It is not a snare because the public benefit is structurally real, not a cover story. It is not a rope because the cost is not symmetrically shared: rights holders bear a concentrated, identifiable loss. It is not a piton because the theater ratio is low and the function is not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the user-centric reading of fair use a stable legal interpretation or a contested frame that could collapse under rights-holder lobbying or doctrinal retrenchment?',
    'Track Supreme Court and Circuit Court opinions for dominance of user-centric versus creator-centric framing over successive terms.',
    'If the creator-centric reading regains dominance, the constraint''s effective extraction from rights holders would fall and its classification could shift toward rope; if the user-centric reading solidifies, it remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contested kernel reading stability').

omega_variable(
    market_harm_vs_public_access,
    'Does the user-centric reading cause measurable market harm that undermines creator incentives, or does the access it enables generate net cultural surplus?',
    'Empirical studies on creator income trends in sectors with high fair-use prevalence (e.g., documentary film, education, remix culture).',
    'If net harm is proven, the reading''s extractiveness metric rises and its coordination function weakens; if net surplus, the low epsilon is validated and the reading leans toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_vs_public_access, empirical, 'Net effect of fair use on creator markets').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_uc_tr_t0, fair_use_four_factor_test__user_centric_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fair_use_uc_tr_t8, fair_use_four_factor_test__user_centric_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(fair_use_uc_tr_t16, fair_use_four_factor_test__user_centric_reading, theater_ratio, 16, 0.13).
narrative_ontology:measurement(fair_use_uc_tr_t24, fair_use_four_factor_test__user_centric_reading, theater_ratio, 24, 0.15).
narrative_ontology:measurement(fair_use_uc_tr_t32, fair_use_four_factor_test__user_centric_reading, theater_ratio, 32, 0.16).
narrative_ontology:measurement(fair_use_uc_tr_t40, fair_use_four_factor_test__user_centric_reading, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(fair_use_uc_be_t0, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(fair_use_uc_be_t8, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 8, 0.14).
narrative_ontology:measurement(fair_use_uc_be_t16, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 16, 0.18).
narrative_ontology:measurement(fair_use_uc_be_t24, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 24, 0.22).
narrative_ontology:measurement(fair_use_uc_be_t32, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 32, 0.25).
narrative_ontology:measurement(fair_use_uc_be_t40, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 40, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(fair_use_four_factor_test__user_centric_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__user_centric_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
