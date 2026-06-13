% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__creator_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__creator_centric_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fair_use_four_factor_test__creator_centric_reading
 *   human_readable: Fair Use as Narrow Exception: Creator-Centric Four-Factor Test
 *   domain: legal/intellectual_property
 *
 * SUMMARY:
 *   Fair use under US copyright law is a four-factor test: (1) purpose and
 *   character of use; (2) nature of the copyrighted work; (3) amount and
 *   substantiality of the portion used; (4) effect on the market for the
 *   original. This constraint instantiates ONE READING of how those factors
 *   should be weighted — the creator-centric reading, which treats fair use
 *   as a narrow exception meant to preserve author incentives while keeping
 *   most secondary uses within the permission-and-licensing regime. Under
 *   this reading, transformative use is not presumptively fair; market harm
 *   is heavily weighted; and commercial non-criticism uses are presumptively
 *   infringing. The constraint operates by courts applying this doctrine in
 *   adjudication, with doctrinal precedent (especially Harper & Row v. Nation
 *   Enterprises, Campbell v. Acuff-Rose, and Sony v. Universal shaping the
 *   baseline). The rival readings — transformative-use-centric and
 *   user-centric — are OTHER constraints with different epsilon values and
 *   beneficiary/victim structures; they are NOT described here. This story
 *   describes ONLY the creator-centric reading as a single, ε-invariant
 *   constraint.
 *
 * KEY AGENTS:
 *   - copyright_holders: institutional beneficiary, agenda-setter; controls litigation strategy and standard-setting through major copyright suits; extracts licensing revenue from transformative uses
 *   - original_creators: moderate-power beneficiary; framed as primary constituency but often benefit only indirectly through assignment to holders
 *   - transformative_users: moderate-power payers; face litigation risk, licensing costs, and chilling effect; suppression is high for identity-locked derivative creators
 *   - derivative_creators: powerless, identity-locked payers; their creative practice IS transformation; exit means abandoning the practice
 *   - cultural_commons: organized, trapped-exit payers; libraries, archives, heritage organizations constrained by narrow fair use in digitization and preservation
 *   - courts: institutional agenda-setters; apply the four-factor test and weight creator incentives heavily in precedent
 *   - public_interest_advocates: excluded from doctrine development; would benefit from broader fair use but are not seated in standard-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.68).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.61).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use as Narrow Exception: Creator-Centric Four-Factor Test").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal/intellectual_property").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, 'a9782f6e-022c-430e-b43e-bbcbfcffa30d').
narrative_ontology:cs_kernel_codification('a9782f6e-022c-430e-b43e-bbcbfcffa30d', fixed_text).
narrative_ontology:cs_authority_grounding('a9782f6e-022c-430e-b43e-bbcbfcffa30d', lineage).
narrative_ontology:cs_interpretation_layer_present('a9782f6e-022c-430e-b43e-bbcbfcffa30d').
narrative_ontology:cs_reading_relation('a9782f6e-022c-430e-b43e-bbcbfcffa30d', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9782f6e-022c-430e-b43e-bbcbfcffa30d', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_axiom('a9782f6e-022c-430e-b43e-bbcbfcffa30d', foundational, market_harm_primacy_in_fair_use).
narrative_ontology:cs_axiom_status(market_harm_primacy_in_fair_use, holdable).
narrative_ontology:cs_axiom_grounding('a9782f6e-022c-430e-b43e-bbcbfcffa30d', market_harm_primacy_in_fair_use, deontological).
narrative_ontology:cs_axiom('a9782f6e-022c-430e-b43e-bbcbfcffa30d', foundational, creator_incentives_justify_narrow_exception).
narrative_ontology:cs_axiom_status(creator_incentives_justify_narrow_exception, holdable).
narrative_ontology:cs_axiom_grounding('a9782f6e-022c-430e-b43e-bbcbfcffa30d', creator_incentives_justify_narrow_exception, instrumental).
narrative_ontology:cs_reference_frame('a9782f6e-022c-430e-b43e-bbcbfcffa30d', narrow_exception_to_exclusive_rights).
narrative_ontology:cs_drift_state('a9782f6e-022c-430e-b43e-bbcbfcffa30d', contemporary_digital_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a9782f6e-022c-430e-b43e-bbcbfcffa30d', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, original_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_users).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, derivative_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, cultural_commons).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(fair_use_four_factor_test__creator_centric_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__creator_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__creator_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.68 (endpoint) reflects the constraint's operation as a suppression mechanism that requires licensing for most transformative uses and imposes litigation risk on secondary creators. The rise from 0.42 to 0.68 over the interval tracks the accumulation of precedent strengthening the creator-centric weight (mid-interval surge from t=10 to t=25, then plateau). Suppression at 0.61 is driven by litigation risk (copyright holders' willingness to sue) and chilling effects (creators avoiding risky uses). The suppression rise from 0.38 to 0.61 parallels the extraction rise, indicating that the constraint's persistence is active — enforcement machinery intensifies as licensing revenue opportunities grow. Theater_ratio at 0.28 is moderate: the constraint's stated coordination function (preserving creator incentives) is genuine and partially real, but a growing share of enforcement activity serves rent extraction (licensing fees from transformative uses that provide no harm to the original creator's market). The one-shared-time-grid discipline is maintained: every metric is authored at every examined point (t=0,5,10,15,20,25,30,35,40), and all measurements are observed (not projected, not imputed).
 *
 * PERSPECTIVAL GAP:
 *   The copyright_holders seat experiences this constraint as legitimate coordination (creator incentives are real, licensing is reasonable compensation). The transformative_users and derivative_creators seats experience it as pure extraction (the doctrine suppresses their practice, raises costs, and extracts value without corresponding benefit). Courts apply the doctrine as neutral doctrine-development, but their weighted interpretation favors rights holders, creating a structural asymmetry. The engine should compute divergent type classifications across seats: copyright_holders may see rope or tangled-rope-favorable-to-coordination; transformative_users should compute as snare or tangled-rope-unfavorable. The authored claim (tangled_rope) reflects the story-level reality that BOTH coordination and extraction are present, but the architecture amplifies extraction for powerless targets (derivative_creators) and for trapped-exit payers (cultural_commons).
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright_holders are beneficiaries (low d): the constraint protects their control, generates licensing revenue, and suppresses competition from transformative uses. Original_creators are beneficiaries (d~0.3): they benefit from incentive signals but often do not directly control licensing and are insulated from fair use disputes by assignment. Transformative_users are payers (d~0.75): they must license or face litigation risk. Derivative_creators are payers with high d (~0.85): their creative practice is directly suppressed by this reading; exit means abandoning the practice (identity_locked). Cultural_commons are payers with moderate d (~0.72): trapped exit (they cannot cease preserving) amplifies suppression. Courts are neutral but structurally positioned to apply the creator-centric weight, so their directionality is analytical (observer seat). Public_interest_advocates are excluded — their absence from the doctrine-setting seat keeps d derivation focused on present parties.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (creator incentives and copyright justification) is contested as to status. Copyright holders argue incentives are still needed (founding_problem_status = live). Transformative-use advocates and educational organizations argue the founding problem is solved — authors still create, blanket licensing and digital distribution exist, and the doctrine persists as rent extraction (founding_problem_status = dead). This reading (creator-centric) ASSUMES the founding problem is live and uses that assumption to justify narrow fair use. The mismatch is: this reading claims the constraint is COORDINATION (tangled_rope) to solve the incentive problem, but if the founding problem is dead, the constraint becomes pure EXTRACTION (snare or piton). The engine's mismatch-checker (founding_problem_status x disappearance_verdict) will flag this: dead + world_rearranges = mandatrophy (function is gone but constraint persists). This is NOT a defect in the authoring — it is exactly the tension the corpus is designed to detect. The constraint's persistence after the founding problem dies is the signal of extractive drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_boundary_transformative_vs_creator,
    'This constraint is ONE READING of the fair use kernel. Do the four factors logically foreclose a transformative-use reading, or do they coexist as competing legitimate interpretations of the same statutory text?',
    'Examine court decisions that prioritize transformativeness against decisions that weight market harm and commercial vs. non-commercial use equally. If both lines persist and neither is logically foreclosed by the statute, the relations are coexistence, not foreclosure. If one reading''s axioms directly contradict the other''s core premise (e.g., if creator-centric reading asserts market-harm precedence and transformative reading asserts transformativeness precedence), assess whether a single framework of statutory interpretation can hold both or whether they are truly incompatible.',
    'If foreclosure: the creator-centric reading excludes the transformative-use reading from legitimate legal space. If coexistence: both are live doctrinal positions and the constraint maps a site of ongoing dispute. If influences: the creator-centric reading shifts legitimacy conditions but does not rule out transformativeness — the transformative-use reading must work within stronger market-harm constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_transformative_vs_creator, conceptual, 'Whether readings of the four-factor test logically foreclose or coexist.').

omega_variable(
    creator_incentive_sufficiency,
    'Is the degree of extraction under this reading (high epsilon, suppression of transformative use) actually necessary to sustain author/creator incentives, or do authors continue to create and invest under broader fair use regimes?',
    'Comparative analysis of creation rates, investment in new works, and author earnings across jurisdictions with narrow fair use (US), broad fair use (Canada, EU), and no copyright (pre-industrial). If creation thrives in broad-fair-use jurisdictions, the creator-incentive claim is empirically overstated. If creation collapses, the claim holds.',
    'If empirical evidence shows creation is robust under broader fair use, the vindicated proposition (creator_incentive_doctrine) is factually undermined, and the extracted constraint becomes pure rent-seeking, not justified coordination overhead. The constraint would reclassify from tangled_rope (genuine coordination + extraction) to snare (pure extraction with a coordination cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_incentive_sufficiency, empirical, 'Whether the degree of suppression is empirically necessary for creator incentives.').

omega_variable(
    suppression_mechanism_chilling_vs_legal,
    'Is the measured suppression of transformative users structural (legal barriers: licensing costs, litigation risk, enforcement machinery) or internalized (the constraint has convinced users it is legitimate to refrain from transformative use)?',
    'Jurisdictions that adopt broader fair use: do transformative creators immediately resume practices, or do they carry suppression forward even when legal barriers are removed? If suppression persists post-barrier-removal, it is partially internalized. If practices resume immediately, suppression is primarily structural.',
    'If structural: removing the legal constraint should rapidly liberate transformative use. If internalized: the constraint''s effect persists even after legal change, suggesting the doctrine has reshaped users'' beliefs about legitimacy. Internalized suppression is more extractive (the target carries the constraint with them after formal exit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_chilling_vs_legal, empirical, 'Whether suppression of transformative use is structural or internalized.').

omega_variable(
    sibling_reading_foreclosure_analysis,
    'Does this reading''s core axiom (creator-incentive-dominance) logically foreclose the user-centric reading''s axiom (public-access-as-fundamental-right), or are both live statutory interpretations that compete but neither rules out the other?',
    'Statutory interpretation: can 17 U.S.C. § 107 (the fair use statute) be consistently read to prioritize creator incentives WHILE ALSO prioritizing public access as a fundamental right, or do these priorities necessarily conflict? If both can be read from the same text by different methodologies (originalism vs. living constitution, economic-efficiency vs. rights-based framing), they coexist. If one interpretation directly negates the logical possibility of the other, they foreclose.',
    'Foreclosure: the creator-centric reading claims exclusive legitimacy and the constraint operates as a near-total suppression of the user-centric reading. Coexistence: the constraint is a site of ongoing contestation and multiple readings remain institutionally live. Influences: the creator-centric reading shifts which side bears burden of proof but does not rule out user-centric wins.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_analysis, conceptual, 'Whether the creator-centric reading forecloses, coexists with, or influences the user-centric reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fair_tr_t5, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(fair_tr_t10, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(fair_tr_t15, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(fair_tr_t20, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(fair_tr_t25, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(fair_tr_t30, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(fair_tr_t35, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement(fair_tr_t40, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fair_be_t5, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fair_be_t10, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(fair_be_t15, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(fair_be_t20, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(fair_be_t25, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(fair_be_t30, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(fair_be_t35, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(fair_be_t40, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(fair_su_t5, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 5, 0.43).
narrative_ontology:measurement(fair_su_t10, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(fair_su_t15, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(fair_su_t20, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(fair_su_t25, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 25, 0.59).
narrative_ontology:measurement(fair_su_t30, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 30, 0.61).
narrative_ontology:measurement(fair_su_t35, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 35, 0.61).
narrative_ontology:measurement(fair_su_t40, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__creator_centric_reading, 0.18).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__transformative_use_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__user_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, copyright_licensing_market__creator_revenue_extraction).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, digital_preservation_constraint__public_access).

% DUAL FORMULATION NOTE:
% The fair_use_four_factor_test kernel decomposes into three constraint stories, each representing a different reading of how the four statutory factors should be weighted. The creator_centric_reading (this story) treats market harm and creator incentives as primary; it produces high epsilon because the reading suppresses transformative uses through licensing requirements and litigation risk. The transformative_use_reading (sibling constraint) weights transformativeness as dominant and produces lower epsilon because transformative uses are presumptively fair. The user_centric_reading (sibling constraint) prioritizes public access and educational use, producing the lowest epsilon because broad fair use serves the public interest. These are three ε-invariant constraints with different beneficiary/victim structures, not three observables of one constraint. The epsilon values differ because the readings instantiate genuinely different structural arrangements (different weights shift who benefits and who bears costs). All three stories link via network.affects_constraints to document the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
