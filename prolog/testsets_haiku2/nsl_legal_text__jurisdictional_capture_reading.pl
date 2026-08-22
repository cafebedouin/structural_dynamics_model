% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__jurisdictional_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__jurisdictional_capture_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: nsl_legal_text__jurisdictional_capture_reading
 *   human_readable: National Security Law as Jurisdictional Capture and Legal System Transplantation
 *   domain: constitutional/political/international
 *
 * SUMMARY:
 *   The National Security Law (NSL), enacted in 2020, operates as a vehicle
 *   for mainland legal system transplantation into Hong Kong. Framed as a
 *   security measure responding to 2019 unrest, the NSL functions as a
 *   jurisdictional capture mechanism: it gives mainland security apparatus
 *   authority over cases nominally within Hong Kong courts, introduces
 *   mainland legal doctrines (state security supremacy, administrative
 *   discretion, restricted due process) into common law space, and erodes the
 *   institutional autonomy of Hong Kong's judiciary and legal profession.
 *   This constraint story models THIS READING — the jurisdictional-capture
 *   reading — independently: the NSL as a mechanism through which mainland
 *   institutions systematically extract Hong Kong's institutional
 *   independence and legal system autonomy, not as legitimate sovereign
 *   restoration (the sovereignty_restoration_reading) or as permanent
 *   democratic closure (the democratic_enclosure_reading). The claim/metric
 *   gap is intentional: the constraint is claimed as tangled rope (nominally
 *   coordinating security with legal procedure) while the metrics describe
 *   substantially extractive, actively enforced operation; the engine
 *   measures that divergence as diagnostic of institutional capture.
 *
 * KEY AGENTS:
 *   - hong_kong_judiciary: victim seat (institutional autonomy compromised, identity-locked to common law training, no exit except system departure)
 *   - hong_kong_legal_profession: victim seat (constrained practice scope, professional identity fusion, elevated political risk)
 *   - mainland_security_apparatus: agenda-setter and primary beneficiary (sets NSL scope, enforces across jurisdictions, extracts institutional authority without legitimacy cost)
 *   - central_government_authority: secondary beneficiary (vindicates sovereignty doctrines, establishes precedent for legal transplantation)
 *   - hong_kong_civil_society: excluded (would object but has no seat in NSL interpretation; exclusion is structural)
 *   - international_legal_community: observer (documents drift, lacks enforcement authority)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.68).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.72).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "National Security Law as Jurisdictional Capture and Legal System Transplantation").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional/political/international").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, 'aad4097e-3242-40da-9e9a-1cd325489805').
narrative_ontology:cs_kernel_codification('aad4097e-3242-40da-9e9a-1cd325489805', formalized).
narrative_ontology:cs_authority_grounding('aad4097e-3242-40da-9e9a-1cd325489805', extraction).
narrative_ontology:cs_interpretation_layer_present('aad4097e-3242-40da-9e9a-1cd325489805').
narrative_ontology:cs_reading_relation('aad4097e-3242-40da-9e9a-1cd325489805', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('aad4097e-3242-40da-9e9a-1cd325489805', nsl_legal_text__democratic_enclosure_reading, influences).
narrative_ontology:cs_axiom('aad4097e-3242-40da-9e9a-1cd325489805', foundational, common_law_autonomy_is_institutional_good).
narrative_ontology:cs_axiom_status(common_law_autonomy_is_institutional_good, holdable).
narrative_ontology:cs_axiom_grounding('aad4097e-3242-40da-9e9a-1cd325489805', common_law_autonomy_is_institutional_good, deontological).
narrative_ontology:cs_axiom('aad4097e-3242-40da-9e9a-1cd325489805', foundational, mainland_legal_doctrines_incompatible_with_adversarial_independence).
narrative_ontology:cs_axiom_status(mainland_legal_doctrines_incompatible_with_adversarial_independence, holdable).
narrative_ontology:cs_axiom_grounding('aad4097e-3242-40da-9e9a-1cd325489805', mainland_legal_doctrines_incompatible_with_adversarial_independence, instrumental).
narrative_ontology:cs_reference_frame('aad4097e-3242-40da-9e9a-1cd325489805', hong_kong_common_law_autonomy).
narrative_ontology:cs_drift_state('aad4097e-3242-40da-9e9a-1cd325489805', contemporary_post_nsl_normalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('aad4097e-3242-40da-9e9a-1cd325489805', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, central_government_authority).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, common_law_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Hong Kong courts are subject to the NSL's expanded security jurisdiction, which overlaps and in critical cases overrides their common law authority. Judges face political pressure in sensitive cases, diminished control over statutory interpretation, and reduced autonomy in bail, sentencing, and evidence admission decisions. Exit for institutional judges means departing the entire judicial system or accepting subordinate roles — both culturally and professionally devastating for those trained and identified with the common law bench.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary, payer,
    institutional, generational, identity_locked, local).

% Lawyers face NSL-backed constraints on client representation, statutory interpretation, and courtroom advocacy in security cases. The Law Society operates under implicit political scrutiny. Senior barristers and solicitors with decade-long careers find their professional identity and practice area compressed. Some exit Hong Kong; most remain but operate in a narrowed scope with elevated professional risk.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession, payer,
    powerful, biographical, constrained, local).

% Sets the NSL's scope and interpretation, coordinates enforcement with Hong Kong authorities, and benefits from the law's expansion into areas traditionally governed by Hong Kong common law. Gains enforcement reach without bearing the legitimacy cost of explicit legal system replacement. Can appeal NSL cases to mainland authorities or use political pressure to override Hong Kong judicial outcomes in sensitive cases.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).

% Uses the NSL as a vehicle to extend mainland legal doctrines (state security supremacy, Party-aligned interpretation, administrative discretion) into Hong Kong without formally abolishing common law. Vindicates the principle of sovereignty over autonomous institutions and establishes precedent for legal system transplantation in other jurisdictions. Bears no cost; the institutional autonomy loss is borne by Hong Kong actors.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, central_government_authority, beneficiary,
    institutional, civilizational, arbitrage, continental).

% Young lawyers, judges in training, and mid-career practitioners face a compressed career path in a system where their core professional identity — adversarial common law practice, presumption of innocence, judicial independence as a first principle — is now subordinate to security apparatus directives. Professional identity fusion makes exit psychologically and economically devastating even when legally possible.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, common_law_practitioners, payer,
    moderate, biographical, identity_locked, local).

% Would argue that common law protections for civil liberties, due process, and public participation are irreplaceable; they have no seat at the table where NSL scope and enforcement are determined. Their exclusion is structural: the constraint operates to prevent their advocacy from influencing legal interpretation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_civil_society, excluded,
    powerless, biographical, trapped, local).

% Documents the drift from common law norms, files amicus briefs, and publishes analyses of NSL application. Can pressure Hong Kong through statements and sanctions but cannot directly influence the law's operation. Observes the constraint from outside; lacks enforcement authority.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, international_legal_community, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:fixing_cost_class(nsl_legal_text__jurisdictional_capture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified security legal framework addressing spillover effects from 2019 unrest into Hong Kong's institutional space, centralizing threat assessment and response rather than fragmenting enforcement across local courts and common law doctrines.
% TRANSFER_FUNCTION: Moves institutional autonomy and legal interpretive authority from Hong Kong's judiciary and legal profession to mainland security apparatus and central government, through a nominally Hong Kong statute that incorporates mainland legal doctrines and enforcement procedures.
% ABSENT_VOICES: Civil society groups, human rights advocates, and the international legal community are structurally excluded from NSL interpretation and scope-setting. They would argue that common law protections for due process and judicial independence are non-negotiable; their exclusion is the mechanism by which the law persists without addressing those objections.
% DISAPPEARANCE_RATIONALE: If the NSL and its security apparatus enforcement vanished overnight, Hong Kong's judiciary would recover its common law interpretive authority within months, professional constraints on lawyers would lift, and the legal profession would reorganize around adversarial norms. The institutional architecture of mainland security influence would have to find other channels — the NSL is the primary vehicle for jurisdictional capture into legal system operation.
% FOUNDING_PROBLEM: The 2019 pro-democracy unrest was framed as creating security vacuums: protests, civil disobedience, and challenges to police authority that Hong Kong's common law courts were treating as protected speech rather than security threats; the need for unified, rapid threat response that common law's adversarial and due-process-heavy procedures could not provide.
% FOUNDING_PROBLEM_CORROBORATION: Mainland authorities and Hong Kong security officials attest the founding problem is live and serious. Hong Kong legal professionals, international observers, and civil society attest the founding problem was either exaggerated or solved by 2020 police force restoration and argue the NSL's persistence serves institutional capture rather than genuine security need. Legislative testimony and post-2021 academic analysis from outside beneficiary seats support the captured-function reading.
narrative_ontology:disappearance_verdict(nsl_legal_text__jurisdictional_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__jurisdictional_capture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__jurisdictional_capture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nsl_legal_text__jurisdictional_capture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__jurisdictional_capture_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.68 by interval end because the constraint systematically relocates institutional authority from Hong Kong to mainland: judicial decisions on security cases are subject to mainland review or override; statutory interpretation defers to mainland doctrines; the law's scope expands into areas (journalism, academia, protest) traditionally governed by common law. Suppression is higher (0.72) because the constraint's persistence depends on active enforcement by security apparatus and on judicial self-restraint (the mainland-preferred interpretation being the threat implicit in every judgment). Theater is moderate (0.41): the NSL's security-rationale frame is maintained rhetorically, but the enforcement pattern increasingly targets institutional independence rather than defined security threats. The measurement series show extractiveness and suppression rising sharply through year 5, then plateauing as judges and lawyers internalize the constraints and enforcement needs decline (normalization effect). Theater rises throughout, indicating growing performative activity relative to security function.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary and victim seats should compute radically differently under this reading. From the mainland security apparatus and central government positions, the constraint solves a genuine coordination problem (unified threat response, preventing jurisdictional gaps). From the Hong Kong judiciary and legal profession positions, the same structure operates as asymmetric extraction of institutional authority. The engine computes this gap from the structural data: beneficiaries with arbitrage exit and victims with identity_locked exit produce opposing directionality values, and the same constraint classifies as coordination from one seat and extraction from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainland security apparatus: beneficiary, institutional power, arbitrage exit (can withdraw and enforce through other channels) → low d, low χ for this actor. Hong Kong judiciary: victim, institutional power but identity_locked exit (judges are formed in and committed to common law practice; departing means leaving the profession) → high d, high χ. Hong Kong legal profession: victim, powerful but constrained exit (lawyers can leave Hong Kong but not easily leave the profession, and leaving means abandoning the system where their identity is anchored) → d in the 0.65-0.75 range. The asymmetry is structural: the mainland actors can exit while the Hong Kong actors cannot without shedding professional identity.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the false positive of treating the NSL as pure coordination (rope) by naming and classifying the asymmetry: it IS coordination (unified security authority) AND extraction (capture of Hong Kong institutional independence). The tangled-rope classification prevents mandate-drift confusion: the constraint started as security coordination (responding to 2019 unrest), but what it coordinates is increasingly the suppression of alternative institutional voices rather than genuine threat response. By year 10, the security mandate is contested or dead (unrest resolved, but NSL scope remains), yet the extraction and suppression persist. The tangled-rope frame captures this: the coordination story (security needs) is cover for the extraction story (mainland authority over Hong Kong institutions). A piton reading would require theater to be much higher and beneficiary extraction to be near-zero; a snare reading would require suppression near 0.9+ and total victim isolation. This constraint sits between: genuine coordination function but asymmetric benefit distribution and active enforcement to maintain it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mainland_intent_vs_jurisdictional_outcome,
    'Does mainland security apparatus INTEND to capture Hong Kong legal system autonomy, or does jurisdictional capture arise as an unintended side effect of legitimate security coordination?',
    'Internal policy documents, testimony from mainland and Hong Kong officials about scope-setting and case-disposition decisions, pattern analysis of NSL application across sensitive cases over 10+ years.',
    'If intent is documented, the constraint becomes structurally a snare with extracted institutional authority as the primary goal; if it is unintended side effect, tangled rope (coordination + unintended extraction) remains appropriate. Either way, the extraction is real; intent affects framing of legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mainland_intent_vs_jurisdictional_outcome, empirical, 'Whether jurisdictional capture of Hong Kong legal system is planned or emergent from security coordination.').

omega_variable(
    identity_lock_durability,
    'How durable is the identity-lock for Hong Kong judges and lawyers once the NSL enforcement machinery is normalized and overt political pressure declines?',
    'Longitudinal study of Hong Kong legal profession exit rates, career satisfaction, and professional identity markers post-NSL implementation; post-exit trajectory of departed judges (return, re-practice, psychological outcomes).',
    'If identity-lock is durable despite normalization, the suppression metric is stable and the constraint persists as tangled rope. If identity-lock erodes and exit accelerates, the institutional victim set shrinks and the constraint degrades toward piton (performative maintenance by remaining judges, bulk exit by younger cohort).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_durability, empirical, 'Whether professional identity-fusion sustains constraint compliance after overt enforcement pressure eases.').

omega_variable(
    reading_distinctness_from_democratic_enclosure,
    'Is jurisdictional capture structurally distinct from democratic enclosure, or do the two readings describe the same constraint from different observational angles?',
    'Empirical analysis of NSL application patterns: does enforcement concentrate on (A) legal system autonomy questions (jurisdictional capture focus) or (B) political speech and dissent (democratic enclosure focus)? Do the victim sets and extraction targets overlap or diverge?',
    'If victim sets are distinct (one reading targets judges/lawyers, the other targets activists/protesters), the readings are genuinely different constraints and both stories are valid independently. If the same cases and actors are targeted by both framings, the distinction is observational only and one reading might subsume the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_distinctness_from_democratic_enclosure, conceptual, 'Whether jurisdictional-capture reading is a distinct constraint or an observational framing of the democratic-enclosure constraint.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.72) structural — external barriers imposed by security apparatus — or internalized — Hong Kong legal actors self-restraining in anticipation of political pressure?',
    'Post-NSL exit and re-practice trajectories: if judges/lawyers departing Hong Kong rapidly reduce self-restraint and resume full common-law practice, suppression was primarily structural. If they maintain restraint or report continued psychological identity-fusion with Hong Kong constraints, suppression is partially internalized.',
    'If structural, removing the NSL enforcement machinery would restore legal autonomy quickly. If internalized, the constraint''s effective suppression persists even after the formal NSL structure is removed; institutional capture has fused identity-level expectations into legal professionals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is exerted by security apparatus (structural) or absorbed into legal professionals'' self-perception (internalized).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(nsl__tr_t0, observed).
narrative_ontology:measurement(nsl__tr_t5, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(nsl__tr_t5, observed).
narrative_ontology:measurement(nsl__tr_t10, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(nsl__tr_t10, observed).
narrative_ontology:measurement(nsl__tr_t15, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(nsl__tr_t15, observed).
narrative_ontology:measurement(nsl__tr_t20, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(nsl__tr_t20, observed).
narrative_ontology:measurement(nsl__tr_t25, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(nsl__tr_t25, observed).
narrative_ontology:measurement(nsl__tr_t30, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(nsl__tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(nsl__be_t0, observed).
narrative_ontology:measurement(nsl__be_t5, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement_basis(nsl__be_t5, observed).
narrative_ontology:measurement(nsl__be_t10, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement_basis(nsl__be_t10, observed).
narrative_ontology:measurement(nsl__be_t15, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(nsl__be_t15, observed).
narrative_ontology:measurement(nsl__be_t20, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(nsl__be_t20, observed).
narrative_ontology:measurement(nsl__be_t25, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(nsl__be_t25, observed).
narrative_ontology:measurement(nsl__be_t30, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(nsl__be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(nsl__su_t0, observed).
narrative_ontology:measurement(nsl__su_t5, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement_basis(nsl__su_t5, observed).
narrative_ontology:measurement(nsl__su_t10, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(nsl__su_t10, observed).
narrative_ontology:measurement(nsl__su_t15, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(nsl__su_t15, observed).
narrative_ontology:measurement(nsl__su_t20, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(nsl__su_t20, observed).
narrative_ontology:measurement(nsl__su_t25, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(nsl__su_t25, observed).
narrative_ontology:measurement(nsl__su_t30, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(nsl__su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__jurisdictional_capture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nsl_legal_text__jurisdictional_capture_reading, 0.14).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__democratic_enclosure_reading).

% DUAL FORMULATION NOTE:
% NSL kernel family: three structurally distinct readings of the same formalized text. Jurisdictional-capture reading models NSL as a vehicle for legal system transplantation and institutional autonomy extraction; it shares the formal text with sovereignty-restoration reading (which frames NSL as legitimate sovereign security response) and democratic-enclosure reading (which frames NSL as mechanism for permanent political voice closure). The three readings have different ε referents (legal system autonomy vs. state security needs vs. political voice), different victim/beneficiary assignments, and different claim/metric profiles. They compete as readings in Hong Kong policy and judicial interpretation; the Deferential Realism model treats them as three independent constraint stories linked by the shared kernel. This story's classification (tangled rope with moderate-high extractiveness focused on legal system capture) is independent of how the other readings classify; the engine computes per-reading per-seat, not a consensus across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nsl_legal_text__jurisdictional_capture_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
