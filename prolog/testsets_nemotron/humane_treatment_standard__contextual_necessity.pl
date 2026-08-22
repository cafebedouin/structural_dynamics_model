% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__contextual_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__contextual_necessity, []).

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
 *   constraint_id: humane_treatment_standard__contextual_necessity
 *   human_readable: Contextual Necessity Reading of Common Article 3 Humane Treatment
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This constraint story captures the 'contextual necessity' reading of
 *   Common Article 3's humane treatment standard — the interpretation that
 *   baseline protections yield to national security imperatives, permitting
 *   enhanced interrogation and conditional detainee protections. The reading
 *   originated in post-9/11 legal architecture (Bybee/Yoo memoranda, military
 *   commission rules, CIA detention program authorizations) but draws on
 *   pre-existing necessity doctrines in IHL. It is structurally a tangled
 *   rope: it performs a genuine coordination function (providing any shared
 *   baseline in asymmetric conflicts where the enemy rejects all law) while
 *   simultaneously extracting protection from a designated victim class
 *   through active enforcement (classification systems, secrecy regimes,
 *   legal immunities). The claimed_type is tangled_rope; the metrics reflect
 *   high extractiveness, high suppression, and moderate theater — the
 *   coordination function is real but increasingly subordinated to the
 *   extraction function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.82).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.88).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.82).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Contextual Necessity Reading of Common Article 3 Humane Treatment").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, 'bc1ef007-4fac-4bf9-9a7c-4fd78cd26d11').
narrative_ontology:cs_kernel_codification('bc1ef007-4fac-4bf9-9a7c-4fd78cd26d11', fixed_text).
narrative_ontology:cs_authority_grounding('bc1ef007-4fac-4bf9-9a7c-4fd78cd26d11', extraction).
narrative_ontology:cs_interpretation_layer_present('bc1ef007-4fac-4bf9-9a7c-4fd78cd26d11').
narrative_ontology:cs_reading_relation('bc1ef007-4fac-4bf9-9a7c-4fd78cd26d11', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('bc1ef007-4fac-4bf9-9a7c-4fd78cd26d11', humane_treatment_standard__proportionality_balancing, influences).
narrative_ontology:cs_axiom('bc1ef007-4fac-4bf9-9a7c-4fd78cd26d11', foundational, national_security_imperatives_override_humane_treatment_baseline).
narrative_ontology:cs_axiom_status(national_security_imperatives_override_humane_treatment_baseline, holdable).
narrative_ontology:cs_axiom_grounding('bc1ef007-4fac-4bf9-9a7c-4fd78cd26d11', national_security_imperatives_override_humane_treatment_baseline, instrumental).
narrative_ontology:cs_axiom('bc1ef007-4fac-4bf9-9a7c-4fd78cd26d11', foundational, executive_discretion_defines_humane_treatment_boundary).
narrative_ontology:cs_axiom_status(executive_discretion_defines_humane_treatment_boundary, holdable).
narrative_ontology:cs_axiom_grounding('bc1ef007-4fac-4bf9-9a7c-4fd78cd26d11', executive_discretion_defines_humane_treatment_boundary, conventional).
narrative_ontology:cs_reference_frame('bc1ef007-4fac-4bf9-9a7c-4fd78cd26d11', common_article_3_baseline_protections).
narrative_ontology:cs_drift_state('bc1ef007-4fac-4bf9-9a7c-4fd78cd26d11', post_911_legal_architecture, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bc1ef007-4fac-4bf9-9a7c-4fd78cd26d11', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, national_security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, executive_branch_legal_advisors).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, high_value_detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, conflict_detainees_in_necessity_scenarios).
narrative_ontology:constraint_vindicates(humane_treatment_standard__contextual_necessity, state_sovereignty_in_security_interpretation).
narrative_ontology:constraint_vindicates(humane_treatment_standard__contextual_necessity, executive_discretion_in_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and apply 'humane treatment' standards through the lens of operational necessity. Define interrogation techniques, detention conditions, and the threshold where security imperatives override baseline protections. Control the classification apparatus that determines which detainees fall into necessity categories. Benefit from expanded discretion and institutional authority to act without external review.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, national_security_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Provide the legal architecture that translates 'context-dependent' into actionable authorization. Draft memoranda, legal opinions, and interpretive guidance that expand the perimeter of permissible treatment. Their professional standing and institutional access depend on producing the frameworks that security agencies require. Career advancement tracks with institutional alignment.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, executive_branch_legal_advisors, beneficiary,
    institutional, biographical, mobile, national).

% Designated as falling outside full Common Article 3 protections due to intelligence value or threat designation. Subject to 'enhanced interrogation,' prolonged isolation, sensory manipulation, and other techniques classified as non-torture under the contextual reading. No meaningful exit, no access to counsel, no judicial review of their designation. Bear the physical and psychological costs of the discretion granted to security agencies.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, high_value_detainees, payer,
    powerless, immediate, trapped, global).

% Detainees held in contexts where authorities invoke necessity — active hostilities, ticking-bomb scenarios, ongoing threat environments. Their treatment standards are ratcheted down by the same interpretive machinery. Unlike high-value detainees, they may not be individually targeted but are swept into the lowered baseline by categorical necessity claims. Exit depends on conflict termination, which the necessity framework treats as indefinitely deferred.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, conflict_detainees_in_necessity_scenarios, payer,
    powerless, biographical, trapped, global).

% Monitor, document, and challenge the erosion of Common Article 3 through contextual necessity readings. Operate through treaty bodies, NGO reporting, and domestic litigation. Their leverage is reputational and normative — they cannot compel compliance but shape the interpretive environment in which security agencies operate. Split between those who engage the necessity framework to constrain it and those who reject engagement as legitimating.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, international_humanitarian_law_practitioners, observer,
    moderate, generational, analytical, global).

% Adjudicate detention and treatment claims when detainees or their representatives access domestic courts. Their authority to review is itself contested — security agencies invoke state secrets, political question doctrines, and deference frameworks to limit judicial reach. When they do reach the merits, they apply the contextual necessity standard or its siblings. Their decisions feed back into the interpretive ecology.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, domestic_courts_involved_in_habeas_review, observer,
    organized, biographical, constrained, national).

% Not directly detained but subject to the same security architecture that produces necessity designations. Checkpoints, screenings, administrative detention, and collective measures justified by the same imperatives. Their voices are absent from the legal-authorization process; they experience the downstream effects of a framework that treats protection as conditional. Would object to the normalization of conditional humanity but have no seat at the table where necessity is defined.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, civilian_populations_in_conflict_zones, excluded,
    powerless, generational, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared interpretive baseline for 'humane treatment' in non-international armed conflict — a common reference point that all parties to a conflict can invoke, even if they interpret it differently. Solves the coordination problem of having any minimum standard at all in conflicts where no state recognizes the other's legitimacy.
% TRANSFER_FUNCTION: Transfers protection from detainees to security agencies: the discretion to define the boundary of 'humane' moves from a fixed legal standard to an executive judgment call. The cost (risk of abusive treatment, loss of dignity, physical/psychological harm) is borne by designated detainees; the benefit (operational flexibility, intelligence yield, institutional autonomy) accrues to security agencies and their legal enablers.
% ABSENT_VOICES: The detainees themselves — especially high-value detainees held incommunicado — are structurally excluded from the interpretive process that defines their protections. Civilian populations in conflict zones experience the downstream normalization of conditional humanity but have no role in drafting the legal opinions. Rival readings (absolute prohibition advocates, proportionality balancers) are present in the discourse but the contextual necessity reading controls the authorization machinery in key jurisdictions.
% DISAPPEARANCE_RATIONALE: If the contextual necessity reading vanished overnight, security agencies would lose the primary legal architecture authorizing enhanced interrogation and conditional protections. Detainee treatment would revert to the absolute prohibition baseline (or the proportionality balancing reading), requiring new legal authorities for any departure. The institutional machinery of designation, memoranda, and classification would become legally inoperable without replacement. The world of detention operations would reorganize around a stricter standard.
% FOUNDING_PROBLEM: Common Article 3 was drafted for conventional non-international conflicts with identifiable parties. The founding problem was ensuring minimum protections where no formal recognition existed between belligerents. The contextual necessity reading emerged later to address a different problem: how to maintain legal authorization for detention and interrogation operations against non-state actors in asymmetric, transnational conflicts where the enemy does not wear uniforms, does not follow laws of war, and poses catastrophic threats.
% FOUNDING_PROBLEM_CORROBORATION: Security agencies and their legal advisors attest the asymmetric threat problem is live and growing — citing transnational terrorism, decentralized insurgencies, and WMD proliferation risks. Human rights treaty bodies (UN CAT, ICRC), international courts (ECtHR, IACtHR), and domestic courts in multiple jurisdictions (UK, Israel, Canada, South Africa) have rejected the necessity override as legally impermissible, attesting that the founding problem of Common Article 3 — minimum protections in all non-international conflicts — remains live and that the contextual reading is a distortion, not an adaptation. No neutral third-party corroboration exists; the dispute is structural.
narrative_ontology:disappearance_verdict(humane_treatment_standard__contextual_necessity, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__contextual_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(humane_treatment_standard__contextual_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__contextual_necessity, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__contextual_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__contextual_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the reading transfers the power to define 'humane' from a fixed treaty standard to executive discretion — the protection detainees lose is the gain security agencies collect. Suppression (0.88) is very high because the reading's persistence depends on active exclusion: classification regimes, state secrets privileges, denial of habeas access, and legal immunities for interrogators. Theater (0.42) reflects that the coordination function (Common Article 3 as a shared reference point) remains partially operational — the language of 'humane treatment' is still invoked, monitoring still occurs, some constraints still bind — but a growing share of the interpretive apparatus serves to legitimize the extraction. Accessibility collapse (0.65) is moderate-high: alternatives (absolute prohibition, proportionality balancing) exist and are advocated, but the necessity framework has colonized the authorization machinery in key jurisdictions. Resistance (0.72) is high: treaty bodies, courts, NGOs, and some domestic legal systems actively contest the reading, but their resistance has not displaced it from the seats of operational power.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute different per-seat classifications from this structural data. From the national_security_agencies seat (d ~0.1, beneficiary), the constraint appears as coordination with manageable overhead — a rope-like structure they built and control. From the high_value_detainees seat (d ~0.95, trapped payer), the same constraint appears as a snare — pure extraction enforced by overwhelming power with no exit. From the executive_branch_legal_advisors seat (d ~0.2, beneficiary), it appears as a scaffold — a transitional framework for a new conflict paradigm that has become permanent. From the IHL_practitioners seat (d ~0.5, analytical), it appears as a tangled rope — genuine coordination function corrupted by asymmetric extraction. This divergence IS the measurement; the authored claim (tangled_rope) is the generating model's structural judgment, not an adjudication of the seat views.
 *
 * DIRECTIONALITY LOGIC:
 *   National security agencies are the primary agenda_setters with institutional power, generational horizon, and arbitrage-grade exit (they can shift legal frameworks, jurisdictions, and operational paradigms). Executive branch legal advisors are beneficiaries — they collect professional and institutional rewards for producing the necessity architecture, with mobile exit (they can rotate to academia, private practice, or other agencies). High-value detainees and necessity-scenario detainees are payers — powerless, trapped, immediate/biographical horizons, bearing the full cost of the discretion. IHL practitioners and domestic courts are observers — moderate/organized power, analytical/constrained exit, generational/biographical horizons, able to contest but not control. Civilian populations in conflict zones are excluded — powerless, trapped, experiencing downstream effects with zero voice in the authorization process.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy: the founding problem (asymmetric transnational threats requiring legal adaptation) may be live, but the reading's solution (executive discretion to define humane treatment downward) has outlived any plausible necessity justification in many operational contexts. The coordination function (shared baseline) has been captured by the extraction function (discretionary power). The theater ratio tracks this capture — the performative invocation of 'humane treatment' and 'necessity' masks a standing arrangement that no longer requires case-by-case justification. The constraint persists not because it solves the founding problem better than alternatives, but because the institutional machinery that produces it (legal advisors, classification authorities, immunity frameworks) has become self-sustaining. The mandatrophy is not resolved — the reading's proponents still argue necessity, but the structural evidence points to a constraint maintained by inertia and capture, not by continuing existential need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_threshold_ambiguity,
    'Does the contextual necessity reading contain an internal threshold that limits its own expansion, or does the logic of ''national security imperatives override'' structurally tend toward unlimited discretion?',
    'Trace the evolution of authorized techniques and designation criteria from 2001-2024: if the set of permissible treatments and necessity-designated categories has monotonically expanded without internal correction, the reading lacks a structural brake. Compare with jurisdictions that adopted necessity frameworks but later contracted them (e.g., UK post-1970s, Israel post-1999 HCJ ruling).',
    'If the reading lacks an internal threshold, it is structurally a snare masquerading as a tangled rope — the coordination function is a vestigial cover for unbounded extraction. If a threshold exists and operates, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_threshold_ambiguity, conceptual, 'Whether the necessity override contains its own limiting principle').

omega_variable(
    coordination_function_genuineness,
    'Is the coordination function (providing a shared baseline in asymmetric conflicts) genuinely served by this reading, or is it a cover story for the extraction function?',
    'Compare treatment standards and detainee outcomes in conflicts where the contextual necessity reading operates vs. conflicts where absolute prohibition or proportionality balancing operates. If the necessity reading produces materially better coordination (reciprocity, reduced escalation, functional prisoner exchanges) without higher abuse, the coordination function is genuine. If outcomes are worse on both dimensions, the coordination claim is cover.',
    'If cover, the claimed_type should be snare, not tangled_rope. The engine''s classification will reflect the metric profile regardless, but this omega documents the structural ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_genuineness, empirical, 'Whether the coordination function is real or pretextual').

omega_variable(
    reading_relations_foreclosure,
    'Does the contextual necessity reading logically foreclose the absolute prohibition reading within a single legal framework, or do they coexist as competing interpretations?',
    'Analyze whether a legal system can simultaneously hold that (a) Common Article 3 permits enhanced interrogation when necessity overrides, and (b) Common Article 3 establishes non-derogable minimums. If the two premises are logically contradictory such that no coherent framework can contain both, the relation is ''forecloses.'' If different institutions within one system can hold different readings (e.g., executive vs. judiciary), the relation is ''coexists_with.''',
    'Determines the cs_structure.reading_relations entry for absolute_prohibition. Affects whether the kernel is modeled as a genuine dispute or a structural fracture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relations_foreclosure, conceptual, 'Logical relationship between contextual necessity and absolute prohibition readings').

omega_variable(
    suppression_mechanism_composition,
    'Is the measured suppression (0.88) primarily structural (classification regimes, legal immunities, jurisdictional bars) or does it include a significant internalized component (detainees'' self-censorship, normalization of conditional humanity among affected populations)?',
    'Post-release trajectory studies: if former detainees and affected populations continue to self-limit claims and behaviors after the structural suppression mechanisms are removed (e.g., after transfer, release, or regime change), the suppression has an internalized component. Compare with contexts where structural suppression was lifted but behavioral suppression persisted.',
    'If internalized suppression is significant, the constraint''s effective suppression exceeds the structural measure — the target carries the suppression with them. This would increase the effective extraction for trapped/identity_locked agents beyond the engine''s structural computation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Structural vs. internalized suppression mechanisms in the necessity reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t1949, humane_treatment_standard__contextual_necessity, theater_ratio, 1949, 0.05).
narrative_ontology:measurement(huma_tr_t1977, humane_treatment_standard__contextual_necessity, theater_ratio, 1977, 0.08).
narrative_ontology:measurement(huma_tr_t2001, humane_treatment_standard__contextual_necessity, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(huma_tr_t2003, humane_treatment_standard__contextual_necessity, theater_ratio, 2003, 0.35).
narrative_ontology:measurement(huma_tr_t2006, humane_treatment_standard__contextual_necessity, theater_ratio, 2006, 0.41).
narrative_ontology:measurement(huma_tr_t2009, humane_treatment_standard__contextual_necessity, theater_ratio, 2009, 0.38).
narrative_ontology:measurement(huma_tr_t2014, humane_treatment_standard__contextual_necessity, theater_ratio, 2014, 0.4).
narrative_ontology:measurement(huma_tr_t2024, humane_treatment_standard__contextual_necessity, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(huma_be_t1949, humane_treatment_standard__contextual_necessity, base_extractiveness, 1949, 0.15).
narrative_ontology:measurement(huma_be_t1977, humane_treatment_standard__contextual_necessity, base_extractiveness, 1977, 0.18).
narrative_ontology:measurement(huma_be_t2001, humane_treatment_standard__contextual_necessity, base_extractiveness, 2001, 0.45).
narrative_ontology:measurement(huma_be_t2003, humane_treatment_standard__contextual_necessity, base_extractiveness, 2003, 0.62).
narrative_ontology:measurement(huma_be_t2006, humane_treatment_standard__contextual_necessity, base_extractiveness, 2006, 0.71).
narrative_ontology:measurement(huma_be_t2009, humane_treatment_standard__contextual_necessity, base_extractiveness, 2009, 0.68).
narrative_ontology:measurement(huma_be_t2014, humane_treatment_standard__contextual_necessity, base_extractiveness, 2014, 0.75).
narrative_ontology:measurement(huma_be_t2024, humane_treatment_standard__contextual_necessity, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t1949, humane_treatment_standard__contextual_necessity, suppression_requirement, 1949, 0.3).
narrative_ontology:measurement(huma_su_t1977, humane_treatment_standard__contextual_necessity, suppression_requirement, 1977, 0.35).
narrative_ontology:measurement(huma_su_t2001, humane_treatment_standard__contextual_necessity, suppression_requirement, 2001, 0.65).
narrative_ontology:measurement(huma_su_t2003, humane_treatment_standard__contextual_necessity, suppression_requirement, 2003, 0.82).
narrative_ontology:measurement(huma_su_t2006, humane_treatment_standard__contextual_necessity, suppression_requirement, 2006, 0.87).
narrative_ontology:measurement(huma_su_t2009, humane_treatment_standard__contextual_necessity, suppression_requirement, 2009, 0.85).
narrative_ontology:measurement(huma_su_t2014, humane_treatment_standard__contextual_necessity, suppression_requirement, 2014, 0.86).
narrative_ontology:measurement(huma_su_t2024, humane_treatment_standard__contextual_necessity, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(humane_treatment_standard__contextual_necessity, 0.12).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__proportionality_balancing).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, command_responsibility_doctrine).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, state_secrets_privilege_in_national_security).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, habeas_corpus_scope_in_non_international_conflict).

% DUAL FORMULATION NOTE:
% Part of the humane_treatment_standard constraint family (kernel_id: humane_treatment_standard). This reading (contextual_necessity) claims the kernel's baseline yields to security imperatives. The absolute_prohibition reading claims non-derogable minimums. The proportionality_balancing reading claims structured weighing. All three share the kernel but instantiate different constraints with different ε, different victim sets, different enforcement logics. The ε-invariance principle requires separate stories: this reading's ε (0.82) differs radically from absolute_prohibition's (near 0) and proportionality_balancing's (estimated 0.4-0.5).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(humane_treatment_standard__contextual_necessity, institutional, 0.12).
constraint_indexing:directionality_override(humane_treatment_standard__contextual_necessity, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
