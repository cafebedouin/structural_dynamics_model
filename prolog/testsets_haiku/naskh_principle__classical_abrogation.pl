% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__classical_abrogation, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: naskh_principle__classical_abrogation
 *   human_readable: Classical Naskh (Quranic Abrogation) Principle
 *   domain: religious/jurisprudential/legal_theory
 *
 * SUMMARY:
 *   The naskh principle—the doctrine that later Quranic verses abrogate
 *   earlier verses on the same legal or theological topic—is a cornerstone of
 *   classical Islamic jurisprudence. Orthodox schools (Hanafi, Maliki,
 *   Shafi'i, Hanbali, and Shi'i variants) use chronological revelation order
 *   to resolve apparent contradictions in Islamic law, granting them decisive
 *   authority in fatwa and legal determination. The principle is CLAIMED as
 *   tangled_rope (genuine coordination function—legal certainty—combined with
 *   asymmetric extraction—suppression of contextual readings and
 *   theological-coherence approaches). The authored metrics (extractiveness
 *   0.68, suppression 0.42, modest theater_ratio 0.19) describe a constraint
 *   that solves a real problem but through mechanisms that benefit
 *   institutional juridical schools and marginalize alternative readings. The
 *   claim/metric gap is intentional and reflects the kernel contest:
 *   classical-abrogation is ONE reading of a contested naskh principle.
 *   Sibling readings (contextual-harmonization, progressive-restriction)
 *   would produce different ε values and victim/beneficiary sets. This story
 *   instantiates only the classical-abrogation reading.
 *
 * KEY AGENTS:
 *   - Orthodox juridical schools: institutional agenda-setters and primary beneficiaries; administer the principle, train successors, issue fatwas using naskh hierarchy; benefit from decision-reduction and institutional authority.
 *   - Certainty-privileging jurists: organized beneficiaries; collect from naskh's provision of clear, binary rulings; their epistemological preference for rule over relationality aligns with the principle's mechanism.
 *   - Contextual hermeneutics proponents: moderate-powered victims; suppressed from orthodox discourse; their readings are marginalized as non-rigorous or theologically confused; pay the cost of professional isolation.
 *   - Theological-coherence seekers: powerless victims; identity-locked (cannot exit Islamic tradition); experience naskh as forcing a choice between textual completeness and logical consistency; suppressed: coherence objections are treated as naive.
 *   - Chronological-revelation historians: institutional observers; their reconstruction of revelation order enables naskh application but is not controlled by jurists.
 *   - Reformation-leaning movements: moderate-powered excluded parties; would argue for progressive-restriction or contextual-harmonization if admitted to jurisprudential authority-setting.
 *   - Quran students and believers: powerless, identity-locked, dual-positioned (beneficiary of clarity + payer of lost-textual-completeness); receive stable rulings and lose the right to hold all verses simultaneously valid.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.68).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.42).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.19).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.68).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.19).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Naskh (Quranic Abrogation) Principle").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "religious/jurisprudential/legal_theory").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, '0bc28638-f26f-455a-ab0e-1ce0f85def5f').
narrative_ontology:cs_kernel_codification('0bc28638-f26f-455a-ab0e-1ce0f85def5f', fixed_text).
narrative_ontology:cs_authority_grounding('0bc28638-f26f-455a-ab0e-1ce0f85def5f', extraction).
narrative_ontology:cs_interpretation_layer_present('0bc28638-f26f-455a-ab0e-1ce0f85def5f').
narrative_ontology:cs_reading_relation('0bc28638-f26f-455a-ab0e-1ce0f85def5f', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_reading_relation('0bc28638-f26f-455a-ab0e-1ce0f85def5f', naskh_principle__progressive_restriction, coexists_with).
narrative_ontology:cs_axiom('0bc28638-f26f-455a-ab0e-1ce0f85def5f', foundational, chronological_revelation_supersession).
narrative_ontology:cs_axiom_status(chronological_revelation_supersession, holdable).
narrative_ontology:cs_axiom_grounding('0bc28638-f26f-455a-ab0e-1ce0f85def5f', chronological_revelation_supersession, empirically_contingent).
narrative_ontology:cs_axiom('0bc28638-f26f-455a-ab0e-1ce0f85def5f', foundational, abrogated_verse_legal_invalidity).
narrative_ontology:cs_axiom_status(abrogated_verse_legal_invalidity, holdable).
narrative_ontology:cs_axiom_grounding('0bc28638-f26f-455a-ab0e-1ce0f85def5f', abrogated_verse_legal_invalidity, deontological).
narrative_ontology:cs_created_at('0bc28638-f26f-455a-ab0e-1ce0f85def5f', '').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, orthodox_juridical_schools).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, certainty_privileging_jurists).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, contextual_hermeneutics_proponents).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, theological_coherence_seekers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__classical_abrogation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(naskh_principle__classical_abrogation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__classical_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__classical_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__classical_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 (early Islamic period, when abrogation was emerging) to 0.68 (contemporary orthodoxy, when the principle is fully institutionalized). This trajectory reflects progressive consolidation of naskh doctrine as institutional orthodoxy and progressive marginalization of contextual approaches. Theater_ratio remains low (0.19 at end) because the performative component is minimal—the suppression is mostly structural (institutional gatekeeping) rather than theatrical; the principle is genuinely used for fatwa issuance, not merely performed. Suppression_requirement rises from 0.25 to 0.42, reflecting increasing institutional effort to maintain naskh orthodoxy as contemporary challenges from reformists, modernists, and Quranic scholars mount. The one-shared-grid measurement strategy ensures every metric is authored at every time point, preventing OQ-105-style misalignment. Observations (basis=observed) cover periods when the doctrine was established; projections (basis=projected) extend into the contemporary competitive environment where reformist and contextual readings gain institutional foothold in some Islamic communities.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting institutional schools and certainty-privileging jurists should compute as beneficiaries experiencing a rope or coordination function. From their structural position, naskh solves a genuine problem: how to operate unified jurisprudence when the text appears contradictory. Contextual hermeneutics proponents and theological-coherence seekers should compute as targets experiencing a snare: the principle suppresses their readings, forecloses their interpretive labor, and imposes a hierarchy they did not choose. The powerless believers experience a tangled constraint: genuine benefit (clear rulings) coupled with imposed cost (lost textual coherence). The engine computes these divergences from power, exit_options, and beneficiary/victim status. The classical-abrogation reading CLAIMS tangled_rope (both coordination and extraction present) but the victim's perspective would strengthen the snare classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Orthodox schools (institutional power, identity_locked exit, collective beneficiary role) have directionality near the full-beneficiary end (~0.15–0.25): they set the rules, collect interpretive authority, and cannot exit the principle without losing their jurisprudential monopoly (identity-locked). Contextual hermeneutics proponents (moderate power, constrained exit, victim role) sit nearer the target end (~0.65–0.75): they pay through professional marginalization and cannot easily exit Islamic discourse. Theological-coherence seekers (powerless, identity_locked, victim role) sit at the target extreme (~0.80–0.90): they bear suppression and cannot exit Islamic belief without abandoning their identity. Quran students (powerless, identity_locked, dual role) sit symmetric (~0.45–0.55): they benefit from clarity but pay through lost coherence. No directionality overrides are needed; structural derivation from the declared beneficiary/victim and exit axes captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The naskh principle was founded to solve the real problem of legal determination when texts appeared contradictory. That founding problem remains formally live (orthodox scholars attest it), but is substantially contested (contemporary Islamic scholars argue most apparent contradictions are contextually resolvable, not abrogations). The constraint persists despite this contest because institutional orthodoxy has become self-perpetuating: the schools maintain naskh because it protects their jurisprudential role, not because the founding problem uniquely requires it. This is a classic mandatrophy situation: the mandate (legal certainty) has not disappeared, but the method (naskh) has become questionable while the institutional incentive to defend it has grown. The tangled_rope classification captures this: genuine coordination (legal certainty) fused with institutional extraction (monopoly authority). The measurement trajectory showing rising extractiveness (0.38→0.68) and rising suppression_requirement (0.25→0.42) indicates that as alternatives (contextual, progressive-restriction readings) gain scholarly traction, orthodoxy must exert more institutional effort to suppress them—the extraction is increasingly active enforcement rather than natural outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_kernel_contest,
    'Is the classical-abrogation reading of the naskh kernel the correct interpretation of Quranic practice, or is contextual-harmonization or progressive-restriction a better fit to the text and early jurisprudential practice?',
    'Detailed historical-critical analysis of (1) how the earliest Muslims (Companions and Successors) actually handled apparent contradictions; (2) whether explicit abrogation language appears in the Quran itself or only in later hadith; (3) whether alternative frameworks (harmonization, progressive moral development) explain the text as coherently. Comparative study of how each reading performs on a corpus of challenging verse-pairs.',
    'If contextual-harmonization or progressive-restriction better capture Quranic intent and early practice, the classical-abrogation reading is a later institutional imposition, reclassifying it from mountain-like ''natural jurisprudential necessity'' to snare-like ''imposed constraint benefiting orthodoxy.'' Conversely, if classical-abrogation best explains the text, it approaches mountain status and the contextual readings are the victims of a real coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contest, empirical, 'Which of the three naskh readings best fits Quranic text and early Islamic jurisprudential practice.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.42) of alternative readings structural (institutional control of jurisprudential credentials and publication) or internalized (believers and scholars self-censor because they have come to regard naskh as obviously correct)?',
    'Post-exit trajectory analysis: if scholars who leave orthodox institutions continue to avoid contextual readings even after exiting, suppression is partly internalized. Comparative study of heterodox Muslim communities that operate outside institutional jurisprudence to see whether contextual reading emerges naturally or requires sustained effort. Survey of believer and scholar attitudes to understand whether contextual readings feel intellectually impermissible or institutionally risky.',
    'If suppression is mostly structural, removing institutional orthodoxy would enable contextual reading to flourish; if internalized, the constraint travels with believers into new institutional contexts. Higher internalization means effective suppression exceeds the measured 0.42 by some unknown factor, strengthening the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression of contextual readings is structural or internalized.').

omega_variable(
    coordination_vs_extraction_decoupling,
    'How much of the measured extractiveness (0.68) is the irreducible cost of providing legal certainty (coordination), and how much is monopolistic rent extraction by orthodox schools?',
    'Test whether the coordination benefit (legal clarity) requires the suppression of alternatives. Could a system provide legal clarity while holding multiple readings simultaneously valid (e.g., marking them as orthodox-approved variants rather than abrogated)? Examine whether orthodox schools defend naskh for its decision-reduction value or for its protection of institutional authority.',
    'If clarity requires suppression, extraction is coordination cost; if clarity is achievable without suppression, the extraction is rent seeking. The tangled-rope classification assumes both are true; if separation is possible, the reading shifts toward pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_decoupling, conceptual, 'Whether legal certainty requires the suppression of contextual readings or whether suppression is an independent extraction mechanism.').

omega_variable(
    authority_grounding_oscillation,
    'Does the naskh principle''s authority rest on fixed textual grounds (the Quran and hadith genuinely support classical-abrogation reading) or on institutional self-perpetuation (schools maintain naskh because it protects their jurisprudential monopoly)?',
    'Genealogical analysis of naskh''s emergence in 2nd-3rd century Islamic jurisprudence: did it arise to solve a real textual problem or to consolidate school authority? Study whether the principle is applied consistently (symmetrically across all schools and all verse-pairs) or selectively (schools modify its application when it threatens their doctrinal positions). Examine whether schools would defend naskh in a counterfactual world where alternative readings had equal institutional power.',
    'If textually grounded, naskh approaches mountain status and its beneficiaries are legitimate coordinators; if institutionally grounded, it is a snare-like capture mechanism. The classical-abrogation reading claims textual grounding; this omega probes whether that claim withstands scrutiny.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_grounding_oscillation, empirical, 'Whether naskh''s authority is textual or institutional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__classical_abrogation, theater_ratio, 0, 0.08).
narrative_ontology:measurement(nask_tr_t3, naskh_principle__classical_abrogation, theater_ratio, 3, 0.1).
narrative_ontology:measurement(nask_tr_t6, naskh_principle__classical_abrogation, theater_ratio, 6, 0.12).
narrative_ontology:measurement(nask_tr_t9, naskh_principle__classical_abrogation, theater_ratio, 9, 0.14).
narrative_ontology:measurement(nask_tr_t12, naskh_principle__classical_abrogation, theater_ratio, 12, 0.16).
narrative_ontology:measurement(nask_tr_t15, naskh_principle__classical_abrogation, theater_ratio, 15, 0.18).
narrative_ontology:measurement(nask_tr_t18, naskh_principle__classical_abrogation, theater_ratio, 18, 0.19).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__classical_abrogation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(nask_be_t3, naskh_principle__classical_abrogation, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(nask_be_t6, naskh_principle__classical_abrogation, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(nask_be_t9, naskh_principle__classical_abrogation, base_extractiveness, 9, 0.58).
narrative_ontology:measurement(nask_be_t12, naskh_principle__classical_abrogation, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(nask_be_t15, naskh_principle__classical_abrogation, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(nask_be_t18, naskh_principle__classical_abrogation, base_extractiveness, 18, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__classical_abrogation, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(nask_su_t3, naskh_principle__classical_abrogation, suppression_requirement, 3, 0.29).
narrative_ontology:measurement(nask_su_t6, naskh_principle__classical_abrogation, suppression_requirement, 6, 0.33).
narrative_ontology:measurement(nask_su_t9, naskh_principle__classical_abrogation, suppression_requirement, 9, 0.36).
narrative_ontology:measurement(nask_su_t12, naskh_principle__classical_abrogation, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(nask_su_t15, naskh_principle__classical_abrogation, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(nask_su_t18, naskh_principle__classical_abrogation, suppression_requirement, 18, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__classical_abrogation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(naskh_principle__classical_abrogation, 0.18).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, quranic_interpretation_authority).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, islamic_legal_school_gatekeeping).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, theological_textual_coherence_expectation).

% DUAL FORMULATION NOTE:
% The naskh principle is a contested kernel with three structurally distinct readings. This file instantiates classical-abrogation (fixed legal rulings, chronological supersession, benefits legal certainty, suppresses contextual approaches). Sibling readings contextual-harmonization and progressive-restriction are separate constraint stories with different ε values and victim/beneficiary structures. All three readings coexist as live positions in contemporary Islamic scholarship; no single reading currently dominates all institutional contexts, though classical-abrogation remains orthodoxy-privileged. The three stories are linked via network.affects_constraints to form a constraint family documenting how one kernel (the need to resolve apparent contradictions) generates three competing institutional arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naskh_principle__classical_abrogation, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
