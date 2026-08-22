% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__jurisdictional_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: NSL as Jurisdictional Capture of Hong Kong Common Law
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint story reads the Hong Kong National Security Law (NSL) as
 *   a mechanism for transplanting mainland Chinese legal concepts,
 *   procedures, and jurisdictional authority into Hong Kong's common law
 *   system, thereby eroding the institutional autonomy previously guaranteed
 *   under 'one country, two systems.' The NSL was promulgated by the NPCSC in
 *   June 2020 and establishes a parallel security enforcement architecture
 *   with extraterritorial reach, mainland interpretation authority, and
 *   removal of certain cases from Hong Kong's independent judiciary. The
 *   reading treats the constraint as a Tangled Rope: it coordinates a genuine
 *   security governance function while asymmetrically extracting
 *   jurisdictional autonomy from Hong Kong's legal institutions and
 *   transferring it to mainland security apparatus.
 *
 * KEY AGENTS:
 *   - mainland_security_apparatus: Primary agenda-setter and beneficiary (institutional/arbitrage) â designs, interprets, and benefits from expanded jurisdictional reach into Hong Kong.
 *   - hk_judiciary: Primary target (institutional/constrained) â loses jurisdictional finality and case control over security-related matters.
 *   - hk_legal_profession: Primary target (organized/identity_locked) â bears procedural and ethical costs as common law norms are subordinated to mainland security logic.
 *   - hk_executive: Secondary beneficiary (institutional/constrained) â gains delegated security powers but operates under central oversight.
 *   - international_common_law_observers: Analytical observer (analytical/analytical) â monitors erosion of judicial independence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.72).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.78).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "NSL as Jurisdictional Capture of Hong Kong Common Law").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, 'f248ea96-34c4-43c8-8bf3-4ab7b58c78f9').
narrative_ontology:cs_kernel_codification('f248ea96-34c4-43c8-8bf3-4ab7b58c78f9', formalized).
narrative_ontology:cs_authority_grounding('f248ea96-34c4-43c8-8bf3-4ab7b58c78f9', extraction).
narrative_ontology:cs_interpretation_layer_present('f248ea96-34c4-43c8-8bf3-4ab7b58c78f9').
narrative_ontology:cs_reading_relation('f248ea96-34c4-43c8-8bf3-4ab7b58c78f9', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('f248ea96-34c4-43c8-8bf3-4ab7b58c78f9', nsl_legal_text__democratic_enclosure_reading, influences).
narrative_ontology:cs_axiom('f248ea96-34c4-43c8-8bf3-4ab7b58c78f9', foundational, hk_judicial_autonomy_constitutionally_mandated).
narrative_ontology:cs_axiom_status(hk_judicial_autonomy_constitutionally_mandated, holdable).
narrative_ontology:cs_axiom_grounding('f248ea96-34c4-43c8-8bf3-4ab7b58c78f9', hk_judicial_autonomy_constitutionally_mandated, conventional).
narrative_ontology:cs_axiom('f248ea96-34c4-43c8-8bf3-4ab7b58c78f9', foundational, local_finality_required_for_common_law_integrity).
narrative_ontology:cs_axiom_status(local_finality_required_for_common_law_integrity, holdable).
narrative_ontology:cs_axiom_grounding('f248ea96-34c4-43c8-8bf3-4ab7b58c78f9', local_finality_required_for_common_law_integrity, conventional).
narrative_ontology:cs_reference_frame('f248ea96-34c4-43c8-8bf3-4ab7b58c78f9', hk_common_law_jurisdictional_autonomy).
narrative_ontology:cs_drift_state('f248ea96-34c4-43c8-8bf3-4ab7b58c78f9', post_nsl_imposition_2020_2025, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('f248ea96-34c4-43c8-8bf3-4ab7b58c78f9', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, hk_executive).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hk_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hk_legal_profession).
narrative_ontology:constraint_vindicates(nsl_legal_text__jurisdictional_capture_reading, national_security_supremacy_over_local_procedure).
narrative_ontology:constraint_vindicates(nsl_legal_text__jurisdictional_capture_reading, npcsc_interpretive_authority_in_hk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Office for Safeguarding National Security in Hong Kong and exercises direct jurisdiction over selected national security cases. Interprets and enforces the NSL according to mainland legal principles, overriding local procedural norms where conflicts arise. Benefits from expanded territorial reach and the subordination of Hong Kong common law institutions to mainland security priorities.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, beneficiary).

% Previously possessed final appellate jurisdiction and independent constitutional review authority over Hong Kong affairs. Under the NSL, certain categories of national security cases are removed from its docket, NPCSC interpretations bind the courts, and common law procedural safeguards are subordinated. Individual judges have been disqualified or have resigned; the institution faces structural curtailment of its autonomy.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hk_judiciary, payer,
    institutional, generational, constrained, regional).

% Practices within a common law tradition whose procedural norms â attorney-client privilege, adversarial disclosure, presumption of bail â are progressively displaced by mainland security logic in NSL cases. Individual practitioners face revised professional conduct rules and state-secret classifications that limit their ability to mount standard defenses. Many experience professional identity conflict between common law training and the new security framework; emigration is increasingly common but dissolves professional standing.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hk_legal_profession, payer,
    organized, biographical, identity_locked, regional).

% Receives expanded executive powers under the NSL to formulate subsidiary legislation, designate judges, and oversee security investigations within Hong Kong. However, operates under the oversight of the mainland security apparatus and the NPCSC, with limited autonomy to resist central directives. Benefits from enhanced authority relative to the legislature and judiciary but remains subordinate to Beijing.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hk_executive, beneficiary,
    institutional, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national security governance across a sovereign territory by establishing unified legal standards for secession, subversion, terrorism, and collusion with foreign forces, eliminating jurisdictional fragmentation between a common law sub-system and the mainland legal system.
% TRANSFER_FUNCTION: Transfers interpretive authority, case jurisdiction, and procedural control over national security matters from Hong Kong's common law judiciary and legal profession to mainland security apparatus and centrally appointed institutions.
% ABSENT_VOICES: Hong Kong common law judges who have resigned or been disqualified; defense lawyers operating under expanded state-secret provisions; overseas jurists formerly appointed to Hong Kong's Court of Final Appeal; legal scholars whose work has been removed from curricula. They would contest the necessity and scope of mainland procedural transplantation but are structurally excluded from interpretive authority.
% DISAPPEARANCE_RATIONALE: If the NSL and its mainland-interpretive apparatus vanished overnight, Hong Kong's judiciary would regain full jurisdiction over all criminal matters in the territory, the mainland security office would lose its extraterritorial and extrajudicial enforcement powers in Hong Kong, and mainland legal concepts would recede, causing a major constitutional rearrangement.
% FOUNDING_PROBLEM: The 2019 protests and associated unrest were interpreted by central authorities as exposing a dangerous gap in Hong Kong's legal capacity to address threats to national security under a common law system perceived as procedurally permissive.
% FOUNDING_PROBLEM_CORROBORATION: The central government and mainland security apparatus attest the founding problem remains live. The Hong Kong judiciary, legal profession, and international human rights bodies contest that the problem justified the specific mainland-legal-transplant mechanism, arguing existing laws were sufficient. International bar associations and foreign legal monitors provide corroboration from outside the beneficiary set that the autonomy erosion exceeds security necessity.
narrative_ontology:disappearance_verdict(nsl_legal_text__jurisdictional_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__jurisdictional_capture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__jurisdictional_capture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nsl_legal_text__jurisdictional_capture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__jurisdictional_capture_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is set at 0.72 (moderate-high) because the constraint systematically removes classes of cases from local judicial oversight and replaces common law procedural safeguards with mainland security logic, constituting a transfer of institutional control. Suppression is 0.78 because the constraint's persistence depends on NPCSC interpretive authority, exclusion of certain defenses, and the sidelining of dissenting judicial voices; alternatives (full common law adjudication of security cases) are structurally barred. Accessibility_collapse is 0.82 because once the NSL is understood as overriding local constitutional safeguards, the alternative of robust judicial review collapses â judges self-limit or are removed. Resistance is 0.68 from ongoing professional criticism, resignations, and international monitoring. Theater_ratio is 0.45: common law forms (open trials, legal representation, habeas corpus appearances) are partially maintained as performative cover while substantive jurisdiction is exercised by mainland apparatus. The claim/metric gap is intentional â claimed_type is tangled_rope (genuine coordination plus asymmetric extraction) while metrics describe the extraction side without reconciling to the claim.
 *
 * PERSPECTIVAL GAP:
 *   The mainland security apparatus experiences the NSL as a necessary correction of jurisdictional fragmentation and a legitimate extension of sovereign authority; the HK judiciary and legal profession experience it as a capture of their interpretive and adjudicative role by an external legal logic. The engine computes this divergence from the same structural data â the directionality derivation amplifies extraction for the institutional payers and dampens it for the agenda-setting beneficiary.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainland security apparatus is declared in beneficiaries and serves as agenda_setter; structurally they sit near full beneficiary (d near 0.0) because the constraint subsidizes their jurisdictional expansion and extracts no costs from them. HK judiciary and legal profession are declared in victims and hold payer roles; they sit near full target (d near 1.0) because the constraint extracts institutional autonomy and procedural control from them. HK executive is a secondary beneficiary with constrained exit, sitting at moderate-low d. International observers are analytical with analytical exit, d irrelevant for classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) by acknowledging the genuine coordination function of national security governance, while the declared victim set and moderate-high extractiveness prevent mislabeling it as mere rope by capturing the asymmetric erosion of common law autonomy. The theater_ratio (0.45) signals that some common law forms are maintained performatively even as substantive jurisdiction is captured, but not high enough to suggest the constraint is merely a piton â the extraction is real and ongoing, administered by beneficiaries who actively enforce it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transplantation_vs_convergence,
    'Is the mainland legal system genuinely being transplanted into Hong Kong, or is a hybrid convergence emerging that preserves common law forms while subordinating their substance?',
    'Longitudinal analysis of judicial reasoning patterns and case outcomes under NSL to determine whether mainland legal logic is substituting for or merely supplementing common law procedure.',
    'If pure transplantation, extraction is higher and common law autonomy is more severely eroded; if hybrid convergence, the constraint may be less extractive but the tangled rope structure is more deeply embedded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transplantation_vs_convergence, conceptual, 'Ambiguity about whether the legal change is transplantation or convergence.').

omega_variable(
    autonomy_erosion_measurement,
    'How much of the measured extraction represents erosion of institutional autonomy versus legitimate security coordination?',
    'Comparative analysis of similar national security frameworks in common law jurisdictions to establish a baseline for procedural restriction that does not constitute capture.',
    'Would calibrate whether the extraction is inherent to security coordination or specific to jurisdictional capture, affecting the effective epsilon attribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_erosion_measurement, empirical, 'Baseline ambiguity for distinguishing security coordination from institutional capture.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of common law alternatives structural (direct override by NPCSC interpretation, removal of judges) or internalized (self-censorship within the judiciary and legal profession)?',
    'Post-removal judicial behavior analysis and anonymized practitioner surveys measuring self-censorship versus formal compliance.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the legal community carries the suppression with them even after individual exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in institutional capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl_cap_tr_t0, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(nsl_cap_tr_t1, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 1, 0.32).
narrative_ontology:measurement(nsl_cap_tr_t2, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2, 0.38).
narrative_ontology:measurement(nsl_cap_tr_t3, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 3, 0.42).
narrative_ontology:measurement(nsl_cap_tr_t4, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 4, 0.45).
narrative_ontology:measurement(nsl_cap_tr_t5, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 5, 0.46).
narrative_ontology:measurement(nsl_cap_tr_t6, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 6, 0.45).

% Extraction over time
narrative_ontology:measurement(nsl_cap_be_t0, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(nsl_cap_be_t1, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 1, 0.64).
narrative_ontology:measurement(nsl_cap_be_t2, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2, 0.67).
narrative_ontology:measurement(nsl_cap_be_t3, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 3, 0.69).
narrative_ontology:measurement(nsl_cap_be_t4, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 4, 0.71).
narrative_ontology:measurement(nsl_cap_be_t5, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(nsl_cap_be_t6, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 6, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(nsl_cap_su_t0, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(nsl_cap_su_t1, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 1, 0.72).
narrative_ontology:measurement(nsl_cap_su_t2, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2, 0.76).
narrative_ontology:measurement(nsl_cap_su_t3, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 3, 0.79).
narrative_ontology:measurement(nsl_cap_su_t4, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 4, 0.8).
narrative_ontology:measurement(nsl_cap_su_t5, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 5, 0.79).
narrative_ontology:measurement(nsl_cap_su_t6, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 6, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__jurisdictional_capture_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
