% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__synchronic_diachronic_seam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__synchronic_diachronic_seam, []).

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
 *   constraint_id: ip_category_emergence__synchronic_diachronic_seam
 *   human_readable: IP Category Emergence: Synchronic-Diachronic Seam (M4/M5 Collapse Test)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This reading tests whether the IP category kernel has two formally
 *   independent dimensions—thinkability (whether 'authored expression' is
 *   conceptually coherent as a legal category) and first-holding (whether
 *   authors entered the legitimate claimant set)—or whether these are
 *   temporal artifacts of a single event where occupancy change *created* the
 *   thinkability retroactively. The synchronic-diachronic seam reading argues
 *   that the M4/M5 test (examining whether category emergence and occupancy
 *   change can vary independently across cases) is the decisive evidence. If
 *   thinkability and first-holding always co-occur, the kernel claims
 *   independence falsely. The constraint is CLAIMED as tangled_rope
 *   (coordination function + enforced extraction) and the metrics track how
 *   far the doctrine's theater (justifying the category as pre-existing) has
 *   drifted from its function (actually organizing literary production under
 *   new property rules).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, 0.62).
domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, 0.71).
domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, extractiveness, 0.62).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__synchronic_diachronic_seam, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__synchronic_diachronic_seam, "IP Category Emergence: Synchronic-Diachronic Seam (M4/M5 Collapse Test)").
narrative_ontology:topic_domain(ip_category_emergence__synchronic_diachronic_seam, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__synchronic_diachronic_seam).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__synchronic_diachronic_seam, '5c5a0663-3c90-482a-a787-465722861277').
narrative_ontology:cs_kernel_codification('5c5a0663-3c90-482a-a787-465722861277', fixed_text).
narrative_ontology:cs_authority_grounding('5c5a0663-3c90-482a-a787-465722861277', extraction).
narrative_ontology:cs_interpretation_layer_present('5c5a0663-3c90-482a-a787-465722861277').
narrative_ontology:cs_reading_relation('5c5a0663-3c90-482a-a787-465722861277', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c5a0663-3c90-482a-a787-465722861277', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_axiom('5c5a0663-3c90-482a-a787-465722861277', foundational, thinkability_first_holding_independence).
narrative_ontology:cs_axiom_status(thinkability_first_holding_independence, holdable).
narrative_ontology:cs_axiom_grounding('5c5a0663-3c90-482a-a787-465722861277', thinkability_first_holding_independence, conventional).
narrative_ontology:cs_axiom('5c5a0663-3c90-482a-a787-465722861277', foundational, category_recognition_vs_creation).
narrative_ontology:cs_axiom_status(category_recognition_vs_creation, holdable).
narrative_ontology:cs_axiom_grounding('5c5a0663-3c90-482a-a787-465722861277', category_recognition_vs_creation, deontological).
narrative_ontology:cs_reference_frame('5c5a0663-3c90-482a-a787-465722861277', pre_existing_authored_expression_category).
narrative_ontology:cs_drift_state('5c5a0663-3c90-482a-a787-465722861277', contemporary_m4_m5_testing, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5c5a0663-3c90-482a-a787-465722861277', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, literary_authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, copyright_doctrine).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, folk_tradition_custodians).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, collaborative_knowledge_commons).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__synchronic_diachronic_seam, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ip_category_emergence__synchronic_diachronic_seam, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 endpoint) is substantial because the doctrine imposes a property regime that benefits authors but extracts from folk tradition and collaborative commons—extraction rises as the doctrine hardens and extends protection to broader categories of expression. Theater ratio (0.48 endpoint) reflects rising performative maintenance of the category boundary; the doctrine increasingly relies on abstract justifications (natural rights, pre-existing thinkability) rather than pragmatic ones (efficiency gains from attribution). Suppression requirement (0.71 endpoint) reflects active enforcement needed to exclude alternatives: folk traditions cannot claim the same protection, collaborative work must be disaggregated into attributed pieces, and alternative property regimes (commons, public domain by default) must be suppressed. The measurement series track extraction accumulation as the doctrine extends, theater drift as justifications become more abstract, and suppression intensification as more cultural forms must be excluded to maintain the boundary. All metrics authored on the same time grid spanning 1710 to ~2010.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (copyright doctrine) and the observer seat (historical analysis) compute radically different directionalities because they read the kernel differently. The doctrine needs thinkability and first-holding to be independent (pre-existing category, doctrine merely recognizes it) to justify its extraction as coordination rather than construction. The observer seat finds that occupancy change created thinkability, which would reclassify the constraint as pure extraction wearing a coordination mask. The engine computes per-seat types; the gap between them is the diagnostic signal.
 *
 * DIRECTIONALITY LOGIC:
 *   The copyright doctrine itself sits in the agenda-setter position (institutional power, creates and enforces rules). Literary authors are beneficiaries (arbitrary d near 0.0—they collect the exclusive rights the doctrine grants). Folk tradition custodians and collaborative commons are victims (trapped powerless agents, d near 1.0—they lose access to legal standing and protection under the regime). The synchronic-diachronic seam reading creates a structural asymmetry: from the doctrine's position (administrative seat), thinkability and first-holding are independent and the category is natural; from the historical analyst's position (observer), the two collapse and the category is constructed. This divergence is structural, not empirical—it follows from different epistemic access to the founding moment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (authors exploited by printers) may or may not be dead: if authors still need licensing mechanisms and protection from copying, the problem is live; if commercial publishing has solved copying via technology and authors now negotiate directly, the problem is dead but the doctrine persists. The seam reading argues that this distinction is precisely undecidable *within* the doctrine's own framework—because the doctrine conflates thinkability with occupancy. If the two are independent, you can ask whether the founding problem is live. If they are not independent, asking the question dissolves the premise. Mandatrophy resolution turns on whether the M4/M5 test produces independence or collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    m4_m5_collapse_empirical,
    'Do thinkability and first-holding vary independently across the historical record and across different legal regimes, or do they always co-occur?',
    'Comparative analysis: (1) examine cases where a legal system grants rights (first-holding) but the category is not described as newly thinkable (suggests independence); (2) examine discourses where authorship is described as a conceptual category before any legal grant (suggests independence); (3) check whether post-1710 extensions of IP to new media (photography, film, software) show thinkability preceding occupancy rights or vice versa.',
    'If independence holds across domains, the kernel''s claim to independence is vindicated and thinkability is a real, prior conceptual achievement the law recognizes. If collapse is systematic, thinkability is a retroactive narrative artifact; occupancy-granting is the engine and thinkability is the justification. The constraint would be reclassified from tangled_rope (coordination + extraction) to snare (pure extraction disguised as recognition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m4_m5_collapse_empirical, empirical, 'Whether category emergence and occupancy change are empirically independent or always co-occur.').

omega_variable(
    natural_rights_ante_dating_statute,
    'Did pre-1710 natural rights theorists (Hobbes, Locke, Blackstone) explicitly theorize authored expression as a property category, or do they discuss labor/property in work without treating authored expression as a distinct ownable kind?',
    'Close reading of primary texts and secondary scholarly consensus on Locke''s labor theory, Blackstone''s commentaries, and pre-Statute jurists. Does the category appear fully formed in natural rights discourse, or do authors appear as cases of the labor principle without a dedicated category?',
    'If the category appears fully theorized before 1710, thinkability predates first-holding, supporting independence. If the category is read into pre-Statute texts retroactively (what scholars call ''natural rights mythology''), then occupancy-granting (the Statute) created the conceptual apparatus; thinkability collapsed with first-holding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_rights_ante_dating_statute, empirical, 'Whether natural rights theorists pre-formed the ''authored expression as ownable'' category or read it in retroactively.').

omega_variable(
    alternative_property_regimes_suppression,
    'Is the suppression of alternative property regimes (commons-based, attribution-only, open distribution) a necessary component of maintaining the IP category''s coherence, or separable from it?',
    'Test via jurisdictions that allow parallel regimes: if Creative Commons and similar frameworks can coexist with copyright doctrine without the category becoming incoherent, suppression is enforcement-overhead, not category-constitutive. If the category requires that alternatives be suppressed, it is category-constitutive and suppression is part of what makes thinkability stick.',
    'If suppression is constitutive, the constraint''s extractive power lies in enforcing a single regime over competitors; the coordination function is real but so is the coercive exclusivity. If suppression is enforcement-overhead, the coordination and extraction components are more clearly separable. Either way, the theater ratio rises if the doctrine increasingly frames the category as natural rather than contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_property_regimes_suppression, empirical, 'Whether alternative property regimes can coexist with IP category coherence or whether suppression is category-constitutive.').

omega_variable(
    kernel_independence_reading_foreclosure,
    'Does this reading''s finding (M4/M5 collapse) foreclose the kernel''s independence claim, or does the kernel survive by redefining independence?',
    'If M4/M5 shows systematic co-occurrence (collapse), can the doctrine reframe by saying ''thinkability means *enforceable legal thinkability*, which is independent of first-holding as a historical moment''? Or does the doctrine abandon the independence claim entirely?',
    'A successful reframing would preserve the kernel''s legitimacy narrative by shifting what counts as thinkability; collapse of the independence claim would require the doctrine to justify its extraction on grounds other than recognizing a pre-existing category (e.g., utilitarian incentive provision). This is the highest-stakes ambiguity: whether the M4/M5 test actually forecloses the doctrine''s founding claim or merely forces a redefinition of it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_independence_reading_foreclosure, conceptual, 'Whether M4/M5 collapse forecloses the kernel''s independence claim or merely triggers a conceptual redefinition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__synchronic_diachronic_seam, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t0, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ip_c_tr_t50, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 50, 0.38).
narrative_ontology:measurement(ip_c_tr_t100, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 100, 0.42).
narrative_ontology:measurement(ip_c_tr_t150, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 150, 0.45).
narrative_ontology:measurement(ip_c_tr_t225, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 225, 0.47).
narrative_ontology:measurement(ip_c_tr_t300, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 300, 0.48).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t0, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ip_c_be_t50, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(ip_c_be_t100, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 100, 0.58).
narrative_ontology:measurement(ip_c_be_t150, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 150, 0.61).
narrative_ontology:measurement(ip_c_be_t225, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 225, 0.62).
narrative_ontology:measurement(ip_c_be_t300, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 300, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t0, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(ip_c_su_t50, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 50, 0.62).
narrative_ontology:measurement(ip_c_su_t100, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 100, 0.66).
narrative_ontology:measurement(ip_c_su_t150, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 150, 0.69).
narrative_ontology:measurement(ip_c_su_t225, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 225, 0.71).
narrative_ontology:measurement(ip_c_su_t300, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 300, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__first_holding_reading).

% DUAL FORMULATION NOTE:
% The IP category emergence kernel decomposes into three constraint stories testing whether category emergence and occupancy change are independent axes or a single collapsed event. The synchronic-diachronic seam reading (this story) tests the M4/M5 independence hypothesis. The thinkability_reading story isolates the moment 'ownable authored expression' became conceptually coherent. The first_holding_reading story isolates the moment authors became legitimate rights-holders. All three stories share the same interval and measurement grid; they differ in which structural axis they isolate. Decomposition is necessary because a single story trying to adjudicate all three dimensions would conflate metric independence with the conceptual independence being tested, violating DP-001 (ε-invariance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ip_category_emergence__synchronic_diachronic_seam, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
