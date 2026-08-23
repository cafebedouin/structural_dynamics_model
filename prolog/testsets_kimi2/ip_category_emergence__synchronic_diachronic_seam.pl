% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__synchronic_diachronic_seam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: ip_category_emergence__synchronic_diachronic_seam
 *   human_readable: Conflation of Category Emergence and First-Holding in Anglo-IP Historiography
 *   domain: legal philosophy/intellectual property/historical jurisprudence
 *
 * SUMMARY:
 *   The constraint under analysis is the dominant legal-historiographical
 *   framing in Anglophone intellectual property scholarship that couples the
 *   emergence of copyright as a coherent legal category (thinkability, M4)
 *   with the entry of authors into the set of legitimate proprietary
 *   claimants (first-holding, M5) at the Statute of Anne 1710. This
 *   readingâsynchronic_diachronic_seamâtests whether that coupling is a
 *   structural feature of legal history or merely a temporal framing
 *   artifact. The constraint coordinates a shared pedagogical and scholarly
 *   origin myth while extracting visibility and resources from comparative,
 *   critical, and non-Western historiographical traditions.
 *
 * KEY AGENTS:
 *   - anglo_ip_legal_educators: agenda_setter (institutional/constrained) â administers the canonical curriculum
 *   - anglo_ip_scholars: beneficiary (organized/constrained) â collects prestige and citations from the dominant frame
 *   - comparative_legal_historians: payer (moderate/constrained) â bears marginalization costs for non-Anglo genealogies
 *   - critical_ip_theorists: payer (moderate/constrained) â bears costs for challenging the teleological narrative
 *   - methodological_analysts: observer (analytical/analytical) â evaluates kernel authenticity from outside
 *   - indigenous_knowledge_holders: excluded (powerless/trapped) â entirely outside the origin discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, 0.65).
domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, 0.52).
domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, extractiveness, 0.65).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__synchronic_diachronic_seam, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__synchronic_diachronic_seam, "Conflation of Category Emergence and First-Holding in Anglo-IP Historiography").
narrative_ontology:topic_domain(ip_category_emergence__synchronic_diachronic_seam, "legal philosophy/intellectual property/historical jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__synchronic_diachronic_seam).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__synchronic_diachronic_seam, '89a7bf4d-7abf-40bb-a16c-47dee7b9d5e9').
narrative_ontology:cs_kernel_codification('89a7bf4d-7abf-40bb-a16c-47dee7b9d5e9', fixed_text).
narrative_ontology:cs_authority_grounding('89a7bf4d-7abf-40bb-a16c-47dee7b9d5e9', lineage).
narrative_ontology:cs_interpretation_layer_present('89a7bf4d-7abf-40bb-a16c-47dee7b9d5e9').
narrative_ontology:cs_reading_relation('89a7bf4d-7abf-40bb-a16c-47dee7b9d5e9', ip_category_emergence__thinkability_reading, influences).
narrative_ontology:cs_reading_relation('89a7bf4d-7abf-40bb-a16c-47dee7b9d5e9', ip_category_emergence__first_holding_reading, influences).
narrative_ontology:cs_axiom('89a7bf4d-7abf-40bb-a16c-47dee7b9d5e9', foundational, category_emergence_and_occupancy_may_vary_independently).
narrative_ontology:cs_axiom_status(category_emergence_and_occupancy_may_vary_independently, holdable).
narrative_ontology:cs_axiom_grounding('89a7bf4d-7abf-40bb-a16c-47dee7b9d5e9', category_emergence_and_occupancy_may_vary_independently, empirically_contingent).
narrative_ontology:cs_reference_frame('89a7bf4d-7abf-40bb-a16c-47dee7b9d5e9', statute_of_anne_synchronic_moment).
narrative_ontology:cs_drift_state('89a7bf4d-7abf-40bb-a16c-47dee7b9d5e9', contemporary_methodological_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('89a7bf4d-7abf-40bb-a16c-47dee7b9d5e9', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, anglo_ip_scholars).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, anglo_ip_legal_educators).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, comparative_legal_historians).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, critical_ip_theorists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and administer IP law curricula that treat the Statute of Anne 1710 as the canonical origin point, implicitly coupling the emergence of copyright as a legal category with the recognition of authors as proprietary claimants. Their professional standing and curricular stability depend on reproducing this narrative.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, anglo_ip_legal_educators, agenda_setter,
    institutional, generational, constrained, global).

% Publish within a citation network that rewards scholarship centered on the 1710 origin moment and the Anglo-American development of authorial rights. Collect prestige, tenure, and grant funding from the dominance of this historiographical frame.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, anglo_ip_scholars, beneficiary,
    organized, biographical, constrained, global).

% Research non-Anglo genealogies of intellectual propertyâsuch as continental printing privileges, droit moral, and indigenous knowledge regimesâbut face marginalization in top journals and core curricula. Their work is treated as comparative context rather than constitutive history.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, comparative_legal_historians, payer,
    moderate, biographical, constrained, global).

% Challenge the teleological narrative that projects modern author-proprietor concepts onto the past. They bear the cost of disciplinary stigma and reduced access to mainstream publication venues for arguing that the 1710 moment is over-determined by presentist concerns.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, critical_ip_theorists, payer,
    moderate, biographical, constrained, global).

% Propose the M4/M5 collapse test to determine whether category emergence (thinkability) and occupancy change (first-holding) are formally independent variables or merely artifacts of temporal framing. They occupy an analytical seat outside the disciplinary contest, evaluating the kernel's structural authenticity.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, methodological_analysts, observer,
    analytical, civilizational, analytical, universal).

% Maintain knowledge governance systems that operate as proprietary regimes outside the Anglo-legal framework. They are entirely absent from the canonical origin narrative and would contest the privileging of the 1710 statutory moment if included in the discourse.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, indigenous_knowledge_holders, excluded,
    powerless, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified historical narrative and pedagogical framework for teaching the origins of intellectual property law across Anglophone legal education, solving the coordination problem of a shared curricular starting point and canonical vocabulary.
% TRANSFER_FUNCTION: Moves scholarly prestige, citations, curriculum space, and institutional resources from comparative, critical, and pre-1710 historiographical traditions to Anglophone legal educators and scholars centered on the Statute of Anne.
% ABSENT_VOICES: Continental European legal historians who trace IP genealogies through droit moral and printing privileges; indigenous knowledge scholars who identify non-Western propriety regimes; gradualist historians who reject punctuated origin moments in legal development. They are absent from canonical textbooks and core curricula.
% DISAPPEARANCE_RATIONALE: If the conflated framing vanished, the standard first-week IP syllabus would lose its organizing principle; textbooks would require rewriting around either non-Anglo genealogies or a methodological introduction problematizing the relationship between category emergence and rights-holder recognition; the distribution of scholarly prestige and citation would shift away from the 1710 specialist toward comparative and critical scholars.
% FOUNDING_PROBLEM: The need for a coherent, teachable origin point for intellectual property law in common-law legal education, providing students and scholars with a stable reference for the emergence of author-centered proprietary rights in expression.
% FOUNDING_PROBLEM_CORROBORATION: Comparative legal historians and critical IP scholars attest that the founding problem is overstated and that the arrangement persists as disciplinary convention; no corroboration exists from outside the benefiting parties that the specific coupling of category emergence and occupancy change was necessary to solve the pedagogical problem.
narrative_ontology:disappearance_verdict(ip_category_emergence__synchronic_diachronic_seam, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__synchronic_diachronic_seam, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__synchronic_diachronic_seam, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ip_category_emergence__synchronic_diachronic_seam, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__synchronic_diachronic_seam, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.65) is authored at moderate-high because the canonical narrative structurally redirects scholarly attention, curriculum space, and citation flows toward the 1710 moment and away from alternative genealogies. Suppression (0.52) reflects active but non-total institutional gatekeeping: comparative work can be published but rarely in elite venues or required courses. Theater ratio (0.42) captures the performative dimension of legal historiography that presents the 1710 coupling as discovered historical fact rather than constructed periodization. Accessibility collapse (0.38) is moderate because alternatives remain visible to motivated searchers but are structurally disadvantaged. Resistance (0.35) reflects the steady output of critical and comparative scholarship that contests the frame. Measurements trace a gradual intensification as the field professionalized and citation metrics hardened.
 *
 * PERSPECTIVAL GAP:
 *   From the Anglo-IP scholar and educator seats, the constraint is experienced as necessary pedagogical coordination: a common origin point is required to teach a complex field, and the Statute of Anne is the most convenient and well-documented candidate. From the comparative historian and critical theorist seats, the same structure is experienced as extractive marginalization: the convenience of the Anglo canon comes at the cost of rendering their research peripherally relevant. The engine computes this divergence from the structural dataâsame constraint, opposite directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (anglo_ip_scholars, anglo_ip_legal_educators) sit at low directionality: the constraint subsidizes their professional standing, citation networks, and curricular stability. Payers (comparative_legal_historians, critical_ip_theorists) sit at high directionality: the constraint extracts from their research visibility and career opportunities by treating their objects of study as comparative rather than foundational. The indigenous_knowledge_holders seat is excluded entirely, carrying no directionality in the computation. The methodological_analysts seat is analytical and external. No overrides are needed: the structural derivation from beneficiary/victim declarations plus exit options (constrained for all embedded actors) correctly maps the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâproviding a teachable origin point for IP law in common-law legal educationâwas genuine. However, the constraint has outlived its solving phase: the specific coupling of category emergence and occupancy change was not necessary to solve the pedagogical problem (a decoupled or multi-genealogy approach would serve), and the arrangement now persists because it benefits identifiable parties. The classification as tangled_rope captures this precisely: there is real coordination (shared scholarly vocabulary and pedagogy) but also real extraction (marginalization of non-Anglo voices) that requires active enforcement (peer review, curriculum control). A snare classification would miss the coordination; a rope classification would miss the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anglo_origin_myth_naturalness,
    'Is the coupling of category emergence and occupancy change at 1710 a discovered historical fact or a constructed narrative serving the Anglo-American legal tradition?',
    'Systematic comparative historiography finding instances of category emergence without author-occupancy (or vice versa) before 1710, or failure to find such instances despite global archival search.',
    'If constructed, the constraint is a false summit mountain or snare; if discovered, it may be a rope of historiographical coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anglo_origin_myth_naturalness, empirical, 'Natural-law vs constructed ambiguity of the 1710 origin kernel').

omega_variable(
    suppression_mechanism_in_discourse,
    'Is the marginalization of comparative and critical IP historiography enforced through structural institutional barriers or through internalized disciplinary consensus?',
    'Tracking citation networks, curriculum adoption rates, and editorial board composition across Anglophone vs non-Anglophone journals; post-exit trajectory of scholars who leave the canon.',
    'If internalized, effective suppression exceeds structural measures and the constraint behaves more like identity-coordination; if purely structural, reform targets gatekeeping institutions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_in_discourse, conceptual, 'Structural vs internalized suppression in legal historiography').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__synchronic_diachronic_seam, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_cat_sync_diach_tr_t0, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ip_cat_sync_diach_tr_t10, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 10, 0.26).
narrative_ontology:measurement(ip_cat_sync_diach_tr_t20, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 20, 0.31).
narrative_ontology:measurement(ip_cat_sync_diach_tr_t30, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 30, 0.36).
narrative_ontology:measurement(ip_cat_sync_diach_tr_t40, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 40, 0.4).
narrative_ontology:measurement(ip_cat_sync_diach_tr_t50, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(ip_cat_sync_diach_be_t0, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ip_cat_sync_diach_be_t10, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(ip_cat_sync_diach_be_t20, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(ip_cat_sync_diach_be_t30, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(ip_cat_sync_diach_be_t40, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(ip_cat_sync_diach_be_t50, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ip_cat_sync_diach_su_t0, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(ip_cat_sync_diach_su_t10, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(ip_cat_sync_diach_su_t20, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(ip_cat_sync_diach_su_t30, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 30, 0.51).
narrative_ontology:measurement(ip_cat_sync_diach_su_t40, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(ip_cat_sync_diach_su_t50, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__synchronic_diachronic_seam, identity_coordination).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, first_holding_reading).

% DUAL FORMULATION NOTE:
% The kernel ip_category_emergence decomposes into three readings per the epsilon-invariance principle: thinkability_reading (category emergence), first_holding_reading (occupancy change), and synchronic_diachronic_seam (collapse test). Each reading has distinct structural properties and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
