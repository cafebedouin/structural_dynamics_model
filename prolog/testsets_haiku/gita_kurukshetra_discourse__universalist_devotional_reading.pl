% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__universalist_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_universalist_devotional, []).

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
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Bhagavad Gita Universalist Devotional Reading: Path-Independent Salvation
 *   domain: religious_philosophy/textual_hermeneutics/ethics
 *
 * SUMMARY:
 *   The Bhagavad Gita is the most contested sacred text in Hindu philosophy.
 *   The universalist devotional reading claims the text teaches that personal
 *   devotion (bhakti) to the divine is accessible to all regardless of caste,
 *   gender, or Vedic learning, and that dharma means surrender to divine will
 *   rather than fulfillment of caste-assigned social role. This reading
 *   directly challenges orthodox Brahminical interpretations that use the
 *   same text to legitimize caste hierarchy and caste-based duty. The
 *   constraint is the reading itself—the institutionalization of the
 *   universalist interpretation as a lived framework that grants spiritual
 *   legitimacy to non-Brahminical devotees and delegitimizes
 *   caste-gatekeeping. This story instantiates the universalist reading as
 *   ONE interpretation of a contested kernel, not as 'the truth' of the text.
 *   The orthodox literal reading and Gandhian allegorical reading are
 *   separate constraints with their own ε values and beneficiary structures.
 *
 * KEY AGENTS:
 *   - non_brahmin_devotees: Spiritual beneficiaries; gain direct salvific access without caste mediation.
 *   - lower_caste_spiritual_seekers: Powerless beneficiaries; were structurally excluded under orthodoxy; gain legitimacy through the reading.
 *   - women_excluded_from_vedic_ritual: Powerless beneficiaries; gain equal standing in devotional practice.
 *   - universalist_reform_movements: Institutional agenda-setters; actively promote and institutionalize the reading; derive authority from its success.
 *   - brahminical_ritual_gatekeepers: Institutional payers; lose monopoly interpretive authority; experience institutional decay and revenue loss as the reading spreads.
 *   - orthodox_vedic_authority_structures: Institutional payers; lose normative grounding for caste-based hierarchy; face delegitimation.
 *   - contemporary_textual_scholars: Analytical observers; document the hermeneutical contest and historical emergence of the reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.38).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.51).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Bhagavad Gita Universalist Devotional Reading: Path-Independent Salvation").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious_philosophy/textual_hermeneutics/ethics").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__universalist_devotional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, '5230984d-19e4-4bb0-b9b9-3abc1f214fd1').
narrative_ontology:cs_kernel_codification('5230984d-19e4-4bb0-b9b9-3abc1f214fd1', fixed_text).
narrative_ontology:cs_authority_grounding('5230984d-19e4-4bb0-b9b9-3abc1f214fd1', lineage).
narrative_ontology:cs_interpretation_layer_present('5230984d-19e4-4bb0-b9b9-3abc1f214fd1').
narrative_ontology:cs_reading_relation('5230984d-19e4-4bb0-b9b9-3abc1f214fd1', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('5230984d-19e4-4bb0-b9b9-3abc1f214fd1', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_axiom('5230984d-19e4-4bb0-b9b9-3abc1f214fd1', foundational, bhakti_sufficiency_for_salvation).
narrative_ontology:cs_axiom_status(bhakti_sufficiency_for_salvation, holdable).
narrative_ontology:cs_axiom_grounding('5230984d-19e4-4bb0-b9b9-3abc1f214fd1', bhakti_sufficiency_for_salvation, deontological).
narrative_ontology:cs_axiom('5230984d-19e4-4bb0-b9b9-3abc1f214fd1', foundational, caste_spiritually_irrelevant).
narrative_ontology:cs_axiom_status(caste_spiritually_irrelevant, holdable).
narrative_ontology:cs_axiom_grounding('5230984d-19e4-4bb0-b9b9-3abc1f214fd1', caste_spiritually_irrelevant, deontological).
narrative_ontology:cs_reference_frame('5230984d-19e4-4bb0-b9b9-3abc1f214fd1', universal_devotional_access).
narrative_ontology:cs_drift_state('5230984d-19e4-4bb0-b9b9-3abc1f214fd1', contemporary_post_reform_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('5230984d-19e4-4bb0-b9b9-3abc1f214fd1', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, non_brahmin_devotees).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, lower_caste_spiritual_seekers).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, women_excluded_from_vedic_ritual).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, universalist_reform_movements).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, brahminical_ritual_gatekeepers).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_vedic_authority_structures).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).
:- end_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the universalist reading accomplishes real coordination—it legitimates devotional practice outside ritual hierarchies and provides a coherent framework for spiritual inclusion. The reading is not pure extraction (snare-level extractiveness would be 0.65+) because it does solve a genuine coordination problem: how can the Gita speak to non-elite communities while caste gatekeeping claims the text as authority? However, extractiveness is not negligible because the reading's circulation requires suppressing and delegitimizing orthodox interpretations, and the institutional cost of defending hermeneutical orthodoxy against this alternative is substantial. Suppression is moderate-high (0.51) because maintaining orthodox gatekeeping against the universalist reading's logic requires active institutional enforcement—textual exegesis, pulpit authority, control of educational institutions—rather than merely passive absence of alternatives. The reading must be actively suppressed by orthodox authorities because once it circulates, alternatives become visible. Theater is low-moderate (0.29 at interval end, declining from 0.45) because the universalist reading's core function—legitimating non-elite spiritual access—is genuine; theatrical maintenance is modest. The declining theater_ratio over the interval reflects institutional normalization: early in the reading's adoption, reform movements had to perform legitimacy aggressively; as the reading became established (cultural shift post-reform era), the performance requirement declined. Accessibility collapse is high (0.72) because once the universalist logic is understood, caste-based gatekeeping becomes textually incoherent and alternatives (orthodoxy, other gate-keeping frames) are closed off for devotees committed to the reading's egalitarian premise. Resistance is high (0.68) because orthodox authorities mount active counter-readings and institutional defense; the contest is live, not settled.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (non-Brahmin devotees, reform movements) and payer seats (Brahminical authorities) experience fundamentally different constraint structures. From the beneficiary seat, the universalist reading is liberation—a coordinate solution to exclusion. From the payer seat, it is extraction—a delegitimating attack on the institutional foundations of interpretive authority. The engine should compute this divergence from the structural data: the beneficiary seats should show low effective extraction (they gain spiritual legitimacy without cost); the payer seats should show high effective extraction (they lose authority and institutional resources). The claim and metrics are deliberately misaligned to reveal this structure—I claim tangled_rope because the reading performs real coordination AND involves asymmetric extraction (losers are the gatekeepers). The engine computes per-seat types and surfaces whether the reading consolidates into rope (universal benefit) or snare (pure extraction with coordination cover).
 *
 * DIRECTIONALITY LOGIC:
 *   Non-Brahmin devotees and lower-caste seekers are full beneficiaries (d near 0.0) under this reading: they gain direct salvific access without bearing the cost of Brahminical gatekeeping. Women gain equivalent standing in spiritual practice (low d for this reading specifically, though they may carry d > 0.5 under other constraints). Brahminical gatekeepers are targets (d near 1.0): they lose monopoly interpretive authority, see institutional resources decline, and must actively defend against the reading's logic. Reform movements derive authority from the reading's success (d near 0.0 as beneficiaries, though they carry agenda-setter power—power does not determine directionality, structural relationship to extraction does). The directionality is asymmetric: the reading coordinates salvation-access for the many (lower extraction vectors) and extracts authority from the few (gatekeepers). This asymmetry is what makes it tangled_rope, not rope—the coordination is real but so is the concentrated extraction from the dispossessed authority structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading avoids the mandatrophy trap because its founding problem remains live: Brahminical gatekeeping of spiritual access persists, and the reading continues to address it by providing an alternative framework. Contemporary lower-caste communities, women practitioners, and Dalit theologians attest the founding problem is not solved—institutional gatekeeping has weakened but not disappeared. The reading is not zombie-function; it has active constituency defending it because the problem it was designed to solve still exists. The orthodox reading contests the problem's existence (arguing caste hierarchy is eternal dharma, not unjust exclusion), but the contest itself shows the mandate is live, not dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Does the Bhagavad Gita text itself permit only one coherent reading, or does the Sanskrit afford multiple legitimate interpretations that support both the universalist devotional and orthodox literal framings?',
    'Detailed linguistic and contextual analysis of key passages (especially 2.31, 9.32, 10.11) examining whether the text unambiguously forecloses the orthodox reading or whether both readings remain hermeneutically defensible from the same source material.',
    'If the text genuinely supports both readings, the constraint is a contest of interpretive authority rather than a textual determination—the reading lives by institutional and social power, not by textual proof. If one reading is textually forced, the defeated reading is foreclosed rather than coexisting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the kernel text logically forecloses the orthodox reading or permits both readings as legitimate.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.51) structural—enforcement machinery defending orthodoxy—or internalized—devotees from lower castes having absorbed caste hierarchy as cosmically just despite the universalist reading''s logic?',
    'Post-reform social dynamics: if lower-caste devotees who adopt the universalist reading experience complete suppression-relief after institutional gatekeeping is removed, suppression is primarily structural; if internalized shame and belief-patterns persist despite institutional opening, suppression carries a psychological component that the reading alone does not dissolve.',
    'If substantial internalization is present, the constraint''s effective suppression is higher than the structural measure suggests, and dissolution requires not just textual reframing but active social reconstruction and shame-working.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural (external gatekeeping) or internalized (absorbed self-limitation).').

omega_variable(
    extractiveness_floor_for_hermeneutical_contestation,
    'What portion of the measured extractiveness (0.38) reflects genuine coordination costs—the institutional labor of maintaining a contested textual tradition—versus pure rent-seeking by orthodox authorities defending interpretive monopoly?',
    'Cost-structure analysis: what resources does the universalist reading require to maintain institutional presence (teaching lineages, publishing, commentary production) versus what resources the orthodox reading requires to defend its gates? The cost floor is the minimum extractiveness required for any dominant hermeneutical position to be institutionally stable.',
    'If coordination costs are substantial (0.25+), the extractiveness is partially legitimate overhead; if gatekeeping is the primary cost driver, extraction is nearly pure. This affects the boundary between tangled_rope and snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extractiveness_floor_for_hermeneutical_contestation, empirical, 'The proportion of extractiveness that is institutional coordination cost versus monopoly rent.').

omega_variable(
    caste_dissolution_ontology,
    'When the universalist reading declares ''caste is not a spiritual barrier,'' does it mean caste is ontologically irrelevant (dissolved at the level of ultimate reality) or merely that caste-based exclusion from salvific access is morally unjustified even if caste hierarchies persist socially?',
    'Textual analysis and contemporary theological articulation: does the reading entail caste hierarchies are illusory/unreal, or does it entail caste is real but spiritually insignificant? This determines whether the reading''s beneficiaries are contesting Brahminical interpretive authority only (power struggle) or asserting a fundamental metaphysical claim (ontological counter-claim).',
    'If ontological, the reading forecloses the orthodox reading''s metaphysical foundation; if merely axiological (spiritual insignificance), both readings can coexist as different value frameworks on the same ontology—a weaker foreclosure claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_dissolution_ontology, conceptual, 'Whether the reading entails caste is ontologically dissolved or merely spiritually insignificant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(gita_tr_t5, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(gita_tr_t10, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(gita_tr_t15, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(gita_tr_t25, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(gita_tr_t30, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 40, 0.29).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(gita_be_t5, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(gita_be_t10, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(gita_be_t15, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(gita_be_t25, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(gita_be_t30, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(gita_su_t5, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(gita_su_t10, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(gita_su_t15, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(gita_su_t25, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 25, 0.54).
narrative_ontology:measurement(gita_su_t30, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(gita_su_t40, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 40, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__universalist_devotional_reading, 0.12).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, brahminical_gatekeeping_authority).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, vedic_ritual_monopoly).

% DUAL FORMULATION NOTE:
% The Gita kernel spawns three separate constraints, one per reading (universalist_devotional, orthodox_literal, gandhian_allegorical). Each reading instantiates a different constraint with a different ε, beneficiary/victim structure, and type. The universalist reading (this story) has moderate extractiveness (0.38) and earned-legitimacy suppression because it performs real coordination (including non-Brahmins in salvific access) while extracting from orthodox authority. The orthodox reading would show high extractiveness if authored (the constraint protects Brahminical monopoly with no coordination benefit to the majority). The three stories are linked through network.affects_constraints to show they are readings of one kernel, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra_discourse__universalist_devotional_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
