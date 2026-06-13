% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__reformist_egalitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__reformist_egalitarian_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vedic_dharmic_corpus__reformist_egalitarian_reading
 *   human_readable: Vedic Textual Authority Reinterpreted Through Constitutional Equality
 *   domain: religious/political/social
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).
domain_priors:suppression_score(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.52).
domain_priors:theater_ratio(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__reformist_egalitarian_reading, "Vedic Textual Authority Reinterpreted Through Constitutional Equality").
narrative_ontology:topic_domain(vedic_dharmic_corpus__reformist_egalitarian_reading, "religious/political/social").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__reformist_egalitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__reformist_egalitarian_reading, '1cad804b-95bb-415c-9703-8760670e7ea8').
narrative_ontology:cs_kernel_codification('1cad804b-95bb-415c-9703-8760670e7ea8', fixed_text).
narrative_ontology:cs_authority_grounding('1cad804b-95bb-415c-9703-8760670e7ea8', extraction).
narrative_ontology:cs_interpretation_layer_present('1cad804b-95bb-415c-9703-8760670e7ea8').
narrative_ontology:cs_reading_relation('1cad804b-95bb-415c-9703-8760670e7ea8', vedic_dharmic_corpus__hereditary_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('1cad804b-95bb-415c-9703-8760670e7ea8', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('1cad804b-95bb-415c-9703-8760670e7ea8', foundational, constitutional_equality_textual_supremacy).
narrative_ontology:cs_axiom_status(constitutional_equality_textual_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('1cad804b-95bb-415c-9703-8760670e7ea8', constitutional_equality_textual_supremacy, deontological).
narrative_ontology:cs_axiom('1cad804b-95bb-415c-9703-8760670e7ea8', foundational, rational_critique_over_lineage_authority).
narrative_ontology:cs_axiom_status(rational_critique_over_lineage_authority, holdable).
narrative_ontology:cs_axiom_grounding('1cad804b-95bb-415c-9703-8760670e7ea8', rational_critique_over_lineage_authority, empirically_contingent).
narrative_ontology:cs_axiom('1cad804b-95bb-415c-9703-8760670e7ea8', secondary, caste_hierarchy_historical_not_essential).
narrative_ontology:cs_axiom_status(caste_hierarchy_historical_not_essential, holdable).
narrative_ontology:cs_axiom_grounding('1cad804b-95bb-415c-9703-8760670e7ea8', caste_hierarchy_historical_not_essential, empirically_contingent).
narrative_ontology:cs_reference_frame('1cad804b-95bb-415c-9703-8760670e7ea8', egalitarian_constitutional_supremacy).
narrative_ontology:cs_drift_state('1cad804b-95bb-415c-9703-8760670e7ea8', contemporary_post_constitutional_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1cad804b-95bb-415c-9703-8760670e7ea8', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_social_movements).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_secular_state).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, brahmin_reform_intellectuals).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahmin_institutions).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, traditional_ritual_monopolists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, conservative_hindu_laity).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_ritual_monopolists).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, conservative_hindu_laity).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_equality_supremacy).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, rational_textual_critique).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, historical_accretion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mobilize using the reformist-egalitarian reading to challenge caste-based ritual exclusion and claim equal spiritual standing within Hindu tradition and Indian constitutional framework. The reading provides intellectual and legal ground to contest hereditary monopoly on religious authority. Constraint persists: orthodox institutions still control much temple administration and ritual interpretation, and state enforcement of egalitarian access is incomplete and contested.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_social_movements, beneficiary,
    organized, generational, constrained, national).

% Lose exclusive interpretive authority and ritual monopoly as state law and reformist scholarship reframe what counts as 'authentic' textual meaning. The reading requires these institutions to justify hereditary hierarchy in constitutional and egalitarian terms rather than traditional authority. They retain control over some temple administration and liturgical practice, but increasingly must defend those practices against constitutional challenge and reinterpretation.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahmin_institutions, payer,
    institutional, generational, constrained, national).

% Enforces the reformist-egalitarian reading through constitutional law, civil rights legislation, educational curricula, and regulation of temple institutions. The state arbitrates between competing readings and privileges egalitarian interpretation through legal force and institutional authority. It collects no direct extraction but expands state authority over domains formerly controlled by religious institutions, producing a secondary benefit of state power consolidation.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_secular_state, agenda_setter,
    institutional, generational, analytical, national).

% Establish themselves as authoritative interpreters of Hindu tradition by reframing it as fundamentally egalitarian and rational. They occupy universities, publishing platforms, and advisory roles to government. Their authority derives from coupling this reading to modern education and state power, displacing hereditary interpretive monopoly. They benefit from credibility as custodians of 'real' Hindu tradition against both orthodox institutions and secular critics.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, brahmin_reform_intellectuals, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__reformist_egalitarian_reading, brahmin_reform_intellectuals, agenda_setter).

% Priests and ritual specialists whose livelihood and identity depend on hereditary authority and caste-segregated ritual access. The reading undermines their market for caste-based ritual practice and requires them to justify exclusion or abandon it. Their identity is fused with hereditary ritual authority; they experience this reading as existential threat even when material position endures.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_ritual_monopolists, payer,
    moderate, biographical, identity_locked, regional).

% Raised in traditions treating caste as divinely ordained; encounter state and reformist intellectual pressure to abandon exclusionary practices while often maintaining affective attachment to hereditary identity and practice. The reading dissolves received meaning without replacing it; they bear the cost of cognitive dissonance and social reorganization. They also benefit from expanded access to institutions previously closed by caste rules, if they are lower-caste.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, conservative_hindu_laity, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__reformist_egalitarian_reading, conservative_hindu_laity, beneficiary).

% Analyze and debate whether the reading represents genuine recovery of egalitarian scriptural basis or defensive reinterpretation that capitulates to tradition rather than rejecting it. Some support it as progress against hierarchy; others argue it perpetuates Hindu institutional dominance. Their analysis shapes academic discourse and policy formation without directly setting the constraint.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, secular_critics_of_hinduism, observer,
    powerful, biographical, analytical, national).

% Dalit thinkers and activists who argue the reformist reading capitulates to brahminical intellectual frameworks rather than rejecting the entire Hindu tradition as irredeemable. They would claim the constraint perpetuates Hindu institutional dominance and brahmin intellectual authority even while reforming caste logic. Their voice is largely excluded from mainstream reformist coalitions and official religious dialogue.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, excluded_dalit_critics, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__reformist_egalitarian_reading, brahmin_reform_intellectuals).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__reformist_egalitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for Hindus across caste boundaries to claim unified religious tradition while abandoning ritual segregation: reinterprets shared textual heritage to enable egalitarian participation without rejecting tradition entirely.
% TRANSFER_FUNCTION: Moves interpretive authority from hereditary brahmin institutions and ritual monopolists to the secular state and reformist intellectuals, reducing the material and symbolic rents that hereditary priests and orthodox institutions collect from caste-segregated ritual access and interpretive monopoly.
% ABSENT_VOICES: Dalit critics who reject the entire Hindu framework as irredeemable (rather than reformable) are largely excluded from mainstream reformist coalitions. Conservative Hindu laity whose identity is constituted through hereditary practice are represented only through their reactions and resistance, not through their own framework for understanding what is being lost. Dalit individuals without access to intellectual or activist platforms are largely voiceless.
% DISAPPEARANCE_RATIONALE: If this reading's constitutional enforcement disappeared overnight, orthodox institutions would reassert hereditary interpretive monopoly and ritual exclusion, state regulation of temples would cease, and the intellectual terrain would revert to unmediated competition between readings without egalitarian legal privilege. Dalit political mobilization would lose its primary institutional and intellectual foothold within Hindu tradition. Hindu-secular-state alliance would dissolve, returning religious authority largely to traditional institutional hands.
% FOUNDING_PROBLEM: Hindu tradition was institutionalized in ways that entrenched caste hierarchy as divinely ordained and textually prescribed. Dalit and lower-caste movements challenged both the hierarchy and its theological justification; reformers sought to recover egalitarian elements within tradition itself rather than abandon it entirely.
% FOUNDING_PROBLEM_CORROBORATION: Dalit social movements, constitutional authorities, and human-rights organizations attest the founding problem persists: caste-based discrimination in temple access, ritual segregation, and interpretive authority continues despite constitutional prohibition. Academic scholars document ongoing discrimination in institutional practice. Orthodox institutions contest whether discrimination is a problem or legitimate religious practice; some acknowledge the problem but argue the solution is incomplete enforcement of their own egalitarian teachings rather than reinterpretation.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__reformist_egalitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__reformist_egalitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__reformist_egalitarian_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).
:- end_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (Dalit movements, state apparatus, reformist intellectuals) and the payer seats (orthodox institutions, ritual monopolists) compute fundamentally different constraint types from the same structural data. From beneficiary positions, this is tangled-rope solving a genuine coordination problem (unified Hindu identity across caste lines) while redistributing authority. From payer positions, it is snare or forced extraction of their interpretive monopoly through state power. The reformist intellectual seat is internally unstable: it gains authority through reform, but that authority depends on continued state enforcement and Dalit political support. If either withdraws, the reading collapses. The engine's per-seat computation captures this perspectival gap; no single seat's experience describes the constraint completely.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows incipient mandatrophy (rising theater ratio 0.08→0.28) but not yet resolved. The founding problem (caste hierarchy institutionalized as divine) is live; the solution (reinterpretation plus state enforcement) addresses it but does not solve it. Discrimination persists despite constitutional prohibition and egalitarian rhetoric. Rising theater indicates institutions adopt egalitarian language while maintaining segregation in practice—a Goodhart-drift pattern characteristic of piton formation. However, active enforcement persists: constitutional litigation continues, Dalit movements remain mobilized, state regulation of temples is ongoing. Mandatrophy resolution would require either (a) Dalit movements and state enforcement to relax while institutions perform compliance theatrically (true piton), or (b) state enforcement to strengthen and behavioral change to become genuine (constraint resolves positively). Current trajectory is ambiguous—theater rising faster than extractiveness, which is a yellow-flag pattern for incipient piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_essentialism_vs_accretion,
    'Is caste hierarchy a core scriptural teaching (the hereditary-monopoly reading), a historical institutional interpretation (the reformist-egalitarian reading), or something else entirely—such that both readings misrepresent the textual actual content?',
    'Systematic historical-philological analysis of Vedic and Dharmaśāstra texts using methods outside both traditionalist and reformist frameworks; comparison with non-Hindu approaches to historical textual stratification.',
    'If caste is demonstrably central to original Vedic teaching, the reformist reading is imposed reinterpretation and loses its claim to authenticity. If caste is demonstrably a later accretion, the reading gains evidential support. If both are partial and the texts are genuinely ambiguous, the contest is epistemic rather than resolvable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_essentialism_vs_accretion, empirical, 'Whether caste hierarchy is scriptural essence or historical institutional accretion.').

omega_variable(
    identity_lock_persistence,
    'Is the high suppression in orthodox ritual monopolists and conservative Hindu laity primarily structural (loss of material monopoly rents, loss of institutional control) or primarily internalized (identity fused with hereditary authority such that suppression persists even if material conditions change)?',
    'Post-displacement observation of ritual monopolists and laity in contexts where the reformist reading is institutionally dominant: do suppression and resistance decline when material conditions stabilize, or does resistance persist as identity-defense? Longitudinal studies of individuals crossing caste and ritual boundaries.',
    'If suppression is primarily structural, dismantling the hereditary monopoly would resolve the constraint. If primarily internalized, the constraint persists even after institutional change—the reading would require additional cultural work (identity decoupling) beyond institutional reform. This affects whether the constraint is resolvable or must be replaced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether suppression in this constraint is structural or identity-internalized.').

omega_variable(
    reformist_brahmin_capture_risk,
    'Does the reformist-egalitarian reading''s reliance on brahmin intellectual authority and state apparatus constitute a new form of brahminical dominance (brahmin reformists replacing brahmin traditionalists as interpreters), or does it genuinely democratize interpretive authority?',
    'Analysis of who controls reformist intellectual institutions (universities, publications, official interpretation bodies); comparison of decision-making access for Dalit-led vs brahmin-led movements; long-term trajectories of institutional control.',
    'If reformist interpretation becomes a new brahmin monopoly (brahminical reinterpretation instead of brahminical tradition), the constraint is tangled-rope or snare using equality language as cover for new hierarchy. If Dalit movements actually capture interpretive authority, the constraint becomes genuine rope or resolves. If control remains contested, the constraint persists as tangled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_brahmin_capture_risk, empirical, 'Whether reformist interpretation constitutes brahmin capture under new framing or genuine democratization.').

omega_variable(
    theater_ratio_behavioral_meaning,
    'Does rising theater ratio (0.08→0.28 from 1947–2026) represent increasing performative compliance while discrimination persists (Goodhart drift toward piton), or increasing ease of egalitarian practice as institutions internalize the norm?',
    'Behavioral audit: measurement of actual access to temples, rituals, and authority by Dalit and lower-caste individuals; comparison of de jure egalitarian policies with de facto segregation; longitudinal follow-up of institutional practice changes.',
    'If performative: the constraint is moving toward mandatrophy (piton status) and will eventually collapse or be replaced. If behavioral change is genuine and theater reflects compliance costs rather than fraud: the constraint is working and theater ratio should decline as norms internalize. If mixed (some theatricality, some genuine change): the constraint is in a contested stabilization zone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_behavioral_meaning, empirical, 'Whether rising theater ratio indicates performative drift toward piton or adaptive compliance cost.').

omega_variable(
    kernel_reading_contention_frame,
    'Are the three readings (reformist-egalitarian, hereditary-monopoly, bhakti-devotional) structurally incompatible such that adopting one forecloses the others, or do they coexist as rival frameworks that different Hindu actors hold simultaneously?',
    'Ethnographic and institutional analysis of actual Hindu religious practice: do individuals and institutions compartmentalize readings (holding multiple readings simultaneously for different purposes), or do they genuinely commit to one at the exclusion of others? Analysis of institutional conflict trajectories.',
    'If coexistence is structural (readers compartmentalize), the readings are constraint siblings and both persist indefinitely. If foreclosure is real, the winner reading will eventually dominate and force consolidation. This affects the stability of the triplet and the long-term trajectory of institutional change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contention_frame, conceptual, 'Whether the three kernel readings coexist or foreclose each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__reformist_egalitarian_reading, 1947, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1947, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1947, 0.08).
narrative_ontology:measurement_basis(vedi_tr_t1947, observed).
narrative_ontology:measurement(vedi_tr_t1965, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement_basis(vedi_tr_t1965, observed).
narrative_ontology:measurement(vedi_tr_t1985, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement_basis(vedi_tr_t1985, observed).
narrative_ontology:measurement(vedi_tr_t2005, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 2005, 0.26).
narrative_ontology:measurement_basis(vedi_tr_t2005, observed).
narrative_ontology:measurement(vedi_tr_t2015, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement_basis(vedi_tr_t2015, observed).
narrative_ontology:measurement(vedi_tr_t2026, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(vedi_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1947, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1947, 0.28).
narrative_ontology:measurement_basis(vedi_be_t1947, observed).
narrative_ontology:measurement(vedi_be_t1965, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement_basis(vedi_be_t1965, observed).
narrative_ontology:measurement(vedi_be_t1985, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1985, 0.41).
narrative_ontology:measurement_basis(vedi_be_t1985, observed).
narrative_ontology:measurement(vedi_be_t2005, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 2005, 0.43).
narrative_ontology:measurement_basis(vedi_be_t2005, observed).
narrative_ontology:measurement(vedi_be_t2015, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 2015, 0.45).
narrative_ontology:measurement_basis(vedi_be_t2015, observed).
narrative_ontology:measurement(vedi_be_t2026, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 2026, 0.45).
narrative_ontology:measurement_basis(vedi_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1947, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1947, 0.35).
narrative_ontology:measurement_basis(vedi_su_t1947, observed).
narrative_ontology:measurement(vedi_su_t1965, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1965, 0.42).
narrative_ontology:measurement_basis(vedi_su_t1965, observed).
narrative_ontology:measurement(vedi_su_t1985, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement_basis(vedi_su_t1985, observed).
narrative_ontology:measurement(vedi_su_t2005, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 2005, 0.51).
narrative_ontology:measurement_basis(vedi_su_t2005, observed).
narrative_ontology:measurement(vedi_su_t2015, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement_basis(vedi_su_t2015, observed).
narrative_ontology:measurement(vedi_su_t2026, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 2026, 0.52).
narrative_ontology:measurement_basis(vedi_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__reformist_egalitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.12).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus__bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, indian_constitutional_equality_enforcement).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, brahmin_institutional_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel vedic_dharmic_corpus. The kernel is contested because three structurally distinct readings—hereditary-monopoly, bhakti-devotional, and reformist-egalitarian—instantiate from the same textual source (the Vedas and Dharmaśāstra) but produce different ε values, beneficiary structures, and institutional configurations. Each reading is a separate constraint story with its own ε. The hereditary-monopoly reading has higher ε (orthodox institutions benefit from exclusion); the bhakti reading has lower ε (individual devotion bypasses institution entirely); this reformist-egalitarian reading is intermediate (genuine egalitarian coordination benefit alongside reallocation of institutional authority). All three are linked via network.affects_constraints because institutional changes in one reading affect the viability of the others. The reformist reading is downstream of constitutional equality enforcement (the state apparatus constrains all Hindu institutional readings through law) and upstream of brahmin institutional authority (it restructures who counts as authoritative).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_dharmic_corpus__reformist_egalitarian_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
