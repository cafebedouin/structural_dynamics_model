% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__orthodox_literalist, []).

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
 *   constraint_id: dharmasastra_corpus__orthodox_literalist
 *   human_readable: Orthodox Literalist Dharmasastra Varna/Jati Hierarchy
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This constraint instantiates the orthodox literalist reading of the
 *   Dharmasastra kernel, which treats varna and jati hierarchy as eternal,
 *   revealed truth (sruti/smriti) requiring literal observance. The reading
 *   is a commitment system grounded in a fixed canonical text, interpreted by
 *   a Brahminical lineage that holds hermeneutic authority. Structurally, the
 *   constraint extracts labor, deference, and ritual exclusion from Dalits,
 *   Shudras, and women, concentrating benefit in upper-caste ritual and
 *   social privilege. It persists through active enforcement of
 *   untouchability, social boycott, and the suppression of alternative
 *   readings.
 *
 * KEY AGENTS:
 *   - Brahminical interpreters: agenda_setter (institutional/identity_locked) â adjudicate the eternal text and capture interpretive rent
 *   - Upper-caste beneficiaries: beneficiary (powerful/constrained) â receive ritual privilege and deference
 *   - Shudra excluded: payer (powerless/trapped) â perform service labor under ritual exclusion
 *   - Dalit victims: payer (powerless/trapped) â bear polluting labor and untouchability
 *   - Women gendered subjects: payer (powerless/identity_locked) â subjected to stridharma and exclusion from ritual agency
 *   - Reformist movements: excluded (organized/mobile) â argue alternative readings but are kept out of orthodox adjudication
 *   - Colonial/modern state: observer (institutional/analytical) â enacts law contradicting the orthodox prescription
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.82).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.88).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.82).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, snare).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Orthodox Literalist Dharmasastra Varna/Jati Hierarchy").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, '4d0679fc-7129-423f-a4c1-d8597d24b94d').
narrative_ontology:cs_kernel_codification('4d0679fc-7129-423f-a4c1-d8597d24b94d', fixed_text).
narrative_ontology:cs_authority_grounding('4d0679fc-7129-423f-a4c1-d8597d24b94d', lineage).
narrative_ontology:cs_interpretation_layer_present('4d0679fc-7129-423f-a4c1-d8597d24b94d').
narrative_ontology:cs_reading_relation('4d0679fc-7129-423f-a4c1-d8597d24b94d', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_reading_relation('4d0679fc-7129-423f-a4c1-d8597d24b94d', dharmasastra_corpus__abolitionist_rejection, forecloses).
narrative_ontology:cs_axiom('4d0679fc-7129-423f-a4c1-d8597d24b94d', foundational, sruti_smriti_apaurusheya).
narrative_ontology:cs_axiom_status(sruti_smriti_apaurusheya, holdable).
narrative_ontology:cs_axiom_grounding('4d0679fc-7129-423f-a4c1-d8597d24b94d', sruti_smriti_apaurusheya, theological).
narrative_ontology:cs_axiom('4d0679fc-7129-423f-a4c1-d8597d24b94d', foundational, varna_dharma_literal_binding).
narrative_ontology:cs_axiom_status(varna_dharma_literal_binding, holdable).
narrative_ontology:cs_axiom_grounding('4d0679fc-7129-423f-a4c1-d8597d24b94d', varna_dharma_literal_binding, deontological).
narrative_ontology:cs_reference_frame('4d0679fc-7129-423f-a4c1-d8597d24b94d', varnasrama_dharma_ideal).
narrative_ontology:cs_drift_state('4d0679fc-7129-423f-a4c1-d8597d24b94d', contemporary_postcolonial_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('4d0679fc-7129-423f-a4c1-d8597d24b94d', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahminical_interpreters).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, upper_caste_beneficiaries).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudra_excluded).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalit_victims).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, women_gendered_subjects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the hermeneutic monopoly on Dharmasastra texts, adjudicate varna and jati status, perform rituals reserved for the twice-born, and transmit the reading that these prescriptions are eternal, authorless revelation. Their social authority, patronage networks, and ritual livelihood depend on the literal observance of hierarchy.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahminical_interpreters, agenda_setter,
    institutional, civilizational, identity_locked, continental).

% Receive ritual privilege, access to Vedic education, favorable marriage networks, and deference from lower castes. They enforce boundaries through purity rules and social boycott, receiving the fruits of Shudra labor and Dalit service without equivalent return, justified as the natural order of svadharma.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, upper_caste_beneficiaries, beneficiary,
    powerful, generational, constrained, regional).

% Perform agricultural and service labor for upper-caste households, excluded from Vedic study and initiation rituals, and expected to serve the higher varnas without entitlement to reciprocal teaching, ritual recognition, or status mobility.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, shudra_excluded, payer,
    powerless, generational, trapped, regional).

% Relegated to tasks deemed polluting, denied entry to temples and schools, subjected to untouchability practices, and compelled to provide labor under threat of social and physical violence, with the Dharmasastra cited as eternal justification for their exclusion.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalit_victims, payer,
    powerless, generational, trapped, local).

% Excluded from independent Vedic ritual agency and full property rights under stridharma, directed toward domestic service and reproduction; their ritual and legal status is derivative of male kin, and their bodies are treated as sites of lineage purity to be guarded.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, women_gendered_subjects, payer,
    powerless, biographical, identity_locked, regional).

% Argue for contextual or abolitionist readings of Dharmasastra and oppose caste hierarchy, but are treated as heretical or outside the fold by orthodox interpreters; their voices are absent from orthodox textual adjudication and ritual authority.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, reformist_movements, excluded,
    organized, generational, mobile, national).

% Has enacted legal codes outlawing untouchability and caste discrimination, operating as an external institutional force whose statutes contradict the orthodox literalist prescription; it collects census data and enacts reform without entering the theological framework.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, colonial_modern_state, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__orthodox_literalist, upper_caste_beneficiaries).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__orthodox_literalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates ritual, economic, and social roles across a hierarchical society, ostensibly reducing conflict over status and resource access by fixing each person's duties (svadharma) according to birth in a cosmic order.
% TRANSFER_FUNCTION: Moves labor, deference, ritual service, and educational or ritual exclusion from Shudras, Dalits, and women to Brahminical interpreters and upper castes, justified as the eternal order of varna.
% ABSENT_VOICES: Reformist contextual readers, abolitionist critics, Dalit theologians, and women Vedic scholars are excluded from the interpretive framework; their objections are ruled out by the premise that the text is eternal revealed truth beyond human revision.
% DISAPPEARANCE_RATIONALE: If the literal observance of varna/jati hierarchy vanished, caste-based access to ritual, education, and occupation would collapse, and the Brahminical authority deriving from textual monopoly would lose its primary grounding â the social order would reorganize around different principles of legitimacy.
% FOUNDING_PROBLEM: Social coordination and cosmic order (rita/dharma) in a stratified society; preventing ritual pollution and maintaining the authority of revealed scripture as the basis for law and social station.
% FOUNDING_PROBLEM_CORROBORATION: Modern state institutions and Dalit scholars outside the Brahminical beneficiary framework attest the founding problem was constructed to legitimate extraction and is now superseded by constitutional equality; corroboration from within the benefiting parties alone asserts it remains live.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__orthodox_literalist, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__orthodox_literalist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dharmasastra_corpus__orthodox_literalist, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__orthodox_literalist, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__orthodox_literalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__orthodox_literalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the hierarchy systematically transfers labor, status, and ritual access from lower to upper strata with no reciprocal flow. Suppression is higher (0.88) because the constraint depends on actively excluding rival readings (reformist, abolitionist) and enforcing purity boundaries; alternatives exist but are structurally blocked. Theater ratio (0.55) reflects that a substantial share of activity is performative maintenance of ritual purity and textual recitation that legitimizes the extraction rather than functional coordination. Accessibility collapse (0.75) is high because the divine revelation frame makes equality-based alternatives cognitively inaccessible within the orthodox worldview. Resistance (0.70) reflects persistent subaltern movements (Bhakti, Buddhism, modern Dalit politics) that the suppression machinery must continuously counter.
 *
 * PERSPECTIVAL GAP:
 *   The Brahminical and upper-caste seats experience the constraint as the preservation of cosmic order and hereditary duty; the Dalit, Shudra, and women seats experience it as enforced extraction with no exit. The engine computes this divergence from the structural data â beneficiary concentration versus victimization â without the authored claim reconciling it.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahminical interpreters and upper-caste beneficiaries are structural beneficiaries (low d, subsidized by the constraint's operation). Dalits, Shudras, and women are structural targets (high d, amplified extraction). Reformist movements are excluded from the constraint's discourse entirely. The modern state sits outside the theological framework as an analytical observer with a divergent classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint claims to solve the problem of cosmic and social order, but its persistence depends on coercion, ritual pollution ideology, and the suppression of egalitarian alternatives rather than on addressing a live coordination problem that lacks other solutions. The founding problem of social order is contested as a cover story: legal and democratic mechanisms now provide alternative coordination, yet the hierarchy persists through inertia and active defense of privilege.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_historicity_ambiguity,
    'Are the Dharmasastra texts authorless eternal revelation (apaurusheya) or historically evolved legal and social documents?',
    'Textual and historical philology tracing interpolation layers, manuscript variation, and sociological correspondence between legal prescriptions and historical power structures.',
    'If historically evolved, the orthodox literalist claim to eternality collapses and the constraint reclassifies as constructed extraction rather than divine coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_historicity_ambiguity, empirical, 'Empirical ambiguity over the textual origin and historical development of Dharmasastra').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is caste observance maintained primarily by structural enforcement (economic boycott, physical violence, state leniency) or by internalized identity fusion?',
    'Post-exit trajectory studies: if caste identity and deference persist after structural barriers are legally removed, suppression is partially internalized.',
    'If internalized, effective extraction exceeds the structural measure because the target carries the constraint after exit; this amplifies the snare classification for affected seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in caste hierarchy').

omega_variable(
    kernel_reading_boundary,
    'Does the structural disagreement between the orthodox literalist and reformist contextual readings lie in the kernel''s codification (fixed eternal text vs. historically conditioned text) or in the authority grounding (theological lineage vs. ethical practice)?',
    'Comparative analysis of which axioms must be denied to move from one reading to the other; tracing whether reformist readings reinterpret the same kernel or silently replace it.',
    'If the disagreement is at the codification level, the kernel itself may need decomposition into multiple constraints; if at authority grounding, the readings are competing interpreters of a shared kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Committer ambiguity: location of the structural split between orthodox literalist and reformist readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__orthodox_literalist, theater_ratio, 0, 0.3).
narrative_ontology:measurement(dhar_tr_t500, dharmasastra_corpus__orthodox_literalist, theater_ratio, 500, 0.35).
narrative_ontology:measurement(dhar_tr_t1000, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1000, 0.4).
narrative_ontology:measurement(dhar_tr_t1500, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1500, 0.5).
narrative_ontology:measurement(dhar_tr_t1800, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1800, 0.6).
narrative_ontology:measurement(dhar_tr_t2000, dharmasastra_corpus__orthodox_literalist, theater_ratio, 2000, 0.55).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(dhar_be_t500, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 500, 0.78).
narrative_ontology:measurement(dhar_be_t1000, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1000, 0.8).
narrative_ontology:measurement(dhar_be_t1500, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1500, 0.82).
narrative_ontology:measurement(dhar_be_t1800, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1800, 0.85).
narrative_ontology:measurement(dhar_be_t2000, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 2000, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(dhar_su_t500, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 500, 0.65).
narrative_ontology:measurement(dhar_su_t1000, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1000, 0.7).
narrative_ontology:measurement(dhar_su_t1500, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1500, 0.75).
narrative_ontology:measurement(dhar_su_t1800, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1800, 0.85).
narrative_ontology:measurement(dhar_su_t2000, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 2000, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__orthodox_literalist, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the dharmasastra_corpus kernel, which decomposes into structurally distinct claims: orthodox literalist (eternal hierarchy, high extraction), reformist contextual (historically conditioned, separable ethical core), and abolitionist rejection (illegitimate oppression). Each reading has a distinct epsilon, beneficiary/victim structure, and classification. The orthodox reading is historically upstream and structurally shapes the field in which the sibling readings operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
