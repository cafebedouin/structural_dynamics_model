% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__hereditary_monopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__hereditary_monopoly_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: vedic_dharmic_corpus__hereditary_monopoly_reading
 *   human_readable: Hereditary Brahmin Ritual and Interpretive Monopoly
 *   domain: religious/social/authority
 *
 * SUMMARY:
 *   This constraint instantiates the hereditary_monopoly_reading of the
 *   vedic_dharmic_corpus kernel: ritual and interpretive authority derive
 *   exclusively from birth into Brahmin lineage; varna hierarchy is claimed
 *   as divinely ordained and textually prescribed in the Vedas and
 *   Dharmashastras. The arrangement operates through temple control, ritual
 *   economy (dakshina, endowments), purity-pollution enforcement, and state
 *   recognition of 'orthodox' priesthood. It extracts ritual fees, labor,
 *   land, and status from lower castes and women while coordinating Vedic
 *   textual transmission. The claimed type is snare — high extraction, active
 *   suppression of alternatives (bhakti, reformist), identifiable victims.
 *   The engine will compute per-seat classifications from the structural
 *   data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.65).
domain_priors:suppression_score(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.78).
domain_priors:theater_ratio(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__hereditary_monopoly_reading, snare).
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Hereditary Brahmin Ritual and Interpretive Monopoly").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "religious/social/authority").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, 'af43f8d5-7023-4b15-a10d-eb677c00ede3').
narrative_ontology:cs_kernel_codification('af43f8d5-7023-4b15-a10d-eb677c00ede3', fixed_text).
narrative_ontology:cs_authority_grounding('af43f8d5-7023-4b15-a10d-eb677c00ede3', lineage).
narrative_ontology:cs_interpretation_layer_present('af43f8d5-7023-4b15-a10d-eb677c00ede3').
narrative_ontology:cs_reading_relation('af43f8d5-7023-4b15-a10d-eb677c00ede3', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_reading_relation('af43f8d5-7023-4b15-a10d-eb677c00ede3', vedic_dharmic_corpus__reformist_egalitarian_reading, coexists_with).
narrative_ontology:cs_axiom('af43f8d5-7023-4b15-a10d-eb677c00ede3', foundational, varna_hierarchy_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_hierarchy_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('af43f8d5-7023-4b15-a10d-eb677c00ede3', varna_hierarchy_divinely_ordained, theological).
narrative_ontology:cs_axiom('af43f8d5-7023-4b15-a10d-eb677c00ede3', foundational, brahmin_birth_necessary_for_ritual_authority).
narrative_ontology:cs_axiom_status(brahmin_birth_necessary_for_ritual_authority, holdable).
narrative_ontology:cs_axiom_grounding('af43f8d5-7023-4b15-a10d-eb677c00ede3', brahmin_birth_necessary_for_ritual_authority, theological).
narrative_ontology:cs_axiom('af43f8d5-7023-4b15-a10d-eb677c00ede3', secondary, textual_prescription_binds_contemporary_practice).
narrative_ontology:cs_axiom_status(textual_prescription_binds_contemporary_practice, holdable).
narrative_ontology:cs_axiom_grounding('af43f8d5-7023-4b15-a10d-eb677c00ede3', textual_prescription_binds_contemporary_practice, conventional).
narrative_ontology:cs_reference_frame('af43f8d5-7023-4b15-a10d-eb677c00ede3', varna_dharma_orthodoxy).
narrative_ontology:cs_drift_state('af43f8d5-7023-4b15-a10d-eb677c00ede3', post_colonial_constitutional_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('af43f8d5-7023-4b15-a10d-eb677c00ede3', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_caste_groups).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, women).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, divine_ordination_of_varna).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, textual_prescription_of_hierarchy).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_exclusive_ritual_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds hereditary monopoly over Vedic ritual performance, textual interpretation, and temple administration. Collects dakshina (ritual fees), land endowments, and social deference. Controls entry to priestly education and ritual office. Exit means voluntary renunciation of hereditary privilege — structurally available but identity-costly.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class, agenda_setter,
    institutional, generational, arbitrage, continental).

% Barred from Vedic study, ritual performance, and temple priesthood by birth. Bear economic extraction through mandatory ritual fees and labor obligations. Subject to purity-pollution enforcement that regulates spatial access, water rights, and social mobility. Exit requires religious conversion or geographic displacement — both carry severe community severance costs.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_caste_groups, payer,
    powerless, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_caste_groups, excluded).

% Excluded from Vedic initiation (upanayana), textual recitation, and ritual office regardless of caste. Even Brahmin women cannot perform priestly functions or receive full textual education. Bear extraction through patriarchal ritual economy (dowry, stridhana restrictions, widowhood rites). Exit via bhakti movements or reformist spaces exists but remains socially penalized.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, women, payer,
    powerless, biographical, trapped, continental).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, women, excluded).

% Assert direct devotional access bypassing caste and priestly mediation. Historically drew from lower castes and women. Their temples and mathas operate parallel ritual economies but lack state recognition for Vedic rites. Constrained by Brahmin control of 'orthodox' legitimacy and temple endowment law.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, bhakti_practitioners, excluded,
    moderate, biographical, constrained, continental).

% Advocate textual reinterpretation aligning varna with constitutional equality. Operate through legal challenges (temple entry cases), educational institutions, and political mobilization. Face counter-mobilization from hereditary institutions and state hesitation to intervene in 'religious matters'. Mobile exit to secular/legal frameworks available but politically contested.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, reformist_activists, excluded,
    organized, biographical, mobile, national).

% Study the textual, historical, and sociological dimensions of the constraint from outside the devotional/commitment frame. Provide evidence for reformist arguments (textual critique, historical contingency) and traditional defenses (philological continuity). Their authority is epistemic, not ritual.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, academic_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains ritual order and textual transmission fidelity through a hereditary specialist class; solves the coordination problem of who may perform Vedic rites and adjudicate dharmic interpretation across generations.
% TRANSFER_FUNCTION: Moves ritual authority, economic resources (dakshina, temple endowments, land grants), and status capital from lower castes and women to the Brahmin priestly class; moves spiritual legitimacy from direct devotional access to priestly mediation.
% ABSENT_VOICES: Lower castes (especially Dalits) and women are structurally excluded from the interpretive conversation; their objection is encoded in bhakti and reformist movements but those voices are not seated in the hereditary authority structure. Bhakti saints (e.g., Ravidas, Kabir) and reformist leaders (e.g., Phule, Ambedkar) articulate the absent critique from outside the constraint.
% DISAPPEARANCE_RATIONALE: If the hereditary monopoly vanished overnight, temple endowment law, priestly succession, ritual fee structures, and the legal definition of 'orthodox' Hinduism would all require restructuring. The ritual economy, caste-based occupational reservations in temple service, and the interpretive authority of Brahmin mathas would collapse or transform.
% FOUNDING_PROBLEM: Preserving Vedic textual integrity and ritual efficacy across generations in an oral tradition context; preventing ritual corruption by unqualified performers; maintaining a dedicated class for textual memorization and ritual precision.
% FOUNDING_PROBLEM_CORROBORATION: Traditional authorities (Shankara mathas, Vedic pathashalas) attest the problem remains live — oral transmission still requires hereditary specialization. Reformist scholars (Ambedkar, contemporary Dalit historians) and Indologists (Pollock, Olivelle) attest the founding problem is substantially solved by print/textual standardization and constitutional equality — the arrangement persists as caste privilege. No neutral arbiter exists; corroboration splits along the kernel's reading lines.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__hereditary_monopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__hereditary_monopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__hereditary_monopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the decoupling of ritual fees and temple revenues from marginal service costs — the monopoly rent of hereditary exclusivity. Suppression (0.78) is high because the constraint actively excludes rival claimants (bhakti saints, reformist interpreters, women priests) through legal, social, and ritual barriers. Theater ratio (0.42) captures the growing gap between the coordination function (textual preservation) and the extractive function (caste rent) — print and institutionalization have reduced the coordination necessity of hereditary monopoly. Accessibility collapse (0.82) is near-mountain level because the purity-pollution framework structurally closes alternatives: conversion, legal challenge, or bhakti are the only exits, all costly. Resistance (0.55) is moderate — bhakti and reformist movements persist but have not displaced the hereditary structure.
 *
 * PERSPECTIVAL GAP:
 *   From the Brahmin seat, the arrangement is genuine coordination: they preserve a fragile oral tradition, maintain ritual precision, and administer temple complexes. From lower caste and women's seats, the same structure is enforced extraction: they pay for rituals they cannot perform, serve a hierarchy that deems them impure, and face violence for transgressing boundaries. The engine computes this divergence — the authored claim (snare) reflects the structural asymmetry, not the beneficiary's self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin priestly class: structural beneficiary (collects rents, controls rules, arbitrage exit) → d near 0.0. Lower caste groups: full targets (pay extraction, trapped exit, identity-locked via caste) → d near 1.0. Women: full targets (excluded from authority, bear patriarchal ritual costs, trapped) → d near 1.0. Bhakti practitioners: excluded but mobile → d ~0.6 (constrained exit). Reformist activists: excluded but organized/mobile → d ~0.4. Academic scholars: analytical → d = 0.5 (symmetric observer). The engine derives these from beneficiary/victim declarations + exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (oral textual preservation) is substantially solved by print, recording, and institutional archives — yet the hereditary monopoly persists and has expanded into temple endowment control and legal definition of orthodoxy. This is mandatrophy: the mandate (preservation) has atrophied but the constraint (hereditary exclusivity) has hypertrophied into rent extraction. The classification prevents mislabeling this as pure coordination (rope) because the victim structure and active suppression of alternatives are structural, not incidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested vedic_dharmic_corpus kernel, or a standalone constraint?',
    'Commitment-system analysis: if the authority structure grounds legitimacy in a stabilized kernel (Vedic corpus) with sibling readings, it is a kernel reading. The declared reading_relations and axioms confirm this.',
    'If kernel reading, classification must be ε-invariant for this reading only; sibling readings generate separate constraint stories. Cross-reading contamination is prevented by ε-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system framing of this constraint as a kernel reading').

omega_variable(
    sibling_reading_deltas,
    'What structural elements would change if the bhakti or reformist reading were instantiated instead?',
    'Comparative constraint authoring: author the sibling readings as separate stories and compare ε, beneficiary/victim sets, enforcement mechanisms.',
    'Bhakti reading: ε drops (~0.2), beneficiaries shift to devotees, victims disappear, enforcement becomes voluntary. Reformist reading: ε drops (~0.15), beneficiaries become all citizens, victims disappear, enforcement becomes constitutional. This reading''s high ε is reading-dependent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_deltas, conceptual, 'Structural delta between this reading and its siblings').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.78) primarily structural (legal barriers, temple control, violence) or internalized (caste identity, purity internalization, devotional acceptance)?',
    'Post-exit suppression trajectory: track individuals who exit via conversion, migration, or bhakti — if suppression persists (stigma, family severance, internalized unworthiness), reclassify as partially internalized.',
    'If internalized component is substantial, effective suppression exceeds structural measure — targets carry the constraint after formal exit. This raises χ for identity-locked seats beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in caste constraint').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Does the constraint''s self-presentation as divinely ordained natural law (Mountain claim) reflect genuine natural-law structure or a constructed extraction arrangement?',
    'False Summit Mountain test: if beneficiaries exist (brahmin_priestly_class) and ε > 0.3 with active enforcement, FSM signature triggers reclassification. Historical evidence of textual contestation (bhakti, Buddhist, reformist) supports constructed reading.',
    'If constructed, the Mountain self-presentation is a false summit — the constraint is a snare/tangled_rope masquerading as natural law. FSM override would reclassify to tangled_rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'False summit detection for divine ordination claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedic_hereditary_tr_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(vedic_hereditary_tr_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(vedic_hereditary_tr_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(vedic_hereditary_tr_t60, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(vedic_hereditary_tr_t80, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(vedic_hereditary_tr_t100, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(vedic_hereditary_be_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(vedic_hereditary_be_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(vedic_hereditary_be_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(vedic_hereditary_be_t60, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(vedic_hereditary_be_t80, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 80, 0.64).
narrative_ontology:measurement(vedic_hereditary_be_t100, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vedic_hereditary_su_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(vedic_hereditary_su_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(vedic_hereditary_su_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(vedic_hereditary_su_t60, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(vedic_hereditary_su_t80, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 80, 0.77).
narrative_ontology:measurement(vedic_hereditary_su_t100, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.08).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial 'Vedic authority' into three structurally distinct readings with divergent ε values. The hereditary_monopoly_reading (ε=0.65) extracts via hereditary monopoly; bhakti_devotional_reading (ε~0.2) coordinates via voluntary devotion; reformist_egalitarian_reading (ε~0.15) coordinates via constitutional equality. They share the kernel (Vedic corpus) but instantiate different constraints. Linkage via affects_constraints enables contamination analysis: if hereditary monopoly's purity degrades, bhakti and reformist readings gain legitimacy-space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_dharmic_corpus__hereditary_monopoly_reading, institutional, 0.1).
constraint_indexing:directionality_override(vedic_dharmic_corpus__hereditary_monopoly_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
