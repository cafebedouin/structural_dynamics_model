% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__abolitionist_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__abolitionist_rejection, []).

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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Dharmasastra Caste-Textual Order (Abolitionist Reading)
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   The abolitionist reading of the Dharmasastra corpus treats the entire
 *   textual-legal-hierarchical complex as a snare: a pure extraction
 *   mechanism that uses claimed divine authority to naturalize caste
 *   oppression. The reading does not seek reinterpretation or reform — it
 *   identifies the corpus itself as the source of legitimacy for a hierarchy
 *   that extracts labor, dignity, autonomy, and life-chances from Dalits,
 *   Shudras, women, and tribal communities. The constraint story models the
 *   Dharmasastra system as the abolitionist reading analyzes it: high base
 *   extractiveness (0.87), near-total suppression (0.91), low theater (0.12)
 *   because the oppression is materially real, not performative. The
 *   stakeholder surface maps the brahminical authorities and upper-caste
 *   elites as agenda-setters and beneficiaries of the system, while Dalits,
 *   Shudras, women, and tribal communities are payers trapped by
 *   identity-locked or constrained exit. The abolitionist activists appear as
 *   a second agenda-setter seat — they coordinate the rejection — while
 *   orthodox literalists and reformist contextualists are excluded voices
 *   whose premises the reading forecloses or bypasses.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, 0.87).
domain_priors:suppression_score(dharmasastra_corpus__abolitionist_rejection, 0.91).
domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, extractiveness, 0.87).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Dharmasastra Caste-Textual Order (Abolitionist Reading)").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__abolitionist_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, '9a230b2a-52fd-4f5c-88ec-b6761e3645b8').
narrative_ontology:cs_kernel_codification('9a230b2a-52fd-4f5c-88ec-b6761e3645b8', fixed_text).
narrative_ontology:cs_authority_grounding('9a230b2a-52fd-4f5c-88ec-b6761e3645b8', extraction).
narrative_ontology:cs_interpretation_layer_present('9a230b2a-52fd-4f5c-88ec-b6761e3645b8').
narrative_ontology:cs_reading_relation('9a230b2a-52fd-4f5c-88ec-b6761e3645b8', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('9a230b2a-52fd-4f5c-88ec-b6761e3645b8', dharmasastra_corpus__reformist_contextual, coexists_with).
narrative_ontology:cs_axiom('9a230b2a-52fd-4f5c-88ec-b6761e3645b8', foundational, dharmasastra_categorically_illegitimate).
narrative_ontology:cs_axiom_status(dharmasastra_categorically_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('9a230b2a-52fd-4f5c-88ec-b6761e3645b8', dharmasastra_categorically_illegitimate, deontological).
narrative_ontology:cs_axiom('9a230b2a-52fd-4f5c-88ec-b6761e3645b8', foundational, caste_hierarchy_intrinsically_oppressive).
narrative_ontology:cs_axiom_status(caste_hierarchy_intrinsically_oppressive, holdable).
narrative_ontology:cs_axiom_grounding('9a230b2a-52fd-4f5c-88ec-b6761e3645b8', caste_hierarchy_intrinsically_oppressive, deontological).
narrative_ontology:cs_reference_frame('9a230b2a-52fd-4f5c-88ec-b6761e3645b8', brahminical_textual_authority).
narrative_ontology:cs_drift_state('9a230b2a-52fd-4f5c-88ec-b6761e3645b8', contemporary_ambedkarite_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('9a230b2a-52fd-4f5c-88ec-b6761e3645b8', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, brahminical_authorities).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, upper_caste_elites).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, traditional_institutions).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, dalits).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, shudras).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, women_oppressed_by_caste).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, ati_shudras).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, tribal_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control textual interpretation, ritual authority, and pedagogical transmission of the Dharmasastra corpus. Their institutional continuity depends on the texts' unrevisable authority. Exit would mean surrendering the epistemic and social capital accumulated over millennia.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, brahminical_authorities, agenda_setter,
    institutional, generational, identity_locked, continental).

% Hold disproportionate land, capital, educational access, and political representation justified by varna ideology. The textual framework legitimates their privilege as cosmic order. Exit from the belief system threatens material advantage and social standing.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, upper_caste_elites, beneficiary,
    powerful, biographical, constrained, continental).

% Mathas, temples, gurukulas, and caste associations derive funding, recruitment, and legal recognition from the Dharmasastra framework. Their organizational survival is fused with the texts' authority.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, traditional_institutions, beneficiary,
    organized, generational, identity_locked, continental).

% Subjected to untouchability, manual scavenging, landlessness, and ritual exclusion prescribed by Dharmasastra injunctions. Structural exit is blocked by social enforcement, economic dependency, and internalized stigma. Resistance manifests in conversion movements, Ambedkarite politics, and assertion of constitutional rights.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dalits, payer,
    powerless, biographical, trapped, continental).

% Denied Vedic education, priestly functions, and ritual purity; confined to service roles. Some upward mobility through sanskritization, but the textual ceiling remains. Exit requires rejecting the framework that defines their subordination.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, shudras, payer,
    moderate, biographical, constrained, continental).

% Subject to compounded oppression: caste endogamy controls reproduction, widowhood rituals, property exclusion, and purity-pollution norms fall hardest on women. The texts authorize patriarchal control across caste lines. Exit is nearly impossible within the framework.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, women_oppressed_by_caste, payer,
    powerless, biographical, trapped, continental).

% Groups placed below the four-varna structure entirely — 'unseeable,' 'unapproachable.' Face the most extreme ritual and material exclusion. The textual framework renders their oppression natural and permanent.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, ati_shudras, payer,
    powerless, biographical, trapped, continental).

% Incorporated into the caste order as 'low' or 'outside' through Sanskritization and state classification. Lose land, forest rights, and autonomous governance when absorbed into the Dharmasastra taxonomy.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, tribal_communities, payer,
    powerless, biographical, constrained, continental).

% Assert the Dharmasastra as eternal, authorless revelation requiring literal observance. They are excluded from the abolitionist conversation because their premise (textual infallibility) is the very target of abolitionist critique. Their voice would defend the hierarchy the reading seeks to dismantle.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, orthodox_literalists, excluded,
    organized, generational, identity_locked, continental).

% Argue for ethical core (dharma as righteous conduct) separable from caste prescriptions. Excluded from the abolitionist frame because they retain textual authority and seek reform within the tradition, not abandonment. Their presence would complicate the 'wholly abandoned' claim.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, reformist_contextualists, excluded,
    moderate, biographical, constrained, continental).

% Study the texts philologically, historically, and anthropologically. Neither collect rents nor pay costs from the living hierarchy. Provide evidence for textual stratification, historical contingency, and the constructedness of varna — data the abolitionist reading mobilizes.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, academic_scholars, observer,
    moderate, biographical, analytical, global).

% Articulate and organize the rejection of Dharmasastra authority. Draw on Ambedkarite thought, constitutional law, and transnational human rights frameworks. Their coordination function is dismantling the textual-hierarchical complex. They bear reputational and physical risk but retain exit options through constitutional and international discourse.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, abolitionist_activists, agenda_setter,
    organized, generational, mobile, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the total rejection of caste hierarchy and its textual justification, enabling formerly oppressed groups to claim dignity, legal equality, and epistemic authority without negotiating within the Dharmasastra framework.
% TRANSFER_FUNCTION: Moves interpretive authority, moral legitimacy, and resource control from brahminical institutions and upper-caste elites to Dalit-Bahujan movements, constitutional institutions, and secular democratic discourse.
% ABSENT_VOICES: Orthodox literalist authorities (who would assert textual eternality) and reformist contextualists (who would argue for salvaging an ethical core) are structurally absent from the abolitionist frame. The former are excluded because their premise is the target; the latter because they preserve the authority the reading rejects.
% DISAPPEARANCE_RATIONALE: If the abolitionist reading vanished overnight, the Dharmasastra framework would retain its grip on personal law, temple administration, educational curricula, and social consciousness. Caste atrocities would continue with scriptural sanction. Constitutional protections would lack the interpretive counter-tradition that makes them enforceable against religious custom.
% FOUNDING_PROBLEM: The problem of caste oppression — graded inequality, untouchability, patriarchal control, and epistemic closure — justified and stabilized by a textual corpus claiming divine authority and immutability.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by Dalit-Bahujan movements (Ambedkarite organizations, Dalit Panthers, Bhim Army), constitutional jurisprudence (Art. 17, SC/ST Act), UN human rights bodies (CERD General Recommendation 29), and independent historians of caste (e.g., Gail Omvedt, Anupama Rao) — sources outside the abolitionist activist core.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__abolitionist_rejection, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__abolitionist_rejection, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__abolitionist_rejection, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dharmasastra_corpus__abolitionist_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__abolitionist_rejection, 0.87, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__abolitionist_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__abolitionist_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is scored at 0.87 because the Dharmasastra system, read abolitionistically, extracts not merely surplus labor but the capacity for self-definition, bodily integrity, and epistemic participation. Suppression at 0.91 reflects the convergence of religious sanction, social enforcement, legal disability (pre-1947), and internalized stigma that makes exit nearly impossible for the most oppressed. Theater ratio is low (0.12) because the system's rituals, texts, and institutions are not primarily performative covers — they are the active machinery of hierarchy. Accessibility collapse at 0.82 captures how completely the varna-jati framework closes off alternative social imaginaries once internalized. Resistance at 0.68 registers the sustained, organized opposition from Bhakti movements through Ambedkarite politics to contemporary Dalit assertion — resistance that the system must actively suppress to persist.
 *
 * PERSPECTIVAL GAP:
 *   The brahminical agenda-setter seat experiences the Dharmasastra as a mountain (cosmic order, natural law). The Dalit payer seat experiences it as a snare (total extraction, no exit). The abolitionist activist seat experiences it as a snare to be dismantled. The engine computes this divergence from the structural data: identity_locked exit for authorities vs. trapped exit for the oppressed produces opposite directionalities, yielding opposite χ values. The reformist contextualist seat, if included, would compute a tangled_rope (coordination function: ethical continuity; extraction function: caste preservation).
 *
 * DIRECTIONALITY LOGIC:
 *   Brahminical authorities and traditional institutions are identity-locked agenda-setters: their institutional self-concept is fused with textual authority (d ≈ 0.05, full beneficiary). Upper-caste elites are constrained beneficiaries (d ≈ 0.15) — they could exit ideologically but pay high material and social costs. Dalits, ati-shudras, and caste-oppressed women are trapped payers (d ≈ 0.95) — exit requires dismantling the system itself. Shudras and tribal communities are constrained payers (d ≈ 0.8) — some mobility exists but within the hierarchy. Orthodox literalists are identity-locked excluded (their premise is the target). Reformist contextualists are constrained excluded (they share the textual frame but contest its application). Academic scholars are analytical observers (d ≈ 0.5). Abolitionist activists are mobile agenda-setters of the rejection (d ≈ 0.3 — they bear risk but gain epistemic authority).
 *
 * MANDATROPHY ANALYSIS:
 *   The Dharmasastra system exhibits clear mandatrophy: its founding problem (social order in a ritual-polity) is dead — modern constitutional democracy, industrial economy, and human rights frameworks have superseded the coordination needs the texts once served. Yet the system persists because the authorities who benefit from it (brahminical institutions, upper-caste elites) control its interpretation and enforcement. The abolitionist reading names this mandatrophy explicitly: the mandate is exhausted, the authority is illegitimate, the structure must be abandoned. This prevents mislabeling the system as a rope (genuine coordination) or tangled_rope (mixed) — the coordination function is historically exhausted, leaving only extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading,
    'Is this constraint story one reading of the dharmasastra_corpus kernel, and does the kernel have other live readings?',
    'Committer-frame verification: the SCOPE manifest identifies kernel_id=dharmasastra_corpus with readings abolitionist_rejection, orthodox_literalist, reformist_contextual. This story is the abolitionist_rejection reading.',
    'Confirms the ε-invariance discipline: this story models the Dharmasastra system from one reading''s lights only. Other readings generate separate constraint stories with different ε, beneficiaries, victims, and claimed_type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading, conceptual, 'Committer-frame identity: this story = abolitionist_rejection reading of dharmasastra_corpus kernel').

omega_variable(
    sibling_reading_structural_delta,
    'How would the orthodox_literalist and reformist_contextualist readings change the beneficiary/victim structure and ε?',
    'Author the sibling constraint stories and compare: orthodox_literalist would claim mountain/rope with beneficiaries = all varna-compliant Hindus, victims = none (or adharmic actors); reformist_contextualist would claim tangled_rope with beneficiaries = reformist interpreters, victims = those harmed by residual casteism. The structural delta is the abolitionist reading''s unique elimination of textual authority and total victim-set dismantling.',
    'If sibling readings produce similar ε and victim sets, the kernel may not be genuinely contested at the structural level. If they diverge sharply (as expected), the kernel decomposition is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural divergence across sibling readings of the same kernel').

omega_variable(
    disagreement_locus_textual_authority,
    'Where exactly in the structural analysis do the three readings disagree?',
    'Map each reading''s axioms: abolitionist = dharmasastra_categorically_illegitimate + caste_hierarchy_intrinsically_oppressive (deontological). Literalist = dharmasastra_eternal_revelation + varna_divinely_ordained (theological). Contextualist = dharma_separable_from_caste + texts_historically_conditioned (conventional/instrumental). The locus is the authority_grounding of the kernel: extraction (abolitionist), lineage (literalist), practice (contextualist).',
    'Locates the contestation precisely: not in empirical facts about caste oppression (all three may acknowledge it) but in the normative warrant the kernel claims. This determines which CS classification pattern each reading generates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_locus_textual_authority, conceptual, 'Disagreement located in kernel''s authority_grounding and axioms, not in empirical observations').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.91) primarily structural (legal, economic, violent enforcement) or internalized (stigma, fatalism, identity fusion with oppression)?',
    'Post-liberation suppression trajectory: where constitutional protections and Ambedkarite consciousness have weakened structural enforcement, does suppression persist at similar levels? If yes, internalized component is significant. Compare Dalit communities with strong movement organization vs. isolated ones.',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression after formal exit. This affects the engine''s χ computation for identity_locked vs. trapped exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in caste hierarchy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 1890, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t1890, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1890, 0.05).
narrative_ontology:measurement(dhar_tr_t1920, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1920, 0.08).
narrative_ontology:measurement(dhar_tr_t1947, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1947, 0.15).
narrative_ontology:measurement(dhar_tr_t1970, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(dhar_tr_t1990, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1990, 0.14).
narrative_ontology:measurement(dhar_tr_t2025, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(dhar_be_t1890, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1890, 0.92).
narrative_ontology:measurement(dhar_be_t1920, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1920, 0.9).
narrative_ontology:measurement(dhar_be_t1947, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1947, 0.85).
narrative_ontology:measurement(dhar_be_t1970, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1970, 0.83).
narrative_ontology:measurement(dhar_be_t1990, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1990, 0.88).
narrative_ontology:measurement(dhar_be_t2025, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 2025, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t1890, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1890, 0.95).
narrative_ontology:measurement(dhar_su_t1920, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1920, 0.93).
narrative_ontology:measurement(dhar_su_t1947, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1947, 0.88).
narrative_ontology:measurement(dhar_su_t1970, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1970, 0.85).
narrative_ontology:measurement(dhar_su_t1990, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1990, 0.9).
narrative_ontology:measurement(dhar_su_t2025, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 2025, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__abolitionist_rejection, identity_coordination).
narrative_ontology:boltzmann_floor_override(dharmasastra_corpus__abolitionist_rejection, 0.08).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__reformist_contextual).

% DUAL FORMULATION NOTE:
% This story is the abolitionist_rejection reading of the dharmasastra_corpus kernel. It decomposes the colloquial label 'Dharmasastra' into a specific structural claim: the corpus functions as a snare (pure extraction) with zero legitimate authority. The orthodox_literalist reading models the same corpus as a mountain/rope (legitimate coordination). The reformist_contextualist reading models it as a tangled_rope (partial coordination, partial extraction). The three stories form a constraint family linked by network.affects_constraints. The ε values diverge sharply: abolitionist ε≈0.87, literalist ε≈0.05, contextualist ε≈0.45 — confirming they are distinct constraints, not measurement variants.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dharmasastra_corpus__abolitionist_rejection, institutional, 0.05).
constraint_indexing:directionality_override(dharmasastra_corpus__abolitionist_rejection, powerful, 0.15).
constraint_indexing:directionality_override(dharmasastra_corpus__abolitionist_rejection, powerless, 0.95).
constraint_indexing:directionality_override(dharmasastra_corpus__abolitionist_rejection, moderate, 0.8).
constraint_indexing:directionality_override(dharmasastra_corpus__abolitionist_rejection, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
