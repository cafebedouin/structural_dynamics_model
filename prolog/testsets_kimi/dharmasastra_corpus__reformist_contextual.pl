% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__reformist_contextual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__reformist_contextual, []).

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
 *   constraint_id: dharmasastra_corpus__reformist_contextual
 *   human_readable: Dharmasastra Reformist Contextual Reading
 *   domain: religious/law/textual
 *
 * SUMMARY:
 *   The reformist contextual reading of Dharmasastra treats the tradition as
 *   containing a separable ethical core (dharma as righteous conduct)
 *   embedded in historically contingent social prescriptions (caste
 *   hierarchy). It coordinates Hindu identity across modernity by preserving
 *   textual authority while discarding strict literal observance, but
 *   continues to extract symbolic status from subaltern communities through
 *   spiritual-stage reinterpretations of varna. This is one reading of a
 *   three-way contested kernel; the other readings are orthodox literalist
 *   (eternal truth) and abolitionist rejection (no legitimate authority).
 *
 * KEY AGENTS:
 *   - reformist_scholarly_class: agenda_setter (organized/mobile) â administers the hermeneutic framework
 *   - dominant_caste_reformers: primary beneficiary (powerful/mobile) â collects social capital without literalist stigma
 *   - subaltern_caste_communities: primary target (powerless/identity_locked) â bears residual symbolic and material costs
 *   - orthodox_literalist_authorities: excluded (organized/constrained) â rejects contextualization entirely
 *   - abolitionist_critics: excluded (moderate/mobile) â rejects textual authority entirely
 *   - secular_historians: observer (institutional/analytical) â documents without endorsing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, 0.48).
domain_priors:suppression_score(dharmasastra_corpus__reformist_contextual, 0.42).
domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, extractiveness, 0.48).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Dharmasastra Reformist Contextual Reading").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious/law/textual").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, 'a1cca2bd-2c5f-4d72-bfc4-e4aa7102962d').
narrative_ontology:cs_kernel_codification('a1cca2bd-2c5f-4d72-bfc4-e4aa7102962d', fixed_text).
narrative_ontology:cs_authority_grounding('a1cca2bd-2c5f-4d72-bfc4-e4aa7102962d', lineage).
narrative_ontology:cs_interpretation_layer_present('a1cca2bd-2c5f-4d72-bfc4-e4aa7102962d').
narrative_ontology:cs_reading_relation('a1cca2bd-2c5f-4d72-bfc4-e4aa7102962d', dharmasastra_corpus__orthodox_literalist, coexists_with).
narrative_ontology:cs_reading_relation('a1cca2bd-2c5f-4d72-bfc4-e4aa7102962d', dharmasastra_corpus__abolitionist_rejection, influences).
narrative_ontology:cs_axiom('a1cca2bd-2c5f-4d72-bfc4-e4aa7102962d', foundational, dharma_transcends_caste_container).
narrative_ontology:cs_axiom_status(dharma_transcends_caste_container, holdable).
narrative_ontology:cs_axiom_grounding('a1cca2bd-2c5f-4d72-bfc4-e4aa7102962d', dharma_transcends_caste_container, deontological).
narrative_ontology:cs_axiom('a1cca2bd-2c5f-4d72-bfc4-e4aa7102962d', foundational, textual_authority_requires_historical_hermeneutics).
narrative_ontology:cs_axiom_status(textual_authority_requires_historical_hermeneutics, holdable).
narrative_ontology:cs_axiom_grounding('a1cca2bd-2c5f-4d72-bfc4-e4aa7102962d', textual_authority_requires_historical_hermeneutics, theological).
narrative_ontology:cs_reference_frame('a1cca2bd-2c5f-4d72-bfc4-e4aa7102962d', historically_situated_revelation).
narrative_ontology:cs_drift_state('a1cca2bd-2c5f-4d72-bfc4-e4aa7102962d', post_colonial_modern_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a1cca2bd-2c5f-4d72-bfc4-e4aa7102962d', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, reformist_scholarly_class).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, dominant_caste_reformers).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, subaltern_caste_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Dharmasastra texts to distinguish eternal ethics from historical caste prescriptions. Publishes commentaries, teaches in seminaries and universities, and sets the hermeneutic framework for modern Hindu practice. Retains institutional prestige and employment by preserving textual relevance in a secularizing context.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, reformist_scholarly_class, agenda_setter,
    organized, generational, mobile, national).

% Adopt the reformist reading to retain Hindu religious and cultural identity while adopting progressive social norms publicly. Retain structural advantages in marriage networks, temple access, and social capital without being required to defend strict scriptural literalism.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, dominant_caste_reformers, beneficiary,
    powerful, biographical, mobile, national).

% Receive softened rhetoric such as spiritual stages and guna-karma theory, but continue to face discrimination in inter-caste marriage, temple entry, and ritual status justified by the same textual corpus. Must either accept the reformist framing to remain in the religious community or bear the severe cost of complete exit from Hindu identity.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, subaltern_caste_communities, payer,
    powerless, biographical, identity_locked, national).

% Reject any contextualization of caste prescriptions as illegitimate departure from shruti and smriti. Not invited to reformist theological forums; their authority is systematically undermined by the reformist claim that caste rules are historically contingent rather than eternal.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, orthodox_literalist_authorities, excluded,
    organized, generational, constrained, national).

% Argue that any preservation of Dharmasastra textual authority perpetuates caste ideology regardless of interpretive frame. Excluded from reformist platforms that seek to rehabilitate the tradition rather than abandon it.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, abolitionist_critics, excluded,
    moderate, biographical, mobile, national).

% Document the historical emergence of Dharmasastra texts and the nineteenth-twentieth century reformist project. Provide external corroboration that the texts reflect historical social conditions, without endorsing the normative authority claims of any reading.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, secular_historians, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves Dharmasastra textual authority and Hindu ethical continuity across colonial and post-colonial modernity by distinguishing eternal moral principles from historically contingent social regulations, thereby coordinating Hindu identity without strict literal observance.
% TRANSFER_FUNCTION: Moves authority to define legitimate Hindu practice from literalist orthodoxy to contextual interpreters; moves social and symbolic capital to dominant-caste reformers who adopt progressive language without relinquishing structural advantage; moves a continued compliance burden to subaltern communities who must accept spiritual-stage framings of their social position.
% ABSENT_VOICES: Orthodox pandits who reject any contextualization as heresy; abolitionist critics who reject all textual authority; lower-caste voices whose lived experience of continued discrimination contradicts the spiritual-stages framing.
% DISAPPEARANCE_RATIONALE: If the reformist contextual reading vanished overnight, the institutional center of modern Hindu jurisprudence and ethics would lose its primary framework for managing caste in a progressive register; dominant-caste reformers would be forced toward either orthodox literalism or secular abolition, and the scholarly apparatus sustaining the hermeneutic would collapse.
% FOUNDING_PROBLEM: How to preserve Dharmasastra and Hindu social coherence in the face of colonial critique, modernity, and anti-caste movements without abandoning the textual tradition entirely.
% FOUNDING_PROBLEM_CORROBORATION: Colonial-era ethnographers and early Indologists documented the social conditions of text production. Modern Dalit scholars and activists contest that the problem was ever solved by reinterpretation. Secular historians of religion corroborate the nineteenth-century emergence of the reformist project but do not endorse its claim to have successfully separated ethics from hierarchy.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__reformist_contextual, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__reformist_contextual, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__reformist_contextual, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dharmasastra_corpus__reformist_contextual, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__reformist_contextual, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__reformist_contextual_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__reformist_contextual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is medium (0.48) because while strict caste enforcement is disclaimed, the symbolic hierarchy persists in marriage, ritual, and social boundaries, constituting ongoing status extraction. Suppression (0.42) reflects the soft but real delegitimization of caste-free alternatives within the reformist frameworkâalternatives are labeled Western or anti-Hindu. Theater ratio (0.40) is significant: the reformist reading performs progressive interpretation while dominant-caste networks retain structural advantage. Accessibility collapse (0.45) is moderate because the orthodox literalist and abolitionist alternatives remain intellectually available, though the reformist frame captures the institutional center. Resistance (0.60) is high because the reading is contested by orthodox authorities defending literal eternality and by abolitionist critics rejecting all textual authority.
 *
 * PERSPECTIVAL GAP:
 *   From the reformist scholarly seat, the constraint is hermeneutic recoveryâsaving scripture from obsolescence. From the dominant-caste reformer seat, it is identity managementâretaining Hindu affiliation without social stigma. From the subaltern seat, it is hierarchy with better rhetoricâthe same exclusions justified by guna-karma rather than birth. The engine computes these divergences from the same structural data: low d for authority-preserving beneficiaries, high d for identity-locked targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars (organized, mobile) and dominant-caste reformers (powerful, mobile) derive authority and social capital from the constraint, placing them near the beneficiary end. Subaltern communities (powerless, identity-locked) bear the residual costs of symbolic hierarchy, placing them near the target end. Orthodox and abolitionist critics are structurally excluded from the reformist conversation; their exclusion is part of what maintains the reformist center. Secular historians observe without stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The reformist reading risks mislabeling in both directions. Read as pure coordination (Rope), one would miss the victim set and the symbolic extraction that persists. Read as pure extraction (Snare), one would miss the genuine coordination function of preserving ethical continuity and community identity across colonial and post-colonial disruption. Tangled Rope captures the hybrid: the coordination is real (textual authority preserved, ethical core identified) and the extraction is real (subaltern communities still pay status costs). The founding problemâhow to preserve Dharmasastra in modernityâis contested: reformists claim it is live, orthodox critics claim it is a betrayal of the actual founding problem (eternal observance), and abolitionists claim the problem was always illegitimate. The temporal measurements show theater_ratio rising over the interval, suggesting the coordination function has not dissolved but has accumulated performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the reformist_contextual reading of kernel dharmasastra_corpus. Would classification change if the orthodox_literalist reading (eternal literal observance) or abolitionist_rejection reading (no legitimate authority) were adopted as the operative constraint?',
    'Compare the sibling constraint stories in the family; evaluate which reading currently holds institutional dominance in specific social domains (temple governance, marriage practice, legal personal law).',
    'If orthodox_literalist is dominant, extractiveness rises to near-total and victim set expands; if abolitionist_rejection is dominant, the coordination function vanishes and the constraint dissolves entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Position of this reading within the contested kernel and its structural alternatives.').

omega_variable(
    symbolic_extraction_materiality,
    'Does the reformist reading''s symbolic hierarchy (spiritual stages) still generate material extraction in marriage, employment, and temple access, or is it purely performative?',
    'Empirical sociological studies comparing caste-based outcomes across reformist-affiliated versus orthodox-affiliated communities, and across reformist-identified versus abolitionist-identified subaltern populations.',
    'If material extraction persists, the reformist reading functions as tangled_rope with genuine coordination but continued asymmetric extraction; if purely symbolic, it trends toward rope with minimal victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_extraction_materiality, empirical, 'Whether softened hierarchy remains materially extractive or has become purely theatrical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhr_reform_tr_t0, dharmasastra_corpus__reformist_contextual, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dhr_reform_tr_t20, dharmasastra_corpus__reformist_contextual, theater_ratio, 20, 0.24).
narrative_ontology:measurement(dhr_reform_tr_t40, dharmasastra_corpus__reformist_contextual, theater_ratio, 40, 0.3).
narrative_ontology:measurement(dhr_reform_tr_t60, dharmasastra_corpus__reformist_contextual, theater_ratio, 60, 0.34).
narrative_ontology:measurement(dhr_reform_tr_t80, dharmasastra_corpus__reformist_contextual, theater_ratio, 80, 0.38).
narrative_ontology:measurement(dhr_reform_tr_t100, dharmasastra_corpus__reformist_contextual, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(dhr_reform_be_t0, dharmasastra_corpus__reformist_contextual, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(dhr_reform_be_t20, dharmasastra_corpus__reformist_contextual, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(dhr_reform_be_t40, dharmasastra_corpus__reformist_contextual, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(dhr_reform_be_t60, dharmasastra_corpus__reformist_contextual, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(dhr_reform_be_t80, dharmasastra_corpus__reformist_contextual, base_extractiveness, 80, 0.45).
narrative_ontology:measurement(dhr_reform_be_t100, dharmasastra_corpus__reformist_contextual, base_extractiveness, 100, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(dhr_reform_su_t0, dharmasastra_corpus__reformist_contextual, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(dhr_reform_su_t20, dharmasastra_corpus__reformist_contextual, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(dhr_reform_su_t40, dharmasastra_corpus__reformist_contextual, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(dhr_reform_su_t60, dharmasastra_corpus__reformist_contextual, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(dhr_reform_su_t80, dharmasastra_corpus__reformist_contextual, suppression_requirement, 80, 0.42).
narrative_ontology:measurement(dhr_reform_su_t100, dharmasastra_corpus__reformist_contextual, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, abolitionist_rejection).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the dharmasastra_corpus kernel. The natural-language label 'Dharmasastra' conflates three structurally distinct constraints: orthodox literalist observance (high extraction, snare-like), reformist contextual interpretation (medium extraction, tangled rope), and abolitionist rejection (no coordination function, constraint dissolution). Each reading has a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
