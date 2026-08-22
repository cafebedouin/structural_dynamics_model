% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__orthodox_varna_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__orthodox_varna_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: vedic_corpus_social_prescription__orthodox_varna_reading
 *   human_readable: Vedic Varna Hierarchy as Divine Cosmic Order (Orthodox Reading)
 *   domain: religious/social/hermeneutical
 *
 * SUMMARY:
 *   This story instantiates the ORTHODOX READING of the Vedic Varna hierarchy
 *   kernel: a reading that takes Vedic cosmological passages (purusha sukta,
 *   varna allocations in Rigveda) as literal, prescriptive mandates for human
 *   social organization bound to birth-caste assignment. This reading has
 *   been monopolized by brahminical exegetes for over 2,000 years and
 *   codified into law by colonial administrators. Under this reading, the
 *   Shudra and Dalit castes are prescribed by sacred texts to occupy
 *   subordinate roles with severe occupational, ritual, and social
 *   restrictions; the constraint extracts labor and ritual deference while
 *   suppressing exit by positing the hierarchy as cosmic necessity. This is a
 *   READING of a contested kernel; sibling readings (reformist spiritual
 *   reading, colonial orientalist reading) offer different structural
 *   interpretations of the same Vedic texts.
 *
 * KEY AGENTS:
 *   - Brahmin caste: agenda-setter and beneficiary. Controls Vedic interpretation, monopolizes ritual authority, extracts labor and deference from lower castes while justifying it as cosmic order.
 *   - Shudra caste: payer. Prescribed occupational restrictions limit mobility; forbidden Vedic study denies access to the legitimacy structure itself. Trapped by custom and economic dependency.
 *   - Dalit castes: payer. Placed outside Varna hierarchy (untouchable); subject to pollution restrictions, occupational exclusion, and legal disabilities that compound Shudra restrictions.
 *   - Brahminical authority structure: institutional framework maintaining the reading through text monopoly and ritual gatekeeping.
 *   - Reformist interpreters: excluded from interpretive authority; their readings would dissolve Varna prescription if admitted.
 *   - Colonial administrators: external observer-enforcers who hardened the reading into fixed administrative law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, 0.82).
domain_priors:suppression_score(vedic_corpus_social_prescription__orthodox_varna_reading, 0.79).
domain_priors:theater_ratio(vedic_corpus_social_prescription__orthodox_varna_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__orthodox_varna_reading, snare).
narrative_ontology:human_readable(vedic_corpus_social_prescription__orthodox_varna_reading, "Vedic Varna Hierarchy as Divine Cosmic Order (Orthodox Reading)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__orthodox_varna_reading, "religious/social/hermeneutical").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__orthodox_varna_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__orthodox_varna_reading, '67b9bb37-3304-41de-9f6f-3d4aa1083739').
narrative_ontology:cs_kernel_codification('67b9bb37-3304-41de-9f6f-3d4aa1083739', fixed_text).
narrative_ontology:cs_authority_grounding('67b9bb37-3304-41de-9f6f-3d4aa1083739', lineage).
narrative_ontology:cs_interpretation_layer_present('67b9bb37-3304-41de-9f6f-3d4aa1083739').
narrative_ontology:cs_reading_relation('67b9bb37-3304-41de-9f6f-3d4aa1083739', vedic_corpus_social_prescription__reformist_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('67b9bb37-3304-41de-9f6f-3d4aa1083739', vedic_corpus_social_prescription__colonial_orientalist_reading, influences).
narrative_ontology:cs_axiom('67b9bb37-3304-41de-9f6f-3d4aa1083739', foundational, vedic_cosmology_is_social_prescription).
narrative_ontology:cs_axiom_status(vedic_cosmology_is_social_prescription, holdable).
narrative_ontology:cs_axiom_grounding('67b9bb37-3304-41de-9f6f-3d4aa1083739', vedic_cosmology_is_social_prescription, conventional).
narrative_ontology:cs_axiom('67b9bb37-3304-41de-9f6f-3d4aa1083739', foundational, varna_birth_determined_eternal).
narrative_ontology:cs_axiom_status(varna_birth_determined_eternal, holdable).
narrative_ontology:cs_axiom_grounding('67b9bb37-3304-41de-9f6f-3d4aa1083739', varna_birth_determined_eternal, deontological).
narrative_ontology:cs_reference_frame('67b9bb37-3304-41de-9f6f-3d4aa1083739', eternal_cosmic_varna_order).
narrative_ontology:cs_drift_state('67b9bb37-3304-41de-9f6f-3d4aa1083739', contemporary_post_dalit_movement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('67b9bb37-3304-41de-9f6f-3d4aa1083739', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_castes).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, occupational_restriction_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_caste).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_caste).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the interpretation and transmission of Vedic texts through hereditary priestly authority and monopoly on Sanskrit literacy. Sets the orthodox reading and enforces it through ritual gatekeeping, textual authority, and social ostracism. Collects the direct gains: exclusive ritual authority generates material support (gifts, land grants), occupational monopoly on brahminical functions, and the deference and labor obligation of lower castes. Could exit by reinterpreting texts differently (advocating for the reformist spiritual reading), but doing so would dissolve institutional authority and the extraction it sustains.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, beneficiary).

% Prescribed by the orthodox reading to serve the three higher varnas with occupational restrictions enforced by caste councils and social pressure. Forbidden from Vedic study, ritual performance, and entry into brahminical or kshatriya roles. Must provide agricultural labor, craft services, and economic support to upper castes with no access to the knowledge or authority structure that justifies this subordination. Trapped because caste was hereditary, occupational alternatives were blocked, and land and trade access flowed through caste networks. Exiting meant social death and economic destitution.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste, payer,
    powerless, civilizational, trapped, regional).

% Placed entirely outside the Varna hierarchy under this reading (declared 'untouchable'); subject to pollution restrictions that forbade physical proximity to upper castes, occupational exclusion to tasks deemed ritually polluting (corpse handling, leather work, sanitation), and legal disabilities (historically forbidden to own land, enter temples, draw from public wells, wear certain clothes, ring bells in public). Bore the deepest extraction: not just labor obligation but complete social exclusion and dehumanization. Trapped by law, custom, ritual prohibition, and complete denial of access to the legitimacy system. Exit was nearly impossible; even religious conversion was sometimes blocked by brahminical gatekeepers.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_castes, payer,
    powerless, civilizational, trapped, regional).

% Prescribed as warrior-rulers; benefits from occupational monopoly (governance, martial authority, land grant revenue) and from Shudra labor obligation. Superior in power to Vaishya, but subordinate to Brahmins in ritual/spiritual status. Authority derives from brahminical sanction; cannot reinterpret texts without losing that legitimacy foundation. Constrained exit because brahminical legitimacy is the basis of their authority.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_caste, beneficiary,
    powerful, civilizational, constrained, regional).

% Prescribed as merchants and farmers; benefits from occupational monopoly (commerce, certain agricultural roles) and from Shudra labor availability. Lower ritual status than both Brahmin and Kshatriya; dependent on brahminical legitimacy for authority. Constrained exit because brahminical-certified status is the basis of their occupational monopoly.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_caste, beneficiary,
    moderate, civilizational, constrained, regional).

% The institutional practice of brahminical text interpretation: Sanskrit study restricted to brahmin males, Vedic exegesis monopolized by brahmin scholars, interpretive authority certified through brahminical lineage and guru-disciple transmission. Not an agent but the structural mechanism through which the orthodox reading is maintained and enforced as authoritative.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, brahminical_exegetical_tradition, agenda_setter,
    institutional, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(vedic_corpus_social_prescription__orthodox_varna_reading, brahminical_exegetical_tradition).

% Hindu reformers (Brahmo Samaj, Arya Samaj, modern scholars) who read the same Vedic texts as describing spiritual unity without prescriptive social content. Their interpretations would dissolve Varna hierarchy if adopted as canonical. Historically excluded from interpretive authority through brahminical gatekeeping (denied Sanskrit education, their commentaries dismissed as heterodox or modern invention). Their voices are absent from the classical reading but present as live challenge in the contemporary contest.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, reformist_interpreters, excluded,
    moderate, biographical, constrained, regional).

% British colonial authorities who encountered the orthodox Vedic reading and, seeking to administer India through understood legal systems, codified Varna hierarchy into fixed administrative law ('Hindu law'). Consulted brahminical scholars as authorities, thus hardening the orthodox reading and extending its enforcement beyond brahminical social control into state legal apparatus. External observer-enforcer that amplified the constraint.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, colonial_administrators, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__orthodox_varna_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Vedic cosmology under the orthodox reading provides a metaphysical justification for occupational specialization and role differentiation: Brahmins perform ritual; Kshatriya govern; Vaishya handle trade; Shudra provide labor. The reading frames this as coordinating a coherent cosmic order, making specialization feel inevitable rather than chosen.
% TRANSFER_FUNCTION: Moves labor value, agricultural surplus, and ritual service obligation from Shudra and Dalit castes to the three upper varnas, with the majority captured by the Brahmin caste through monopolized ritual authority and textual interpretation. The constraint enforces this transfer by forbidding Shudra Vedic study, restricting occupational mobility, and imposing ritual pollution rules on Dalit castes.
% ABSENT_VOICES: Reformist interpreters who read the same Vedic texts as spiritually egalitarian; Dalit and Shudra castes whose interests are structured OUT of the interpretive conversation (they are bound by Brahmin interpretation but denied access to the texts themselves); non-brahminical Hindu traditions and saints (Kabir, Basavanna, Jotiba Phule traditions) that explicitly rejected caste hierarchy but were positioned as heterodox or folk-level by brahminical gatekeepers. Under brahminical control of scriptural authority, these voices remained inaudible in the canonical reading.
% DISAPPEARANCE_RATIONALE: If the orthodox Vedic varna prescription were abandoned, brahminical ritual monopoly would collapse (Vedic study would be open to all), occupational restrictions on lower castes would dissolve, ritual authority would no longer flow exclusively through brahminical lineage, and the entire institutional structure of brahminical authority — which depends on positing the hierarchy as cosmic necessity — would require fundamental reorganization. The social world organized around varna hierarchy (caste councils, occupational boundaries, ritual purity enforcement) would rearrange radically.
% FOUNDING_PROBLEM: Early Vedic texts (particularly purusha sukta in Rigveda 10.90) describe a cosmological order in which different social functions (ritual, governance, trade, labor) are necessary for cosmic coherence. The orthodox reading takes this cosmological description as a literal social prescription: that humans are born into fixed Varna roles determined by cosmic order, and that adhering to Varna maintains universal balance.
% FOUNDING_PROBLEM_CORROBORATION: Brahminical exegetes attest the founding problem remains live: maintaining varna adherence is necessary for cosmic order. Modern Hindu reformers, Dalit scholars, and comparative cosmologists attest the founding problem is dead: modern knowledge shows Vedic cosmology is metaphorical, not literal social law; universal balance does not empirically depend on caste adherence; the founding problem was a retroactive CONSTRUCTION by Dharmashastra authors (centuries after the Vedas) who transformed descriptive cosmology into prescriptive law. Philological scholarship (including colonial-era archive studies) supports the reformist reading: early Vedic varna was role-based and fluid; the hardening into birth-determined, eternally-fixed caste is a Dharmashastra innovation, not Vedic original content. The founding problem (cosmic order as varna justification) is substantially CONTESTED by contemporary scholarship outside brahminical orthodoxy.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__orthodox_varna_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__orthodox_varna_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__orthodox_varna_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.82) and rising: Shudra and Dalit castes bear occupational restrictions, ritual exclusion, and compelled service without access to the legitimacy system (Vedic knowledge) that justifies it. The constraint is enforced: brahminical gatekeeping of text interpretation, ritual monopoly, and social ostracism punish violation. Theater is moderate (0.41, rising): substantial proportion of brahminical activity consists of ritual performance and textual recitation whose primary function is reproducing brahminical authority rather than meeting stated coordination needs. Resistance is non-trivial (0.62) because Dalit and Shudra castes, despite structural powerlessness, mounted persistent resistance via reform movements, bhakti rejection of caste, Dalit movements, and intellectual critique. Accessibility collapse is high (0.88): under this reading, the Vedic texts literally close all alternative possibilities — the hierarchy is cosmically mandated, not human choice. Measurements run on one shared time grid; trajectories show extraction hardening over time (brahminical codification in Dharmashastra texts, colonial codification), theater increasing (performance becomes more elaborate as orthodoxy requires heightened legitimacy work), and suppression intensifying (colonial governance added legal enforcement to brahminical social enforcement).
 *
 * PERSPECTIVAL GAP:
 *   From the brahminical seat, this constraint is divine coordination: cosmic order itself, proven by scriptural authority and secured through brahminical stewardship. From the Shudra/Dalit seat, the same structure is coercive extraction: occupational theft, knowledge monopoly, and cosmological cover story. The engine computes this divergence from the power/exit/beneficiary structure: the agenda-setter with arbitrage exit and gain flow computes as a beneficiary; the powerless payer with trapped exit computes as a target. Neither seat misperceives the facts; they occupy genuinely different structural positions within the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin caste enters as the structural beneficiary (d → 0.0 beneficiary end): they set the reading, monopolize ritual authority, extract labor value, and have arbitrage-grade exit (can reinterpret texts to dissolve the reading, though doing so would cost institutional authority). Shudra and Dalit castes enter as targets (d → 1.0 target end): they bear extraction, face occupational restriction, possess trapped exit (custom and material dependency make departure suicidal), and are literally denied access to the knowledge structure (Vedic texts) that justifies their subjugation. The asymmetry is structural: only Brahmins can interpret the texts that prescribe their superiority; non-Brahmins are bound by that interpretation but denied the tools to contest it. Kshatriya and Vaishya sit in the benefit zone (d moderate-to-low) but subordinate to Brahmins in the hierarchy, so their exit is constrained by brahminical legitimacy dependency.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cosmic order requiring Varna coherence) is CONTESTED. Brahminical interpreters attest it remains live; reformist scholars and Dalit intellectuals attest the founding problem is dead (modern social knowledge shows cosmic order is metaphor, not law) yet the constraint persists as pure institutional extraction (brahminical authority seeking maintenance, not functional coordination). The disappearance verdict is WORLD_REARRANGES, indicating the constraint's removal would reorganize brahminical authority structures. The mandatrophy signal fires: founding_problem_status=dead + disappearance_verdict=world_rearranges indicates the constraint persists after its stated justification atrophied, maintained purely by institutional inertia and theater. This is NOT a piton (theater is moderate, not high; extraction is substantial, not diffuse), but it shows mandatrophy dynamics: the founding problem about cosmic order no longer drives brahminical behavior; what drives it is the institutional interest in maintaining interpretive authority. Piton would require even higher theater and more diffuse extraction across powerless seats; Snare is appropriate when extraction is concentrated (Brahmins capture it) and enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prescriptive_vs_descriptive_ambiguity,
    'Are the Vedic cosmological passages (purusha sukta, varna allocations in Rigveda) literal prescriptions for human social organization, or metaphorical/descriptive accounts of cosmic/social function that do not prescribe individual birth-based role assignment?',
    'Philological analysis of original Vedic language and context; comparison of cosmological metaphor across ancient traditions; examination of how Dharmashastra texts (composed centuries later) transformed cosmological description into prescriptive law.',
    'If descriptive, the constraint is a Snare grounded in hermeneutical choice by brahminical interpreters, not in what the texts literally state. If prescriptive, some extraction may be framed as coordination cost. This is the PRIMARY structural ambiguity in the kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prescriptive_vs_descriptive_ambiguity, conceptual, 'The kernel ontology: do Vedic texts prescribe Varna as law, or describe cosmic order metaphorically?').

omega_variable(
    brahminical_gatekeeping_mechanism,
    'To what degree does the orthodox reading persist because of brahminical institutional control over text interpretation, versus because of genuine scriptural content that unambiguously mandates Varna?',
    'Analysis of alternative readings that were historically suppressed or marginalized; documentation of how brahminical authority was maintained through monopoly on Sanskrit literacy and ritual expertise; comparison with reform movements that reinterpreted the same texts to endorse equality.',
    'If the reading persists primarily through gatekeeping, the constraint is a pure institutional snare; if scriptural content drives it, the extraction is at least anchored in an external legitimacy claim. This determines whether the beneficiary (Brahmin caste) is actively defending an interpretive choice or defending control of interpretation itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(brahminical_gatekeeping_mechanism, empirical, 'Whether the orthodox reading''s persistence depends on brahminical institutional monopoly or on scriptural prescription.').

omega_variable(
    varna_fluidity_vs_birth_essentialism,
    'Do early Vedic passages describe Varna as fluid role-assignment open to merit or change, or as a fixed, birth-determined, unchangeable category?',
    'Close reading of Rigvedic passages describing varna assignment; examination of Vedic hymns describing Varna change or achievement; analysis of when Jati (birth-caste) became conflated with Varna (function) in textual evolution.',
    'If early Vedic Varna is role-based and fluid, the constraint is a hardening of meaning over time by Dharmashastra authors (dates: 500 BCE – 500 CE), representing an extractive reinterpretation. If birth-determined from the start, the constraint is more directly prescriptive. The conflation of Varna and Jati is the crux.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(varna_fluidity_vs_birth_essentialism, empirical, 'Whether Vedic Varna is function-assigned or birth-determined.').

omega_variable(
    internalised_suppression_vs_structural_coercion,
    'Is the measured suppression (0.79) primarily structural (legal prohibition, occupational exclusion, material dependency) or internalized (victims believe the hierarchy is cosmically legitimate and morally binding)?',
    'Historical analysis of resistance and dissent movements (Dalit movements, bhakti rejection of caste, Ambedkarite thought); examination of post-suppression trajectories when legal caste restrictions were removed (do Dalit communities spontaneously re-embrace caste identity, or does suppression require ongoing institutional enforcement).',
    'If primarily structural, exit from the constraint becomes possible with institutional change. If substantially internalized (victims absorbed brahminical legitimacy claims), the constraint carries higher effective suppression even after legal change, and the extractive framework persists through cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalised_suppression_vs_structural_coercion, empirical, 'Mechanism of suppression: external coercion versus internalized brahminical cosmic authority.').

omega_variable(
    reading_identity_of_orthodox_exegesis,
    'What makes THIS reading (orthodox varna) distinct as a reading, rather than simply ''what the texts say''? What hermeneutical move (or non-move) does the orthodox reading commit to that a sibling reading rejects?',
    'Explicit documentation of the hermeneutical principles the orthodox reading applies: literal interpretation, cosmological-to-prescriptive translation rule, birth-based essentialism, brahminical authority as canonical. Comparison with reformist reading''s hermeneutical moves (metaphorical interpretation, spiritual-not-social content rule, transcendence of caste identity).',
    'Identifies the reading as a CHOICE (hermeneutical framing), not an inevitable textual meaning. This supports the Snare classification: the constraint persists because brahminical interpreters CHOOSE the literal, prescriptive, birth-based reading and gatekeep alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_of_orthodox_exegesis, conceptual, 'Hermeneutical identity: what interpretive moves distinguish the orthodox reading from its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__orthodox_varna_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(vedi_tr_t5, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(vedi_tr_t10, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(vedi_tr_t15, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(vedi_tr_t20, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(vedi_tr_t25, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(vedi_be_t5, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(vedi_be_t10, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(vedi_be_t15, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(vedi_be_t20, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(vedi_be_t25, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 25, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(vedi_su_t5, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 5, 0.71).
narrative_ontology:measurement(vedi_su_t10, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(vedi_su_t15, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(vedi_su_t20, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(vedi_su_t25, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 25, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__orthodox_varna_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__orthodox_varna_reading, 0.12).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested kernel 'vedic_corpus_social_prescription'. The sibling readings (reformist_spiritual_reading, colonial_orientalist_reading) are separate constraint stories with different epsilon values, beneficiary/victim structures, and classifications. They are linked via network.affects_constraints because they are readings of the same kernel — each reading's adoption by a constituency makes the sibling readings' positions differently viable. The reformist reading's adoption weakens brahminical orthodoxy's authority; the colonial reading's adoption hardened the orthodox reading into fixed law. The three stories are NOT three perspectives on one constraint; they are three structurally distinct constraints arising from one contested kernel. Decomposition follows the epsilon-invariance principle: measuring the constraint via the 'orthodox literal interpretation' path yields high extraction; measuring via the 'spiritual metaphor' path yields low/zero extraction; these are different constraints, not the same constraint measured differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_corpus_social_prescription__orthodox_varna_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
