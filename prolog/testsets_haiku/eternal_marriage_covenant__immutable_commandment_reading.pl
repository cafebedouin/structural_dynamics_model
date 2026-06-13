% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__immutable_commandment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__immutable_commandment_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: D&C 132 Polygamy as Eternal, Immutable Divine Law Required for Exaltation
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   D&C 132 (received 1831, publicly taught from 1852) establishes polygamy
 *   as eternally binding divine law required for celestial exaltation. The
 *   immutable-commandment reading claims this doctrine is fixed, immutable,
 *   and admits no legitimate revision path — God's eternal will cannot be
 *   superseded by prophetic choice or political pressure. However, the 1890
 *   Manifesto officially suspended the practice. From the immutable-law
 *   reading's perspective, this creates an intractable paradox: if the law is
 *   eternally immutable, how can it be suspended? The resolution offered by
 *   this reading is that the doctrine remains eternally valid, compliance is
 *   temporarily suspended under duress, and legitimacy lies only in adherence
 *   to the immutable principle despite federal coercion. This reading
 *   uniquely creates a martyrdom constraint: practitioners must choose
 *   between breaking federal law (and facing prosecution) or breaking what
 *   they understand as God's eternal law (and facing spiritual damnation).
 *   Women in plural marriages occupy a particularly constrained position,
 *   with no clear exit and responsibility for outcomes shaped by male
 *   leadership's doctrinal authority.
 *
 * KEY AGENTS:
 *   - Male church leadership: sets doctrine, controls interpretation, claims prophetic authority, faces federal prosecution as head of household
 *   - Polygamist practitioners: believe themselves bound by eternal law, caught between federal prosecution and spiritual damnation, trapped in identity-locked commitment
 *   - Women in plural marriages: least agentive, structurally positioned as parts of an eternal hierarchy, economically dependent, excluded from decision-making authority
 *   - Federal authorities: enforce anti-polygamy law, create the external pressure that intensifies the constraint's suppressive requirement
 *   - Reform-minded members: excluded from authority, call for reinterpretation but lack institutional power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.81).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.88).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "D&C 132 Polygamy as Eternal, Immutable Divine Law Required for Exaltation").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, 'd712ada0-33a2-4d47-a3cf-2e1c78aa4b30').
narrative_ontology:cs_kernel_codification('d712ada0-33a2-4d47-a3cf-2e1c78aa4b30', fixed_text).
narrative_ontology:cs_authority_grounding('d712ada0-33a2-4d47-a3cf-2e1c78aa4b30', lineage).
narrative_ontology:cs_interpretation_layer_present('d712ada0-33a2-4d47-a3cf-2e1c78aa4b30').
narrative_ontology:cs_reading_relation('d712ada0-33a2-4d47-a3cf-2e1c78aa4b30', eternal_marriage_covenant__prophetic_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('d712ada0-33a2-4d47-a3cf-2e1c78aa4b30', eternal_marriage_covenant__temporal_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('d712ada0-33a2-4d47-a3cf-2e1c78aa4b30', foundational, eternality_forecloses_revision).
narrative_ontology:cs_axiom_status(eternality_forecloses_revision, holdable).
narrative_ontology:cs_axiom_grounding('d712ada0-33a2-4d47-a3cf-2e1c78aa4b30', eternality_forecloses_revision, deontological).
narrative_ontology:cs_axiom('d712ada0-33a2-4d47-a3cf-2e1c78aa4b30', secondary, immutable_principle_transcends_circumstance).
narrative_ontology:cs_axiom_status(immutable_principle_transcends_circumstance, holdable).
narrative_ontology:cs_axiom_grounding('d712ada0-33a2-4d47-a3cf-2e1c78aa4b30', immutable_principle_transcends_circumstance, theological).
narrative_ontology:cs_reference_frame('d712ada0-33a2-4d47-a3cf-2e1c78aa4b30', eternal_immutable_polygamy_doctrine).
narrative_ontology:cs_drift_state('d712ada0-33a2-4d47-a3cf-2e1c78aa4b30', post_1860_federal_enforcement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d712ada0-33a2-4d47-a3cf-2e1c78aa4b30', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, male_church_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, patriarchal_authority_doctrine).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, polygamist_practitioners_under_federal_pressure).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, women_in_plural_marriages).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at endpoint) because the immutable-law reading demands compliance with plural marriage as a condition of salvation, and the doctrine is enforced by institutional authority and internalized identity-fusion (you cannot leave without renouncing your salvation and your relationship to God as interpreted by the church). Suppression rises sharply from 1844 onward (0.65 → 0.88) as federal prosecution intensifies, creating a dual-suppression structure: the doctrine itself suppresses exit (apostasy = damnation), and federal enforcement suppresses the practice (polygamy = criminal). Theater rises gradually (0.15 → 0.42) because by the 1880s, the church's public framing shifts increasingly toward suffering-for-principle narrative, martyrdom rhetoric, and claims of religious persecution, even as the doctrine's internal logic remains presented as immutable. The measurement series reflects the reading's historical claim: that the immutable law persisted as the authoritative framing even as external and internal pressure mounted. All metrics are authored on one shared time grid (six decades, six time points).
 *
 * PERSPECTIVAL GAP:
 *   Male church leadership experiences the constraint as a principle to defend against federal tyranny — their exit from the immutable-law reading would mean renouncing prophetic authority, the doctrinal foundation of the entire institution. Polygamist practitioners experience it as a binding divine law they cannot renounce without spiritual death, while federal pressure forces a choice between legal compliance (sin) and legal violation (imprisonment). Women experience it as fixed hierarchy with no legitimate voice in its maintenance or revision. Federal authorities experience it as criminal conspiracy masked by religious language. The engine computes these divergent directionalities from the structural data: the beneficiary seat (leadership) sits near d=0.0 (subsidized by the arrangement), target seats (practitioners, women) sit near d=1.0 (extraction and suppression). This divergence is the measurement; the authored claim (tangled_rope) reflects the reading's own framing, not the engine's verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Male church leadership benefits from and administers the arrangement (d near beneficiary end, ~0.1-0.2). They collect authority, legitimate patriarchal power, and the capacity to enforce gender hierarchy under the guise of eternal principle. Their exit from the immutable-law reading is identity-locked: the prophetic authority claim is the foundation of institutional power. Polygamist practitioners are targets (d near 0.85-0.95): they bear legal jeopardy, social stigma, and internal conflict; their exit is identity-locked because the doctrine defines their spiritual identity and salvation trajectory. Women are doubly targeted: they bear the extraction (sexual and reproductive autonomy transferred) with minimal voice in the decision (d=0.9+). Federal authorities are structurally opposed but not formally inside the constraint's beneficiary/victim structure — they are excluded stakeholders whose enforcement actions reshape the operational environment.
 *
 * MANDATROPHY ANALYSIS:
 *   The immutable-commandment reading prevents a false charity: it refuses to reinterpret the doctrine as benign coordination (a reading available in the temporal-accommodation frame, which claims eternal validity but temporary suspension). Instead, this reading asserts that the doctrine is extractive precisely because it claims immutability — the extraction persists under the cover of eternal principle. The Manifesto appears not as a solution but as a suspension that leaves the doctrine intact, creating ongoing cognitive dissonance and the martyrdom constraint. Mandatrophy is present in the kernel as a whole (the covenant's founding problem is contested and its status is deadlocked), but this reading specifically instantiates the refusal to resolve the deadlock: immutability means no legitimate revision path, so the problem cannot be solved, only endured. The reading's structural claim is that the constraint is a tangled rope (coordination + extraction, requires enforcement) that has become partly theatrical (the martyrdom narrative substitutes for functional coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutable_vs_revisable,
    'Can an eternally binding divine principle revealed through prophecy be legitimately rescinded through the same prophetic channel, or does eternality foreclose revision?',
    'Doctrinal interpretation by the church authority structure (which has already chosen the temporal-accommodation and prophetic-override readings as live alternatives); or external analysis of the logical structure of immutability claims in religious commitment systems.',
    'If immutability is compatible with prophetic rescission, the immutable-commandment reading is internally incoherent and collapses into the prophetic-override reading. If immutability forecloses rescission, the temporal-accommodation reading''s claim of eternal validity without current practice is unsustainable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immutable_vs_revisable, conceptual, 'Whether eternality is logically compatible with prophetic revision.').

omega_variable(
    extraction_under_identity_lock,
    'Does the immutable-law reading extract autonomy and reproductive choice from practitioners and women, or does it legitimately coordinate eternal family structure as the doctrine claims?',
    'Comparative analysis of consent, exit options, and agency within the constraint versus alternative family structures; testimony from women in plural marriages about their actual agency and choices.',
    'If extraction is high, the constraint should be reclassified as snare or high-extraction tangled rope; if coordination is genuine, it remains tangled rope with lower effective extraction and higher beneficiary legitimacy. The identity-lock on exit options is the critical variable: exit from this reading means accepting damnation, not merely leaving a practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_under_identity_lock, empirical, 'Whether the constraint''s operation reflects extraction or genuine identity-based coordination.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression structural (federal prosecution, church discipline) or internalized (practitioners'' own belief that the law binds them eternally, women''s acceptance of subordinate status as divinely mandated)?',
    'Post-exit suppression trajectory: do practitioners who leave the church report that the suppression ends, or do they report ongoing psychological, identity-based suppression from internalized belief?',
    'If suppression is substantially internalized, the constraint carries supposition of itself even after external enforcement ceases. The measured 0.88 at endpoint may understate the effective suppression because it is partly internalized; women and practitioners who exit often report ongoing identity-fusion effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression is structural or internalized.').

omega_variable(
    reading_foreclosure,
    'Does the immutable-commandment reading logically foreclose the temporal-accommodation reading within the doctrine''s own framework?',
    'Close reading of the logical structure of each reading''s core claim: does immutability entail that a practice cannot be suspended without renouncing the principle? Or can the principle be eternal while the practice is suspended?',
    'If immutability forecloses temporal accommodation, the three readings are not all live — one must be rejected. If temporal accommodation is logically compatible with immutability, all three readings coexist as live but contradictory interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure, conceptual, 'Whether the immutable-law reading logically rules out the temporal-accommodation reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 1830, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1830, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1830, 0.15).
narrative_ontology:measurement(eter_tr_t1844, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1844, 0.2).
narrative_ontology:measurement(eter_tr_t1860, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1860, 0.28).
narrative_ontology:measurement(eter_tr_t1875, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1875, 0.35).
narrative_ontology:measurement(eter_tr_t1885, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1885, 0.41).
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1890, 0.42).

% Extraction over time
narrative_ontology:measurement(eter_be_t1830, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1830, 0.65).
narrative_ontology:measurement(eter_be_t1844, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1844, 0.72).
narrative_ontology:measurement(eter_be_t1860, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1860, 0.78).
narrative_ontology:measurement(eter_be_t1875, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1875, 0.8).
narrative_ontology:measurement(eter_be_t1885, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1885, 0.81).
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1890, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1830, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1830, 0.52).
narrative_ontology:measurement(eter_su_t1844, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1844, 0.65).
narrative_ontology:measurement(eter_su_t1860, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1860, 0.76).
narrative_ontology:measurement(eter_su_t1875, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1875, 0.84).
narrative_ontology:measurement(eter_su_t1885, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1885, 0.87).
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1890, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__immutable_commandment_reading, 0.12).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__prophetic_override_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__temporal_accommodation_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, federal_polygamy_suppression_law).

% DUAL FORMULATION NOTE:
% The eternal-marriage-covenant kernel admits three structurally distinct readings, each instantiating a different ε and victim set. The immutable-commandment reading (this story) claims the doctrine is fixed and admits no revision path (ε=0.81, tangled rope with high suppression, identity-locked exit). The prophetic-override reading claims continuing revelation allows the living prophet to supersede prior revelation (lower ε, rope-like coordination). The temporal-accommodation reading claims the principle is eternal but the practice is suspended pending obedience to the law of the land (moderate ε, hybrid). Each reading has a distinct constraint family; they share a kernel but decompose into different operational structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eternal_marriage_covenant__immutable_commandment_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
