% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__uncreated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__uncreated_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_ontological_status__uncreated_reading
 *   human_readable: Uncreated Qur'an as Eternal Ontic Constraint
 *   domain: theological/political/linguistic
 *
 * SUMMARY:
 *   This constraint instantiates the uncreated reading of the Qur'an's
 *   ontological status: the doctrine that the Qur'an is kalÄm AllÄh qadÄ«m,
 *   eternal and uncreated divine speech coeternal with God. Within this
 *   reading, the text functions as a fixed ontological anchor for Islamic
 *   law, theology, and identity. The constraint is presented by its holders
 *   as a Mountainâa divine given independent of human will. Structurally,
 *   however, it operates as a Tangled Rope: it genuinely coordinates communal
 *   practice and identity around a stable textual kernel, while
 *   asymmetrically extracting interpretive authority from rational
 *   theologians, metaphorical interpreters, and reform movements who require
 *   textual flexibility or historicization. The beneficiaries (traditional
 *   jurists, literalist communities, anti-rationalist schools) derive
 *   institutional and epistemic rents from the doctrine's permanence; the
 *   victims bear the cost of closed hermeneutic space. The kernel context:
 *   this is one reading of the contested kernel quran_ontological_status;
 *   sibling readings (created_reading, state_enforced_creation_reading)
 *   produce structurally different constraints with different
 *   victim/beneficiary distributions.
 *
 * KEY AGENTS:
 *   - Traditional jurists: Primary agenda-setter and beneficiaryâadminister fiqh and derive institutional authority from eternal text (institutional/identity_locked/global).
 *   - Literalist communities: Primary beneficiaryâcommunal identity fused with literal hermeneutic (organized/identity_locked/global).
 *   - Anti-rationalist schools: Beneficiaryâintellectual identity constituted by subordination of reason to text (organized/identity_locked/global).
 *   - Rational theologians: Primary targetâinterpretive methods ruled out by ontological fiat (moderate/constrained/global).
 *   - Metaphorical interpreters: Targetânon-literal hermeneutic strategies foreclosed (moderate/constrained/global).
 *   - Reform movements: Targetârequire textual flexibility for modern reconciliation (organized/constrained/global).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.72).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.78).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, tangled_rope).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "Uncreated Qur'an as Eternal Ontic Constraint").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "theological/political/linguistic").

domain_priors:requires_active_enforcement(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, 'b36f185e-8005-4482-ae17-11e5be764d10').
narrative_ontology:cs_kernel_codification('b36f185e-8005-4482-ae17-11e5be764d10', fixed_text).
narrative_ontology:cs_authority_grounding('b36f185e-8005-4482-ae17-11e5be764d10', lineage).
narrative_ontology:cs_interpretation_layer_present('b36f185e-8005-4482-ae17-11e5be764d10').
narrative_ontology:cs_reading_relation('b36f185e-8005-4482-ae17-11e5be764d10', quran_ontological_status__created_reading, forecloses).
narrative_ontology:cs_reading_relation('b36f185e-8005-4482-ae17-11e5be764d10', quran_ontological_status__state_enforced_creation_reading, forecloses).
narrative_ontology:cs_axiom('b36f185e-8005-4482-ae17-11e5be764d10', foundational, quran_coeternal_with_divine_essence).
narrative_ontology:cs_axiom_status(quran_coeternal_with_divine_essence, holdable).
narrative_ontology:cs_axiom_grounding('b36f185e-8005-4482-ae17-11e5be764d10', quran_coeternal_with_divine_essence, theological).
narrative_ontology:cs_axiom('b36f185e-8005-4482-ae17-11e5be764d10', foundational, literal_meaning_fixed_and_exhaustive).
narrative_ontology:cs_axiom_status(literal_meaning_fixed_and_exhaustive, holdable).
narrative_ontology:cs_axiom_grounding('b36f185e-8005-4482-ae17-11e5be764d10', literal_meaning_fixed_and_exhaustive, theological).
narrative_ontology:cs_reference_frame('b36f185e-8005-4482-ae17-11e5be764d10', eternal_uncreated_revelation).
narrative_ontology:cs_drift_state('b36f185e-8005-4482-ae17-11e5be764d10', contemporary_modernity, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b36f185e-8005-4482-ae17-11e5be764d10', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_communities).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_schools).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, rational_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, metaphorical_interpreters).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, reform_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer classical fiqh and tafsir traditions whose authority depends on the Qur'an being an eternal, uncreated divine speech. Their institutional role, pedagogical transmission chains, and claims to interpretive finality are subsidized by the doctrine. Exit would require reconstructing their authority on a created or historicized text, which their professional identity treats as impossible.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, traditional_jurists, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, traditional_jurists, beneficiary).

% Derive communal identity and boundary maintenance from the literal accessibility of divine speech. The uncreatedness doctrine guarantees that the text is not a human artifact subject to revision or metaphorical displacement. Their social cohesion is fused with this ontological commitment; exit means dissolving the community's epistemic anchor.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, literalist_communities, beneficiary,
    organized, generational, identity_locked, global).

% Benefit from a theological framework that subordinates rational speculation to the fixed text. The doctrine delegitimizes kalam and philosophical theology as arrogance against God's eternal speech. Their intellectual identity is constituted by this subordination; exit would require embracing the rationalist methods they define themselves against.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, anti_rationalist_schools, beneficiary,
    organized, generational, identity_locked, global).

% Bear the cost of having their interpretive methodsâmetaphorical extension, historical contextualization, and rational harmonizationâruled out by ontological fiat. They are excluded from orthodox pedagogical and institutional positions and must operate in marginalized academic or dissenting spaces. Their alternatives are intellectually available but institutionally blocked.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, rational_theologians, payer,
    moderate, biographical, constrained, global).

% Pay through the foreclosure of non-literal hermeneutic strategies. Where the text's apparent meaning conflicts with scientific, ethical, or historical knowledge, they are forbidden from reading metaphorically because the uncreated doctrine treats every lexical item as fixed divine intention. Their scholarly voice is restricted to esoteric or peripheral circles.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, metaphorical_interpreters, payer,
    moderate, biographical, constrained, global).

% Require textual flexibility to reconcile scripture with modern legal, gender, or scientific norms. The uncreated doctrine locks the text to its seventh-century linguistic surface, making reform read as apostasy rather than reinterpretation. They bear the cost of a hermeneutic freeze that privileges continuity over adaptation.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, reform_movements, payer,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:fixing_cost_class(quran_ontological_status__uncreated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates communal identity, liturgical practice, and legal methodology across diverse Islamic societies by grounding them in a single, eternal, uncreated divine text that is immune to historical relativization or textual revision.
% TRANSFER_FUNCTION: Moves interpretive authority and hermeneutic legitimacy from rational theologians, metaphorical interpreters, and reform movements to traditional jurists and literalist communities, by ontologically privileging literal readings and delegitimizing innovative or historicizing interpretation.
% ABSENT_VOICES: Rational theologians (Mu'tazila and modern heirs), historical-critical scholars, and reformists who advocate for a created or historically situated Qur'an are structurally excluded from orthodox interpretive institutions and from pedagogical transmission chains.
% DISAPPEARANCE_RATIONALE: If the uncreated doctrine vanished, classical fiqh would lose its ontological foundation, traditional jurists would face hermeneutic pluralism, literalist communities would lose their epistemic anchor, and reform movements would gain institutional spaceâthe Islamic scholarly and political landscape would reorganize.
% FOUNDING_PROBLEM: How to stabilize prophetic authority and communal legal-theological coherence in the early expansion period when multiple interpretive strands, sects, and rationalist challenges threatened unity.
% FOUNDING_PROBLEM_CORROBORATION: Traditional hadith scholars and jurists attest the problem from within the beneficiary class. Critical historians and rational theologians outside the beneficiary class dispute that the specific doctrine of uncreatedness was necessary to solve it; no neutral corroboration existsâthe founding narrative is self-asserted by the seats that benefit from its persistence.
narrative_ontology:disappearance_verdict(quran_ontological_status__uncreated_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__uncreated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_ontological_status__uncreated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__uncreated_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__uncreated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_ontological_status__uncreated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.72) is high because the doctrine concentrates interpretive authority in a narrow class and suppresses alternative reading strategies. Suppression (0.78) is higher because institutional and social mechanisms actively exclude metaphorical, rationalist, and historicizing approaches. Theater ratio (0.45) reflects moderate-to-high performative maintenance of orthodoxyâsubstantial scholarly and communal energy is devoted to displaying correct belief rather than resolving genuine interpretive questions. Accessibility collapse (0.55) is moderate: alternatives exist intellectually and historically but are institutionally inaccessible within orthodox spaces. Resistance (0.60) captures persistent pushback from rational theologians and reformers despite suppression. The measurement series share one time grid and show extraction, theater, and enforcement intensifying as modern challenges pressure the doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (traditional jurists) experiences the constraint as necessary coordinationâwithout the eternal text, legal and communal coherence dissolves and relativism ensues. The payer seats (rational theologians, reform movements) experience the same structure as hermeneutic captureâtheir interpretive methods are ruled out not by argument but by ontological fiat. The engine computes this divergence from shared structural data: same constraint, opposite directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional jurists and literalist communities sit near the beneficiary end (low d): the constraint subsidizes their authority and collective identity. Rational theologians, metaphorical interpreters, and reform movements sit near the target end (high d): the constraint extracts their interpretive freedom and epistemic standing. Anti-rationalist schools are dual-positionedâthey benefit from the closed hermeneutic but are also locked into an identity that limits their own intellectual mobility.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was forged in the early theological struggles (Mihna) to stabilize prophetic authority and communal unity. The founding problemâhow to ground law and identity in revelationâwas genuine. However, the specific solution (uncreated eternal speech) has outlived its stabilizing function and now operates partly as a rent-preservation mechanism for traditional jurists. The mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) flags potential mandatrophy: the arrangement persists because rearrangement would displace beneficiary authority, not because the founding problem is still live in its original form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    uncreated_kernel_reading_ambiguity,
    'This constraint is the uncreated_reading of the quran_ontological_status kernel. Would classification change if the created_reading were adopted instead?',
    'Compare sibling constraint stories; the created_reading would redistribute beneficiaries and victims, reducing extraction for reformists and increasing it for traditional jurists.',
    'If the created reading were adopted, the constraint''s extraction profile would invert and its type might shift toward scaffold or rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(uncreated_kernel_reading_ambiguity, conceptual, 'Kernel reading ambiguity for uncreated vs created classification').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional exclusion, legal penalties for heresy) or internalized (theological conviction that doubting uncreatedness is itself apostasy)?',
    'Post-exit trajectory analysis: if former traditional jurists or literalists continue to enforce the constraint on themselves after leaving the institution, suppression is partially internalized.',
    'If internalized, effective suppression exceeds structural measure and the constraint operates more like a snare for identity-locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    founding_problem_genuineness,
    'Was the doctrine of uncreatedness developed to solve a genuine coordination problem (communal unity, legal coherence), or was it always primarily an authority-consolidation mechanism?',
    'Historical philology of 2nd-3rd century AH theological texts; detection of whether uncreatedness was a prior commitment or a retrospective justification.',
    'If primarily authority-consolidation, the coordination story is cover and the constraint leans snare; if genuine coordination, tangled_rope holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_genuineness, empirical, 'Whether the founding problem was genuine or retrospective').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__uncreated_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(qura_tr_t350, quran_ontological_status__uncreated_reading, theater_ratio, 350, 0.25).
narrative_ontology:measurement(qura_tr_t700, quran_ontological_status__uncreated_reading, theater_ratio, 700, 0.3).
narrative_ontology:measurement(qura_tr_t1050, quran_ontological_status__uncreated_reading, theater_ratio, 1050, 0.38).
narrative_ontology:measurement(qura_tr_t1400, quran_ontological_status__uncreated_reading, theater_ratio, 1400, 0.45).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__uncreated_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(qura_be_t350, quran_ontological_status__uncreated_reading, base_extractiveness, 350, 0.55).
narrative_ontology:measurement(qura_be_t700, quran_ontological_status__uncreated_reading, base_extractiveness, 700, 0.6).
narrative_ontology:measurement(qura_be_t1050, quran_ontological_status__uncreated_reading, base_extractiveness, 1050, 0.68).
narrative_ontology:measurement(qura_be_t1400, quran_ontological_status__uncreated_reading, base_extractiveness, 1400, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__uncreated_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(qura_su_t350, quran_ontological_status__uncreated_reading, suppression_requirement, 350, 0.6).
narrative_ontology:measurement(qura_su_t700, quran_ontological_status__uncreated_reading, suppression_requirement, 700, 0.65).
narrative_ontology:measurement(qura_su_t1050, quran_ontological_status__uncreated_reading, suppression_requirement, 1050, 0.72).
narrative_ontology:measurement(qura_su_t1400, quran_ontological_status__uncreated_reading, suppression_requirement, 1400, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
