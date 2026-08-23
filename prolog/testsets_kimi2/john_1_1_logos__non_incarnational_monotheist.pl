% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__non_incarnational_monotheist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_john_1_1_logos__non_incarnational_monotheist, []).

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
 *   constraint_id: john_1_1_logos__non_incarnational_monotheist
 *   human_readable: Non-Incarnational Monotheist Reading of John 1:1 Logos
 *   domain: theology/biblical_hermeneutics/christology
 *
 * SUMMARY:
 *   This constraint story models the non_incarnational_monotheist reading of
 *   the John 1:1 Logos kernel. In this reading, Logos is poetic or functional
 *   language for divine wisdom, plan, or creative speechâan attribute or
 *   action of God rather than a distinct hypostasis or incarnate being. The
 *   kernel is contested: the orthodox christological reading identifies Logos
 *   with the second person of the Trinity, while the subordinationist reading
 *   treats Logos as a created, subordinate divine agent. The
 *   non-incarnational reading structurally benefits strict monotheist
 *   interpreters and interfaith allies by removing ontological plurality from
 *   the prologue, while extracting doctrinal legitimacy from Trinitarian and
 *   sacramental traditions that depend on a pre-existent divine person for
 *   christological and soteriological coherence.
 *
 * KEY AGENTS:
 *   - non_hypostatic_theologians (agenda_setter/beneficiary): Organized scholars and apologists who advance the functional reading and set interpretive boundaries against hypostatic Christology.
 *   - interfaith_monotheist_allies (beneficiary): Jewish and Muslim dialogue partners and non-Trinitarian movements who gain hermeneutical support for strict monotheism.
 *   - christological_orthodox_communities (payer): Institutional churches and theologians requiring Christ's full divinity; lose Johannine grounding for hypostatic pre-existence.
 *   - sacramental_institutions (payer): Churches whose sacramental authority and liturgical practice depend on incarnation grounded in Logos theology.
 *   - academic_biblical_scholars (observer): Neutral analytical seat observing the hermeneutical contest without institutional stake in the outcome.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__non_incarnational_monotheist, 0.58).
domain_priors:suppression_score(john_1_1_logos__non_incarnational_monotheist, 0.42).
domain_priors:theater_ratio(john_1_1_logos__non_incarnational_monotheist, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__non_incarnational_monotheist, extractiveness, 0.58).
narrative_ontology:constraint_metric(john_1_1_logos__non_incarnational_monotheist, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(john_1_1_logos__non_incarnational_monotheist, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__non_incarnational_monotheist, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(john_1_1_logos__non_incarnational_monotheist, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__non_incarnational_monotheist, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__non_incarnational_monotheist, "Non-Incarnational Monotheist Reading of John 1:1 Logos").
narrative_ontology:topic_domain(john_1_1_logos__non_incarnational_monotheist, "theology/biblical_hermeneutics/christology").

domain_priors:requires_active_enforcement(john_1_1_logos__non_incarnational_monotheist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__non_incarnational_monotheist, 'a3ae36b2-bc3d-4417-a46a-c9f548bc1d27').
narrative_ontology:cs_kernel_codification('a3ae36b2-bc3d-4417-a46a-c9f548bc1d27', fixed_text).
narrative_ontology:cs_authority_grounding('a3ae36b2-bc3d-4417-a46a-c9f548bc1d27', lineage).
narrative_ontology:cs_interpretation_layer_present('a3ae36b2-bc3d-4417-a46a-c9f548bc1d27').
narrative_ontology:cs_reading_relation('a3ae36b2-bc3d-4417-a46a-c9f548bc1d27', john_1_1_logos__orthodox_christological, forecloses).
narrative_ontology:cs_reading_relation('a3ae36b2-bc3d-4417-a46a-c9f548bc1d27', john_1_1_logos__subordinationist, forecloses).
narrative_ontology:cs_axiom('a3ae36b2-bc3d-4417-a46a-c9f548bc1d27', foundational, logos_is_non_personal_wisdom).
narrative_ontology:cs_axiom_status(logos_is_non_personal_wisdom, holdable).
narrative_ontology:cs_axiom_grounding('a3ae36b2-bc3d-4417-a46a-c9f548bc1d27', logos_is_non_personal_wisdom, theological).
narrative_ontology:cs_axiom('a3ae36b2-bc3d-4417-a46a-c9f548bc1d27', foundational, monotheism_precludes_hypostatic_plurality).
narrative_ontology:cs_axiom_status(monotheism_precludes_hypostatic_plurality, holdable).
narrative_ontology:cs_axiom_grounding('a3ae36b2-bc3d-4417-a46a-c9f548bc1d27', monotheism_precludes_hypostatic_plurality, theological).
narrative_ontology:cs_reference_frame('a3ae36b2-bc3d-4417-a46a-c9f548bc1d27', strict_monotheist_exegesis).
narrative_ontology:cs_drift_state('a3ae36b2-bc3d-4417-a46a-c9f548bc1d27', contemporary_nicene_dominance, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a3ae36b2-bc3d-4417-a46a-c9f548bc1d27', '').
narrative_ontology:cs_kernel_id(john_1_1_logos__non_incarnational_monotheist, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__non_incarnational_monotheist, non_hypostatic_theologians).
narrative_ontology:constraint_beneficiary(john_1_1_logos__non_incarnational_monotheist, interfaith_monotheist_allies).
narrative_ontology:constraint_victim(john_1_1_logos__non_incarnational_monotheist, christological_orthodox_communities).
narrative_ontology:constraint_victim(john_1_1_logos__non_incarnational_monotheist, sacramental_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advance and defend the reading that Logos in John 1:1 is functional or poetic language for divine wisdom, plan, or creative speech rather than a distinct hypostasis. Set interpretive boundaries against hypostatic and incarnational readings through scholarly publication, interfaith apologetics, and theological education. Their professional standing and community authority depend on maintaining this reading against Nicene alternatives.
narrative_ontology:constraint_stakeholder(john_1_1_logos__non_incarnational_monotheist, non_hypostatic_theologians, agenda_setter,
    organized, generational, identity_locked, global).

% Jewish, Muslim, and non-Trinitarian Christian dialogue partners who benefit from a Johannine text that affirms strict monotheism without introducing a second divine person. The reading provides them with a canonical Christian text that appears to support their rejection of ontological plurality in God.
narrative_ontology:constraint_stakeholder(john_1_1_logos__non_incarnational_monotheist, interfaith_monotheist_allies, beneficiary,
    organized, generational, identity_locked, global).

% Trinitarian churches and theologians whose doctrinal coherence depends on Logos as a pre-existent divine person. This reading denies their foundational prooftext, extracting biblical legitimacy from their christology and forcing reliance on later conciliar formulations rather than the Fourth Gospel itself.
narrative_ontology:constraint_stakeholder(john_1_1_logos__non_incarnational_monotheist, christological_orthodox_communities, payer,
    institutional, generational, identity_locked, global).

% Churches whose sacramental authority, liturgical theology, and soteriology are grounded in the incarnation of a divine Logos. Without a hypostatic Logos becoming flesh, their sacramental economy loses direct Johannine grounding and must be re-argued from other texts or traditions.
narrative_ontology:constraint_stakeholder(john_1_1_logos__non_incarnational_monotheist, sacramental_institutions, payer,
    institutional, generational, identity_locked, global).

% Neutral analytical observers in biblical studies departments who track the hermeneutical contest over John 1:1 without institutional commitment to any single christological outcome. They publish comparative exegesis and historical-critical analysis that measures the relative linguistic plausibility of each reading.
narrative_ontology:constraint_stakeholder(john_1_1_logos__non_incarnational_monotheist, academic_biblical_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(john_1_1_logos__non_incarnational_monotheist, non_hypostatic_theologians).
narrative_ontology:fixing_cost_class(john_1_1_logos__non_incarnational_monotheist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates strict monotheist interpretation of the Fourth Gospel across Jewish, Muslim, and non-Trinitarian Christian communities by reading Logos as divine wisdom or plan rather than a second person, thereby maintaining a shared boundary against theological pluralism and tri-theistic error.
% TRANSFER_FUNCTION: Moves biblical authority and doctrinal legitimacy away from Trinitarian and sacramental institutions toward communities that reject hypostatic pre-existence, transferring the Johannine prologue from evidence of incarnation to evidence of monotheist theology.
% ABSENT_VOICES: Nicene laity whose worship, spirituality, and sacramental lives depend on Logos Christology but lack representation in academic hermeneutical debates; also early patristic writers whose linguistic and theological frameworks are retroactively classified as misreadings or errors by this functional hermeneutic.
% DISAPPEARANCE_RATIONALE: If this reading vanished, strict monotheist interpreters would lose their primary Johannine warrant for rejecting a second divine person; Trinitarian communities would reclaim the prologue as direct biblical evidence of pre-existent divine personhood; sacramental theology would regain a foundational text; interfaith apologetics would require alternative strategies.
% FOUNDING_PROBLEM: How to interpret John 1:1 without introducing ontological plurality into the one God; how to read Logos in continuity with Hebrew Bible wisdom and speech traditions rather than Hellenistic hypostatic metaphysics.
% FOUNDING_PROBLEM_CORROBORATION: Jewish and Muslim interlocutors attest the problem of plurality in John 1:1 from outside the Christian beneficiary set, corroborating that strict monotheist coordination is a live concern. Trinitarian historians attest the problem was manufactured by subordinationist and anti-Nicene movements, corroborating that the status is contested rather than settled. No neutral corroborator exists; both sides speak from committed seats.
narrative_ontology:disappearance_verdict(john_1_1_logos__non_incarnational_monotheist, world_rearranges).
narrative_ontology:founding_problem_status(john_1_1_logos__non_incarnational_monotheist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__non_incarnational_monotheist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(john_1_1_logos__non_incarnational_monotheist, 'none', 1).
narrative_ontology:epsilon_provenance(john_1_1_logos__non_incarnational_monotheist, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(john_1_1_logos__non_incarnational_monotheist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(john_1_1_logos__non_incarnational_monotheist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(john_1_1_logos__non_incarnational_monotheist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high because the reading systematically transfers authority from incarnational traditions to monotheist ones by denying their foundational text. Suppression (0.42) reflects moderate discursive gatekeeping: within its sphere the reading presents functional language as the only scholarly option, but global orthodox resistance prevents higher suppression. Theater ratio (0.40) captures the increasing apologetic performance required to maintain that a text historically read as ontological is merely poetic. Accessibility collapse (0.65) is high within the reading's communityâonce the functional framework is accepted, hypostatic alternatives appear as category errorsâbut globally contested. Resistance (0.78) is very high because Nicene traditions actively defend the ontological reading across academia, liturgy, and institutional magisteria.
 *
 * PERSPECTIVAL GAP:
 *   From the non-hypostatic theologian's seat, the constraint is correct exegesis restoring biblical monotheism and coordinating interfaith dialogue; computed directionality is low. From the Trinitarian seat, the same arrangement extracts the biblical foundation of sacramental authority and christological orthodoxy; computed directionality is high. The identity_locked exit option amplifies the gap because theological commitments are often constitutive of religious identity, making cross-seat perception difficult to bridge.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-hypostatic theologians and interfaith allies are beneficiaries (low d): the constraint subsidizes their monotheist framework by providing a Johannine warrant. Christological orthodox and sacramental institutions are payers (high d): the constraint extracts doctrinal legitimacy from them by denying the textual basis of their core commitments. The academic observer sits near symmetric with analytical exit. Identity_locked exit dominates because switching readings typically requires converting theological traditions or abandoning professional community.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents two errors: (1) treating the reading as pure Rope (monotheist coordination) ignores the asymmetric extraction from Trinitarian traditions; (2) treating it as pure Snare (anti-Christian polemic) ignores the genuine coordination problem it solves for strict monotheist communities. The founding problemâhow to read John 1:1 without tri-theismâremains contested, so mandatrophy is not resolved and the arrangement is not yet classifiable as Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    logos_ontology_ambiguity,
    'Does the Johannine prologue''s Greek syntax and Septuagintal background structurally require a hypostatic reading, or is the evidence genuinely compatible with a functional/poetic Logos?',
    'Interdisciplinary linguistic analysis combining Hellenistic Greek semantics, Second Temple Jewish wisdom literature, and narrative-critical readings of the Fourth Gospel.',
    'If hypostatic readings are syntactically required, the non-incarnational reading''s extraction is higher because it overrides the text''s plain sense; if genuinely ambiguous, the reading''s coordination function gains legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(logos_ontology_ambiguity, empirical, 'Linguistic and historical ambiguity in Logos ontology').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the constraint''s persistence maintained by scholarly argumentation and interfaith apologetics alone, or by institutional gatekeeping in theological academies and publishing?',
    'Citation-network and hiring-pattern analysis in theological studies; examination of peer-review acceptance rates for hypostatic versus functional Logos proposals in key journals.',
    'If institutional gatekeeping is primary, suppression is higher than the argumentative measure suggests; if purely scholarly, suppression reflects genuine epistemic contest rather than structural exclusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus discursive suppression mechanism').

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of the john_1_1_logos kernel. What specific structural element differentiates it from the orthodox christological and subordinationist readings?',
    'Comparative stakeholder analysis across the three sibling constraints to isolate the ontological commitment that drives divergent beneficiary and victim structures.',
    'Clarifies that the locus of disagreement is the ontological status of Logos (functional language versus created being versus uncreated divine person), not merely christological title or devotional language.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural differentiation of the non-incarnational reading within the Logos kernel').

omega_variable(
    sibling_reversal_omega,
    'Would an orthodox christological reading of the same kernel reverse the beneficiary and victim sets, making non-hypostatic theologians the payers and Trinitarian communities the beneficiaries?',
    'Generate the orthodox sibling constraint and compare base_properties beneficiaries and victims; verify structural symmetry or asymmetry.',
    'If reversal is exact, the kernel is a pure zero-sum contest; if asymmetric, one reading carries additional extractive overhead beyond simple role reversal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reversal_omega, conceptual, 'Sibling reading structural reversal hypothesis').

omega_variable(
    identity_lock_uniformity,
    'Does the identity-locked exit option apply uniformly across all religious communities, or do converts between traditions demonstrate higher mobility than the identity_locked atom suggests?',
    'Longitudinal sociological studies of theological migration between Trinitarian and non-Trinitarian communities; biography analysis of theologians who changed positions on John 1:1.',
    'If mobility is higher than identity_locked implies, effective extraction for some payer agents is lower than structurally derived; the constraint may be less binding than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_uniformity, empirical, 'Identity lock uniformity across theological communities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__non_incarnational_monotheist, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(john_1_1_logos_nim_tr_t0, john_1_1_logos__non_incarnational_monotheist, theater_ratio, 0, 0.2).
narrative_ontology:measurement(john_1_1_logos_nim_tr_t25, john_1_1_logos__non_incarnational_monotheist, theater_ratio, 25, 0.25).
narrative_ontology:measurement(john_1_1_logos_nim_tr_t50, john_1_1_logos__non_incarnational_monotheist, theater_ratio, 50, 0.3).
narrative_ontology:measurement(john_1_1_logos_nim_tr_t75, john_1_1_logos__non_incarnational_monotheist, theater_ratio, 75, 0.35).
narrative_ontology:measurement(john_1_1_logos_nim_tr_t100, john_1_1_logos__non_incarnational_monotheist, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(john_1_1_logos_nim_be_t0, john_1_1_logos__non_incarnational_monotheist, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(john_1_1_logos_nim_be_t25, john_1_1_logos__non_incarnational_monotheist, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(john_1_1_logos_nim_be_t50, john_1_1_logos__non_incarnational_monotheist, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(john_1_1_logos_nim_be_t75, john_1_1_logos__non_incarnational_monotheist, base_extractiveness, 75, 0.53).
narrative_ontology:measurement(john_1_1_logos_nim_be_t100, john_1_1_logos__non_incarnational_monotheist, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(john_1_1_logos_nim_su_t0, john_1_1_logos__non_incarnational_monotheist, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(john_1_1_logos_nim_su_t25, john_1_1_logos__non_incarnational_monotheist, suppression_requirement, 25, 0.33).
narrative_ontology:measurement(john_1_1_logos_nim_su_t50, john_1_1_logos__non_incarnational_monotheist, suppression_requirement, 50, 0.36).
narrative_ontology:measurement(john_1_1_logos_nim_su_t75, john_1_1_logos__non_incarnational_monotheist, suppression_requirement, 75, 0.39).
narrative_ontology:measurement(john_1_1_logos_nim_su_t100, john_1_1_logos__non_incarnational_monotheist, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__non_incarnational_monotheist, identity_coordination).
narrative_ontology:affects_constraint(john_1_1_logos__non_incarnational_monotheist, john_1_1_logos__orthodox_christological).
narrative_ontology:affects_constraint(john_1_1_logos__non_incarnational_monotheist, john_1_1_logos__subordinationist).

% DUAL FORMULATION NOTE:
% This constraint is one member of the john_1_1_logos constraint family, decomposed per the epsilon-invariance principle. The three sibling readings have structurally distinct epsilon values, beneficiary sets, and victim sets because they make mutually exclusive ontological claims about the same text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
