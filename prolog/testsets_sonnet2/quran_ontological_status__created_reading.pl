% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__created_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__created_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: quran_ontological_status__created_reading
 *   human_readable: Qur'an as Created Divine Speech (Makhlūq) — Rationalist Reading
 *   domain: religious/philosophical/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading within the contested kernel of the
 *   Qur'an's ontological status: the created-speech (makhlūq) reading
 *   associated with Mu'tazilite and rationalist theology, taken here in its
 *   purely doctrinal-theological form (distinct from the sibling reading in
 *   which this same doctrine is imposed by state coercion during the mihna).
 *   Under this reading, God's absolute transcendence requires that no
 *   temporal, composite, language-instantiated object — including revealed
 *   scripture — be coeternal with the divine essence; therefore the Qur'an,
 *   as a text with grammar, sequence, and historical occasions, must be a
 *   created production of God's eternal will, not itself an eternal
 *   attribute. This reading is generated as a clean, ε-invariant constraint:
 *   it does NOT include the mihna's state-enforced-conformity mechanism (that
 *   is the sibling constraint state_enforced_creation_reading, which carries
 *   much higher suppression and a different victim/beneficiary calculus
 *   arising from imperial coercion) nor does it argue against the uncreated
 *   reading's own internal coherence (that comparison is handled at the
 *   kernel level via reading_relations, not inside this story's metrics).
 *
 * KEY AGENTS:
 *   - rationalist_theologians: primary beneficiary (organized/mobile) — gain hermeneutic authority and doctrinal centrality
 *   - traditionalist_jurists: primary target (organized/constrained) — lose the textual-fixity basis of their authority
 *   - literalist_communities: secondary target (powerless/identity_locked) — devotional identity threatened
 *   - later_sunni_synthesis: analytical observer (institutional/analytical) — eventually absorbs both readings' concerns into kalam nafsi/lafz distinction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__created_reading, 0.42).
domain_priors:suppression_score(quran_ontological_status__created_reading, 0.28).
domain_priors:theater_ratio(quran_ontological_status__created_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__created_reading, rope).
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Qur'an as Created Divine Speech (Makhlūq) — Rationalist Reading").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "religious/philosophical/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, 'a57c6b68-5d9a-439a-96d4-c345d1842d34').
narrative_ontology:cs_kernel_codification('a57c6b68-5d9a-439a-96d4-c345d1842d34', distributed).
narrative_ontology:cs_authority_grounding('a57c6b68-5d9a-439a-96d4-c345d1842d34', expertise).
narrative_ontology:cs_interpretation_layer_present('a57c6b68-5d9a-439a-96d4-c345d1842d34').
narrative_ontology:cs_reading_relation('a57c6b68-5d9a-439a-96d4-c345d1842d34', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('a57c6b68-5d9a-439a-96d4-c345d1842d34', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('a57c6b68-5d9a-439a-96d4-c345d1842d34', foundational, divine_simplicity_precludes_coeternal_composite_speech).
narrative_ontology:cs_axiom_status(divine_simplicity_precludes_coeternal_composite_speech, holdable).
narrative_ontology:cs_axiom_grounding('a57c6b68-5d9a-439a-96d4-c345d1842d34', divine_simplicity_precludes_coeternal_composite_speech, deontological).
narrative_ontology:cs_axiom('a57c6b68-5d9a-439a-96d4-c345d1842d34', secondary, rational_demonstration_has_hermeneutic_priority_over_textual_literalism).
narrative_ontology:cs_axiom_status(rational_demonstration_has_hermeneutic_priority_over_textual_literalism, holdable).
narrative_ontology:cs_axiom_grounding('a57c6b68-5d9a-439a-96d4-c345d1842d34', rational_demonstration_has_hermeneutic_priority_over_textual_literalism, instrumental).
narrative_ontology:cs_reference_frame('a57c6b68-5d9a-439a-96d4-c345d1842d34', tawhid_absolute_transcendence_premise).
narrative_ontology:cs_drift_state('a57c6b68-5d9a-439a-96d4-c345d1842d34', post_mihna_sunni_consolidation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a57c6b68-5d9a-439a-96d4-c345d1842d34', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__created_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, philosophical_schools).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, reform_movements).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, traditionalist_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, literalist_communities).
narrative_ontology:constraint_vindicates(quran_ontological_status__created_reading, divine_transcendence_doctrine).
narrative_ontology:constraint_vindicates(quran_ontological_status__created_reading, tanzih_absolute_otherness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mutakallimūn who argue that treating the Qur'an as uncreated compromises tawhid by positing something coeternal with God. They gain hermeneutic authority: if the text is a created artifact, rational demonstration (not textual fixity) becomes the arbiter of theological truth, and their discipline (kalam) becomes indispensable to correct interpretation.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, rationalist_theologians, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__created_reading, rationalist_theologians, agenda_setter).

% Falsafa-adjacent thinkers who benefit from a cosmos in which revelation is one temporal production among others, open to allegorical and rational reinterpretation. A created Qur'an allows philosophical categories to sit alongside or above literal scriptural claims without direct heresy.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, philosophical_schools, beneficiary,
    moderate, generational, mobile, regional).

% Later reformist and modernist currents draw on the created-Qur'an logic to argue that specific textual injunctions are historically situated products of a particular revelatory moment, licensing reinterpretation for changed circumstances. They benefit from the interpretive flexibility this reading opens even where they do not use Mu'tazilite vocabulary.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, reform_movements, beneficiary,
    moderate, biographical, constrained, regional).

% Ahl al-hadith and traditionalist fuqaha whose juridical and theological authority rests on the Qur'an's words being identical with God's own uncreated speech, making textual fixity itself sacred and non-negotiable. If the text is created, its wording is contingent, and their claim to simply transmit rather than interpret divine speech is undercut. They cannot exit the debate — their institutional legitimacy is staked on the opposite premise.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, traditionalist_jurists, payer,
    organized, generational, constrained, regional).

% Ordinary believers whose religious identity and devotional practice are built on reciting and encountering the Qur'an as unmediated divine speech, word for word as God speaks it. A created-Qur'an doctrine threatens the phenomenology of their worship — the sense that the words on the tongue during recitation are God's own eternal utterance rather than a historically produced artifact. Their exit is not institutional but existential: leaving this framework means renegotiating what revelation itself means to them.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_communities, payer,
    powerless, biographical, identity_locked, local).

% The Ash'ari/Maturidi settlement that eventually stabilizes the debate by distinguishing God's eternal speech (kalam nafsi) from its created recitation/inscription (lafz), attempting to absorb both readings' concerns. Observes the created-reading's arguments about transcendence while rejecting its full ontological conclusion.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, later_sunni_synthesis, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__created_reading, diffuse).
narrative_ontology:fixing_cost_class(quran_ontological_status__created_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a theological mechanism for preserving strict divine unity (tawhid) and absolute transcendence (tanzih) by refusing to locate any temporal, composite, language-bound object as coeternal with God — solving the genuine philosophical problem of how an eternal, simple God could speak in a temporally structured, multi-part text.
% TRANSFER_FUNCTION: Moves interpretive authority away from those whose position depends on textual literalism and unmediated transmission (traditionalist jurists, literalist reciters) toward those whose position depends on rational demonstration and philosophical argument (theologians, philosophers) — authority over what the Qur'an means shifts from the text's fixity to the interpreter's reasoning.
% ABSENT_VOICES: Ordinary reciters and lay literalist communities are rarely direct parties to the technical kalam debate; their devotional experience of scripture is discussed by elite theologians on both sides but they are not themselves seated at the disputation. Sufi experiential traditions, which have their own account of encountering divine speech, are also largely absent from this specific doctrinal contest.
% DISAPPEARANCE_RATIONALE: Proponents would say if the created-reading vanished, unresolved incoherence about divine simplicity and composite eternal speech would resurface with no rational-theological tools to address it, and rationalist theology would lose its central hermeneutic warrant. Traditionalists would say the world is largely unchanged for ordinary practice either way, since even under contested doctrine the Qur'an continued to be recited, transmitted, and applied in law throughout; the dispute is real but its practical stakes are contested by the parties themselves.
% FOUNDING_PROBLEM: How can an eternal, absolutely transcendent, non-composite God be truthfully said to 'speak' a text that has grammar, sequence, historical occasions of revelation, and physical instantiation in ink and recitation — without either compromising divine simplicity or reducing revelation to mere human product?
% FOUNDING_PROBLEM_CORROBORATION: Historians of Islamic theology (outside both the Mu'tazilite and traditionalist camps) attest that the underlying philosophical problem — reconciling divine transcendence with a historically situated text — remains a live question in comparative philosophy of religion, not resolved by either side's preferred doctrine; it persists independently of which faction currently holds institutional power.
narrative_ontology:disappearance_verdict(quran_ontological_status__created_reading, contested).
narrative_ontology:founding_problem_status(quran_ontological_status__created_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__created_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_ontological_status__created_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__created_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__created_reading_tests).
:- end_tests(quran_ontological_status__created_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored as moderate (0.42) rather than low or high: this reading genuinely solves a coordination problem (how to preserve strict tawhid against the incoherence of a coeternal composite text) which argues against pure extraction, but it also reallocates real interpretive authority and institutional standing away from traditionalist jurists toward rationalist theologians, which argues against zero extraction. Suppression is authored as comparatively low (0.28) BECAUSE this reading, absent the mihna's state apparatus, persists through argument, disputation, and voluntary doctrinal allegiance rather than coercion — this is the key structural distinction from the sibling state_enforced_creation_reading, which would carry much higher suppression. Resistance is moderate-high (0.55) because traditionalist and literalist communities have historically mounted sustained doctrinal and political resistance to this reading long after the mihna ended.
 *
 * PERSPECTIVAL GAP:
 *   From the rationalist theologian seat, this reading is a rope: a coordination solution to a genuine metaphysical problem (composite eternal text vs. divine simplicity), voluntarily adopted through argument, benefiting the whole community's theological coherence. From the traditionalist jurist seat, the same doctrine is experienced as an attack on the very ground of their authority — a reclassification of the sacred text from ontic bedrock to contingent artifact, undermining literalist jurisprudence and interpretive practice built over generations. The engine should register this seat divergence: agenda_setter/beneficiary seats compute closer to rope; payer seats compute closer to tangled_rope or even snare-adjacent, depending on enforcement context (which this story deliberately excludes).
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist theologians and philosophical schools are declared beneficiaries with mobile exit options — they can shift schools, cities, and patrons without threat to core identity, and the doctrine itself expands their interpretive reach. Traditionalist jurists are declared victims/payers with constrained exit — their institutional position (qadi appointments, hadith transmission chains, fiqh authority) is built on the premise this reading undermines, and they cannot simply relocate to escape the doctrinal shift without abandoning their vocation. Literalist communities are also victims but with identity_locked exit and powerless standing — the cost to them is existential/devotional rather than institutional, and they have essentially no venue to contest the doctrine directly; this asymmetry (institutional payer vs. lay identity-locked payer) is why two distinct victim groups are named rather than one generic 'traditionalists' group.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling divine transcendence with a temporally structured revealed text) remains genuinely live in comparative philosophy of religion — it is not a dead problem being used as cover. This prevents mislabeling the created-reading as pure extraction: rationalist theologians are not manufacturing a fake coordination problem to seize authority: the incoherence they identify (a composite, sequential, historically occasioned text claimed as literally coeternal with a simple eternal God) is a real philosophical tension that any serious kalam must address. The extraction lies not in raising the problem but in the fact that resolving it in this particular direction happens to also relocate interpretive authority toward exactly the party proposing the resolution — a pattern the reading_relations and founding_problem_corroboration fields are designed to surface without asserting bad faith.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    created_reading_kernel_indeterminacy,
    'Is the createdness of the Qur''an''s wording/instantiation (lafz) separable from the eternality of God''s speech-attribute (kalam nafsi) such that both this reading and the uncreated reading are partially right — or are they genuinely exclusive positions on a single question?',
    'The later Ash''ari/Maturidi synthesis (kalam nafsi vs. lafz al-Qur''an) attempted exactly this separation; whether that synthesis is a real resolution or a diplomatic evasion of an irreducible metaphysical fork is itself contested among historians of kalam and would require closer textual-philosophical analysis of classical sources (al-Ash''ari, al-Baqillani, Ibn Kullab) to adjudicate.',
    'If the synthesis is a genuine resolution, this reading and the uncreated reading are not fully exclusive (weakening any forecloses relation between them); if it is an evasion, the two readings remain in genuine logical tension and the kernel stays a real fork with no stable middle position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(created_reading_kernel_indeterminacy, conceptual, 'Whether the created/uncreated fork admits a genuine synthesis or is an irreducible logical exclusivity.').

omega_variable(
    authority_transfer_intentionality,
    'Did rationalist theologians advance the created-reading primarily to solve a genuine transcendence problem, primarily to gain interpretive authority over traditionalist jurists, or is the distinction itself unrecoverable given available historical sources?',
    'Close reading of early Mu''tazilite theological writings (e.g. Abu al-Hudhayl, al-Nazzam) for whether authority-consolidation arguments precede or follow the metaphysical arguments in the textual record, cross-checked against contemporaneous traditionalist accusations of self-interest.',
    'If intentional authority-seeking dominates, effective extraction for this reading should be revised upward; if the metaphysical concern is primary and authority transfer is a side effect, the coordination framing (rope) is better supported and extraction should stay closer to its currently authored moderate value.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_transfer_intentionality, empirical, 'Whether the doctrine''s authority-reallocation effect was a goal or a byproduct of its theological motivation.').

omega_variable(
    literalist_suppression_mechanism,
    'For literalist lay communities, is the felt threat to devotional identity from this doctrine structural (institutional pressure to accept new doctrine) or internalized (a phenomenological disruption to the experience of recitation once the createdness argument is understood)?',
    'Comparative study of communities that adopted the created-reading voluntarily versus those exposed to it only via elite disputation, examining whether devotional practice measurably changed independent of any institutional pressure.',
    'If largely internalized, the effective suppression borne by literalist communities is higher and more persistent than the structural suppression score suggests, since it would not resolve merely by removing external pressure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literalist_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism for lay literalist communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__created_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(qura_tr_t50, quran_ontological_status__created_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement(qura_tr_t100, quran_ontological_status__created_reading, theater_ratio, 100, 0.2).
narrative_ontology:measurement(qura_tr_t150, quran_ontological_status__created_reading, theater_ratio, 150, 0.22).
narrative_ontology:measurement(qura_tr_t200, quran_ontological_status__created_reading, theater_ratio, 200, 0.21).
narrative_ontology:measurement(qura_tr_t250, quran_ontological_status__created_reading, theater_ratio, 250, 0.2).
narrative_ontology:measurement(qura_tr_t300, quran_ontological_status__created_reading, theater_ratio, 300, 0.22).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__created_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(qura_be_t50, quran_ontological_status__created_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(qura_be_t100, quran_ontological_status__created_reading, base_extractiveness, 100, 0.4).
narrative_ontology:measurement(qura_be_t150, quran_ontological_status__created_reading, base_extractiveness, 150, 0.42).
narrative_ontology:measurement(qura_be_t200, quran_ontological_status__created_reading, base_extractiveness, 200, 0.4).
narrative_ontology:measurement(qura_be_t250, quran_ontological_status__created_reading, base_extractiveness, 250, 0.38).
narrative_ontology:measurement(qura_be_t300, quran_ontological_status__created_reading, base_extractiveness, 300, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quran_ontological_status__created_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__created_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__created_reading, 0.1).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'the createdness of the Qur'an controversy' into structurally distinct claims per the ε-invariance principle: (1) this story, the voluntary rationalist-theological created_reading (ε=0.42, rope-claimed, no state coercion); (2) uncreated_reading, the traditionalist premise that Qur'anic speech is eternal and coeternal with God (expected much lower ε, mountain-claimed, since traditionalists treat it as immutable revealed fact rather than negotiated coordination); (3) state_enforced_creation_reading, the same createdness doctrine but coercively imposed via the Abbasid mihna, which should carry substantially higher suppression and extraction due to imprisonment/flogging of dissenting scholars and the direct involvement of caliphal state power as an enforcement stakeholder. All three share the same underlying kernel text-object (the Qur'an's ontological status) but diverge sharply in ε and structural profile depending on whether the observable is theological argument (this story), traditionalist counter-doctrine (uncreated_reading), or imperial coercive enforcement (state_enforced_creation_reading) — exactly the kind of observable-dependent ε divergence the framework requires decomposing into separate stories rather than resolving within one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
