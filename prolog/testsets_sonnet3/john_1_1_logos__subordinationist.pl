% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__subordinationist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_john_1_1_logos__subordinationist, []).

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
 *   constraint_id: john_1_1_logos__subordinationist
 *   human_readable: John 1:1 Logos as First Created/Subordinate Divine Agent (Subordinationist Reading)
 *   domain: theology/biblical_hermeneutics/christology
 *
 * SUMMARY:
 *   This story authors the subordinationist reading of the John 1:1 Logos
 *   kernel: the Word is a created or derivative divine agent, first and
 *   greatest of God's creations, genuinely called 'divine' but not co-eternal
 *   or consubstantial with the Father. This is the position historically
 *   associated with Arius and his theological descendants (and, in modified
 *   form, some later movements holding a similar Christology), and it was the
 *   losing side of the 4th-century councils. Per the ε-invariance and
 *   kernel-reading rules, this file does NOT contain the
 *   orthodox_christological or non_incarnational_monotheist readings — those
 *   are separate constraints, linked here only through network edges and
 *   omega variables documenting the contest. Extraction here is authored
 *   moderate: this reading itself does not run a large coercive institutional
 *   apparatus today, but historically it depended on imperial and
 *   ecclesiastical enforcement swinging both for and against it, and its
 *   beneficiaries gain real institutional and doctrinal-identity capital at
 *   the expense of institutions whose authority claims depend on the rival
 *   reading being correct.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__subordinationist, 0.42).
domain_priors:suppression_score(john_1_1_logos__subordinationist, 0.58).
domain_priors:theater_ratio(john_1_1_logos__subordinationist, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, extractiveness, 0.42).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__subordinationist, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__subordinationist, "John 1:1 Logos as First Created/Subordinate Divine Agent (Subordinationist Reading)").
narrative_ontology:topic_domain(john_1_1_logos__subordinationist, "theology/biblical_hermeneutics/christology").

domain_priors:requires_active_enforcement(john_1_1_logos__subordinationist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__subordinationist, 'a49a8c63-118e-4454-a731-97001e432088').
narrative_ontology:cs_kernel_codification('a49a8c63-118e-4454-a731-97001e432088', fixed_text).
narrative_ontology:cs_authority_grounding('a49a8c63-118e-4454-a731-97001e432088', lineage).
narrative_ontology:cs_interpretation_layer_present('a49a8c63-118e-4454-a731-97001e432088').
narrative_ontology:cs_reading_relation('a49a8c63-118e-4454-a731-97001e432088', john_1_1_logos__orthodox_christological, forecloses).
narrative_ontology:cs_reading_relation('a49a8c63-118e-4454-a731-97001e432088', john_1_1_logos__non_incarnational_monotheist, coexists_with).
narrative_ontology:cs_axiom('a49a8c63-118e-4454-a731-97001e432088', foundational, logos_is_created_first_being).
narrative_ontology:cs_axiom_status(logos_is_created_first_being, holdable).
narrative_ontology:cs_axiom_grounding('a49a8c63-118e-4454-a731-97001e432088', logos_is_created_first_being, deontological).
narrative_ontology:cs_axiom('a49a8c63-118e-4454-a731-97001e432088', foundational, consubstantiality_denied_to_preserve_divine_unicity).
narrative_ontology:cs_axiom_status(consubstantiality_denied_to_preserve_divine_unicity, holdable).
narrative_ontology:cs_axiom_grounding('a49a8c63-118e-4454-a731-97001e432088', consubstantiality_denied_to_preserve_divine_unicity, deontological).
narrative_ontology:cs_reference_frame('a49a8c63-118e-4454-a731-97001e432088', ante_nicene_subordinationist_christology).
narrative_ontology:cs_drift_state('a49a8c63-118e-4454-a731-97001e432088', post_nicene_constantinopolitan_settlement, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('a49a8c63-118e-4454-a731-97001e432088', '').
narrative_ontology:cs_kernel_id(john_1_1_logos__subordinationist, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, subordinationist_congregations).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, arian_and_arian_descended_bodies).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, lay_readers_seeking_rational_monotheism).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, high_church_sacramental_traditions).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, trinitarian_clergy_hierarchies).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, councils_of_nicaea_dependent_institutions).
narrative_ontology:constraint_vindicates(john_1_1_logos__subordinationist, strict_monotheism_preserved).
narrative_ontology:constraint_vindicates(john_1_1_logos__subordinationist, logos_as_first_creation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold that John 1:1's 'the Word was god' (anarthrous predicate) denotes a divine but derivative being, preserving what they see as a rationally coherent, strictly monotheist framework. They gain doctrinal simplicity and freedom from Trinitarian paradox, but face exclusion from mainstream ecumenical recognition and are frequently labeled heretical by majority bodies, which limits institutional partnership and access to shared seminaries and clergy credentialing pathways.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, subordinationist_congregations, beneficiary,
    moderate, generational, constrained, national).

% Trace continuous doctrinal lineage to the reading condemned at Nicaea (325 CE) and Constantinople (381 CE). This reading vindicates their historical position and theological identity; it also keeps them permanently outside creedal Christianity's institutional recognition, a cost absorbed as the price of doctrinal consistency.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, arian_and_arian_descended_bodies, beneficiary,
    moderate, civilizational, constrained, global).

% Their sacramental system (Eucharistic real presence, apostolic succession, the authority to bind and loose) rests on Christ's full ontological divinity and consubstantiality with the Father established at Nicaea. If Logos is a created subordinate, the metaphysical warrant for treating the sacraments as channels of literal divine grace weakens, and the exclusivity claim that only this hierarchy mediates salvation becomes harder to sustain. They cannot simply exit this contest — their institutional authority is constituted by winning it.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, high_church_sacramental_traditions, payer,
    institutional, civilizational, trapped, global).

% Ordination, teaching authority, and doctrinal policing functions are built on the creedal settlement this reading rejects. A successful subordinationist reading erodes the exclusive claim to correctly transmit apostolic doctrine, threatening institutional legitimacy built over seventeen centuries of enforcement against exactly this position.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, trinitarian_clergy_hierarchies, payer,
    institutional, generational, trapped, global).

% Denominational bodies whose founding charters, creeds, and legal recognitions as 'Christian' churches in various jurisdictions depend on Nicene-Constantinopolitan orthodoxy. Their historical persecution of subordinationist readings (Arian controversy, Diet of exile, anti-heresy statutes) is the enforcement machinery that this reading's persistence would render unjustified in retrospect.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, councils_of_nicaea_dependent_institutions, payer,
    institutional, civilizational, trapped, global).

% Individual believers drawn to a reading that avoids Trinitarian logical paradox (three persons, one substance) in favor of a hierarchical but singular monotheism. They can move between congregations relatively freely compared to institutional actors, but face social costs of being viewed as doctrinally deviant by majority Christian culture.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, lay_readers_seeking_rational_monotheism, beneficiary,
    powerless, biographical, mobile, local).

% Examine the Greek grammar of John 1:1c (theos en ho logos — anarthrous predicate nominative preceding the verb) and adjudicate whether 'a god,' 'divine,' or 'God' is the more defensible rendering. Their technical findings are cited by all three kernel readings as support, though none of the readings is purely a grammatical conclusion.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, textual_critics_and_translators, observer,
    analytical, generational, analytical, global).

% The sibling reading holding Logos as ontologically identical to the Father in essence. Not a party inside this constraint story (per the ε-invariance and kernel-reading rules it is a separate constraint), but its historical dominance and creedal enforcement apparatus are the backdrop against which this reading's victims measure their loss and its beneficiaries measure their vindication.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, orthodox_christological_reading, excluded,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(john_1_1_logos__subordinationist, diffuse).
narrative_ontology:fixing_cost_class(john_1_1_logos__subordinationist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a doctrinally coherent account of monotheism that avoids the logical strain of asserting three co-equal, co-eternal persons sharing one substance, while still preserving Christ's exalted status as the first and greatest of all created beings and the agent of creation — coordinating belief around a single supreme, unoriginate God with a subordinate but genuinely divine mediator.
% TRANSFER_FUNCTION: Moves institutional legitimacy, sacramental authority, and the social capital of being recognized as 'orthodox Christianity' away from creedal hierarchies whose authority is grounded in full-divinity Christology, toward communities and lineages whose theological identity depends on the subordinationist reading being correct — reallocating who counts as authentically transmitting apostolic teaching.
% ABSENT_VOICES: Fourth-century Nicene bishops and their modern institutional heirs would object strenuously that this reading was examined and formally rejected at two ecumenical councils precisely because it was seen as compromising the deity of Christ and thus the coherence of Christian soteriology; they are represented here only through the payer stakeholders who inherit their doctrinal position, not as a live voice in this story's frame, since the story is authored from the subordinationist reading's own lights.
% DISAPPEARANCE_RATIONALE: If the subordinationist reading of John 1:1 disappeared from the theological landscape entirely, subordinationist congregations and Arian-descended bodies would lose the textual anchor for a defining doctrine central to their institutional identity — a substantial rearrangement for them. High-church sacramental traditions would experience little practical change since their own creedal reading already dominates institutional Christianity; the contest is asymmetric in stakes, which is why the verdict is contested rather than uniform across stakeholders.
% FOUNDING_PROBLEM: Early Christian communities needed to reconcile Jewish monotheism (strict oneness of God) with the exalted, pre-existent, creative role attributed to Christ in texts like John's prologem, Colossians 1, and Hebrews 1, without introducing what looked to some early theologians like a second uncreated deity alongside the Father.
% FOUNDING_PROBLEM_CORROBORATION: Historians of early Christianity (both secular and confessional, e.g. scholars of the Arian controversy writing outside any subordinationist-affiliated institution) attest that the founding problem — reconciling monotheism with an exalted, pre-existent Logos — was a genuine and live theological crisis in the 3rd-4th centuries, not merely a post-hoc justification. Whether the problem remains live today or was definitively resolved by Nicene formulation is disputed precisely along the lines of this kernel's readings, so no source entirely outside the contest can adjudicate status; textual critics (an observer seat with no doctrinal stake) corroborate that the grammatical ambiguity in the Greek text itself is real and not manufactured by either side.
narrative_ontology:disappearance_verdict(john_1_1_logos__subordinationist, contested).
narrative_ontology:founding_problem_status(john_1_1_logos__subordinationist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__subordinationist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(john_1_1_logos__subordinationist, 'none', 1).
narrative_ontology:epsilon_provenance(john_1_1_logos__subordinationist, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(john_1_1_logos__subordinationist_tests).
:- end_tests(john_1_1_logos__subordinationist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects that the reading redistributes doctrinal legitimacy and institutional authority from Nicene-descended hierarchies toward subordinationist bodies, but does not by itself run an active large-scale coercive apparatus in the present era — most of the coercive history (imperial edicts, exile of bishops, anti-heresy laws) sits in the deep past. Suppression (0.58) is set higher than extractiveness because both sides of the historical controversy actively suppressed the other at different points (Arian ascendancy under Constantius II, followed by Nicene suppression of Arianism after 381 CE and its criminalization under Theodosius); the constraint has been actively enforced against, not merely debated. Theater ratio is modest (0.22): most of the activity is genuine doctrinal and textual argument rather than performance, though some modern apologetic exchanges on both sides carry theatrical, point-scoring character. Accessibility collapse is moderate-low (0.35): the grammatical and historical evidence for John 1:1 remains genuinely contested among careful readers, so alternatives have not collapsed the way they would for a settled empirical or mathematical fact. Resistance is high (0.72) because this reading meets sustained, organized doctrinal opposition from the numerically and institutionally dominant Nicene tradition.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinationist congregations and Arian-descended bodies are declared beneficiaries: the reading vindicates their theological identity and provides doctrinal grounds for their separate institutional existence, so directionality sits toward the beneficiary end for them. High-church sacramental traditions, trinitarian clergy hierarchies, and Nicaea-dependent institutions are declared victims/payers: their sacramental exclusivity and hierarchical authority claims are structurally weakened if this reading is treated as correct, and their exit option is effectively trapped — they cannot renounce Nicene Christology without dissolving their own institutional self-definition. Lay readers are beneficiaries with high mobility (low institutional lock-in), which the derivation chain should register as a milder, more exitable form of benefit than the entrenched institutional beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabelings: first, against treating this reading as pure Snare (it does solve a genuine coordination problem — reconciling strict monotheism with an exalted, active Logos — for the communities that hold it, so a Rope-only or Snare-only label would miss the real doctrinal-coherence function); second, against treating it as costless coordination (a pure Rope), since its historical operation required — and required against it — active enforcement, and its persistence continues to impose real institutional stakes on Nicene-descended bodies. Tangled Rope holds both: genuine coordination function for its adherents, asymmetric cost imposed on rival institutional authority structures, sustained by active (historically coercive, presently doctrinal-boundary-policing) enforcement on both sides of the contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grammatical_underdetermination_of_theos,
    'Does the anarthrous predicate nominative ''theos'' preceding the verb in John 1:1c (theos en ho logos) grammatically require, permit, or exclude a qualitative/subordinate reading (''the Word was divine'' / ''a god'') as opposed to the full-identity reading (''the Word was God'')?',
    'Comparative analysis of Colwell''s Rule and its exceptions across the Johannine corpus and contemporary Koine Greek usage; consensus (or documented lack thereof) among specialists in New Testament Greek grammar who are not doctrinally committed to either outcome.',
    'If the grammar genuinely underdetermines the choice between qualitative, indefinite, and definite readings, then the doctrinal contest is resolved by theological commitment brought TO the text rather than derived FROM it, which affects how much epistemic weight either reading can claim as ''the plain sense of scripture.'' If grammar strongly favors one reading, that reading''s rivals face a much higher accessibility-collapse burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grammatical_underdetermination_of_theos, empirical, 'Whether Greek grammar alone settles or merely permits the subordinationist rendering.').

omega_variable(
    kernel_reading_sibling_delta,
    'This constraint is one reading (subordinationist) of the contested John 1:1 Logos kernel. The sibling readings are orthodox_christological (Logos as co-eternal, consubstantial, fully divine) and non_incarnational_monotheist (Logos as functional/poetic language, not a distinct hypostasis). What would change structurally if a sibling reading were adopted instead?',
    'Compare the beneficiary/victim structures and ε values authored in each sibling constraint file, linked via network.affects_constraints; the orthodox reading would invert most beneficiary/victim assignments here (high-church traditions become beneficiaries, subordinationist bodies become victims of continued exclusion), while the non_incarnational reading would likely produce much lower ε across the board since no hypostatic claim is being contested.',
    'Establishes that this story''s ε (0.42) and its beneficiary/victim structure are specific to the subordinationist reading and must not be treated as representative of ''the Logos doctrine'' generally — the disagreement is located specifically at whether the Logos possesses a distinct, subordinate-but-real divine ontology (this reading and orthodox_christological agree it does; non_incarnational_monotheist denies it) and, among those who affirm it, whether that ontology is co-equal/co-eternal (orthodox) or derivative/originated (subordinationist).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_delta, conceptual, 'Locates where this reading''s structural claims would change under a sibling reading; documents the committer structure per Rule 2.').

omega_variable(
    historical_enforcement_symmetry,
    'Was the historical suppression of the subordinationist position (post-381 CE) proportionate retaliation for subordinationist suppression of the Nicene position during Arian imperial ascendancy (350s-360s CE), or does one side bear substantially greater responsibility for the coercive character of the controversy?',
    'Comparative historical analysis of imperial edicts, exile records, and church council canons under both Arian-favoring emperors (Constantius II, Valens) and Nicene-favoring emperors (Theodosius I), assessing relative severity, duration, and scope of enforcement each side deployed when it held state power.',
    'If enforcement was roughly symmetric, the suppression metric authored here (0.58) reflects a genuinely two-sided coercive history rather than one-sided persecution, which matters for whether this reading should be read primarily as historical victim or as a party equally implicated in the coercive apparatus during its own periods of ascendancy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_enforcement_symmetry, empirical, 'Whether historical enforcement of/by this reading was symmetric across its periods of power and powerlessness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__subordinationist, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(john_tr_t0, john_1_1_logos__subordinationist, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(john_tr_t0, observed).
narrative_ontology:measurement(john_tr_t300, john_1_1_logos__subordinationist, theater_ratio, 300, 0.18).
narrative_ontology:measurement_basis(john_tr_t300, observed).
narrative_ontology:measurement(john_tr_t600, john_1_1_logos__subordinationist, theater_ratio, 600, 0.2).
narrative_ontology:measurement_basis(john_tr_t600, observed).
narrative_ontology:measurement(john_tr_t1000, john_1_1_logos__subordinationist, theater_ratio, 1000, 0.2).
narrative_ontology:measurement_basis(john_tr_t1000, observed).
narrative_ontology:measurement(john_tr_t1400, john_1_1_logos__subordinationist, theater_ratio, 1400, 0.21).
narrative_ontology:measurement_basis(john_tr_t1400, observed).
narrative_ontology:measurement(john_tr_t1700, john_1_1_logos__subordinationist, theater_ratio, 1700, 0.22).
narrative_ontology:measurement_basis(john_tr_t1700, observed).

% Extraction over time
narrative_ontology:measurement(john_be_t0, john_1_1_logos__subordinationist, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(john_be_t0, observed).
narrative_ontology:measurement(john_be_t300, john_1_1_logos__subordinationist, base_extractiveness, 300, 0.35).
narrative_ontology:measurement_basis(john_be_t300, observed).
narrative_ontology:measurement(john_be_t600, john_1_1_logos__subordinationist, base_extractiveness, 600, 0.4).
narrative_ontology:measurement_basis(john_be_t600, observed).
narrative_ontology:measurement(john_be_t1000, john_1_1_logos__subordinationist, base_extractiveness, 1000, 0.38).
narrative_ontology:measurement_basis(john_be_t1000, observed).
narrative_ontology:measurement(john_be_t1400, john_1_1_logos__subordinationist, base_extractiveness, 1400, 0.4).
narrative_ontology:measurement_basis(john_be_t1400, observed).
narrative_ontology:measurement(john_be_t1700, john_1_1_logos__subordinationist, base_extractiveness, 1700, 0.42).
narrative_ontology:measurement_basis(john_be_t1700, observed).

% Suppression requirement over time
narrative_ontology:measurement(john_su_t0, john_1_1_logos__subordinationist, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(john_su_t0, observed).
narrative_ontology:measurement(john_su_t300, john_1_1_logos__subordinationist, suppression_requirement, 300, 0.75).
narrative_ontology:measurement_basis(john_su_t300, observed).
narrative_ontology:measurement(john_su_t600, john_1_1_logos__subordinationist, suppression_requirement, 600, 0.68).
narrative_ontology:measurement_basis(john_su_t600, observed).
narrative_ontology:measurement(john_su_t1000, john_1_1_logos__subordinationist, suppression_requirement, 1000, 0.55).
narrative_ontology:measurement_basis(john_su_t1000, observed).
narrative_ontology:measurement(john_su_t1400, john_1_1_logos__subordinationist, suppression_requirement, 1400, 0.5).
narrative_ontology:measurement_basis(john_su_t1400, observed).
narrative_ontology:measurement(john_su_t1700, john_1_1_logos__subordinationist, suppression_requirement, 1700, 0.58).
narrative_ontology:measurement_basis(john_su_t1700, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__subordinationist, identity_coordination).
narrative_ontology:boltzmann_floor_override(john_1_1_logos__subordinationist, 0.1).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__orthodox_christological).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__non_incarnational_monotheist).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the john_1_1_logos kernel, each authored as a separate ε-invariant constraint file per the decomposition principle. orthodox_christological authors full ontological identity between Logos and the Father (highest ε for high-church traditions since it is the reading their authority currently rests on and thus has the LOWEST extraction against them); subordinationist (this file) authors a created/derivative divine Logos (moderate ε, redistributes authority away from Nicene-descended institutions); non_incarnational_monotheist authors Logos as non-hypostatic functional language (structurally the most disruptive to ALL incarnational Christologies, likely the highest ε against both orthodox_christological's and this reading's beneficiaries, since it denies the shared premise that Logos is any kind of distinct personal agent at all). Each file has its own beneficiaries, victims, and ε; none averages across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
