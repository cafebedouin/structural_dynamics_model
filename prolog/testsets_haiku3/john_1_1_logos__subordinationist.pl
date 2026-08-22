% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__subordinationist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: john_1_1_logos__subordinationist
 *   human_readable: Subordinationist Logos Reading: Divine but Created Agency
 *   domain: theology/biblical_hermeneutics/christology
 *
 * SUMMARY:
 *   The subordinationist reading of John 1:1 interprets the Logos as the
 *   highest creation and divine agent—uncreated in a relative sense
 *   (begotten, not made) but ontologically subordinate to the Father, neither
 *   co-eternal nor consubstantial. This reading coordinates intellectual
 *   work: it preserves strict philosophical monotheism while explaining
 *   Christian claims about incarnation and revelation. It is a genuine
 *   coordination solution to a real problem (how divine transcendence
 *   reconciles with divine action). However, it operates as extraction
 *   because it systematically transfers interpretive authority away from
 *   councils and sacramental traditions whose legitimacy depends on the
 *   orthodox (non-subordinationist) reading. The constraint's persistence
 *   depends on active institutional suppression—exclusion from office,
 *   anathema, doctrinal policing—not on the subordinationist reading's
 *   inherent textual or philosophical strength. The measurement trajectory
 *   shows rising suppression and theater ratio over the 40-unit interval
 *   (early Christian era through the consolidation of conciliar authority),
 *   indicating that enforcement machinery intensified and exegetical defense
 *   increasingly became theatrical (elaborate arguments that the text could
 *   not possibly mean what it appears to say) even as the reading's
 *   extractiveness remained high. The claim/metric divergence is deliberate:
 *   the reading IS tangled rope (genuine coordination function + asymmetric
 *   extraction + active enforcement), not rope; the subordinationist movement
 *   benefits from coordination coherence while councils bear the cost of
 *   enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__subordinationist, 0.68).
domain_priors:suppression_score(john_1_1_logos__subordinationist, 0.71).
domain_priors:theater_ratio(john_1_1_logos__subordinationist, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, extractiveness, 0.68).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__subordinationist, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__subordinationist, "Subordinationist Logos Reading: Divine but Created Agency").
narrative_ontology:topic_domain(john_1_1_logos__subordinationist, "theology/biblical_hermeneutics/christology").

domain_priors:requires_active_enforcement(john_1_1_logos__subordinationist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__subordinationist, 'dac0b35f-04eb-4fb3-a078-c0d47d0c501f').
narrative_ontology:cs_kernel_codification('dac0b35f-04eb-4fb3-a078-c0d47d0c501f', fixed_text).
narrative_ontology:cs_authority_grounding('dac0b35f-04eb-4fb3-a078-c0d47d0c501f', extraction).
narrative_ontology:cs_interpretation_layer_present('dac0b35f-04eb-4fb3-a078-c0d47d0c501f').
narrative_ontology:cs_reading_relation('dac0b35f-04eb-4fb3-a078-c0d47d0c501f', john_1_1_logos__orthodox_christological, coexists_with).
narrative_ontology:cs_reading_relation('dac0b35f-04eb-4fb3-a078-c0d47d0c501f', john_1_1_logos__non_incarnational_monotheist, coexists_with).
narrative_ontology:cs_axiom('dac0b35f-04eb-4fb3-a078-c0d47d0c501f', foundational, logos_creaturely_ontology).
narrative_ontology:cs_axiom_status(logos_creaturely_ontology, holdable).
narrative_ontology:cs_axiom_grounding('dac0b35f-04eb-4fb3-a078-c0d47d0c501f', logos_creaturely_ontology, deontological).
narrative_ontology:cs_axiom('dac0b35f-04eb-4fb3-a078-c0d47d0c501f', foundational, divine_transcendence_incomparability).
narrative_ontology:cs_axiom_status(divine_transcendence_incomparability, holdable).
narrative_ontology:cs_axiom_grounding('dac0b35f-04eb-4fb3-a078-c0d47d0c501f', divine_transcendence_incomparability, empirically_contingent).
narrative_ontology:cs_axiom('dac0b35f-04eb-4fb3-a078-c0d47d0c501f', secondary, logos_mediation_via_subordination).
narrative_ontology:cs_axiom_status(logos_mediation_via_subordination, holdable).
narrative_ontology:cs_axiom_grounding('dac0b35f-04eb-4fb3-a078-c0d47d0c501f', logos_mediation_via_subordination, instrumental).
narrative_ontology:cs_reference_frame('dac0b35f-04eb-4fb3-a078-c0d47d0c501f', logos_subordinate_agent).
narrative_ontology:cs_drift_state('dac0b35f-04eb-4fb3-a078-c0d47d0c501f', post_nicene_conciliar_consolidation, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('dac0b35f-04eb-4fb3-a078-c0d47d0c501f', '2026-08-03T14:32:18Z').
narrative_ontology:cs_kernel_id(john_1_1_logos__subordinationist, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, subordinationist_theological_movement).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, philosophical_monotheist_schools).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, councils_of_nicaea_tradition).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, high_church_sacramental_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, early_christian_communities).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, early_christian_communities).
narrative_ontology:constraint_vindicates(john_1_1_logos__subordinationist, logos_creatureliness).
narrative_ontology:constraint_vindicates(john_1_1_logos__subordinationist, divine_transcendence_incomparability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teaches that Logos is the highest creation and divine agent but ontologically subordinate to the Father; not co-eternal, not consubstantial. Interprets John 1:1 as establishing Logos's unique status among creatures while maintaining radical divine transcendence. Advocates for veneration of Logos distinct from worship reserved for the Father alone. Gains institutional coherence and scriptural warrant from this reading; its theological survival depends on sustained exegetical claim to John 1:1.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, subordinationist_theological_movement, beneficiary,
    organized, generational, mobile, regional).

% Philosophers and theologians working within Platonic, Aristotelian, and Neoplatonic frameworks who need the Logos reading to preserve strict philosophical monotheism (radical divine simplicity, divine transcendence) while explaining Christian claims about divine action and revelation. The subordinationist reading reconciles scriptural narrative with philosophical monotheism by placing Logos as the highest instrument of divine action—not divine essence itself. Their intellectual project depends on this reading's viability.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, philosophical_monotheist_schools, beneficiary,
    moderate, biographical, constrained, regional).

% The ecclesiastical and theological authority structure grounded in the Councils of Nicaea (325 CE) and Constantinople (381 CE), which formally anathematized subordinationist readings and established homoousios (consubstantiality) as dogma. This tradition's legitimacy rests on the Logos being fully divine and coeternal. The subordinationist reading directly challenges the doctrinal foundation of their authority. They bear the cost of continuous enforcement—exegetical defense, ecclesiastical discipline, exclusion from communion and office—to maintain the orthodox framing against subordinationist interpretation.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, councils_of_nicaea_tradition, payer,
    institutional, civilizational, trapped, global).

% High-church traditions (Eastern Orthodox, Roman Catholic, Anglo-Catholic) whose sacramental theology and priestly authority depend on the full divinity of Logos incarnate. If Logos is created rather than divine, the Incarnation becomes the indwelling of a creature, not God; the Eucharist becomes communion with a subordinate being, not direct divine presence. This undermines the exclusivity and cosmic significance of the sacraments and the mediating authority of the priesthood. These traditions experience the subordinationist reading as a direct threat to their sacramental and hierarchical legitimacy.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, high_church_sacramental_authority, payer,
    powerful, generational, constrained, global).

% Worshipping communities in the 2nd–4th centuries navigating the decision of what to believe about Logos and how to practice worship accordingly. The subordinationist reading offers a coherent way to honor Logos (as highest creature) without committing to the metaphysical scandal of two divine beings. Once a community adopts the reading and forms its liturgy and identity around it, exit becomes identity-fused—leaving the reading means severing from the community's theological foundation. The constraint extracts unity-cost: communities divided over the reading experience schism and enforcement.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, early_christian_communities, beneficiary,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(john_1_1_logos__subordinationist, early_christian_communities, payer).

% The institutional machinery of ecumenical councils (synods, bishops, patriarchs, later popes and synodal bodies) that actively enforces orthodox Christology and excludes or disciplines subordinationist interpreters. They define which readings of John 1:1 are orthodox and which are heretical, control access to teaching authority and communion, and maintain the exclusion through anathema, excommunication, and doctrinal policing. The enforcement is the structure that keeps the subordinationist reading subordinate despite its exegetical persistence.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, ecumenical_councils_enforcer, agenda_setter,
    institutional, civilizational, analytical, global).

% The textual datum itself: John 1:1–3 and related passages permit multiple readings. The constraint operates by enforcing one reading and suppressing interpretation of alternatives, despite the text's structural ambiguity. Modern exegetical scholarship documents this ambiguity; the constraint persists by institutional closure, not textual clarity.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, scriptural_ambiguity_witness, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(john_1_1_logos__subordinationist, scriptural_ambiguity_witness).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(john_1_1_logos__subordinationist, councils_of_nicaea_tradition).
narrative_ontology:fixing_cost_class(john_1_1_logos__subordinationist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single coherent hermeneutical framework for reading John 1:1 that preserves both divine transcendence (strict monotheism) and the Christian belief in divine action and revelation through a subordinate mediating agent. Solves the intellectual problem: how can divinity act in creation and history without compromising divine transcendence? Answer: through a created but highest agent. This framework coordinates the competing demands of philosophical monotheism and scriptural narrative.
% TRANSFER_FUNCTION: Transfers authority from those who read John 1:1 as asserting full divine incarnation (orthodox councils) to those who read it as subordinationist. The reading redirects interpretive legitimacy and authorizes alternative worship practices (veneration of Logos distinct from worship of the Father). It moves institutional power from councils toward subordinationist theological movements and undermines the exclusive sacramental authority of high-church traditions by reducing the cosmic status of the incarnate being.
% ABSENT_VOICES: Subordinationist interpreters themselves are systematically excluded from authoritative councils and ecclesiastical teaching positions after Nicaea. They would articulate the exegetical case for subordinationism and the philosophical coherence of divine transcendence; instead, they are anathematized and their interpretations are suppressed in mainstream ecclesiastical discourse. Later Protestant exegetes who detect subordinationist language in the text are also marginalized or reinterpreted through orthodox lenses.
% DISAPPEARANCE_RATIONALE: If the subordinationist reading vanished as a live interpretive option, the Nicene-Constantinople framework would cease to need active enforcement; the competing hermeneutical pressure would dissolve. Sacramental theologies grounded in full divine incarnation would face no internal challenge. The intellectual coherence of philosophical monotheism within Christian frameworks would shift: the orthodox model would either stand unchallenged or face different philosophical pressures (from non-incarnational monotheism). Communities currently organized around subordinationist interpretation would reorganize their Christology or fragment.
% FOUNDING_PROBLEM: How can the transcendent God of philosophical monotheism be said to act in creation, speak to prophets, and become flesh in Jesus without violating divine simplicity, immutability, and transcendence? The subordinationist answer: God acts through a highest-created mediating agent (Logos), who is divine (hence fit to mediate divine action) but not divine-essence (hence not compromising God's transcendence). This solves the coordination problem between Jewish monotheism and Christian incarnational claims without requiring the metaphysically scandalous notion of two divine persons.
% FOUNDING_PROBLEM_CORROBORATION: Subordinationist theologians (Arius, Eusebius of Caesarea, and their intellectual heirs) attest that the founding problem is live and their reading solves it coherently. Orthodox councils (Nicaea, Constantinople) attest that the problem is false—that there is no contradiction between two divine persons and monotheism, and that subordinationism creates worse problems (denying the Incarnation's cosmic significance). Modern philosophers working on divine transcendence and action (e.g., classical theists analyzing creation and divine causation) independently attest to the force of the founding problem, though they may accept the orthodox solution or seek others. Exegetical scholars (independent of both readings) document that the textual evidence does not unambiguously rule out subordinationist interpretation, supporting the claim that a genuine exegetical problem exists.
narrative_ontology:disappearance_verdict(john_1_1_logos__subordinationist, world_rearranges).
narrative_ontology:founding_problem_status(john_1_1_logos__subordinationist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__subordinationist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(john_1_1_logos__subordinationist, 'none', 1).
narrative_ontology:epsilon_provenance(john_1_1_logos__subordinationist, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(john_1_1_logos__subordinationist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(john_1_1_logos__subordinationist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(john_1_1_logos__subordinationist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because the reading channels interpretive authority away from institutional authorities whose power derives from the orthodox reading. Suppression (0.71) is higher still because the constraint's persistence depends almost entirely on institutional enforcement (councils, excommunication, doctrinal exclusion), not on the reading's persuasiveness—the text itself permits the subordinationist interpretation, so alternatives must be actively suppressed. Theater (0.42) is moderate: there is real theological work (reconciling monotheism with incarnation) but an increasing share of effort goes to defending the orthodox reading against the plain sense of the text, especially in later periods when the exegetical case for subordinationism becomes harder to ignore. Accessibility collapse (0.64) is moderate: the subordinationist reading is available to anyone who reads John 1:1 carefully, but once institutional suppression is understood and accepted (identity fusion with council-endorsed orthodoxy), alternatives collapse. Resistance (0.58) is moderate-high: subordinationists offered real intellectual resistance from the 2nd century through Arius and beyond; the reading was not suppressed because it was weak but because it threatened conciliar power. The measurement series runs on one shared time grid (0, 5, 10, 15, 25, 40) capturing the interval from pre-Nicene to post-Constantinian consolidation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (councils) and the payer seat (high-church traditions) should perceive different types than the beneficiary seat (subordinationist movement). Councils perceive rope (we maintain orthodoxy through reasonable doctrinal discipline); high-church traditions perceive snare (we bear enforcement costs while losing authority to subordinationist pressure); subordinationists perceive rope (genuine coordination solution, freely chosen intellectual framework). The engine computes these per-seat divergences from power, exit options, and beneficiary/victim structure. The structural asymmetry: councils have institutional power and trapped options (cannot abandon orthodoxy without ceasing to exist as councils); subordinationists have moderate power and mobile options (can relocate to rival communities or intellectual movements); high-church traditions have powerful institutional positions but constrained exit (cannot adopt subordinationism without undermining sacramental claims). This asymmetry produces the divergent perceptions.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinationist theologians and philosophical schools are beneficiaries: they gain coherence, authority, and scriptural warrant for their intellectual frameworks. The councils and high-church traditions are victims: they bear the cost of continuous enforcement to suppress a reading that threatens their doctrinal monopoly. Early Christian communities are dual-positioned: they benefit from the reading's intellectual coherence but pay the cost of identity-lock (once adopted, the reading becomes constitutive of the community's identity, making exit costly). The direction of extraction runs from institutional (councils) to organized (subordinationist movements): councils monopolize interpretive authority and exclude subordinationist interpreters from positions of influence; subordinationists extract concessions through intellectual pressure and exegetical demonstration. The enforcer (councils) must continually defend the orthodox reading not because it is more obvious in the text but because it is dogmatically established and institutionally entrenched. Directionality-wise: councils d ≈ 0.15 (beneficiaries, setting the rules), subordinationist movement d ≈ 0.8 (targets, excluded from authority), high-church traditions d ≈ 0.7 (payers, bearing enforcement cost). No override needed; the structural data (beneficiary/victim + power + exit) derives the correct directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling divine transcendence with incarnation) is LIVE in the interval [0, 40] and remains contested to the present. However, the constraint persists not because the founding problem demands continuous solution but because institutional power (councils, sacramental authority) depends on enforcing the orthodox reading against subordinationist alternatives. The mandate—coherent Christology—has not atrophied; rather, it has been captured by one institutional faction (orthodox councils) that now enforces a particular solution against others. The theater ratio rising from 0.28 to 0.42 indicates that performance (ceremonial reaffirmation of orthodoxy, elaborate defenses against exegetical challenge) increasingly substitutes for genuine coordination work. This is mandatrophy: the original intellectual problem (how to reconcile monotheism and incarnation) remains real, but the constraint's primary function has shifted from solving that problem to defending institutional monopoly over its solution. A tangled rope with rising theater ratio is a candidate for mandatrophy-in-progress: the rope (coordination function) persists, but the institutional leverage it grants (extraction) increasingly becomes the constraint's operative function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_subordination,
    'Does John 1:1 and related Johannine passages textually support subordinationist interpretation, or does the text unambiguously assert Logos''s full divinity?',
    'Modern exegetical consensus and diachronic textual analysis. Examine the Johannine corpus for language of creation (poiesis), agency (arche), mediation, and divine status claims; compare to other New Testament Christologies (Pauline, Synoptic) and Second Temple Jewish mediator figures (Wisdom, Metatron). Determine whether the subordinationist reading is plausibly grounded in the text or represents a misreading driven by philosophical presuppositions.',
    'If the text ambiguously permits subordinationist reading, the suppression is unjustified institutional monopoly—classification remains tangled rope, but with high extractiveness and low legitimacy. If the text unambiguously rules out subordinationism, the reading is exegetically false and the constraint becomes snare (pure extraction defending false doctrine). If the text is genuinely indeterminate, both readings are legitimate, and the constraint should be rope (coordination of interpretation) rather than tangled rope (extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_ambiguity_subordination, empirical, 'Whether the text itself supports subordinationist reading or whether subordinationism is a philosophical imposition.').

omega_variable(
    council_legitimacy_grounding,
    'Does the legitimacy of the ecumenical councils'' authority rest on divine inspiration/infallibility of their doctrinal claims, or on institutional succession and political consolidation?',
    'Historical and theological analysis of councils'' own justifications, appeals to precedent, and bases of authority claims. Examine whether councils claim to discover true doctrine or to define orthodoxy; whether their authority is treated as infallible by contemporaries and successors; whether alternatives existed and were suppressed. Determine the epistemic grounding of conciliar authority.',
    'If councils ground authority in divine inspiration and claim infallibility, then subordinationism is false by fiat (councils cannot err), and suppression is justified discipline. If councils ground authority in institutional continuity and political consolidation, then their exclusion of subordinationism is one faction''s victory over another, not a vindication of truth, and the constraint becomes snare (institutional extraction). This determines whether mandatrophy is resolved (councils correctly solved the problem) or unresolved (councils captured the solution).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(council_legitimacy_grounding, conceptual, 'Whether conciliar authority is epistemic (truth-tracking) or political (power-consolidating).').

omega_variable(
    philosophical_monotheism_compatibility,
    'Is the orthodox trinitarian claim (one God in three divine persons, consubstantial Logos) coherent within philosophical monotheism (especially classical theism, divine simplicity, transcendence)?',
    'Contemporary analytic philosophy of religion examines whether trinitarian doctrine satisfies the constraints of coherence, divine simplicity, immutability, and transcendence. Compare trinitarian and subordinationist strategies for reconciling monotheism and incarnation; assess whether one is more coherent than the other or whether both face equal difficulties.',
    'If trinitarian orthodoxy is incoherent or less coherent than subordinationism, the constraint''s suppression defends an intellectually inferior position, and the constraint is snare (institutional extraction of assent to false doctrine). If trinitarian orthodoxy is more coherent, suppression defends genuine coordination. If both are equally coherent or equally problematic, the constraint is rope (coordination of interpretation through institutional convention), not tangled rope (extraction). This omega addresses the intellectual legitimacy of the suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(philosophical_monotheism_compatibility, empirical, 'Whether trinitarian Christology is more or less coherent than subordinationism within philosophical constraints.').

omega_variable(
    identity_locked_measurement,
    'For early Christian communities that adopted subordinationist interpretation, was the identity-lock (inability to exit without severing from community) structural (legal/social barriers) or internalized (theological identity fusion)?',
    'Examine post-exit trajectories and testimonies: Did communities that shifted from subordinationist to orthodox reading experience external persecution or internal meaning-loss? Did individual believers who left subordinationist communities report structural coercion or identity dissolution? Distinguish suppression mechanisms.',
    'If identity-lock is primarily structural (external enforcement by councils), the measured suppression (0.71) reflects coercive machinery, and the constraint''s extractiveness is justified by institutional force. If primarily internalized, suppression may persist even after enforcement is removed, and the constraint functions as a long-term identity trap (snare-like). This affects whether the constraint can be modified by reducing enforcement or requires deeper cultural/theological work.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_measurement, empirical, 'Whether the constraint''s suppressive effect is structural or internalized.').

omega_variable(
    kernel_vs_reading_distinction,
    'Is John 1:1 sufficiently determinate that the subordinationist reading is simply false (not a defensible reading), or is the kernel genuinely ambiguous such that subordinationist and orthodox are both live interpretations of the same text?',
    'Linguistic and hermeneutical analysis: examine the Johannine Greek, syntactic ambiguities, historical-critical provenance, and the range of pre-conciliar exegetical traditions. Determine whether the text permits multiple coherent readings or forecloses subordinationism on grammatical/semantic grounds.',
    'If the kernel is determinate and forecloses subordinationism, the subordinationist reading is false and should not be a live option in the constraint family (the constraint should be reclassified as defense of true doctrine, possibly rope rather than tangled rope). If the kernel is genuinely ambiguous, the three readings (subordinationist, non-incarnational, orthodox) are all legitimate interpretations of the same text, and the constraint is tangled rope (one reading extracting authority through institutional suppression). This determines whether the reading_relations should include ''forecloses'' (if the kernel is determinate) or ''coexists_with'' (if ambiguous).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_reading_distinction, conceptual, 'Whether the kernel permits multiple coherent readings or determines orthodoxy textually.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__subordinationist, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(john_tr_t0, john_1_1_logos__subordinationist, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(john_tr_t0, observed).
narrative_ontology:measurement(john_tr_t5, john_1_1_logos__subordinationist, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(john_tr_t5, observed).
narrative_ontology:measurement(john_tr_t10, john_1_1_logos__subordinationist, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(john_tr_t10, observed).
narrative_ontology:measurement(john_tr_t15, john_1_1_logos__subordinationist, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(john_tr_t15, observed).
narrative_ontology:measurement(john_tr_t25, john_1_1_logos__subordinationist, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(john_tr_t25, observed).
narrative_ontology:measurement(john_tr_t40, john_1_1_logos__subordinationist, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(john_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(john_be_t0, john_1_1_logos__subordinationist, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(john_be_t0, observed).
narrative_ontology:measurement(john_be_t5, john_1_1_logos__subordinationist, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(john_be_t5, observed).
narrative_ontology:measurement(john_be_t10, john_1_1_logos__subordinationist, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(john_be_t10, observed).
narrative_ontology:measurement(john_be_t15, john_1_1_logos__subordinationist, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(john_be_t15, observed).
narrative_ontology:measurement(john_be_t25, john_1_1_logos__subordinationist, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(john_be_t25, observed).
narrative_ontology:measurement(john_be_t40, john_1_1_logos__subordinationist, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(john_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(john_su_t0, john_1_1_logos__subordinationist, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(john_su_t0, observed).
narrative_ontology:measurement(john_su_t5, john_1_1_logos__subordinationist, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(john_su_t5, observed).
narrative_ontology:measurement(john_su_t10, john_1_1_logos__subordinationist, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(john_su_t10, observed).
narrative_ontology:measurement(john_su_t15, john_1_1_logos__subordinationist, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(john_su_t15, observed).
narrative_ontology:measurement(john_su_t25, john_1_1_logos__subordinationist, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(john_su_t25, observed).
narrative_ontology:measurement(john_su_t40, john_1_1_logos__subordinationist, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(john_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__subordinationist, identity_coordination).
narrative_ontology:boltzmann_floor_override(john_1_1_logos__subordinationist, 0.12).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__orthodox_christological).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__non_incarnational_monotheist).

% DUAL FORMULATION NOTE:
% The subordinationist reading is one of three structurally distinct constraints reading the same kernel (John 1:1). The kernel is ontologically ambiguous: it permits readings that place Logos as (1) poetic/functional (non-incarnational), (2) fully divine (orthodox), or (3) created/subordinate (subordinationist). Each reading interprets the same text but extracts authority differently and produces different constraints on worship and doctrine. The three stories form a constraint family linked via network.affects_constraints; see john_1_1_logos__orthodox_christological and john_1_1_logos__non_incarnational_monotheist for the sibling readings. The subordinationist reading is upstream to the orthodox reading in legitimacy history (subordinationism was live before Nicaea) but downstream in institutional power (Nicaea established orthodoxy as dogma, subordinationism became subordinate). The non-incarnational reading is orthogonal to both and competes for the same kernel-interpretive ground.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(john_1_1_logos__subordinationist, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
