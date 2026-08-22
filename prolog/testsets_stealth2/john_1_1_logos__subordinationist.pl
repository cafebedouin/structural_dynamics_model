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
 *   constraint_id: john_1_1_logos__subordinationist
 *   human_readable: Subordinationist Logos Settlement: First-Created Divine Agent, Venerated Not Worshipped
 *   domain: theology/christology/biblical_hermeneutics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kernel john_1_1_logos: the
 *   subordinationist reading, on which the Logos is a created being or
 *   subordinate divine agent, first and highest of creatures, not co-eternal
 *   or consubstantial with the Father. Where instantiated, the reading
 *   constitutes a working settlement: worship of the Logos is calibrated to
 *   veneration rather than full latria, sacramental exclusivity is reduced,
 *   and the parties whose authority rests on the full-divinity claim lose
 *   warrant. The colloquial label 'what John 1:1 means' decomposes, per the
 *   epsilon-invariance principle, into three structurally distinct
 *   arrangements (this file plus the orthodox_christological and
 *   non_incarnational_monotheist siblings), linked by network edges.
 *   Epsilon's referent here is the subordinationist settlement as it actually
 *   operates where instantiated, assessed by this reading's own lights,
 *   including its acknowledged discipline and displacement costs; the
 *   residual framing ambiguity is recorded in omega epsilon_referent_framing.
 *   The claimed type (tangled_rope) and the metrics are authored
 *   independently: the claim states what I believe structurally true, the
 *   metrics what I believe descriptively true, and the engine computes
 *   per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - - subordinationist_teaching_authority: Agenda-setting administrator (institutional/identity_locked) — runs the confession, its teaching corps, and its worship-boundary discipline
 *   - - strict_monotheist_communities: Primary beneficiary (organized/mobile) — receive coherent monotheistic identity with the Logos honored
 *   - - scriptural_literalist_interpreters: Beneficiary (moderate/mobile) — plain-sense reading vindicated, interpretive deference routed to them
 *   - - anti_sacramental_movements: Beneficiary (moderate/mobile) — clerical gatekeeping loses its full-divinity anchor
 *   - - rulers_seeking_doctrinal_unity: Historical beneficiary (institutional/arbitrage) — cheap unity formula, reversible commitment
 *   - - high_church_episcopal_traditions: Primary payer (institutional/constrained) — office warrant publicly denied; deposition and exile historically
 *   - - nicene_devotees: Payer (organized/identity_locked) — their central devotional act ruled out of bounds
 *   - - sacramental_officiants: Payer (moderate/constrained) — eucharistic warrant must be re-founded or dissolved
 *   - - lay_congregants: Excluded voice (powerless/trapped) — worship recalibrated by decree, exit costly in kin and standing
 *   - - historians_of_dogma: Analytical observer (analytical/analytical) — sees the full settlement cycle from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__subordinationist, 0.55).
domain_priors:suppression_score(john_1_1_logos__subordinationist, 0.52).
domain_priors:theater_ratio(john_1_1_logos__subordinationist, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, extractiveness, 0.55).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__subordinationist, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__subordinationist, "Subordinationist Logos Settlement: First-Created Divine Agent, Venerated Not Worshipped").
narrative_ontology:topic_domain(john_1_1_logos__subordinationist, "theology/christology/biblical_hermeneutics").

domain_priors:requires_active_enforcement(john_1_1_logos__subordinationist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__subordinationist, '8fc4a170-3e83-4b69-a316-431371ce35cc').
narrative_ontology:cs_kernel_codification('8fc4a170-3e83-4b69-a316-431371ce35cc', fixed_text).
narrative_ontology:cs_authority_grounding('8fc4a170-3e83-4b69-a316-431371ce35cc', lineage).
narrative_ontology:cs_interpretation_layer_present('8fc4a170-3e83-4b69-a316-431371ce35cc').
narrative_ontology:cs_reading_relation('8fc4a170-3e83-4b69-a316-431371ce35cc', john_1_1_logos__orthodox_christological, forecloses).
narrative_ontology:cs_reading_relation('8fc4a170-3e83-4b69-a316-431371ce35cc', john_1_1_logos__non_incarnational_monotheist, forecloses).
narrative_ontology:cs_axiom('8fc4a170-3e83-4b69-a316-431371ce35cc', foundational, logos_first_created_not_coeternal).
narrative_ontology:cs_axiom_status(logos_first_created_not_coeternal, holdable).
narrative_ontology:cs_axiom_grounding('8fc4a170-3e83-4b69-a316-431371ce35cc', logos_first_created_not_coeternal, theological).
narrative_ontology:cs_axiom('8fc4a170-3e83-4b69-a316-431371ce35cc', secondary, worship_latria_reserved_to_father_alone).
narrative_ontology:cs_axiom_status(worship_latria_reserved_to_father_alone, holdable).
narrative_ontology:cs_axiom_grounding('8fc4a170-3e83-4b69-a316-431371ce35cc', worship_latria_reserved_to_father_alone, theological).
narrative_ontology:cs_reference_frame('8fc4a170-3e83-4b69-a316-431371ce35cc', apostolic_subordinate_logos_kerygma).
narrative_ontology:cs_drift_state('8fc4a170-3e83-4b69-a316-431371ce35cc', post_nicene_settlement, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('8fc4a170-3e83-4b69-a316-431371ce35cc', '').
narrative_ontology:cs_kernel_id(john_1_1_logos__subordinationist, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, subordinationist_teaching_authority).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, strict_monotheist_communities).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, scriptural_literalist_interpreters).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, anti_sacramental_movements).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, rulers_seeking_doctrinal_unity).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, high_church_episcopal_traditions).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, nicene_devotees).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, sacramental_officiants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Presides over the confession that the Logos is the first and highest created agent: trains teachers, certifies the veneration-versus-worship boundary in liturgy, and disciplines members who offer the Logos the worship reserved for the Father alone. Its standing depends on the confession remaining distinct from both the co-eternity claim and the mere-personification claim; abandoning the confession would dissolve the office's reason for being. Exit would mean joining a rival communion and surrendering its accumulated teaching role.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, subordinationist_teaching_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Communities whose self-understanding rests on undivided devotion to one God, the Father. The confession lets them honor the Logos's preeminence in creation and revelation while keeping the one-God commitment intact. If unsatisfied, they can affiliate with other monotheist bodies or form new ones.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, strict_monotheist_communities, beneficiary,
    organized, generational, mobile, global).

% Teachers and readers whose authority rests on the plain sense of texts calling the Logos firstborn of all creation, only-begotten, and subordinate to the Father. The confession vindicates their reading and routes interpretive deference to them. Their skills transfer to other text-centered communities.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, scriptural_literalist_interpreters, beneficiary,
    moderate, biographical, mobile, global).

% Movements skeptical of priestly mediation and sacramental gatekeeping. With the incarnate mediator confessed as a creature rather than the co-eternal God, claims that sacraments derive exclusive efficacy from his full divinity lose their anchor, and lay-led worship becomes easier to defend. Members can relocate to congregational polities.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, anti_sacramental_movements, beneficiary,
    moderate, biographical, mobile, regional).

% Historically, emperors and princes who needed public religious cohesion and found a simple subordinationist formula cheaper to impose than adjudicating metaphysical disputes. They enforced or abandoned the settlement as political advantage shifted; their commitment was instrumental and reversible.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, rulers_seeking_doctrinal_unity, beneficiary,
    institutional, biographical, arbitrage, continental).

% Episcopal and sacramental bodies whose authority claim runs through the incarnate Logos's full divinity: orders, sees, and liturgies warranted by a priestly share in the one priesthood of the divine Word. Under a subordinationist settlement that warrant is publicly denied; holders historically faced deposition, exile, or replacement of their sees, and today face marginalization in mixed polities. Leaving would mean surrendering the succession-based office itself.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, high_church_episcopal_traditions, payer,
    institutional, generational, constrained, global).

% Believers whose devotion treats the Logos as fully divine and offers him the worship given to the Father. Under the subordinationist boundary such devotion is ruled out of bounds and subject to correction; historically they followed exiled bishops into exile, and in present-day subordinationist congregations they face discipline or quiet pressure to recalibrate. Their worship practice is constitutive of their religious self, so recalibration is experienced as self-betrayal rather than adjustment.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, nicene_devotees, payer,
    organized, generational, identity_locked, global).

% Priests and celebrants whose eucharistic and absolving authority presupposes that the incarnate one is the co-eternal God acting through them. Where the confession holds, that presupposition is denied and their office must be re-founded on lesser warrants or dissolved. Retraining into lay ministry is possible but dismantles career and standing.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, sacramental_officiants, payer,
    moderate, biographical, constrained, regional).

% Ordinary members of disputing communities during settlement episodes: their worship habits were recalibrated by conciliar decree and pulpit instruction without their participation, and leaving meant losing kin networks, burial rights, and community standing. They absorbed each reversal of the settlement.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, lay_congregants, excluded,
    powerless, biographical, trapped, local).

% Scholars reconstructing the controversy from conciliar acts, correspondence, and exile records; they can see the whole settlement cycle of imposition, resistance, reversal, and minority persistence from outside any confessional commitment.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, historians_of_dogma, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(john_1_1_logos__subordinationist, subordinationist_teaching_authority).
narrative_ontology:fixing_cost_class(john_1_1_logos__subordinationist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains unmixed monotheism while preserving the Logos's real preeminence and the plain sense of the subordinationist proof-texts; calibrates worship boundaries so a community can honor the Logos without compromising the one-God confession; supplies a unity formula simple enough for diverse polities to adopt.
% TRANSFER_FUNCTION: Moves interpretive and liturgical authority from hierarchies grounded in the full-divinity claim toward scriptural-literalist and anti-sacramental authority structures; moves devotional practice by ruling full worship of the Logos out of bounds; historically moved sees, offices, and exile sentences between rival clergy.
% ABSENT_VOICES: Lay congregants whose worship lives were recalibrated by decree without consultation, and ordinary devotees whose instinctive devotion to the Logos was ruled out of bounds; both were positioned as objects of instruction rather than participants in the settlement.
% DISAPPEARANCE_RATIONALE: Communities holding the confession would rearrange their worship practice, teaching offices, and identity overnight; historically each imposition or reversal of the settlement rearranged sees, exiles, and liturgies across the Mediterranean and Gothic worlds, and its modern disappearance would reorganize several million adherents' devotional life and authority structures.
% FOUNDING_PROBLEM: How to confess Christ's preeminence and the New Testament's subordinationist language (firstborn of all creation, only-begotten, the Father is greater than I) without compromising unmixed monotheism, preserving the one-God confession against both practical ditheism and modalist collapse of distinctions.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: pro-Nicene theologians devoted major apologetic works to answering precisely these texts (Athanasius's extended treatment of Proverbs 8 and the Gospels' subordinationist sayings concedes their force), and secular academic historiography of the fourth century attests that the exegetical tension drove the controversy. No party denies the texts exist; the dispute is over their ontological force.
narrative_ontology:disappearance_verdict(john_1_1_logos__subordinationist, world_rearranges).
narrative_ontology:founding_problem_status(john_1_1_logos__subordinationist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__subordinationist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(john_1_1_logos__subordinationist, 'none', 1).
narrative_ontology:epsilon_provenance(john_1_1_logos__subordinationist, 0.55, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored at 0.55 (end-state): the settlement genuinely coordinates monotheistic confession and worship calibration, but it also transfers jurisdiction, sees, and liturgical prerogative away from full-divinity authority structures, disciplines devotional practice, and in its historical phase included deposition and exile of rival clergy. Suppression (0.52, end-state) is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. The temporal series runs on one shared grid (318, 325, 341, 360, 381, 500, 589, 1600, 1870, 2025) with every tracked metric authored at every point: suppression rises to an imperial-capture peak at 360 (forced councils, exiles of rival bishops), decays after the 381 reversal, spikes defensively at 589 as the Gothic establishment collapses, then plateaus in the minority era. Theater stays low-moderate except a bump at 589, where formal adherence hollows during establishment collapse; the reading's exegetical and liturgical functions remain real wherever held. Accessibility collapse is low (0.35): rival readings remain fully available and exit between communions is possible, so alternatives never close off. Resistance is high (0.70): the settlement met organized Nicene resistance from 325 onward and ultimately lost the imperial contest. The claim/metric pair is deliberately unreconciled; divergence between the authored claim and any computed seat type is the measurement the corpus exists to take.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute differently. From the teaching authority's position the settlement is faithful transmission of apostolic monarchy: one God, a genuinely preeminent but created agent, worship kept clean. From the high-church payer seat the same settlement is a machine that strips warrant from their office and replaces their clergy. From the identity-locked devotee seat it forbids the central act of their religious life. There is also intra-payer divergence: institutionally powered episcopal payers with constrained exit, moderately powered officiants, and identity-locked devotees experience different effective costs from one structure, because exit options and identity fusion differ at the same nominal side of the dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: strict_monotheist_communities, scriptural_literalist_interpreters, and anti_sacramental_movements are subsidized by the settlement (identity coherence, vindicated reading, flattened gatekeeping) with mobile exit, sitting nearest the beneficiary end. rulers_seeking_doctrinal_unity combines beneficiary position with arbitrage exit, placing it nearest the beneficiary pole despite institutional power. Victim declarations drive high directionality: high_church_episcopal_traditions (institutional power, constrained exit — their office IS the disputed warrant), nicene_devotees (identity_locked — devotion constitutive of self), and sacramental_officiants (constrained — career-bound warrant) sit near the full-target end, with the identity-locked seat effectively trapped nearer it than mobility alone would predict. The teaching authority is declared a beneficiary but administers the arrangement; the engine's per-seat computation weighs the agenda-setter position against the derived beneficiary pull.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: the exegetical tension between subordinationist texts and full-divinity confession persists wherever communities read the canon closely, so the mismatch consumer should find no dead-problem-plus-world_rearranges zombie flag. The tangled_rope classification prevents mislabeling in both directions: a pure-rope reading would hide the real displacement costs borne by high-church seats and the discipline imposed on devotees; a pure-snare reading would erase the genuine monotheism-coordination that voluntary communities still seek and that survives without any state backing. Conditional drift note: if the exegetical tension were ever globally dissolved (by consensus resolution of the proof-texts), the settlement's function would atrophy and the arrangement would drift piton-ward, maintained by theatrical creed recitation; the theater bump at 589 previews that failure mode.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This story instantiates only the subordinationist reading of kernel john_1_1_logos; what structurally changes under each sibling reading?',
    'Compile and compare the sibling stories (orthodox_christological, non_incarnational_monotheist) on the shared kernel; inspect how beneficiary and victim sets invert or dissolve across readings.',
    'Under the orthodox reading the payer and beneficiary sets invert (subordinationist communities become the disciplined party); under the non-incarnational reading the veneration boundary dissolves entirely and the worship-calibration cost structure disappears. Per-seat classifications computed here are valid only for this reading''s arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings change the entire beneficiary/victim topology.').

omega_variable(
    epsilon_referent_framing,
    'Is epsilon correctly assessed over the subordinationist settlement where operative, or should the referent be the Nicene settlement this reading contests, assessed by subordinationist lights?',
    'Cross-reading referent-alignment decision at the corpus level: if the kernel''s stories are meant to share one referent (the standing arrangement under contest), re-author epsilon over the Nicene settlement as seen from this seat, which would load it with anathemas, inquisition-era coercion, and enforced latria.',
    'Shifting the referent would raise measured extraction substantially and could move computed seat types toward snare-flavored profiles; the current authoring treats each reading''s instituted arrangement as its own referent, with the engine''s per-seat computation absorbing part of the difference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_referent_framing, conceptual, 'Framing under-determination: which arrangement epsilon is about for a kernel reading whose own arrangement is the contested one.').

omega_variable(
    imperial_capture_contingency,
    'How much of the measured suppression and extraction is inherent to the confession''s boundary-maintenance versus contingent on fourth-century imperial capture?',
    'Compare subordinationist communities under state establishment (homoian empire, Gothic kingdoms) against voluntary minorities (Socinian Poland, modern unitarian bodies) on discipline intensity and displacement of rival clergy.',
    'If most coercive intensity is capture-contingent, the arrangement computes closer to rope with episodic enforcement; if intrinsic to holding the worship boundary, tangled_rope holds with higher effective extraction on the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_capture_contingency, empirical, 'Separating intrinsic enforcement cost from state-capture amplification across the settlement''s history.').

omega_variable(
    worship_boundary_necessity,
    'Is the veneration-versus-latria boundary structurally necessary to preserve monotheistic coherence, or maintainable by lighter means such as instruction without discipline?',
    'Study communities that retain the confession but dropped disciplinary enforcement of the boundary; measure whether devotional practice drifts toward full worship and whether identity coherence degrades.',
    'If the boundary is separable from enforcement, the enforcement-linked extraction component drops and the arrangement trends rope-ward; if inseparable, the discipline is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worship_boundary_necessity, conceptual, 'Whether the worship-calibration function requires the coercive machinery that generates the payer-seat costs.').

omega_variable(
    devotee_identity_lock_durability,
    'Is the identity_locked exit of full-divinity devotees constitutive (devotion cannot be recalibrated without self-dissolution) or habituated (recalibratable under sustained instruction)?',
    'Longitudinal study of converts between Nicene and subordinationist communions: track whether recalibrated devotion stabilizes or produces exit, schism, or crypto-practice.',
    'If habituated, effective extraction on that seat falls and the seat behaves as merely constrained; if constitutive, the seat sits nearer the full-target end and effective extraction rises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(devotee_identity_lock_durability, empirical, 'Durability of identity fusion on the devotee payer seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__subordinationist, 318, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(j11_sub_tr_t318, john_1_1_logos__subordinationist, theater_ratio, 318, 0.2).
narrative_ontology:measurement(j11_sub_tr_t325, john_1_1_logos__subordinationist, theater_ratio, 325, 0.24).
narrative_ontology:measurement(j11_sub_tr_t341, john_1_1_logos__subordinationist, theater_ratio, 341, 0.28).
narrative_ontology:measurement(j11_sub_tr_t360, john_1_1_logos__subordinationist, theater_ratio, 360, 0.36).
narrative_ontology:measurement(j11_sub_tr_t381, john_1_1_logos__subordinationist, theater_ratio, 381, 0.33).
narrative_ontology:measurement(j11_sub_tr_t500, john_1_1_logos__subordinationist, theater_ratio, 500, 0.26).
narrative_ontology:measurement(j11_sub_tr_t589, john_1_1_logos__subordinationist, theater_ratio, 589, 0.31).
narrative_ontology:measurement(j11_sub_tr_t1600, john_1_1_logos__subordinationist, theater_ratio, 1600, 0.22).
narrative_ontology:measurement(j11_sub_tr_t1870, john_1_1_logos__subordinationist, theater_ratio, 1870, 0.24).
narrative_ontology:measurement(j11_sub_tr_t2025, john_1_1_logos__subordinationist, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(j11_sub_be_t318, john_1_1_logos__subordinationist, base_extractiveness, 318, 0.42).
narrative_ontology:measurement(j11_sub_be_t325, john_1_1_logos__subordinationist, base_extractiveness, 325, 0.47).
narrative_ontology:measurement(j11_sub_be_t341, john_1_1_logos__subordinationist, base_extractiveness, 341, 0.54).
narrative_ontology:measurement(j11_sub_be_t360, john_1_1_logos__subordinationist, base_extractiveness, 360, 0.66).
narrative_ontology:measurement(j11_sub_be_t381, john_1_1_logos__subordinationist, base_extractiveness, 381, 0.58).
narrative_ontology:measurement(j11_sub_be_t500, john_1_1_logos__subordinationist, base_extractiveness, 500, 0.49).
narrative_ontology:measurement(j11_sub_be_t589, john_1_1_logos__subordinationist, base_extractiveness, 589, 0.43).
narrative_ontology:measurement(j11_sub_be_t1600, john_1_1_logos__subordinationist, base_extractiveness, 1600, 0.51).
narrative_ontology:measurement(j11_sub_be_t1870, john_1_1_logos__subordinationist, base_extractiveness, 1870, 0.53).
narrative_ontology:measurement(j11_sub_be_t2025, john_1_1_logos__subordinationist, base_extractiveness, 2025, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(j11_sub_su_t318, john_1_1_logos__subordinationist, suppression_requirement, 318, 0.4).
narrative_ontology:measurement(j11_sub_su_t325, john_1_1_logos__subordinationist, suppression_requirement, 325, 0.5).
narrative_ontology:measurement(j11_sub_su_t341, john_1_1_logos__subordinationist, suppression_requirement, 341, 0.62).
narrative_ontology:measurement(j11_sub_su_t360, john_1_1_logos__subordinationist, suppression_requirement, 360, 0.75).
narrative_ontology:measurement(j11_sub_su_t381, john_1_1_logos__subordinationist, suppression_requirement, 381, 0.66).
narrative_ontology:measurement(j11_sub_su_t500, john_1_1_logos__subordinationist, suppression_requirement, 500, 0.52).
narrative_ontology:measurement(j11_sub_su_t589, john_1_1_logos__subordinationist, suppression_requirement, 589, 0.57).
narrative_ontology:measurement(j11_sub_su_t1600, john_1_1_logos__subordinationist, suppression_requirement, 1600, 0.46).
narrative_ontology:measurement(j11_sub_su_t1870, john_1_1_logos__subordinationist, suppression_requirement, 1870, 0.49).
narrative_ontology:measurement(j11_sub_su_t2025, john_1_1_logos__subordinationist, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__subordinationist, identity_coordination).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__orthodox_christological).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__non_incarnational_monotheist).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the meaning of John 1:1' decomposes into three structurally distinct arrangements with different epsilon values, beneficiary/victim sets, and enforcement profiles. This file is the subordinationist member; the upstream member by empirical entrenchment is orthodox_christological (its settlement holds imperial and majority-institutional position after 381 and its enforcement machinery historically acted ON this reading's communities, which is why contamination propagates from it toward this story). The non_incarnational_monotheist member rejects the hypostasis category both readings share. Each member is epsilon-invariant on its own arrangement; no member averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
