% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__uncreated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Qur'an as Uncreated Eternal Divine Speech (Kalām Allāh Qadīm)
 *   domain: theological/philosophical/political
 *
 * SUMMARY:
 *   The Qur'an's ontological status as uncreated (qadīm) eternal divine
 *   speech (kalām Allāh) is a theological claim that entered Islamic
 *   jurisprudence and philosophy as a constraint on interpretation,
 *   authority, and institutional structure. This is ONE READING of the
 *   contested kernel 'quran_ontological_status'. The uncreated reading treats
 *   revelation as a permanent, fixed fact of reality — not contingent upon
 *   prophecy, not subject to rational modification, not an artifact produced
 *   at a point in time. By instantiating this reading, prophetic authority
 *   becomes absolute (the Qur'an was not Muhammad's creation), literalist
 *   hermeneutics become privileged (the text is eternally univocal), and
 *   rational theology becomes constrained (reason cannot adjudicate
 *   transcendent meaning). Traditional jurists and literalist communities
 *   benefit from this fixed anchor; rational theologians and reform movements
 *   pay through intellectual marginalization and doctrinal rigidity. The
 *   constraint's operation accumulates enforcement intensity between the 2nd
 *   and 6th Islamic centuries (roughly 8th–12th CE), reaching institutional
 *   stability by the Abbasid period, then maintains that intensity with
 *   theatricality increasing as the founding problem becomes less salient but
 *   the institutional solution persists.
 *
 * KEY AGENTS:
 *   - Traditional jurists: benefit from textual fixity as anchor for jurisprudence; identity-locked to this constraint
 *   - Literalist communities: benefit from doctrinal legitimacy of direct textual reading; identity-locked
 *   - Anti-rationalist schools (Hanbali, early Salafi): benefit from epistemic immunity to rational critique; identity-locked
 *   - Orthodox state authority: benefits from doctrinal enforcement as basis of legitimacy; administers the constraint
 *   - Rational theologians (Mu'tazilites, Ash'arites with ta'wīl programs): pay through intellectual marginalization; identity-locked to alternative hermeneutics
 *   - Metaphorical interpreters: pay through heresy accusation; constrained exit
 *   - Reform movements: pay through doctrinal rigidity; constrained space for textual reapplication
 *   - Philosophical rationalists: excluded from orthodox jurisprudence; their voice trapped outside institutional channels
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.62).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.71).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, mountain).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "Qur'an as Uncreated Eternal Divine Speech (Kalām Allāh Qadīm)").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "theological/philosophical/political").

domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, 'a99d8f59-ea00-4361-b9f1-9390a22a09f4').
narrative_ontology:cs_kernel_codification('a99d8f59-ea00-4361-b9f1-9390a22a09f4', fixed_text).
narrative_ontology:cs_authority_grounding('a99d8f59-ea00-4361-b9f1-9390a22a09f4', lineage).
narrative_ontology:cs_interpretation_layer_present('a99d8f59-ea00-4361-b9f1-9390a22a09f4').
narrative_ontology:cs_reading_relation('a99d8f59-ea00-4361-b9f1-9390a22a09f4', quran_ontological_status__created_reading, forecloses).
narrative_ontology:cs_reading_relation('a99d8f59-ea00-4361-b9f1-9390a22a09f4', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('a99d8f59-ea00-4361-b9f1-9390a22a09f4', foundational, quran_eternally_uncreated).
narrative_ontology:cs_axiom_status(quran_eternally_uncreated, holdable).
narrative_ontology:cs_axiom_grounding('a99d8f59-ea00-4361-b9f1-9390a22a09f4', quran_eternally_uncreated, theological).
narrative_ontology:cs_axiom('a99d8f59-ea00-4361-b9f1-9390a22a09f4', foundational, textual_meaning_divinely_fixed).
narrative_ontology:cs_axiom_status(textual_meaning_divinely_fixed, holdable).
narrative_ontology:cs_axiom_grounding('a99d8f59-ea00-4361-b9f1-9390a22a09f4', textual_meaning_divinely_fixed, deontological).
narrative_ontology:cs_reference_frame('a99d8f59-ea00-4361-b9f1-9390a22a09f4', classical_orthodox_consensus).
narrative_ontology:cs_drift_state('a99d8f59-ea00-4361-b9f1-9390a22a09f4', contemporary_islamic_scholarship, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a99d8f59-ea00-4361-b9f1-9390a22a09f4', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_communities).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_schools).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, orthodox_state_authority).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, rational_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, metaphorical_interpreters).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, reform_movements).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, mu_tazilite_advocates).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, prophetic_authority_absolute).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, textual_meaning_eternally_fixed).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, divine_speech_coeternal_with_god).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jurisprudential authority rests on the Qur'an's status as eternally valid, fixed divine law. If the text is uncreated, its meaning is permanent and univocal — interpretive disputes resolve via textual authority rather than rational reconstruction. Their institutional identity and career path depends on this status: to deny it is to dissolve the foundation of Islamic jurisprudence as they practice it.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, traditional_jurists, beneficiary,
    institutional, civilizational, identity_locked, universal).

% Direct-reading hermeneutics (reading the text as it stands, without rational mediation) is legitimated by treating the Qur'an as uncreated eternal speech. Literalist authority does not require philosophical justification if the text IS eternal truth. Accepting creation doctrine would require defending literalism on rational grounds, which undermines literalist epistemology.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, literalist_communities, beneficiary,
    organized, civilizational, identity_locked, global).

% The uncreated-Qur'an doctrine prevents reason from adjudicating divine speech. If the Qur'an is created, it becomes an object of rational scrutiny like any artifact. If eternal and uncreated, reason cannot resolve contradictions or anomalies — they must be accepted as part of transcendent meaning. Their school identity centers on this refusal of rationalist reduction.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, anti_rationalist_schools, beneficiary,
    organized, civilizational, identity_locked, universal).

% Orthodoxy requires enforcing doctrinal consensus around uncreated status. State authority uses this doctrine to legitimize its role as defender of Islamic law and correct belief. By the 10th century, this reading became institutionalized in state theology, and its suppression of the rationalist alternative became state policy (mihna in reverse: enforcing uncreated rather than created doctrine).
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, orthodox_state_authority, agenda_setter,
    institutional, generational, constrained, national).

% Cannot participate in jurisprudence or theology under uncreated-doctrine orthodoxy without intellectual compromise. Mu'tazilites, Ash'arites, and later rationalist schools must either conceal their views, accept institutional marginalization, or face suppression. Their rational hermeneutics — treating revelation as subject to philosophical analysis — is delegitimized by the constraint that the text is eternally fixed beyond reason.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, rational_theologians, payer,
    powerful, biographical, identity_locked, universal).

% Figurative or contextual reading of the Qur'an (ta'wīl, allegorical interpretation) is treated as heterodox under uncreated-doctrine enforcement. If the text is eternally uncreated, every word is literally divine, and metaphorical reading becomes a denial of truth itself. These interpreters must practice in private circles or accept accusation of heresy.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, metaphorical_interpreters, payer,
    moderate, biographical, constrained, universal).

% Modern Islamic reformers seeking to update jurisprudence or reinterpret doctrine for contemporary contexts are structurally blocked by uncreated-doctrine rigidity. If the text is eternally uncreated and univocal, reform requires either denying the constraint (costly — invites accusations of apostasy) or reinterpreting within literalist bounds (constrains what reform can achieve). Their space for textual flexibility is closed.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, reform_movements, payer,
    organized, biographical, constrained, regional).

% Would argue that all speech, including divine speech, must be contingent or subject to rational analysis. They are excluded from mainstream Islamic jurisprudence by the uncreated-doctrine constraint; their voice appears only in specialist philosophical circles or heterodox traditions (Ismaili philosophy, some Sufi orders). Their permanent exclusion from orthodox authority is the enforcement mechanism.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, philosophical_rationalists, excluded,
    powerful, biographical, trapped, global).

% Administers and enforces the uncreated-doctrine consensus through institutional means: educational curriculum, judicial appointment, suppression of heterodox teaching, and legitimation narratives. The state apparatus can alter the constraint if it rejects the doctrine, but doing so would destabilize the entire institutional order built on Islamic orthodoxy.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, islamic_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Academic historians and philosophers of religion document the constraint's operation without occupying any of the positioned seats. They trace how the doctrine's codification, enforcement, and resistance have shaped Islamic intellectual history.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, observational_analysts, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__uncreated_reading, orthodox_state_authority).
narrative_ontology:fixing_cost_class(quran_ontological_status__uncreated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified framework for Islamic jurisprudence by anchoring legal reasoning to a text treated as eternally authoritative rather than historically contingent. This unified framework solves the coordination problem of diverse Islamic communities maintaining jurisprudential consensus across vast geographies and centuries without rational fragmentation.
% TRANSFER_FUNCTION: Transfers hermeneutical authority from rational philosophers and contextual interpreters to literal textual readers and institutional jurists. Movement of meaning-making power away from those who would update or recontextualize doctrine toward those who enforce textual stability. Also transfers intellectual legitimacy from rationalist theology toward literalist and anti-rationalist schools.
% ABSENT_VOICES: Philosophical rationalists (Mu'tazilites, Ismaili philosophers, later rationalist Ash'arites with metaphorical reading programs) are structurally excluded from orthodox jurisprudence. They would argue the text is created and therefore philosophically contingent, subject to rational interpretation and contextual reapplication. Their exclusion from mainstream institutions is the enforcement mechanism itself.
% DISAPPEARANCE_RATIONALE: If the uncreated-doctrine constraint disappeared overnight, Islamic jurisprudence would lose its anchor in textual fixity. Rationalist approaches to interpretation would resurface; reform movements would gain institutional legitimacy; theological schools suppressed under orthodoxy would reorganize; the intellectual landscape would fracture into competing schools without the unifying principle of an eternally stable text. The entire structure of Islamic law and authority would require reconstruction.
% FOUNDING_PROBLEM: How can Islam maintain jurisprudential unity and preserve prophetic authority across generations and geographies without allowing the text to become subject to human revision or rational deconstruction? How can revelation be treated as permanently authoritative rather than as a historical artifact subject to contextual reinterpretation?
% FOUNDING_PROBLEM_CORROBORATION: Traditional jurists and orthodox theologians attest the problem is live and that the uncreated-doctrine solution is necessary. Rationalist philosophers and modern reformers attest the problem is either misdiagnosed (unity does not require textual immutability) or that the solution has become a cover story for institutional conservatism. Contemporary Islamic scholarship documents the constraint's persistence without consensus on whether the founding problem remains binding.
narrative_ontology:disappearance_verdict(quran_ontological_status__uncreated_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__uncreated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_ontological_status__uncreated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__uncreated_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__uncreated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, ExtMetricName, E),
    domain_priors:suppression_score(quran_ontological_status__uncreated_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quran_ontological_status__uncreated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is authored as MOUNTAIN (emerges_naturally=true) from the reading's own epistemic frame: if the Qur'an IS uncreated and eternal, this is not a human construct but an ontic fact — it emerges from reality itself, not from anyone's choice. However, the beneficiary structure (traditional jurists, literalist schools, orthodox state) is substantial enough to trigger FSM evaluation: an omega variable documents the natural-law vs.-constructed ambiguity. Extractiveness at interval-end (0.62) is lower than a pure snare because the constraint does carry a genuine coordination function (unifying jurisprudence across space and time). Suppression (0.71) is high because the constraint's persistence depends actively on excluding rationalist hermeneutics from mainstream institutions — this exclusion is not passive but enforced through institutional gatekeeping, curriculum control, and marginalization. Theater ratio (0.28) remains moderate because the textual-stability function is real; but by the medieval period, increasing proportion of enforcement energy goes to defending the doctrine against rationalist challenge rather than solving the original coordination problem. Resistance (0.59) is substantial because rational theology never stopped; Ash'arite and Maturidi schools developed sophisticated philosophical defenses that acknowledged tension with literalist doctrine, representing persistent resistance from within orthodoxy. The measurement series shows extraction and suppression rising sharply during institutionalization (0–600), then stabilizing (600–1200) at elevated levels with theater ratio climbing as the institutional answer becomes inherited rather than actively renewed. The one shared time grid enforces every metric at every point so temporal analysis has coherent data.
 *
 * PERSPECTIVAL GAP:
 *   From the traditional jurist's seat, this is a natural fact coeternal with God — a mountain of reality they do not choose but on which all jurisprudence rests. From the rational theologian's seat, it is an extractive institutional doctrine that forces intellectual compromise. From the reform movement's seat, it is a constraint that has become a cage. From the orthodox state's seat, it is a necessary anchor for institutional legitimacy. The engine computes per-seat classifications from the same structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (traditional jurists, literalist schools, orthodox state) have low directionality d — they benefit from the constraint without bearing its costs; their institutional identities are constituted by adherence to it. Victims (rational theologians, metaphorical interpreters, reformers) have high d — they pay through intellectual marginalization and doctrinal rigidity, with constrained or identity-locked exit (leaving means denying a core part of their theological commitment). Excluded actors (philosophical rationalists) have trapped exit — their hermeneutic programs are excluded from the mainstream apparatus itself. The no-directionality-override rule applies: the derived directionality from beneficiary/victim + exit + power captures the structural relationship accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   A false-summit detection candidate: the uncreated-doctrine claim presents itself as a mountain (natural, eternal, ontic), but its beneficiary structure reveals institutional interests (jurisprudential authority, literalist legitimacy, state orthodoxy). The core question an omega addresses: is the uncreated status a genuine ontic fact independent of human choice, or is it a constructed doctrine that benefits identifiable agents by appearing natural? The measurement series shows increasing theater ratio, suggesting the founding coordination problem (preserving jurisprudential unity) is increasingly solved by institutional inheritance rather than active functional necessity. At interval-end, the constraint still carries extraction and suppression, but the functional payload appears lighter. This is not mandatrophy-resolved (the founding problem remains contested and the constraint persists), but it is a candidate for mandatrophy accumulation if theater ratio continues rising and the founding problem's salience declines further.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the uncreated status of the Qur''an a genuine ontic fact independent of human choice, or is it a constructed institutional doctrine that benefits identifiable agents by appearing natural?',
    'Comparative analysis of which parties benefit from treating the claim as natural (traditional jurists, literalist schools, orthodox state) vs. which parties bear costs from its enforcement (rational theologians, reformers). If the beneficiary structure shows institutional interests served by the naturality claim, and if the doctrine''s persistence correlates with enforcement intensity rather than independent verification, the constructed hypothesis gains support. Conversely, if the doctrine persists despite active institutional opposition and at cost to powerful interests, the natural-law hypothesis gains support.',
    'If the uncreated status is genuinely natural (ontic fact), the constraint is correctly classified as a mountain and its beneficiary structure is incidental. If constructed, the constraint is a false summit and should be reclassified as tangled_rope or snare depending on enforcement intensity and victim count.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Whether the uncreated-Qur''an doctrine is a natural ontic fact or a constructed institutional doctrine masquerading as natural.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of rationalist theology structural (external institutional gatekeeping, curriculum exclusion, legal sanction) or internalized (rationalist theologians have absorbed literalist epistemology and constrain their own interpretation)?',
    'Post-suppression-lift observation: if rationalist schools reorganize and thrive after institutional suppression ends (as happened in Ismaili and some Sufi contexts), suppression was primarily structural; if rationalist scholars continue to accept literalist constraints even after institutional pressure lifts, suppression is partially internalized (the target has fused its identity with the constraint).',
    'If primarily structural, the constraint''s effective suppression (0.71) is accurately measured as external force and would drop if institutional enforcement ceased. If internalized, the effective suppression is higher than the structural measure suggests — targets carry the constraint with them even after external force ends; the constraint has become self-reinforcing through identity fusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of rationalist theology is structural institutional gatekeeping or internalized identity lock.').

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the uncreated-doctrine constraint functionally necessary for maintaining jurisprudential unity, or does unity persist independently of the claim''s truth?',
    'Comparative institutional analysis: do Islamic schools that reject the uncreated claim (some Shi''a traditions, Ismaili jurisprudence, reformed schools) maintain coherent jurisprudence without it? If yes, the doctrine is not functionally necessary for coordination; its persistence is driven by extraction and institutional interests rather than coordination value.',
    'If functionally necessary, the constraint is a rope with extraction as side effect. If unnecessary, the coordination story is a cover and the constraint is snare-flavored; theater ratio should rise further as the functional justification erodes but institutional enforcement persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether textual fixity doctrine is functionally necessary for jurisprudential unity or a cover story for institutional gatekeeping.').

omega_variable(
    reading_identity_fusion_literalism,
    'Among literalist communities benefiting from the constraint, how much of their commitment to the uncreated-reading is philosophical conviction vs. fused identity (literalist reading is who they ARE, not merely what they believe)?',
    'Ethnographic or historical analysis of identity markers in literalist communities: do members describe literalism as a choice they could revise, or as a core element of what makes them who they are? Comparative study of exit costs: what do literalist communities lose if they adopt metaphorical hermeneutics?',
    'If primarily fused identity, exit_options for literalist beneficiaries should be marked identity_locked (as currently authored). If choice-based, they could be marked constrained or mobile. High fusion indicates the constraint is stronger than the structural analysis alone suggests — beneficiaries cannot defect without existential cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_fusion_literalism, empirical, 'Whether literalist community commitment is philosophical choice or fused identity.').

omega_variable(
    prophecy_closure_vs_ongoing_revelation,
    'Does the uncreated-doctrine constraint foreclose the possibility of ongoing revelation (continuing prophethood), or is it compatible with Shi''a doctrine of continuing imamate and esoteric revelation?',
    'Textual and institutional analysis: does the uncreated-reading explicitly deny post-Muhammadan revelation, or only deny that the Qur''an is a created contingent text? If the former, the reading forecloses other revelation doctrines; if the latter, it coexists with them.',
    'If forecloses, the reading''s relationship to the created-reading is foreclosure (one rules out the other in a single framework). If coexists, the relationship is coexistence (different parties hold different readings simultaneously). Foreclosure would indicate tighter logical constraints on the kernel; coexistence would suggest the kernel admits multiple readings without internal logical contradiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prophecy_closure_vs_ongoing_revelation, conceptual, 'Whether uncreated-doctrine forecloses ongoing revelation or coexists with it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quran_uncreated_tr_t0, quran_ontological_status__uncreated_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(quran_uncreated_tr_t0, projected).
narrative_ontology:measurement(quran_uncreated_tr_t200, quran_ontological_status__uncreated_reading, theater_ratio, 200, 0.08).
narrative_ontology:measurement_basis(quran_uncreated_tr_t200, observed).
narrative_ontology:measurement(quran_uncreated_tr_t400, quran_ontological_status__uncreated_reading, theater_ratio, 400, 0.15).
narrative_ontology:measurement_basis(quran_uncreated_tr_t400, observed).
narrative_ontology:measurement(quran_uncreated_tr_t600, quran_ontological_status__uncreated_reading, theater_ratio, 600, 0.22).
narrative_ontology:measurement_basis(quran_uncreated_tr_t600, observed).
narrative_ontology:measurement(quran_uncreated_tr_t900, quran_ontological_status__uncreated_reading, theater_ratio, 900, 0.28).
narrative_ontology:measurement_basis(quran_uncreated_tr_t900, observed).
narrative_ontology:measurement(quran_uncreated_tr_t1200, quran_ontological_status__uncreated_reading, theater_ratio, 1200, 0.28).
narrative_ontology:measurement_basis(quran_uncreated_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(quran_uncreated_be_t0, quran_ontological_status__uncreated_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(quran_uncreated_be_t0, projected).
narrative_ontology:measurement(quran_uncreated_be_t200, quran_ontological_status__uncreated_reading, base_extractiveness, 200, 0.48).
narrative_ontology:measurement_basis(quran_uncreated_be_t200, observed).
narrative_ontology:measurement(quran_uncreated_be_t400, quran_ontological_status__uncreated_reading, base_extractiveness, 400, 0.58).
narrative_ontology:measurement_basis(quran_uncreated_be_t400, observed).
narrative_ontology:measurement(quran_uncreated_be_t600, quran_ontological_status__uncreated_reading, base_extractiveness, 600, 0.62).
narrative_ontology:measurement_basis(quran_uncreated_be_t600, observed).
narrative_ontology:measurement(quran_uncreated_be_t900, quran_ontological_status__uncreated_reading, base_extractiveness, 900, 0.61).
narrative_ontology:measurement_basis(quran_uncreated_be_t900, observed).
narrative_ontology:measurement(quran_uncreated_be_t1200, quran_ontological_status__uncreated_reading, base_extractiveness, 1200, 0.62).
narrative_ontology:measurement_basis(quran_uncreated_be_t1200, observed).

% Suppression requirement over time
narrative_ontology:measurement(quran_uncreated_su_t0, quran_ontological_status__uncreated_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(quran_uncreated_su_t0, projected).
narrative_ontology:measurement(quran_uncreated_su_t200, quran_ontological_status__uncreated_reading, suppression_requirement, 200, 0.48).
narrative_ontology:measurement_basis(quran_uncreated_su_t200, observed).
narrative_ontology:measurement(quran_uncreated_su_t400, quran_ontological_status__uncreated_reading, suppression_requirement, 400, 0.61).
narrative_ontology:measurement_basis(quran_uncreated_su_t400, observed).
narrative_ontology:measurement(quran_uncreated_su_t600, quran_ontological_status__uncreated_reading, suppression_requirement, 600, 0.71).
narrative_ontology:measurement_basis(quran_uncreated_su_t600, observed).
narrative_ontology:measurement(quran_uncreated_su_t900, quran_ontological_status__uncreated_reading, suppression_requirement, 900, 0.7).
narrative_ontology:measurement_basis(quran_uncreated_su_t900, observed).
narrative_ontology:measurement(quran_uncreated_su_t1200, quran_ontological_status__uncreated_reading, suppression_requirement, 1200, 0.71).
narrative_ontology:measurement_basis(quran_uncreated_su_t1200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__uncreated_reading, 0.14).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% The kernel 'quran_ontological_status' decomposes into three structurally distinct constraints corresponding to three live readings in Islamic history. The uncreated-reading (this story) treats revelation as a permanent mountain anchoring jurisprudence; the created-reading treats it as a contingent artifact subject to rational analysis; the state-enforced-creation-reading weaponizes the created claim as state policy. Each reading has distinct ε values, beneficiary/victim structures, and institutional effects. They are linked via network.affects_constraints because each reading's persistence changes the legitimacy and salience of the others. The uncreated-reading enforcement (state orthodoxy suppressing rationalism) directly shapes the conditions under which the created-reading persists as marginal; similarly, historical moments when the created-reading gains institutional purchase (like the mihna) change the reading-landscape for the uncreated-reading. The three stories together form a constraint family modeling one contested kernel under simultaneous competing readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_ontological_status__uncreated_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
