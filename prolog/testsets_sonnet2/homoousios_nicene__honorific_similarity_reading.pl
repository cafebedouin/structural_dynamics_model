% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__honorific_similarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__honorific_similarity_reading, []).

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
 *   constraint_id: homoousios_nicene__honorific_similarity_reading
 *   human_readable: Homoousios as Honorific Similarity (Homoiousios-Blur Reading)
 *   domain: historical_theology/ecclesiastical_history
 *
 * SUMMARY:
 *   This story authors the honorific-similarity reading of the homoousios
 *   kernel: the claim that at and after Nicaea, 'consubstantial' functioned
 *   primarily as a marker of honorific unity or close likeness between Father
 *   and Son, not as a strict metaphysical identity claim. On this reading the
 *   term's real work was pastoral and political — holding a fractured
 *   episcopate together — and its blur with homoiousios ('like in essence')
 *   was a feature, not a corruption, during the decades between Nicaea (325)
 *   and Constantinople (381). The constraint this story is about is the
 *   standing arrangement of creedal subscription-under-ambiguity as it
 *   actually operated in that window, assessed by this reading's own lights:
 *   an arrangement that let semi-Arian moderates and apophatic theologians
 *   remain in communion while strict Nicene partisans lost their disciplinary
 *   bright line and hard subordinationists were still excluded. This is NOT
 *   the same constraint as the metaphysical_equality_reading (which holds
 *   homoousios always secured strict ontological identity) or the
 *   subordinationist_reading (which holds the term compatible with derived,
 *   unequal divinity) — those are separate constraints with separate epsilon
 *   values, linked here only by network reference to the shared kernel.
 *
 * KEY AGENTS:
 *   - semi_arian_moderates: Primary beneficiary (organized/constrained) — retains communion and influence under the blurred standard
 *   - apophatic_traditions: Secondary beneficiary (moderate/mobile) — vindicated in reluctance to specify divine essence
 *   - local_bishops_seeking_discretion: Agenda-setter (institutional/constrained) — administers subscription locally under the loosened boundary
 *   - strict_nicene_enforcers: Primary target (powerful/constrained) — loses disciplinary bright line, must re-litigate at later councils
 *   - hard_subordinationist_clergy: Secondary target (moderate/trapped) — excluded even by the loosened standard, deposed under heresy charges
 *   - laity_seeking_doctrinal_clarity: Diffuse victim (powerless/trapped) — bears confusion and unstable local rulings with no recourse
 *   - imperial_court: Excluded party (institutional/arbitrage) — wants precision for political unity, has no seat in the doctrinal contest itself
 *   - later_church_historians: Analytical observer (analytical/analytical) — reconstructs the term's actual fourth-century semantic range
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__honorific_similarity_reading, 0.52).
domain_priors:suppression_score(homoousios_nicene__honorific_similarity_reading, 0.58).
domain_priors:theater_ratio(homoousios_nicene__honorific_similarity_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__honorific_similarity_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__honorific_similarity_reading, "Homoousios as Honorific Similarity (Homoiousios-Blur Reading)").
narrative_ontology:topic_domain(homoousios_nicene__honorific_similarity_reading, "historical_theology/ecclesiastical_history").

domain_priors:requires_active_enforcement(homoousios_nicene__honorific_similarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__honorific_similarity_reading, 'a2ac996b-875c-4f80-87d9-84351a4b633a').
narrative_ontology:cs_kernel_codification('a2ac996b-875c-4f80-87d9-84351a4b633a', fixed_text).
narrative_ontology:cs_authority_grounding('a2ac996b-875c-4f80-87d9-84351a4b633a', lineage).
narrative_ontology:cs_interpretation_layer_present('a2ac996b-875c-4f80-87d9-84351a4b633a').
narrative_ontology:cs_reading_relation('a2ac996b-875c-4f80-87d9-84351a4b633a', homoousios_nicene__metaphysical_equality_reading, influences).
narrative_ontology:cs_reading_relation('a2ac996b-875c-4f80-87d9-84351a4b633a', homoousios_nicene__subordinationist_reading, forecloses).
narrative_ontology:cs_axiom('a2ac996b-875c-4f80-87d9-84351a4b633a', foundational, consubstantiality_as_regulative_analogy).
narrative_ontology:cs_axiom_status(consubstantiality_as_regulative_analogy, holdable).
narrative_ontology:cs_axiom_grounding('a2ac996b-875c-4f80-87d9-84351a4b633a', consubstantiality_as_regulative_analogy, conventional).
narrative_ontology:cs_axiom('a2ac996b-875c-4f80-87d9-84351a4b633a', foundational, essence_language_underdetermines_metaphysical_identity).
narrative_ontology:cs_axiom_status(essence_language_underdetermines_metaphysical_identity, holdable).
narrative_ontology:cs_axiom_grounding('a2ac996b-875c-4f80-87d9-84351a4b633a', essence_language_underdetermines_metaphysical_identity, conventional).
narrative_ontology:cs_reference_frame('a2ac996b-875c-4f80-87d9-84351a4b633a', nicene_formula_as_unsettled_pastoral_compromise).
narrative_ontology:cs_drift_state('a2ac996b-875c-4f80-87d9-84351a4b633a', post_constantinopolitan_settlement, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('a2ac996b-875c-4f80-87d9-84351a4b633a', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__honorific_similarity_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, semi_arian_moderates).
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, apophatic_traditions).
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, local_bishops_seeking_discretion).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, strict_nicene_enforcers).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, hard_subordinationist_clergy).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, laity_seeking_doctrinal_clarity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and theologians (largely Eastern, homoiousian-leaning) who accept 'like in essence' language and use the similarity reading of homoousios to stay inside communion while avoiding the sharper metaphysical commitments of strict Nicene identity language. The blurred boundary lets them retain sees and influence without full capitulation to Athanasian formulations.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, semi_arian_moderates, beneficiary,
    organized, generational, constrained, regional).

% Theological currents emphasizing the ultimate unknowability of divine essence treat homoousios as an honorific gesture toward unity rather than a metaphysical claim staking out the inner nature of God. The similarity reading vindicates their reluctance to specify ousia precisely and gives them room to speak of the Father-Son relation analogically.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, apophatic_traditions, beneficiary,
    moderate, civilizational, mobile, regional).

% Provincial bishops who administer creedal subscription in their own sees benefit from a reading that lets them adjudicate borderline confessions locally rather than deferring every case to an imperial or metropolitan tribunal applying a single hard metaphysical test. They set the day-to-day pastoral standard even where they do not control the council's wording.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, local_bishops_seeking_discretion, agenda_setter,
    institutional, generational, constrained, regional).

% Athanasian-aligned bishops and synods who fought for homoousios as strict numerical identity of essence find their hard-won formula diluted into a looser 'family resemblance' claim. Their disciplinary leverage against subordinationist teaching weakens because the term they relied on to draw a bright line no longer does so unambiguously; they must re-litigate the boundary at successive councils (Constantinople 381 and after) to recover precision.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, strict_nicene_enforcers, payer,
    powerful, generational, constrained, continental).

% Clergy holding an explicit ontological subordination of the Son (strong Arian or quasi-Arian positions) are still exposed to heresy charges under the similarity reading, since 'honorific unity' still requires affirming some real likeness of essence that strong subordinationism denies. The blur protects moderates but does not extend far enough to shelter this group, who are deposed or exiled under the same councils that produce the ambiguous language.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, hard_subordinationist_clergy, payer,
    moderate, biographical, trapped, regional).

% Ordinary believers and lower clergy who depend on a stable, teachable formula for catechesis experience the honorific-similarity reading as confusing: they cannot tell, from the creed alone, whether their bishop's preaching is orthodox or borderline, and local excommunications and re-baptisms driven by shifting terminological consensus fall on them without recourse.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, laity_seeking_doctrinal_clarity, payer,
    powerless, biographical, trapped, local).

% Constantinian and successor emperors wanted creedal language precise enough to secure political unity through religious uniformity; the honorific-similarity reading undercuts that project by keeping the boundary soft, yet the imperial court is not a doctrinal party to the theological dispute and has no seat in the interpretive contest itself, only the power to convene or exile.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, imperial_court, excluded,
    institutional, immediate, arbitrage, continental).

% Scholars reconstructing fourth-century Trinitarian controversy from council acts, letters, and later creedal recensions, assessing whether the term functioned at Nicaea (325) primarily as a strict identity claim or as a looser unity marker later hardened by Constantinople and the Cappadocians.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__honorific_similarity_reading, diffuse).
narrative_ontology:fixing_cost_class(homoousios_nicene__honorific_similarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single creedal term flexible enough to hold together a fractious fourth-century episcopate that agreed the Son was truly divine and truly united to the Father but disagreed sharply on how to state that union metaphysically — the blur lets bishops subscribe to one word while meaning importantly different things, avoiding an immediate schism at the moment of subscription.
% TRANSFER_FUNCTION: Moves interpretive authority from the council's own wording (which cannot be made to specify) to local bishops and pastoral discretion; moves disciplinary exposure away from moderate homoiousians and toward both the strict-identity party (who lose their bright line) and the hard subordinationists (who remain outside even the loosened boundary).
% ABSENT_VOICES: The laity who must live under whatever doctrinal ruling their local bishop reaches have no voice in the councils that produce or later re-tighten the formula; hard subordinationist clergy are present at some councils but systematically outvoted and then excluded from the settlement they helped debate.
% DISAPPEARANCE_RATIONALE: If the honorific-similarity reading of homoousios vanished — if the term were forced to bear only strict metaphysical identity or only functional subordination from the outset — the fourth-century councils would have split immediately rather than deferring the metaphysical reckoning to Constantinople 381 and the Cappadocian settlement; semi-Arian sees would have faced earlier expulsion, and the eventual pro-Nicene consensus would have had to be built by force rather than by decades of terminological accommodation.
% FOUNDING_PROBLEM: At Nicaea (325) the assembled bishops needed language that excluded Arius's explicit claim that the Son is a creature, without simultaneously forcing every attending bishop to accept a fully worked-out metaphysics of numerically identical essence that many present were unprepared, philosophically or politically, to affirm.
% FOUNDING_PROBLEM_CORROBORATION: Pro-Nicene historians (from the Cappadocians onward) attest that homoousios always meant strict identity and that the ambiguity was a temporary rhetorical necessity later clarified, not a real deficiency in the term. Independent modern patristic scholarship (outside both the strict-Nicene and semi-Arian factions) documents that the term's semantic range at Nicaea itself was genuinely unsettled and that 'consubstantial' hardened into strict identity only through the interpretive labor of Constantinople 381 and after — supporting the reading that the original founding problem (excluding Arius without demanding full metaphysical precision) was solved by ambiguity, not by identity, and that the ambiguity persisted as a live pastoral tool for at least a generation.
narrative_ontology:disappearance_verdict(homoousios_nicene__honorific_similarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__honorific_similarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__honorific_similarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_nicene__honorific_similarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__honorific_similarity_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__honorific_similarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__honorific_similarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__honorific_similarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52 (moderate) because the coordination function is real — the blur genuinely prevented an earlier, more violent schism — but it also redistributes disciplinary costs asymmetrically onto strict-identity partisans and, more severely, onto hard subordinationists who remain excluded regardless. Suppression (0.58) reflects the active conciliar and imperial machinery (exiles, depositions, re-subscription demands) required to hold the ambiguous formula in place against pressure from both directions; it rises through the 330s-360s as Constantius II's court intervenes repeatedly and falls slightly as Constantinople 381 begins resolving the ambiguity institutionally. Theater ratio (0.44) captures that a substantial share of conciliar activity in this window is performative reaffirmation of a formula whose actual content was still contested rather than settled dispute-resolution. Accessibility collapse is moderate (0.4) — genuine alternative formulations (full identity, explicit subordination) remained live options throughout the period, unlike a true mountain. Resistance is fairly high (0.62), reflecting sustained pushback from both strict Nicenes and hard subordinationists against the ambiguous middle position.
 *
 * DIRECTIONALITY LOGIC:
 *   Semi-Arian moderates and apophatic traditions sit near the beneficiary end: the blur is precisely the structural feature that lets them remain inside communion without conceding a stronger metaphysical claim they reject or cannot verify. Local bishops administering subscription are agenda-setters who gain discretionary power from the ambiguity even though they do not author the creed's wording. Strict Nicene enforcers and hard subordinationist clergy are both payers, but for different structural reasons: the former lose a disciplinary tool they built, the latter are excluded by a boundary that, however blurred, still excludes them — the reading's beneficiary set does not extend to strong ontological subordination. Laity are the most powerless payers, bearing instability with no representation in the councils producing it. The imperial court is excluded from the doctrinal contest itself (it wants an outcome, not a seat in producing one) despite holding enormous coercive power over the process.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — excluding Arius's creature-Christology without forcing immediate metaphysical precision — was live at Nicaea and substantially resolved by Constantinople 381, when the Cappadocian settlement supplied the precision the original formula deliberately withheld. Reading this arrangement as tangled_rope rather than snare or pure rope prevents two mislabelings: it would be wrong to call the ambiguity pure extraction (it did real coordination work, preventing earlier schism), and equally wrong to call it a clean rope (it required continuous imperial and conciliar enforcement, and it produced identifiable losers on both flanks, not mutual benefit). The classification tracks that the founding problem's status is genuinely contested: pro-Nicene tradition holds the term meant strict identity all along and the ambiguity was never real (problem 'dead' in the sense that clarity was never actually lacking), while independent patristic scholarship supports that real ambiguity persisted for decades and was a load-bearing pastoral tool, not merely rhetorical cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nicaea_original_intent_ambiguity,
    'Did the bishops at Nicaea in 325 intend homoousios to establish strict numerical identity of essence, or did the term''s meaning remain genuinely unsettled until later councils imposed precision retroactively?',
    'Close philological and historical analysis of the conciliar acts, contemporaneous letters (Eusebius of Caesarea''s explanatory letter to his see is a key primary witness), and comparison with how homoiousios was used by moderate parties in the immediately following decades.',
    'If original intent was strict identity, this reading is a later loosening (arguably itself a distinct constraint, a drift rather than a founding condition); if original intent was genuinely unsettled, this reading captures the term''s actual founding function and the metaphysical_equality_reading is the retroactive hardening.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nicaea_original_intent_ambiguity, empirical, 'Whether the honorific-similarity reading reflects Nicaea''s original intent or a later interpretive drift.').

omega_variable(
    kernel_framing_creed_vs_reception,
    'Is the correct kernel the creedal text itself (fixed, formalized wording from 325/381) or the decades-long reception and interpretive practice that gave the text its operative meaning?',
    'Compare classification outcomes under a text-as-kernel framing (fixed_text, authority grounded in the conciliar act) versus a reception-as-kernel framing (implicit, authority grounded in the evolving practice of bishops applying the term).',
    'Under the text-as-kernel framing, this reading looks like a contested interpretation of a stable formal object; under the reception-as-kernel framing, this reading IS the kernel during 325-381, and the metaphysical_equality_reading is better understood as a subsequent kernel-replacement (Constantinople''s re-codification) rather than a sibling reading of the same object. This story adopts the text-as-kernel framing per the manifest''s kernel_id designation, but the alternative framing would reclassify the relationship between siblings from coexisting readings to sequential kernels.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_creed_vs_reception, conceptual, 'Whether the kernel is the fixed creedal text or the evolving interpretive practice around it.').

omega_variable(
    beneficiary_apophatic_versus_evasive,
    'Does the apophatic tradition''s endorsement of the similarity reading reflect a genuine theological commitment to divine unknowability, or a strategic use of ambiguity to avoid taking a costly metaphysical position?',
    'Examine whether apophatic theologians in this period (e.g., strands within Cappadocian thought before its later hardening) maintained the same reticence about essence-language in contexts where no political cost attached to precision.',
    'If genuine, apophatic traditions are a stable beneficiary class across contexts; if strategic, their beneficiary status is contingent on the specific fourth-century political stakes and would not generalize.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_apophatic_versus_evasive, conceptual, 'Whether apophatic beneficiary status is principled or strategically contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__honorific_similarity_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_nicene__honorific_similarity_reading, theater_ratio, 325, 0.25).
narrative_ontology:measurement(homo_tr_t336, homoousios_nicene__honorific_similarity_reading, theater_ratio, 336, 0.33).
narrative_ontology:measurement(homo_tr_t350, homoousios_nicene__honorific_similarity_reading, theater_ratio, 350, 0.4).
narrative_ontology:measurement(homo_tr_t360, homoousios_nicene__honorific_similarity_reading, theater_ratio, 360, 0.48).
narrative_ontology:measurement(homo_tr_t370, homoousios_nicene__honorific_similarity_reading, theater_ratio, 370, 0.46).
narrative_ontology:measurement(homo_tr_t381, homoousios_nicene__honorific_similarity_reading, theater_ratio, 381, 0.44).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 325, 0.35).
narrative_ontology:measurement(homo_be_t336, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 336, 0.42).
narrative_ontology:measurement(homo_be_t350, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 350, 0.48).
narrative_ontology:measurement(homo_be_t360, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 360, 0.55).
narrative_ontology:measurement(homo_be_t370, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 370, 0.5).
narrative_ontology:measurement(homo_be_t381, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 381, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 325, 0.3).
narrative_ontology:measurement(homo_su_t336, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 336, 0.45).
narrative_ontology:measurement(homo_su_t350, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 350, 0.55).
narrative_ontology:measurement(homo_su_t360, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 360, 0.66).
narrative_ontology:measurement(homo_su_t370, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 370, 0.6).
narrative_ontology:measurement(homo_su_t381, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 381, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__honorific_similarity_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_nicene__honorific_similarity_reading, homoousios_nicene__metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__honorific_similarity_reading, homoousios_nicene__subordinationist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the homoousios_nicene kernel. honorific_similarity_reading (this story) authors moderate extractiveness (0.52) with tangled_rope classification, reflecting real coordination function plus asymmetric costs on strict Nicenes and hard subordinationists. metaphysical_equality_reading and subordinationist_reading are separate constraint files with their own epsilon values, beneficiary/victim sets, and classifications, reflecting the different claims each makes about what homoousios actually secures. Per the ε-invariance principle, these are not the same constraint measured three ways — they are three structurally distinct claims sharing a textual kernel and a historical episode.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
