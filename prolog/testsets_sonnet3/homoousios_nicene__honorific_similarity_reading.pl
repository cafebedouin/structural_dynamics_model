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
 *   constraint_id: homoousios_nicene__honorific_similarity_reading
 *   human_readable: Homoousios as Honorific Similarity (Homoiousian-Adjacent Reading)
 *   domain: historical_theology/ecclesiastical_history/philosophy_of_religion
 *
 * SUMMARY:
 *   This story instantiates the honorific-similarity reading of the
 *   homoousios kernel: the position that Nicaea's term signals
 *   honorific/functional likeness between Father and Son rather than strict
 *   numerical identity of ousia. This reading gave cover to
 *   homoiousian-sympathetic moderates and apophatic theologians who could
 *   subscribe to the creedal word while resisting a maximally precise
 *   metaphysical claim they saw as either unwarranted or church-dividing. It
 *   coordinated a fractious mid-fourth-century episcopate around shared
 *   vocabulary, but it required active policing — first by the reading's
 *   beneficiaries against being pushed toward strict identity, and later by
 *   the reading itself being squeezed out as the Cappadocian settlement (381)
 *   reasserted a stronger identity reading while retaining looser
 *   hypostasis/ousia distinctions. The rising suppression_requirement (0.25 →
 *   0.68 at Constantinople 359, receding somewhat by 381) tracks the
 *   escalating conciliar and imperial enforcement machinery (Rimini-Seleucia,
 *   Constantinople 360, the Homoian ascendancy under Constantius II) needed
 *   to hold or contest this ambiguity across sees.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__honorific_similarity_reading, 0.52).
domain_priors:suppression_score(homoousios_nicene__honorific_similarity_reading, 0.58).
domain_priors:theater_ratio(homoousios_nicene__honorific_similarity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__honorific_similarity_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__honorific_similarity_reading, "Homoousios as Honorific Similarity (Homoiousian-Adjacent Reading)").
narrative_ontology:topic_domain(homoousios_nicene__honorific_similarity_reading, "historical_theology/ecclesiastical_history/philosophy_of_religion").

domain_priors:requires_active_enforcement(homoousios_nicene__honorific_similarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__honorific_similarity_reading, '86474567-ea15-4636-a821-29bdaf811444').
narrative_ontology:cs_kernel_codification('86474567-ea15-4636-a821-29bdaf811444', formalized).
narrative_ontology:cs_authority_grounding('86474567-ea15-4636-a821-29bdaf811444', lineage).
narrative_ontology:cs_interpretation_layer_present('86474567-ea15-4636-a821-29bdaf811444').
narrative_ontology:cs_reading_relation('86474567-ea15-4636-a821-29bdaf811444', homoousios_nicene__metaphysical_equality_reading, coexists_with).
narrative_ontology:cs_reading_relation('86474567-ea15-4636-a821-29bdaf811444', homoousios_nicene__subordinationist_reading, influences).
narrative_ontology:cs_axiom('86474567-ea15-4636-a821-29bdaf811444', foundational, creedal_terms_admit_analogical_predication).
narrative_ontology:cs_axiom_status(creedal_terms_admit_analogical_predication, holdable).
narrative_ontology:cs_axiom_grounding('86474567-ea15-4636-a821-29bdaf811444', creedal_terms_admit_analogical_predication, conventional).
narrative_ontology:cs_axiom('86474567-ea15-4636-a821-29bdaf811444', foundational, honor_unity_suffices_for_communion_without_essence_identity).
narrative_ontology:cs_axiom_status(honor_unity_suffices_for_communion_without_essence_identity, overridden).
narrative_ontology:cs_axiom_grounding('86474567-ea15-4636-a821-29bdaf811444', honor_unity_suffices_for_communion_without_essence_identity, conventional).
narrative_ontology:cs_reference_frame('86474567-ea15-4636-a821-29bdaf811444', nicene_creedal_minimalism).
narrative_ontology:cs_drift_state('86474567-ea15-4636-a821-29bdaf811444', post_constantinople_381, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('86474567-ea15-4636-a821-29bdaf811444', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__honorific_similarity_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, semi_arian_moderates).
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, apophatic_traditions).
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, local_episcopal_authorities).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, strict_nicene_enforcers).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, hard_subordinationist_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and theologians (broadly homoiousian in sympathy) who read homoousios as securing likeness of the Son to the Father in glory and will without committing to strict numerical identity of essence. The similarity reading lets them affirm Nicaea's creed formally while retaining room to distinguish the persons ontologically. It protects them from the charge of either flat Sabellianism or outright Arianism, and lets them keep sees and synodical standing under councils that might otherwise have deposed them.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, semi_arian_moderates, beneficiary,
    organized, generational, constrained, regional).

% Theological currents (Cappadocian-adjacent, mystical strands) that resist reducing divine unity to a precise metaphysical formula. The honorific-similarity reading gives them cover: they can affirm the Son's honor and worship-worthiness as identical to the Father's without being forced into a technical ousia-identity claim they regard as overreaching what language about God can bear. It preserves their preferred mode of doing theology under negative/analogical predication.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, apophatic_traditions, beneficiary,
    moderate, civilizational, constrained, regional).

% Provincial bishops who administer communion, ordination, and creedal subscription in their sees. Under the similarity reading, they gain interpretive discretion: they can accept clergy and creedal formulas that use homoousios loosely, adjudicating orthodoxy locally rather than deferring wholesale to an imperial or universal council's strict formula. This expands their pastoral and disciplinary latitude and insulates their sees from external doctrinal policing, at the cost of needing to justify their looser standard when challenged by metropolitans or emperors enforcing the stricter reading.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, local_episcopal_authorities, agenda_setter,
    institutional, generational, arbitrage, regional).

% Bishops and imperial ecclesiastical authorities (in the Athanasian line) committed to homoousios as strict numerical identity of essence, treating the honorific-similarity reading as a functional re-importation of subordinationism under cover of orthodox vocabulary. They bear the cost of constant vigilance, repeated councils, and depositions to hold the strict line against a reading that borrows the same creedal word while emptying it of the content they regard as essential to defeating Arianism. Their exit is trapped: abandoning enforcement means losing the creed's substantive content; they cannot walk away from the fight without ceding the term itself.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, strict_nicene_enforcers, payer,
    institutional, civilizational, trapped, continental).

% Clergy holding an explicit ontological-subordination Christology (the Son deriving being from the Father, lesser in nature) who might expect the similarity reading's looser boundary to shelter them, but instead find themselves charged with heresy precisely because the honorific-similarity reading still requires the Son's honor and worship-worthiness to be undiminished — a line hard subordinationists cross. They are excluded from the coalition the similarity reading actually protects (moderate homoiousians, apophatic theologians) even though outsiders often conflate the two positions, and they face deposition under both this reading and the strict metaphysical-equality reading.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, hard_subordinationist_clergy, payer,
    moderate, biographical, trapped, regional).

% Emperors and their appointed councils seeking a single empire-wide creedal formula to end factional strife. The similarity reading's devolution of interpretive authority to local bishops directly undercuts their preferred instrument of unity — a fixed, universally binding formula — yet their preference for uniformity is not itself part of what the reading settles; they are structurally sidelined by a reading whose whole point is to deny that any single metaphysical formula must bind every see.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, imperial_church_authority, excluded,
    institutional, generational, constrained, continental).

% Scholars reconstructing fourth-century Trinitarian disputes from council acta, letters, and creedal texts. They document how homoousios functioned differently across sees and decades, note the persistent homoiousios/homoousios terminological slippage, and assess which reading particular bishops and factions actually held versus which reading later orthodox historiography retrojected onto them.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, ecclesiastical_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__honorific_similarity_reading, diffuse).
narrative_ontology:fixing_cost_class(homoousios_nicene__honorific_similarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared creedal vocabulary broad enough that theologically diverse sees — strict identity theorists, moderate similarity theorists, and apophatic skeptics of precise formulas — can all subscribe to 'homoousios' and remain in communion, avoiding a schism that a maximally precise formula would force.
% TRANSFER_FUNCTION: Moves interpretive authority away from a single universal council-enforced definition and toward local bishops; moves doctrinal legitimacy away from those who require strict ontological identity and toward those who can satisfy communion requirements with a looser honorific reading. Costs fall on enforcers who must now contest ambiguity see-by-see rather than resolve it once centrally, and on hard subordinationists who discover the loosened boundary still excludes them.
% ABSENT_VOICES: The hard subordinationist clergy would object that they are punished under a reading whose looseness they expected to shelter them; they are present at councils but structurally excluded from the coalition the reading actually protects. Non-elite laity experiencing communion fractures over creedal subscription are almost entirely absent from the documentary record and from this analysis.
% DISAPPEARANCE_RATIONALE: If this reading vanished — if 'homoousios' were universally forced back to strict metaphysical identity with no honorific-similarity latitude — semi-Arian moderates and apophatic theologians would lose their basis for creedal subscription, likely triggering depositions or schisms; local episcopal discretion over doctrinal boundary-policing would collapse into centralized conciliar/imperial enforcement; the fourth-century church's fragile compromise structure (which in fact enabled eventual Cappadocian synthesis) would have to find another accommodation or fracture permanently earlier than it did.
% FOUNDING_PROBLEM: The word homoousios itself, adopted at Nicaea (325) partly because it was NOT precisely defined, needed a reading broad enough to hold a theologically fractured post-Nicene church together without forcing an immediate resolution of the ousia/hypostasis terminological chaos that plagued the mid-fourth century (exacerbated by homoousios/homoiousios being one iota apart).
% FOUNDING_PROBLEM_CORROBORATION: Contemporary opponents (Athanasius, later the Cappadocians in their critique of loose homoiousian formulas) attest that the similarity reading was a real and dangerous ambiguity requiring correction, not a legitimate resolution — this is corroboration from OUTSIDE the beneficiary coalition that the founding problem (terminological chaos) was being exploited rather than solved. Modern historians of the Arian controversy (e.g., scholarship on the homoiousian party of Basil of Ancyra) corroborate that the moderate reading was a genuine, non-cynical theological position for many holders, not merely a beneficiary's self-serving gloss — so the status remains genuinely contested rather than resolved in either direction.
narrative_ontology:disappearance_verdict(homoousios_nicene__honorific_similarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__honorific_similarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__honorific_similarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises through the 340s-350s as the ambiguity is actively exploited by court theology (Constantius II's Homoian preference) to marginalize strict Nicenes administratively, then partially recedes post-381 as Constantinople re-narrows the term without eliminating pastoral latitude entirely. Theater ratio tracks the proliferation of creedal formulas (Sirmium, Nike, Constantinople 360) whose profusion increasingly served factional maneuvering over substantive clarification. Suppression is authored as a raw structural property — exile, deposition, and forced subscription — independent of and not scaled by extractiveness; it peaks under Constantius II's enforcement of a Homoian-leaning settlement and only partially eases afterward.
 *
 * PERSPECTIVAL GAP:
 *   From the local episcopal authority seat, this reading is a rope: a working coordination device letting a fractured episcopate remain in communion under one creedal word. From the strict Nicene enforcer seat, the identical arrangement is closer to a snare: a rhetorical concession that permits crypto-subordinationism to persist under cover of Nicene vocabulary, requiring perpetual re-litigation to contain. The engine should register this divergence structurally (institutional agenda_setter power/arbitrage exit vs. institutional payer power/trapped exit at continental scope) rather than through any single narrator's evaluative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Semi-Arian moderates and apophatic theologians are declared beneficiaries because the honorific-similarity reading is the reading that specifically accommodates their theological commitments and preserves their ecclesiastical standing — they get low derived directionality (near-beneficiary). Strict Nicene enforcers and hard subordinationists are declared victims for structurally different reasons: enforcers pay because ambiguity is exactly what they must expend resources fighting to close; hard subordinationists pay because even the loosened boundary still excludes their explicit ontological-subordination claim, so the reading's latitude does not extend to them despite superficial proximity. Both victim groups have trapped exit — neither can abandon the fight without conceding the term's content (enforcers) or their own Christology (subordinationists).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (holding together a theologically diverse post-Nicene church without forcing premature terminological resolution) is genuinely contested as live-vs-dead: proponents of the reading would say it remains live wherever apophatic caution about precise metaphysical formulas is theologically warranted; strict Nicene critics would say the problem was resolved by 381's clearer settlement and the ambiguity's persistence past that point is inertial cover for continuing subordinationist sympathies. The tangled_rope classification (rather than snare or piton) reflects that genuine coordination value coexists with asymmetric cost — this is not pure extraction dressed as coordination, nor mere inertia; it required active contest on both sides throughout the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sincere_theology_vs_strategic_ambiguity,
    'Was the honorific-similarity reading held by its proponents as a sincere, carefully-reasoned theological position (apophatic caution about precise ousia-language) or was it primarily a strategic vehicle for factions (Homoian court theology under Constantius II) to marginalize strict Nicenes while claiming creedal continuity?',
    'Close comparison of private correspondence and treatise-level argumentation (e.g., Basil of Ancyra''s homoiousian writings) against conciliar political maneuvering documented in acta and imperial correspondence, to separate sincere theological content from opportunistic deployment.',
    'If predominantly sincere, this reading functions closer to a rope with contested boundary-policing costs; if predominantly strategic (especially in its court-theology deployment under Constantius II), the extraction component is better understood as closer to snare-like capture of ambiguity for factional advantage, and extractiveness should be revised upward for that sub-period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincere_theology_vs_strategic_ambiguity, conceptual, 'Whether the reading is sincere apophatic theology or strategic factional ambiguity-exploitation.').

omega_variable(
    kernel_disagreement_location,
    'Where exactly does the honorific-similarity reading diverge from its sibling readings — is the disagreement located in the referent of ousia itself, in whether ''sameness'' admits degrees, or in whether creedal language is even the right register for settling the underlying metaphysical question?',
    'Systematic textual comparison of how homoousios, homoiousios, and hypostasis are deployed by representative authors across the three readings (Athanasius for metaphysical_equality, Basil of Ancyra for honorific_similarity, Eusebius of Nicomedia/Arius for subordinationist) to locate the precise axis of disagreement.',
    'If the disagreement is primarily terminological/referential (what ''same'' means) rather than substantively theological, later readings claiming continuity with Nicaea (post-381) may overstate the discontinuity between this reading and metaphysical_equality_reading; if the disagreement is substantively theological, the readings are genuinely incompatible positions and the tangled_rope classification''s victim/beneficiary split is on firmer ground.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'Locating the precise structural axis of disagreement among the three sibling readings.').

omega_variable(
    committer_framework_uniqueness,
    'Is honorific-similarity the only coherent non-strict reading available within fourth-century theological vocabulary, or could a different manifest decomposition have split this kernel differently (e.g., separating ''apophatic caution'' readings from ''Homoian political'' readings that this story currently merges under one stakeholder coalition)?',
    'Compare against alternative historiographical taxonomies of the Arian controversy (e.g., the traditional Arian/semi-Arian/Nicene trichotomy versus more granular modern categorizations distinguishing Homoian, Homoiousian, and Heterousian parties) to assess whether this story''s beneficiary coalition (semi_arian_moderates + apophatic_traditions) conflates positions that a finer-grained decomposition would separate into distinct constraints.',
    'If the coalition should be split further, this story would itself decompose into additional sibling readings, each with potentially different ε and victim/beneficiary structure — most acutely, Homoian court theology under Constantius II may deserve separation from the more theologically motivated Homoiousian position of Basil of Ancyra.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framework_uniqueness, conceptual, 'Whether the beneficiary coalition itself conflates structurally distinct sub-positions warranting further decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__honorific_similarity_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_nicene__honorific_similarity_reading, theater_ratio, 325, 0.2).
narrative_ontology:measurement(homo_tr_t336, homoousios_nicene__honorific_similarity_reading, theater_ratio, 336, 0.28).
narrative_ontology:measurement(homo_tr_t350, homoousios_nicene__honorific_similarity_reading, theater_ratio, 350, 0.35).
narrative_ontology:measurement(homo_tr_t359, homoousios_nicene__honorific_similarity_reading, theater_ratio, 359, 0.45).
narrative_ontology:measurement(homo_tr_t370, homoousios_nicene__honorific_similarity_reading, theater_ratio, 370, 0.4).
narrative_ontology:measurement(homo_tr_t381, homoousios_nicene__honorific_similarity_reading, theater_ratio, 381, 0.38).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 325, 0.3).
narrative_ontology:measurement(homo_be_t336, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 336, 0.4).
narrative_ontology:measurement(homo_be_t350, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 350, 0.48).
narrative_ontology:measurement(homo_be_t359, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 359, 0.55).
narrative_ontology:measurement(homo_be_t370, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 370, 0.5).
narrative_ontology:measurement(homo_be_t381, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 381, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 325, 0.25).
narrative_ontology:measurement(homo_su_t336, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 336, 0.42).
narrative_ontology:measurement(homo_su_t350, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 350, 0.55).
narrative_ontology:measurement(homo_su_t359, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 359, 0.68).
narrative_ontology:measurement(homo_su_t370, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 370, 0.6).
narrative_ontology:measurement(homo_su_t381, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 381, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__honorific_similarity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_nicene__honorific_similarity_reading, 0.1).
narrative_ontology:affects_constraint(homoousios_nicene__honorific_similarity_reading, homoousios_nicene__metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__honorific_similarity_reading, homoousios_nicene__subordinationist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language 'homoousios controversy' per the ε-invariance principle. metaphysical_equality_reading claims ontological identity (higher accessibility_collapse, different beneficiary/victim structure favoring strict Nicenes); subordinationist_reading claims compatibility with ontological subordination (different victim set — this reading and metaphysical_equality_reading both treat hard subordinationists as victims, but for different reasons). All three share the kernel_id homoousios_nicene and must be read as competing live positions among fourth-century factions, not as three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
