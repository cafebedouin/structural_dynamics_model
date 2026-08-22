% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__ontological_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [DISSOLVED (MEIJI SEPARATION EDICTS, 1868)]
% ============================================================================

:- module(constraint_simultaneous_veneration__ontological_fusion_reading, []).

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
 *   constraint_id: simultaneous_veneration__ontological_fusion_reading
 *   human_readable: Honji-Suijaku Doctrinal Order: Enforced Kami-Buddha Ontological Unity (Ontological Fusion Reading)
 *   domain: religious/institutional
 *
 * SUMMARY:
 *   From roughly the tenth century to the Meiji restoration, Japanese
 *   religious institutions operated under a doctrinal order — honji-suijaku —
 *   in which the indigenous kami were declared local manifestations of
 *   buddhas: Amaterasu identified with Dainichi, Hachiman with Amida, and a
 *   spreading system of identifications elaborated by Tendai and Shingon
 *   scholastics. The arrangement was not merely a theory; it structured
 *   institutions. Shrine-temple multiplexes (Kasuga with Kofuku-ji, Hachiman
 *   with Todai-ji) placed Buddhist clergy and chapels inside shrine
 *   precincts, channeled court and bakufu patronage through combined
 *   administrations, and made the monastic schools the only certified
 *   interpreters of what Japan's gods are. This story authors the
 *   ontological-fusion reading's constraint: the standing arrangement that
 *   enforced the kami-buddha identity and the interpretive monopoly that
 *   enforcement required. The claim and the metrics are independent authored
 *   facts: the claimed type (tangled_rope) states what I believe is
 *   structurally true — a genuine coordination function (integrating two
 *   divine orders into one workable ritual-political order, with real
 *   material gains for the kami cults) fused with asymmetric extraction
 *   (permanent transfer of interpretive authority over the kami to the
 *   monasteries, and the subordination of kami standing to buddha standing) —
 *   while the metrics state what I believe is descriptively true of its
 *   operation. Epsilon's referent is the standing arrangement itself — the
 *   enforced hierarchy — assessed by this reading's own lights; it is the
 *   arrangement, not the doctrine's truth-value, that the extraction
 *   measures.
 *
 * KEY AGENTS:
 *   - buddhist_monastic_establishment: agenda-setter and primary beneficiary (institutional power, arbitrage exit) — authors the doctrinal identifications, administers the multiplexes, collects the material and interpretive flows
 *   - court_aristocracy: secondary beneficiary (powerful, constrained) — consumes the unified legitimating frame its own authority rests on
 *   - independent_kami_priesthoods: primary target (moderate, identity_locked) — hereditary shrine lineages bearing the reinterpretation of their own deities
 *   - kami_centered_devotee_networks: secondary target with incidental benefit (powerless, constrained) — lay communities whose practice was absorbed and whose theology was supplied from above
 *   - yoshida_shinto_lineage: internal resister (organized, identity_locked) — inverted the hierarchy from inside the doctrinal idiom
 *   - nativist_scholars: excluded voice (moderate, identity_locked) — kokugaku critique circulating outside the institutional arrangement until Meiji
 *   - religious_studies_analysts: analytical observer (analytical, analytical) — reconstructs the full structure from outside every party's commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.72).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.62).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Honji-Suijaku Doctrinal Order: Enforced Kami-Buddha Ontological Unity (Ontological Fusion Reading)").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious/institutional").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, 'e74d1c37-a397-4032-ba15-35e81440da81').
narrative_ontology:cs_kernel_codification('e74d1c37-a397-4032-ba15-35e81440da81', formalized).
narrative_ontology:cs_authority_grounding('e74d1c37-a397-4032-ba15-35e81440da81', lineage).
narrative_ontology:cs_interpretation_layer_present('e74d1c37-a397-4032-ba15-35e81440da81').
narrative_ontology:cs_reading_relation('e74d1c37-a397-4032-ba15-35e81440da81', simultaneous_veneration__domain_partition_reading, influences).
narrative_ontology:cs_reading_relation('e74d1c37-a397-4032-ba15-35e81440da81', simultaneous_veneration__pragmatic_incoherence_reading, forecloses).
narrative_ontology:cs_axiom('e74d1c37-a397-4032-ba15-35e81440da81', foundational, kami_buddha_ontological_identity).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_identity, holdable).
narrative_ontology:cs_axiom_grounding('e74d1c37-a397-4032-ba15-35e81440da81', kami_buddha_ontological_identity, theological).
narrative_ontology:cs_axiom('e74d1c37-a397-4032-ba15-35e81440da81', secondary, trace_status_requires_dharmic_interpretation).
narrative_ontology:cs_axiom_status(trace_status_requires_dharmic_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('e74d1c37-a397-4032-ba15-35e81440da81', trace_status_requires_dharmic_interpretation, instrumental).
narrative_ontology:cs_reference_frame('e74d1c37-a397-4032-ba15-35e81440da81', honji_suijaku_ontological_hierarchy).
narrative_ontology:cs_drift_state('e74d1c37-a397-4032-ba15-35e81440da81', meiji_separation_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('e74d1c37-a397-4032-ba15-35e81440da81', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_monastic_establishment).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, court_aristocracy).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, independent_kami_priesthoods).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, kami_centered_devotee_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, kami_centered_devotee_networks).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, yoshida_shinto_lineage).
narrative_ontology:constraint_vindicates(simultaneous_veneration__ontological_fusion_reading, honji_suijaku_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The great monastic complexes (Tendai on Hieizan, Shingon at Toji, and the temple-shrine multiplexes they anchored) authored the doctrinal identifications through which the kami became readable — Amaterasu as Dainichi, Hachiman as Amida — and administered the combined shrine-temple institutions where Buddhist clergy performed rites inside shrine precincts. Through the arrangement they collected land grants, court and bakufu patronage, ritual fees, and the standing of being the only certified interpreters of what Japan's gods are. Their position is self-insulating: because they control the scholastic apparatus, doctrinal challenges can be reframed inside their own vocabulary.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_monastic_establishment, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, buddhist_monastic_establishment, beneficiary).

% The court and its great houses patronized temples and shrines within a single legitimating frame in which imperial authority drew on both buddhas and kami. The doctrine let them honor the indigenous cults without conceding that those cults stood outside the buddha-order they governed through. Leaving the frame would mean rebuilding the ritual foundations of their own legitimacy, so they remain inside it while drawing its benefits.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, court_aristocracy, beneficiary,
    powerful, generational, constrained, national).

% Hereditary shrine lineages such as the Watarai at Ise hold offices constituted by service to particular kami. Under the doctrinal order their deities were reclassified as manifestations of buddhas, Buddhist chapels were installed in shrine precincts, and the authoritative account of their own gods was written by monastic scholars. They answered with counter-theologies of their own — Ise doctrine asserted the kami were primordial and the buddhas derivative — but circulated from a weaker institutional base. Exit would mean abandoning the lineage office and the sacred identity it carries, which they do not do.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, independent_kami_priesthoods, payer,
    moderate, generational, identity_locked, regional).

% Village communities and lay confraternities centered on kami shrines gained access to Buddhist rites, festivals, and the material welfare of the combined institutions, while their offerings and pilgrimage flows were channeled through multiplex administration. The account of their own deities was supplied from above; they held no seat in the doctrinal conversation that reclassified their gods.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, kami_centered_devotee_networks, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, kami_centered_devotee_networks, beneficiary).

% The Yoshida house of hereditary diviners produced, in the fifteenth century, a systematic inversion: the kami are primordial and the buddhas their adapted traces. They kept the doctrinal idiom and its prestige while reversing its direction, and secured bakufu recognition for their lineage's authority over kami ordination. Their position is bound to the theological order they contest — they fight inside it rather than leaving it.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, yoshida_shinto_lineage, payer,
    organized, generational, identity_locked, national).

% Kokugaku scholars of the seventeenth and eighteenth centuries argued that the kami tradition was originally self-standing and the Buddhist overlay a corruption to be stripped away. They stood outside the institutional arrangement — no shrine or temple seat, no court office — and their critique circulated in scholarship rather than in the ritual order. Their voice acquired institutional force only when the Meiji state adopted it as policy.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, nativist_scholars, excluded,
    moderate, biographical, identity_locked, national).

% Modern historians of Japanese religion, working from outside every party's commitment, reconstruct the full structure: which identifications were authored by whom, what flows moved through the multiplexes, which alternatives were marginalized and how, and what the arrangement looked like from each seat. They hold no stake in the doctrine and can name its mechanics plainly.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, religious_studies_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__ontological_fusion_reading, buddhist_monastic_establishment).
narrative_ontology:fixing_cost_class(simultaneous_veneration__ontological_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrated two religious orders — indigenous kami cults and imported Buddhist institutions — into one ritual-political order: it allocated sacred legitimacy between shrines and temples, made shared sacred space and shared financing workable through the shrine-temple multiplexes, gave kami cults access to Buddhist protection, literacy, and state patronage channels, and gave the court a single frame for honoring both. It solved once, centrally, a problem that would otherwise have been renegotiated shrine by shrine.
% TRANSFER_FUNCTION: Moved interpretive authority over the kami — the standing right to say what the kami are — from shrine priesthoods to monastic scholastics; moved material resources (land, patronage, offerings, pilgrimage revenue) from court, bakufu, and devotees toward the temple-shrine multiplexes; moved prestige asymmetrically, elevating buddhas as originals and demoting kami to traces.
% ABSENT_VOICES: The kami-centered priesthoods that rejected the fusion had no seat in the scholastic conversation that defined their gods; their counter-theologies circulated from weaker institutional bases. Nativist scholars objected from entirely outside the arrangement and were unrepresented within it. Ordinary devotees — whose actual practice blended the cults without doctrinal systematization — were never consulted; the doctrine was authored by elite institutions on behalf of everyone.
% DISAPPEARANCE_RATIONALE: The sacred order was built on the fusion: shrine-temple multiplexes, the ritual calendar, court and bakufu legitimacy, land-holding patterns, and the priestly offices of both cults all presupposed it. When the Meiji state forcibly separated kami and buddhas in 1868, the rearrangement was immediate and violent — the haibutsu kishaku destroyed thousands of temples, dissolved the multiplexes, stripped shrines of Buddhist assets and clergy, and reorganized the religious landscape within a few years. The world did not stay the same; it was rearranged at enormous cost, which is direct evidence of how much had been organized around the arrangement.
% FOUNDING_PROBLEM: From the sixth century, Japan contained two divine orders — indigenous kami and imported buddhas — worshipped by the same elites and populace. Were the kami inferior beings? Rivals? The same gods under other names? Which order took precedence in ritual, in patronage, in the state's legitimacy? Some workable answer was needed before shrines and temples could share a sacred order at all.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: nativist scholars (Motoori Norinaga's school) attested that the kami tradition was self-standing before the Buddhist overlay — corroborating both that the integration problem had been solved and that the doctrine persisted past its working life. Modern historians of Japanese religion (Kuroda Toshio and his successors) reconstruct the arrangement as a medieval institutional formation whose doctrinal justification outlived its founding function. No beneficiary party disputes that the founding problem existed; what beneficiaries dispute is only whether the arrangement outlived it.
narrative_ontology:disappearance_verdict(simultaneous_veneration__ontological_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__ontological_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__ontological_fusion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(simultaneous_veneration__ontological_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__ontological_fusion_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__ontological_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__ontological_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because the arrangement's core flow — interpretive authority over the kami — moved permanently from the cults to the monasteries, and the material flows (land, patronage, pilgrimage revenue) concentrated in the multiplexes. Suppression (0.62) reflects real enforcement: Buddhist chapels installed in shrine precincts, court and bakufu patronage conditional on the framework, alternative theologies marginalized — but not mass coercion; kami worship continued openly, so suppression is substantial rather than severe. Theater (0.45) is moderate: from the late medieval period the doctrine's scholastic elaboration increasingly served self-maintenance — answering the nativist critique with repetition rather than engagement — while the multiplex economy still ran on it, so the ratio rises across the interval without reaching performative dominance. Accessibility collapse is moderate (0.45): alternatives never fully closed — domain-specialized practice persisted, the Yoshida inversion secured bakufu recognition, nativist scholarship circulated — but an institutionally strong kami-only theology was never again possible inside the order. Resistance (0.5) is real and documented: Watarai counter-claims, the Yoshida reversal, kokugaku critique, and finally the Meiji state's forced dissolution. The measurement series run on one shared time grid (900-1868) with all three tracked metrics authored at every point; the rising suppression_requirement series is authored because the story genuinely traces enforcement-capacity change — enforcement machinery built up through the kenmitsu medieval order and collapsed at the Meiji endpoint.
 *
 * PERSPECTIVAL GAP:
 *   From the monastic seat the arrangement appears as the buddhas' skillful accommodation — the dharma adapting its manifestation to Japanese capacities, a compassionate truth rather than a taking. From the shrine priesthood seat the same structure appears as the dissolution of their gods' standing and of their office's meaning: their deities became illustrations of someone else's doctrine. The court seat sees a workable settlement; the devotee seat sees festivals and welfare with the theology supplied from above. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The monastic establishment sits at the beneficiary end: it authors the framework, administers the institutions, and collects the flows, with arbitrage-grade exit (it can reframe any challenge in its own vocabulary). The court is a net beneficiary but constrained — its legitimacy is embedded in the frame it consumes. The kami priesthoods sit near the full-target end: they bear the reinterpretation of their own deities, their exit is identity-locked (the office is constituted by service to the kami), and their counter-theologies circulate from a weaker base. Devotee networks sit near symmetric with a slight target lean: genuine material benefit, no seat in the doctrinal conversation. Suppression (0.62) is authored as a raw structural property — it is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two mislabelings. Reading the arrangement as pure extraction erases the real integration it delivered: kami cults gained Buddhist protection, literacy, and state standing they could not have secured alone, and Japan avoided the religious warfare that doctrinal exclusion produced elsewhere. Reading it as pure coordination erases the interpretive monopoly: the same structure that integrated the cults dissolved the kami's independent theological standing and concentrated the right to say what the kami are in monastic hands. The mandatrophy question — did the mandate outlive the function? — resolves as yes: the integration problem was substantially solved once the doctrinal framework consolidated, and the arrangement persisted roughly seven further centuries because the institutions built on it had become its constituency. That persistence was maintained by interested parties, not by inertia alone, which is why the classification is tangled_rope rather than piton: there were concentrated beneficiaries with both the means and the motive to enforce, and the cost of fixing the arrangement (as Meiji demonstrated) was prohibitive for the only actor who could attempt it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the ontological_fusion_reading of the simultaneous_veneration kernel — what would the sibling readings change about the structure classified here?',
    'Author and compile the sibling readings as separate constraint files; the disagreement resolves only per-reading, since the kernel''s readings instantiate different constraints rather than competing measurements of one.',
    'The domain-partition reading would dissolve the interpretive monopoly this story measures (no ontological hierarchy to enforce; the arrangement would sit near pure coordination). The pragmatic-incoherence reading would dissolve the constraint itself (no coherent doctrine was ever enforced; what persisted would be habit, not a constraint). This file''s verdict applies to the fusion reading''s constraint only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one reading of the simultaneous_veneration kernel; the sibling readings instantiate different constraints with different victim and beneficiary structures.').

omega_variable(
    truth_claim_extraction_boundary,
    'If the fusion is metaphysically true — this reading''s own commitment — is the trace-to-original hierarchy an imposition at all or an accurate description, and where exactly does the measured extraction reside?',
    'Doctrinal analysis separating the ontological claim (the kami are manifestations of buddhas) from its institutional enforcement (who may interpret, what alternatives are permitted): whatever extraction survives the separation attaches to the enforcement, not to the ontology.',
    'A strong-fusion resolution (the subordination simply is the truth) would drive epsilon toward the coordination floor; a weak-fusion resolution (the interpretive monopoly is human construction riding on the truth-claim) leaves epsilon high. The classification of this reading''s constraint swings with the resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(truth_claim_extraction_boundary, conceptual, 'Whether the reading''s own truth-commitment absorbs the measured extraction or leaves it intact.').

omega_variable(
    enforcement_vs_voluntary_uptake,
    'How much of the doctrine''s persistence was active enforcement, and how much voluntary uptake by kami cults seeking Buddhist protection and state standing?',
    'Comparative regional records: shrines inside multiplex administration versus shrines outside it; dated records of imposed versus requested rites (chapel installation, sutra dedication); patronage conditionality in court and bakufu documents.',
    'A high enforced share supports the tangled-rope reading with the snare end in reach; a predominantly voluntary uptake would move the classification toward pure coordination, with the asymmetry a side effect rather than the structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_voluntary_uptake, empirical, 'Enforced versus voluntary share of the doctrine''s persistence across the interval.').

omega_variable(
    kami_autonomy_retained_extent,
    'How much interpretive autonomy did kami cults actually retain — Ise''s Watarai line maintained a substantial independent theology for centuries — and how does that modulate the victim seat''s position?',
    'Institutional history of the major shrine lineages'' doctrinal production and its official standing: which counter-theologies were recognized, patronized, or marginalized, and by whom.',
    'Greater retained autonomy lowers the victim seat''s effective position and strengthens the coordination side of the ledger; near-total absorption strengthens the target-side reading and pushes the classification toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kami_autonomy_retained_extent, empirical, 'Degree of interpretive autonomy kami cults retained under the enforced fusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__ontological_fusion_reading, 900, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t900, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 900, 0.12).
narrative_ontology:measurement(simu_tr_t1100, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1100, 0.18).
narrative_ontology:measurement(simu_tr_t1300, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1300, 0.25).
narrative_ontology:measurement(simu_tr_t1500, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1500, 0.3).
narrative_ontology:measurement(simu_tr_t1700, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1700, 0.4).
narrative_ontology:measurement(simu_tr_t1868, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1868, 0.45).

% Extraction over time
narrative_ontology:measurement(simu_be_t900, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 900, 0.5).
narrative_ontology:measurement(simu_be_t1100, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1100, 0.58).
narrative_ontology:measurement(simu_be_t1300, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1300, 0.68).
narrative_ontology:measurement(simu_be_t1500, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1500, 0.7).
narrative_ontology:measurement(simu_be_t1700, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1700, 0.74).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1868, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t900, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 900, 0.45).
narrative_ontology:measurement(simu_su_t1100, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1100, 0.52).
narrative_ontology:measurement(simu_su_t1300, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1300, 0.6).
narrative_ontology:measurement(simu_su_t1500, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1500, 0.62).
narrative_ontology:measurement(simu_su_t1700, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1700, 0.66).
narrative_ontology:measurement(simu_su_t1868, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1868, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__ontological_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration__domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration__pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'simultaneous veneration of kami and buddhas' (shinbutsu shugo / honji-suijaku) covers structurally distinct claims, authored as a constraint family per the epsilon-invariance principle: the ontological-fusion reading (this file — one enforced ontological hierarchy with an interpretive monopoly, high epsilon), the domain-partition reading (two functionally distinct orders governing separate domains, division of labor, low epsilon), and the pragmatic-incoherence reading (no coherent doctrine ever enforced; persistence by unexamined habit). Each member carries its own epsilon, beneficiaries, and classification; this file links its siblings so the engine can trace which structural claim each verdict attaches to. Within the family this reading was the doctrinally dominant one through the medieval period — the siblings are best understood as responses to it, one softening its hierarchy and one dissolving its coherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
