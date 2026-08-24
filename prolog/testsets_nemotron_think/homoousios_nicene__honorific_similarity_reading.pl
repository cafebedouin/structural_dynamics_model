% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__honorific_similarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Homoousios as Honorific Similarity (Homoiousios Blur Reading)
 *   domain: historical_theology/ecclesiastical_history/philosophy_of_religion
 *
 * SUMMARY:
 *   The homoiousios ('like in essence') reading of homoousios emerges after
 *   Nicaea as a compromise formula — similarity without identity. It
 *   functions as a coordination constraint across the 350s-360s: imperial
 *   councils impose it to pacify East-West and Nicene-Arian divides. But its
 *   coordination is extractive — strict Nicenes pay by seeing their term
 *   diluted; hard Arians pay by conceding similarity. Local bishops gain
 *   discretion. The constraint peaks at the Council of Constantinople 360,
 *   then collapses at Constantinople 381 when strict homoousios is
 *   re-imposed. The trajectory shows rising extraction and theater as
 *   enforcement hardens, then terminal decay.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__honorific_similarity_reading, 0.48).
domain_priors:suppression_score(homoousios_nicene__honorific_similarity_reading, 0.52).
domain_priors:theater_ratio(homoousios_nicene__honorific_similarity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__honorific_similarity_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__honorific_similarity_reading, "Homoousios as Honorific Similarity (Homoiousios Blur Reading)").
narrative_ontology:topic_domain(homoousios_nicene__honorific_similarity_reading, "historical_theology/ecclesiastical_history/philosophy_of_religion").

domain_priors:requires_active_enforcement(homoousios_nicene__honorific_similarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__honorific_similarity_reading, 'de5228db-2563-4f9e-a236-67a9293d899d').
narrative_ontology:cs_kernel_codification('de5228db-2563-4f9e-a236-67a9293d899d', formalized).
narrative_ontology:cs_authority_grounding('de5228db-2563-4f9e-a236-67a9293d899d', lineage).
narrative_ontology:cs_interpretation_layer_present('de5228db-2563-4f9e-a236-67a9293d899d').
narrative_ontology:cs_reading_relation('de5228db-2563-4f9e-a236-67a9293d899d', homoousios_nicene__metaphysical_equality_reading, coexists_with).
narrative_ontology:cs_reading_relation('de5228db-2563-4f9e-a236-67a9293d899d', homoousios_nicene__subordinationist_reading, coexists_with).
narrative_ontology:cs_axiom('de5228db-2563-4f9e-a236-67a9293d899d', foundational, honorific_unity_without_ontological_reduction).
narrative_ontology:cs_axiom_status(honorific_unity_without_ontological_reduction, holdable).
narrative_ontology:cs_axiom_grounding('de5228db-2563-4f9e-a236-67a9293d899d', honorific_unity_without_ontological_reduction, conventional).
narrative_ontology:cs_axiom('de5228db-2563-4f9e-a236-67a9293d899d', secondary, pastoral_discretion_over_speculative_metaphysics).
narrative_ontology:cs_axiom_status(pastoral_discretion_over_speculative_metaphysics, holdable).
narrative_ontology:cs_axiom_grounding('de5228db-2563-4f9e-a236-67a9293d899d', pastoral_discretion_over_speculative_metaphysics, conventional).
narrative_ontology:cs_reference_frame('de5228db-2563-4f9e-a236-67a9293d899d', homoiousios_blur_framework).
narrative_ontology:cs_drift_state('de5228db-2563-4f9e-a236-67a9293d899d', constantinople_381, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('de5228db-2563-4f9e-a236-67a9293d899d', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__honorific_similarity_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, semi_arian_moderates).
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, apophatic_traditions).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, strict_nicene_enforcers).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, hard_subordinationists).
narrative_ontology:constraint_vindicates(homoousios_nicene__honorific_similarity_reading, pastoral_unity_over_speculative_precision).
narrative_ontology:constraint_vindicates(homoousios_nicene__honorific_similarity_reading, apophatic_adequacy_of_language).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and theologians (e.g., Basil of Ancyra, George of Laodicea) who accept homoousios as 'like in essence' (homoiousios) — a bridge formula preserving Son's divinity without metaphysical identity. They gain legitimacy at councils, avoid anathema, and keep their sees. Exit means accepting strict Nicene equality (loss of distinctive theology) or sliding into Arianism (loss of catholic communion).
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, semi_arian_moderates, beneficiary,
    moderate, biographical, constrained, regional).

% Monastic and mystical traditions (Evagrian, Dionysian, later Cappadocian apophaticism) for whom all divine language is analogical. The honorific reading validates their insistence that no term captures divine reality. They gain theological cover; their exit is mobile because the reading aligns with their existing practice — they need not change, only be recognized.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, apophatic_traditions, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__honorific_similarity_reading, apophatic_traditions, observer).

% Athanasius, the Cappadocians, Western bishops — for whom homoousios means strict numerical identity of essence. The honorific reading extracts from them by redefining their victory term into a vague similarity, forcing them to re-litigate Nicaea's meaning. Their identity is fused to the term's precision; exit means abandoning the doctrinal boundary that defines orthodoxy.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, strict_nicene_enforcers, payer,
    institutional, generational, identity_locked, continental).

% Aetius, Eunomius, radical Arians — for whom the Son is unlike the Father in essence (anomoios). The honorific reading extracts by conceding too much (similarity of essence) while still denying equality. Their exit is constrained: accept a compromise that still subordinates the Son, or remain outside the imperial church entirely.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, hard_subordinationists, payer,
    organized, biographical, constrained, regional).

% Diocesan bishops who gain interpretive discretion under this reading — the formula 'like in essence' permits pastoral flexibility in catechesis and liturgy. They set the local enforcement tone; some lean Nicene, some lean Arian. Their arbitrage exit: they can shift allegiance between imperial factions as political winds change.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, local_bishops, agenda_setter,
    institutional, biographical, arbitrage, regional).

% Constantius II, Valens — emperors who impose the homoiousios formula via councils (Sirmium, Ariminum, Constantinople 360) to secure ecclesiastical peace. They benefit from a pliable unity formula; they pay in political capital when enforcement fails. Their analytical seat: they observe the theological field as a stability problem.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, imperial_authority, agenda_setter,
    institutional, immediate, analytical, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__honorific_similarity_reading, imperial_authority, observer).

% Later patristic, medieval, and modern scholars who read the homoiousios moment as a distinct interpretive option. They neither collect nor pay; they map the constraint's structural logic across the kernel's readings.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, theological_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified theological vocabulary that permits communion between moderate Arians and moderate Nicenes without requiring either side to adopt the other's metaphysical precision — a liturgical and catechetical peace formula for a fractured church.
% TRANSFER_FUNCTION: Moves interpretive authority from centralized conciliar definition (Nicaea's strict homoousios) to local episcopal discretion; moves the cost of theological precision from the imperial center (which funds councils) to the dissenting margins (strict Nicenes and hard Arians who must conform or suffer exile).
% ABSENT_VOICES: The laity and monastics outside episcopal networks — their piety is shaped by the formula but they have no vote at councils. Also the Western church beyond imperial reach (Rome resists homoiousios formulas), and the Gothic/Arian churches beyond the Danube whose theology develops on a different trajectory.
% DISAPPEARANCE_RATIONALE: If the honorific reading vanished overnight (e.g., Constantinople 381 enforces strict homoousios), the semi-Arian coalition collapses — its bishops must subscribe to Nicene equality or be deposed; apophatic traditions lose their conciliar cover; imperial policy shifts from compromise to uniformity; the theological map reorders around a single orthodox metric.
% FOUNDING_PROBLEM: The post-Nicene church fractured into mutually anathematizing parties over the meaning of homoousios. The empire needed a formula that could hold East and West, Greek and Latin, Nicene and Arian together in one communion without endless councils.
% FOUNDING_PROBLEM_CORROBORATION: The problem of imperial ecclesiastical unity was real (attested by Constantius's repeated councils). But the honorific formula failed to solve it — both strict Nicenes (Athanasius, Hilary of Poitiers) and hard Arians (Eunomius) rejected it. The corroboration comes from the opposing parties themselves: their rejection proves the formula did not achieve its stated coordination function.
narrative_ontology:disappearance_verdict(homoousios_nicene__honorific_similarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__honorific_similarity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__honorific_similarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_nicene__honorific_similarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__honorific_similarity_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness 0.48 at peak (360): the formula extracts compliance from both wings. Suppression 0.52: exile of bishops, imperial edicts, but not total — alternatives persist in monastic and Western pockets. Theater 0.28 at peak, rising to 0.65 at 381: the coordination function (peace) decays into performative unity as both sides harden. Accessibility collapse 0.45: alternatives (strict Nicene, Anomoean) remain live but suppressed. Resistance 0.72: exceptionally high — the reading is attacked from both flanks continuously. Claimed tangled_rope: genuine coordination (imperial unity) + asymmetric extraction (both wings pay).
 *
 * PERSPECTIVAL GAP:
 *   From the semi-Arian seat, this is a rope — a genuine peace formula that works until hardliners break it. From the strict Nicene seat, it's a snare — a trap that redefines orthodoxy out of existence. From the hard Arian seat, it's a tangled rope — they get partial recognition but lose their radical distinction. The engine computes these per-seat types from the structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Semi-Arians and apophatics are beneficiaries (d ~0.2-0.3) — they gain legitimacy and cover. Strict Nicenes and hard subordinationists are payers (d ~0.7-0.8) — their distinctive theologies are erased by the compromise. Local bishops sit near symmetric (d ~0.5) — they gain discretion but must enforce. Imperial authority is agenda-setter with analytical exit (d ~0.15). Theological scholars are pure observers (d=0.5). The derivation from beneficiary/victim + exit + power produces this gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (imperial ecclesiastical unity) was real but the formula failed to solve it — both wings rejected it. By 381 the mandate is dead but the constraint's enforcement machinery (imperial councils, episcopal subscriptions) persists theatrically until the next emperor imposes strict Nicene orthodoxy. The mandatrophy is resolved by exogenous regime change (Theodosius), not internal correction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Was the homoiousios formula ever a genuine coordination mechanism, or was it always imperial theater masking a predetermined Nicene outcome?',
    'Counterfactual: if Constantius had lived longer and enforced homoiousios without Theodosius''s Nicene turn, would a stable modus vivendi have emerged? Compare with later henoticon (482) and monothelite compromises — do similarity formulas stabilize or always collapse?',
    'If genuine coordination, the tangled_rope classification holds; if always theater, it''s a snare with a coordination cover story. Affects whether semi-Arians are coded as beneficiaries or as captured payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the honorific reading''s coordination function was real or performative.').

omega_variable(
    apophatic_beneficiary_status,
    'Do apophatic traditions genuinely benefit from the honorific reading, or are they merely tolerated as a byproduct of episcopal politics?',
    'Trace monastic reception: did Evagrian/Dionysian circles cite homoiousios councils as authoritative, or ignore them? Did the formula enable apophatic theology''s later codification (Maximus, Palamas) or was that development independent?',
    'If apophatics are genuine beneficiaries, the beneficiary set is broader and the coordination function stronger. If incidental, the constraint is narrower extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(apophatic_beneficiary_status, empirical, 'Whether apophatic traditions are structural beneficiaries or coincidental aligners.').

omega_variable(
    local_bishop_agenda_setting,
    'Did local bishops actually gain interpretive discretion under homoiousios, or did imperial commissioners (e.g., Eudoxius, Acacius) dictate the formula''s application?',
    'Prosopographical analysis of episcopal subscriptions at Ariminum/Seleucia 359: how many bishops signed under protest, how many added qualifying clauses, how many were deposed for refusal?',
    'If bishops were imperial instruments, the agenda_setter role belongs to the court, not the episcopate. The constraint''s coordination shifts from decentralized to centralized extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_bishop_agenda_setting, empirical, 'Whether local bishops were genuine agenda-setters or imperial proxies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__honorific_similarity_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_nicene__honorific_similarity_reading, theater_ratio, 325, 0.05).
narrative_ontology:measurement(homo_tr_t335, homoousios_nicene__honorific_similarity_reading, theater_ratio, 335, 0.12).
narrative_ontology:measurement(homo_tr_t345, homoousios_nicene__honorific_similarity_reading, theater_ratio, 345, 0.18).
narrative_ontology:measurement(homo_tr_t355, homoousios_nicene__honorific_similarity_reading, theater_ratio, 355, 0.24).
narrative_ontology:measurement(homo_tr_t360, homoousios_nicene__honorific_similarity_reading, theater_ratio, 360, 0.28).
narrative_ontology:measurement(homo_tr_t365, homoousios_nicene__honorific_similarity_reading, theater_ratio, 365, 0.32).
narrative_ontology:measurement(homo_tr_t375, homoousios_nicene__honorific_similarity_reading, theater_ratio, 375, 0.41).
narrative_ontology:measurement(homo_tr_t381, homoousios_nicene__honorific_similarity_reading, theater_ratio, 381, 0.65).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 325, 0.15).
narrative_ontology:measurement(homo_be_t335, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 335, 0.28).
narrative_ontology:measurement(homo_be_t345, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 345, 0.35).
narrative_ontology:measurement(homo_be_t355, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 355, 0.42).
narrative_ontology:measurement(homo_be_t360, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 360, 0.48).
narrative_ontology:measurement(homo_be_t365, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 365, 0.45).
narrative_ontology:measurement(homo_be_t375, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 375, 0.38).
narrative_ontology:measurement(homo_be_t381, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 381, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 325, 0.1).
narrative_ontology:measurement(homo_su_t335, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 335, 0.25).
narrative_ontology:measurement(homo_su_t345, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 345, 0.38).
narrative_ontology:measurement(homo_su_t355, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 355, 0.5).
narrative_ontology:measurement(homo_su_t360, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 360, 0.52).
narrative_ontology:measurement(homo_su_t365, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 365, 0.55).
narrative_ontology:measurement(homo_su_t375, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 375, 0.62).
narrative_ontology:measurement(homo_su_t381, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 381, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__honorific_similarity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_nicene__honorific_similarity_reading, 0.08).
narrative_ontology:affects_constraint(homoousios_nicene__honorific_similarity_reading, homoousios_nicene__metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__honorific_similarity_reading, homoousios_nicene__subordinationist_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the single label 'homoousios' into three structurally distinct readings with different ε, beneficiaries, victims, and coordination functions. The honorific reading (this story) coordinates via functional unity but extracts from strict wings. The metaphysical reading coordinates via strict identity and extracts from subordinationists. The subordinationist reading coordinates via hierarchical derivation and extracts from egalitarians. All three share the kernel but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_nicene__honorific_similarity_reading, institutional, 0.15).
constraint_indexing:directionality_override(homoousios_nicene__honorific_similarity_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
