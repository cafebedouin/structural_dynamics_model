% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__filioque_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creed_381_pneumatology__filioque_reading, []).

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
 *   constraint_id: creed_381_pneumatology__filioque_reading
 *   human_readable: Filioque Pneumatology Reading: Papal Authority to Clarify Trinitarian Doctrine
 *   domain: theology/ecclesiastical_authority
 *
 * SUMMARY:
 *   The Filioque reading of the Creed of 381 asserts that the Holy Spirit
 *   proceeds from the Father AND the Son (Filioque), and that the papal
 *   magisterium possesses the authority to clarify and develop this doctrine
 *   unilaterally, without requiring ecumenical consent. This is ONE reading
 *   of a contested kernel — the creed of 381 itself, which is silent on
 *   pneumatology. The Eastern Orthodox churches maintain a different reading
 *   (monoprocession_reading): the Spirit proceeds from the Father alone, and
 *   381 is inviolable without ecumenical modification. The
 *   ecumenical_reunion_reading offers a third path: both readings are
 *   legitimate regional expressions within a single communion. This
 *   constraint story instantiates ONLY the Filioque reading and its
 *   structural consequences: it establishes Western doctrinal unity under
 *   papal authority at the cost of Orthodox theological autonomy and
 *   institutional separation. The constraint operationalizes one framework
 *   for resolving doctrinal ambiguity (magisterial clarification) by
 *   systematically overriding the alternative framework (conciliar consent).
 *
 * KEY AGENTS:
 *   - Roman papal see: institutional beneficiary; possesses and exercises unilateral magisterial authority; sets the interpretive boundary
 *   - Eastern Orthodox churches: organized victims; identity_locked; bear the cost of institutional separation and doctrinal subordination
 *   - Oriental Orthodox churches: organized victims; identity_locked; drawn into the doctrinal contest despite historical separation from both Western and Eastern disputes
 *   - Latin Western theology: institutional beneficiary; gains coherence, vindication, and institutional protection from the magisterial authority structure
 *   - Ecumenical councils: excluded institutional seat; structurally displaced by unilateral papal pronouncement
 *   - Analytical observer: external seat examining the authority framework conflict
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__filioque_reading, 0.81).
domain_priors:suppression_score(creed_381_pneumatology__filioque_reading, 0.77).
domain_priors:theater_ratio(creed_381_pneumatology__filioque_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__filioque_reading, tangled_rope).
narrative_ontology:human_readable(creed_381_pneumatology__filioque_reading, "Filioque Pneumatology Reading: Papal Authority to Clarify Trinitarian Doctrine").
narrative_ontology:topic_domain(creed_381_pneumatology__filioque_reading, "theology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(creed_381_pneumatology__filioque_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__filioque_reading, '6b7cea27-ae2a-48d6-bd0f-e78f3cbc1350').
narrative_ontology:cs_kernel_codification('6b7cea27-ae2a-48d6-bd0f-e78f3cbc1350', fixed_text).
narrative_ontology:cs_authority_grounding('6b7cea27-ae2a-48d6-bd0f-e78f3cbc1350', extraction).
narrative_ontology:cs_interpretation_layer_present('6b7cea27-ae2a-48d6-bd0f-e78f3cbc1350').
narrative_ontology:cs_reading_relation('6b7cea27-ae2a-48d6-bd0f-e78f3cbc1350', creed_381_pneumatology__monoprocession_reading, forecloses).
narrative_ontology:cs_reading_relation('6b7cea27-ae2a-48d6-bd0f-e78f3cbc1350', creed_381_pneumatology__ecumenical_reunion_reading, influences).
narrative_ontology:cs_axiom('6b7cea27-ae2a-48d6-bd0f-e78f3cbc1350', foundational, papal_magisterial_authority_doctrinal_development).
narrative_ontology:cs_axiom_status(papal_magisterial_authority_doctrinal_development, holdable).
narrative_ontology:cs_axiom_grounding('6b7cea27-ae2a-48d6-bd0f-e78f3cbc1350', papal_magisterial_authority_doctrinal_development, deontological).
narrative_ontology:cs_axiom('6b7cea27-ae2a-48d6-bd0f-e78f3cbc1350', secondary, filioque_pneumatological_necessity).
narrative_ontology:cs_axiom_status(filioque_pneumatological_necessity, holdable).
narrative_ontology:cs_axiom_grounding('6b7cea27-ae2a-48d6-bd0f-e78f3cbc1350', filioque_pneumatological_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('6b7cea27-ae2a-48d6-bd0f-e78f3cbc1350', papal_magisterial_doctrinal_authority).
narrative_ontology:cs_drift_state('6b7cea27-ae2a-48d6-bd0f-e78f3cbc1350', early_medieval_consensus_breakdown, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('6b7cea27-ae2a-48d6-bd0f-e78f3cbc1350', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__filioque_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, roman_papal_see).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, latin_western_theology).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, eastern_orthodox_churches).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, oriental_orthodox_churches).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, eastern_orthodox_churches).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, theological_traditionalists).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, eastern_theological_autonomy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possesses and exercises the authority to clarify and expand the doctrine of the Creed of 381 by unilateral pronouncement. Codifies the Filioque as essential Trinitarian doctrine through liturgical insertion and magisterial teaching. Controls the interpretive apparatus (councils, encyclicals, magisterial statements) that establishes doctrinal boundaries. Frames the addition as a legitimate development of implicit doctrine rather than novelty.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, roman_papal_see, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Maintain the 381 creed as binding, inviolable, and requiring ecumenical consent for modification. The unilateral Filioque insertion violates their understanding of doctrinal procedure and theological autonomy. They bear the cost of doctrinal schism and institutional separation, yet maintain a real coordination function in preserving the original creed against arbitrary amendment. Exit is identity_locked: leaving Orthodox communion means ceasing to embody Orthodoxy.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, eastern_orthodox_churches, payer,
    organized, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__filioque_reading, eastern_orthodox_churches, beneficiary).

% Historically separated from the council of Chalcedon (451) and later from the Filioque disputes; nevertheless drawn into the doctrinal contest as the unilateral magisterium extends its authority claims across theological traditions. Subjected to pressure to accept the Filioque as universal doctrine while their own theological autonomy remains unrecognized. Identity locked to their own liturgical and conciliar traditions.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, oriental_orthodox_churches, payer,
    organized, civilizational, identity_locked, continental).

% Gains doctrinal coherence, intellectual justification, and institutional legitimacy from the Filioque clarification and the magisterial authority structure that enforces it. Western scholastic theology and mystical practice incorporate the Filioque as foundational. The constraint vindicates Western theological method and the authority to develop doctrine.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, latin_western_theology, beneficiary,
    institutional, civilizational, analytical, global).

% Would be the traditional locus for doctrinal clarification and amendment under the conciliar model. The unilateral magisterial assertion of papal authority to clarify doctrine without ecumenical consent structurally displaces councils from their historical role. Their theological voice is subordinated to papal pronouncement.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, ecumenical_councils, excluded,
    institutional, civilizational, trapped, universal).

% Within Western Christianity, theologians committed to the Filioque and to doctrinal development via magisterial authority benefit from the constraint's enforcement. Their intellectual work is vindicated and institutionally protected. Those who dissent face intellectual marginalization or institutional pressure.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, theological_traditionalists, beneficiary,
    moderate, civilizational, constrained, regional).

% Argue for the binding status of 381 and the necessity of ecumenical consent for doctrinal change. They bear the cost of institutional opposition from the papal magisterium, have limited platforms within Western theology, and face structural barriers to having their framework recognized as legitimate.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, eastern_theological_autonomy_advocates, payer,
    moderate, civilizational, constrained, continental).

% External seat examining the structural conflict: a reading of 381 that asserts unilateral papal authority to clarify and develop doctrine versus a reading that treats 381 as binding without ecumenical modification. Observes how the Filioque constraint operationalizes the competing doctrinal authority frameworks.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(creed_381_pneumatology__filioque_reading, roman_papal_see).
narrative_ontology:fixing_cost_class(creed_381_pneumatology__filioque_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, coherent Western Trinitarian theology and liturgical practice by clarifying the implicit pneumatological content of the 381 Creed. Coordinates the Western churches' doctrinal output under a single interpretive authority (papal magisterium) that can resolve theological ambiguity and prevent doctrinal fragmentation.
% TRANSFER_FUNCTION: Transfers doctrinal authority from the conciliar model (ecumenical assemblies, bilateral negotiation) to the unilateral papal magisterium. Moves theological autonomy and interpretive legitimacy from Eastern churches and ecumenical structures to the Roman see. Subordinates competing theological frameworks (Eastern Orthodox, Oriental Orthodox) to Western scholastic doctrine.
% ABSENT_VOICES: Eastern Orthodox and Oriental Orthodox churches are not in the conversation that produces or justifies the Filioque reading — they are excluded from the councils that formalize it and denied equal standing in determining what constitutes doctrinal development. Ecumenical councils lose their historical voice in favor of papal pronouncement. Dissenting Western theologians committed to conciliar authority are similarly marginalized.
% DISAPPEARANCE_RATIONALE: If the Filioque insertion and its magisterial justification vanished, the Eastern and Oriental Orthodox churches would not face an imposed doctrinal boundary, ecumenical reunification would become structurally possible (councils could reconvene without the Filioque as a unilateral precondition), and Western theology would revert to the 381 Creed as the binding baseline — the institutional separation and theological polarization the constraint creates would have no standing.
% FOUNDING_PROBLEM: The Creed of 381 is silent on the precise relationship between pneumatology and Christology; it does not explicitly state whether the Spirit proceeds from the Father alone or from both Father and Son. Western theologians develop doctrinal positions that require the Filioque to maintain coherent Trinitarian theology, especially in response to Arian and Pneumatomachian heresies. The problem: how does the Church clarify implicit doctrine and maintain unity when silence in the creed admits multiple coherent readings?
% FOUNDING_PROBLEM_CORROBORATION: Western scholastic theologians (Augustine, Aquinas, Anselm) attest the founding problem — the creed's silence requires clarification for coherent systematic theology. Eastern theologians attest a different problem: unilateral amendment without ecumenical consent violates the procedure by which the creed was established and the principle of unanimity required for universal doctrine. Legislative and scholarly sources outside the benefiting parties (modern ecumenical commissions, historians of dogma, non-partisan theological analysts) document that the founding problem itself is read differently by the two traditions.
narrative_ontology:disappearance_verdict(creed_381_pneumatology__filioque_reading, world_rearranges).
narrative_ontology:founding_problem_status(creed_381_pneumatology__filioque_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(creed_381_pneumatology__filioque_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(creed_381_pneumatology__filioque_reading, 'none', 1).
narrative_ontology:epsilon_provenance(creed_381_pneumatology__filioque_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creed_381_pneumatology__filioque_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(creed_381_pneumatology__filioque_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(creed_381_pneumatology__filioque_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins low (0.12 at t=0, early post-Creed period) because the issue is nascent theological disputation without institutional enforcement — multiple readings coexist. It rises steeply through the medieval period (0.52 by t=400, post-Aquinas crystallization) as Western scholasticism elaborates the Filioque and papal authority structures institutionalize the reading. By t=800 (post-Great Schism crystallization), extractiveness reaches 0.71 — the constraint now operationalizes institutional separation and doctrinal subordination. It plateaus near t=1200-1600 (Reformation and Counter-Reformation era) at 0.78-0.81 as the reading becomes so deeply embedded in Western magisterial structure that alternatives are structurally marginalized but no longer require active suppression to maintain — they are already excluded from the authorized interpretive apparatus. Suppression follows the same trajectory but more sharply: early theological dispute requires minimal active suppression (alternatives are still in conversation), but as the reading crystallizes institutionally, suppression intensifies (Councils refuse to canonize the monoprocession reading, papal pronouncements assert magisterial prerogative, Eastern churches are gradually excluded from Western theological forums). Theater ratio remains relatively low throughout (0.08-0.41) because the constraint's operation is structural and institutional rather than performative — the exclusion is real, not theatrical — but rises modestly in later periods as institutional theater (repetition of magisterial authority claims, ritualized assertion of doctrine) begins to substitute for active boundary enforcement as the constraint becomes normalized.
 *
 * PERSPECTIVAL GAP:
 *   From the papal/Western seat, the Filioque constraint is genuine coordination: it clarifies implicit doctrine, maintains Western theological unity, and operationalizes a legitimate magisterial function. From the Orthodox seat, the same constraint is asymmetric extraction: unilateral amendment of a binding creed, overriding ecumenical procedure and theological autonomy. The gap is not a matter of incomplete information — both sides see the same institutional facts — but of foundational disagreement about what constitutes legitimate doctrinal authority and change. The engine computes per-seat type from d and other structural metrics; where the papal seat computes a rope-like coordination and the Orthodox seat computes a snare-like extraction, that divergence IS the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The Roman papal see is the structural beneficiary (d near 0.0): it sets the interpretive boundary, controls the magisterial apparatus, collects the authority and institutional prestige. Eastern and Oriental Orthodox churches are the structural victims (d near 1.0): they bear the cost of institutional separation, theological subordination, and exclusion from the authorized interpretive process. Their exit is identity_locked — leaving the Orthodox tradition means ceasing to be Orthodox. Latin Western theology is a beneficiary (d near 0.1): it gains doctrinal vindication and institutional protection. Ecumenical councils are excluded (not directly affected by d, but the constraint's operation displaces them). The analytical observer is analytical (d = 0.5 by definition). Directionality is not overridden; the structural derivation from beneficiary/victim + exit options captures the true relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to clarify implicit doctrine) remains contested — the Eastern and Orthodox reading denies that the problem justifies unilateral papal amendment. The constraint persists not because both parties recognize its necessity, but because one party (papal) has the institutional power to enforce it and the other party (Orthodox) is unable or unwilling to overthrow it. A mandatrophy exists: the constraint's stated function (doctrinal clarity) is satisfied by alternative mechanisms (conciliar clarification, ecumenical dialogue, theological development within Orthodox tradition). The constraint's persistence is better explained by papal institutional interest in magisterial authority than by the necessity of the Filioque for Western theology or Christian unity. The high extractiveness and suppression, combined with the identity_locked exit for Orthodox churches and the evident alternative mechanisms, confirm a tangled_rope reading: real coordination function (Western doctrinal unity) wrapped in asymmetric extraction (subordination of competing theological frameworks).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_doctrine_legitimacy,
    'Does the concept of ''implicit doctrine'' — doctrine not explicitly stated in a binding creed — legitimately permit unilateral papal clarification, or does it require ecumenical consent?',
    'Theological argumentation from competing traditions (Eastern Orthodox, Roman Catholic, Anglican), historical analysis of how 381 was understood at the time of its promulgation, and examination of whether other creedal developments (e.g., Marian doctrines) meet the same standard of consent.',
    'If implicit doctrine legitimately permits unilateral clarification, the Filioque reading is structurally justified and extractiveness is lower (coordination cost rather than pure extraction). If ecumenical consent is required, extractiveness is higher (unilateral amendment) and the constraint is properly read as pure extraction or tangled rope with heavy asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implicit_doctrine_legitimacy, conceptual, 'Whether ''implicit doctrine'' justifies unilateral magisterial amendment or requires ecumenical consent.').

omega_variable(
    alternative_doctrinal_mechanisms,
    'Could Western theology achieve the same doctrinal clarity and coherence about Trinitarian pneumatology through conciliar or dialogical mechanisms that do not require a unilateral magisterial assertion?',
    'Analysis of actual theological solutions achieved through Eastern Orthodox conciliar processes, study of how Western theology develops doctrine in non-magisterial contexts (academic theology, regional synods), and examination of ecumenical reunion proposals that do not require acceptance of the Filioque.',
    'If alternatives exist that achieve the coordination function, the constraint''s extractiveness is confirmed at the high end — the suppression of alternatives is the defining feature. If no alternatives achieve the same function, extractiveness may be lower (coordination cost unavoidable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_doctrinal_mechanisms, empirical, 'Whether the Filioque constraint is structurally necessary or merely convenient for Western institutional authority.').

omega_variable(
    reading_versus_authority_framework,
    'Is this constraint primarily about the Filioque doctrine itself, or primarily about the authority framework (papal magisterium) that enforces the reading?',
    'Counterfactual analysis: if the Filioque had been clarified through ecumenical consent and bilateral agreement (rather than unilateral papal pronouncement), would the constraint''s extractiveness and suppression be different? If yes, the constraint is about the authority framework more than the doctrine.',
    'If the constraint is authority-framework-centered, decomposition into two constraint stories may be warranted: one about the Filioque doctrine per se (potentially rope-like: genuine coordination), and one about magisterial authority (tangled rope or snare). If the constraint is doctrine-centered, the current framing is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_versus_authority_framework, conceptual, 'Whether the constraint primarily concerns pneumatological doctrine or magisterial ecclesiastical authority.').

omega_variable(
    identity_lock_mechanism_orthodoxy,
    'Is the identity_locked exit status for Eastern Orthodox churches a structural feature (leaving Orthodoxy means ceasing to be Orthodox, a fact of definition), or is it a constructed identity fusion (the Orthodox have come to fuse their identity with opposition to the Filioque, making exit psychologically impossible despite being theoretically available)?',
    'Historical analysis of whether Orthodox Christianity existed before the Filioque dispute and maintained identity without reference to it; examination of whether Orthodox churches that have engaged in ecumenical dialogue and accepted limited Filioque language retained Orthodox identity; study of how individuals transition between Orthodox and non-Orthodox traditions.',
    'If identity_locked is purely structural (Orthodoxy IS the tradition that rejects unilateral magisterial amendment of the creed), then the high suppression and extractiveness are accurately measured. If identity_locked is partially constructed (the Filioque became a proxy for broader identity concerns), then some of the measured suppression may be internalized or identity-fusion-based rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_orthodoxy, empirical, 'Whether Orthodox identity_locked exit is structural or constructed.').

omega_variable(
    reading_foreclosure_and_coexistence,
    'Does this Filioque reading logically foreclose the monoprocession reading (so that no single theological framework could hold both), or do the two readings coexist as live options held by different parties?',
    'Examination of attempts at ecumenical dialogue and reunion proposals (e.g., Joint International Commission for Theological Dialogue); analysis of whether Eastern Orthodox theologians can coherently engage the Western Filioque without abandoning their commitment to monoprocession; assessment of whether the council_381_pneumatology kernel is large enough to admit both readings simultaneously.',
    'If foreclosure occurs, the Filioque reading is a definitional assertion that its sibling reading is false — the constraint operationalizes the winning side of a logical contradiction. If coexistence is possible, the readings are rival interpretations of an ambiguous kernel, both livable, and the constraint operationalizes institutional power rather than logical necessity. This affects the classification of reading_relations in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_and_coexistence, conceptual, 'Whether Filioque and monoprocession are logically incompatible or coexistent readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__filioque_reading, 0, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(filioque_theater_t0, creed_381_pneumatology__filioque_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(filioque_theater_t200, creed_381_pneumatology__filioque_reading, theater_ratio, 200, 0.14).
narrative_ontology:measurement(filioque_theater_t400, creed_381_pneumatology__filioque_reading, theater_ratio, 400, 0.22).
narrative_ontology:measurement(filioque_theater_t800, creed_381_pneumatology__filioque_reading, theater_ratio, 800, 0.35).
narrative_ontology:measurement(filioque_theater_t1200, creed_381_pneumatology__filioque_reading, theater_ratio, 1200, 0.38).
narrative_ontology:measurement(filioque_theater_t1600, creed_381_pneumatology__filioque_reading, theater_ratio, 1600, 0.41).

% Extraction over time
narrative_ontology:measurement(filioque_extractiveness_t0, creed_381_pneumatology__filioque_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(filioque_extractiveness_t200, creed_381_pneumatology__filioque_reading, base_extractiveness, 200, 0.31).
narrative_ontology:measurement(filioque_extractiveness_t400, creed_381_pneumatology__filioque_reading, base_extractiveness, 400, 0.52).
narrative_ontology:measurement(filioque_extractiveness_t800, creed_381_pneumatology__filioque_reading, base_extractiveness, 800, 0.71).
narrative_ontology:measurement(filioque_extractiveness_t1200, creed_381_pneumatology__filioque_reading, base_extractiveness, 1200, 0.78).
narrative_ontology:measurement(filioque_extractiveness_t1600, creed_381_pneumatology__filioque_reading, base_extractiveness, 1600, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(filioque_suppression_t0, creed_381_pneumatology__filioque_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(filioque_suppression_t200, creed_381_pneumatology__filioque_reading, suppression_requirement, 200, 0.35).
narrative_ontology:measurement(filioque_suppression_t400, creed_381_pneumatology__filioque_reading, suppression_requirement, 400, 0.48).
narrative_ontology:measurement(filioque_suppression_t800, creed_381_pneumatology__filioque_reading, suppression_requirement, 800, 0.64).
narrative_ontology:measurement(filioque_suppression_t1200, creed_381_pneumatology__filioque_reading, suppression_requirement, 1200, 0.72).
narrative_ontology:measurement(filioque_suppression_t1600, creed_381_pneumatology__filioque_reading, suppression_requirement, 1600, 0.77).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__filioque_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(creed_381_pneumatology__filioque_reading, 0.18).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, creed_381_pneumatology__monoprocession_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, creed_381_pneumatology__ecumenical_reunion_reading).

% DUAL FORMULATION NOTE:
% The kernel Creed of 381 (pneumatology) admits three structurally distinct readings: the Filioque reading (this constraint) asserts unilateral papal authority to clarify that the Spirit proceeds from Father and Son; the monoprocession reading holds that 381 is inviolable and the Spirit proceeds from the Father alone; the ecumenical_reunion reading proposes both as legitimate regional expressions. Each reading instantiates a different constraint with a different epsilon, authority framework, and beneficiary/victim structure. These three stories form a constraint family linked by network.affects_constraints. The Filioque reading influences both siblings by establishing the papal magisterial authority framework that the other readings either accept (ecumenical_reunion, with modifications) or explicitly reject (monoprocession). See creed_381_pneumatology__monoprocession_reading and creed_381_pneumatology__ecumenical_reunion_reading for the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
