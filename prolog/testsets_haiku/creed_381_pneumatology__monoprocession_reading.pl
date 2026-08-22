% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__monoprocession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creed_381_pneumatology__monoprocession_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: creed_381_pneumatology__monoprocession_reading
 *   human_readable: Council of Constantinople 381 Monoprocession Doctrine: Inviolable Without Ecumenical Consent
 *   domain: theology/ecclesiastical authority/commitment systems
 *
 * SUMMARY:
 *   This constraint is the MONOPROCESSION READING of the contested kernel at
 *   creed_381_pneumatology. The kernel is the doctrine that the Holy Spirit
 *   proceeds in some definite way related to Father and/or Son, codified at
 *   Constantinople 381. The monoprocession reading (this story) instantiates
 *   the claim that the Spirit proceeds from the Father alone, that this
 *   doctrinal formulation is inviolable without ecumenical consent, and that
 *   any unilateral amendment (such as the Western Filioque) constitutes a
 *   breach of the conciliar covenant. This reading is held as living doctrine
 *   by the Eastern Orthodox tradition and as a boundary claim by Oriental
 *   Orthodox churches. The sibling readings—filioque_reading and
 *   ecumenical_reunion_reading—instantiate different structural claims about
 *   the same kernel and are separate constraint stories with their own ε
 *   values, beneficiaries, and types. The present story is clean:
 *   monoprocession is the referent, inviolability-without-consent is the
 *   constraint, and the reading's beneficiaries and victims are those whose
 *   institutional position rises or falls with that constraint's operation.
 *
 * KEY AGENTS:
 *   - Eastern autocephalous churches: beneficiary, covenantal consensus bloc, blocking unilateral Western doctrinal innovation
 *   - Rome / Western unilateral actors: payer, bearing institutional cost of consent requirement, identity-locked to Filioque doctrine already adopted
 *   - Ecumenical councils: agenda-setter institution, rare and difficult to convene, sole authorized body for doctrinal amendment
 *   - Reformation and post-Reformation Western theology: payer, carrying cost of doctrine adopted in breach of the constraint
 *   - Ecumenical reunion movements: excluded, advocating for framework rewrite rather than constraint interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__monoprocession_reading, 0.68).
domain_priors:suppression_score(creed_381_pneumatology__monoprocession_reading, 0.72).
domain_priors:theater_ratio(creed_381_pneumatology__monoprocession_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__monoprocession_reading, tangled_rope).
narrative_ontology:human_readable(creed_381_pneumatology__monoprocession_reading, "Council of Constantinople 381 Monoprocession Doctrine: Inviolable Without Ecumenical Consent").
narrative_ontology:topic_domain(creed_381_pneumatology__monoprocession_reading, "theology/ecclesiastical authority/commitment systems").

domain_priors:requires_active_enforcement(creed_381_pneumatology__monoprocession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__monoprocession_reading, 'cb066d64-bfc9-4dae-843b-ced8b694a80b').
narrative_ontology:cs_kernel_codification('cb066d64-bfc9-4dae-843b-ced8b694a80b', fixed_text).
narrative_ontology:cs_authority_grounding('cb066d64-bfc9-4dae-843b-ced8b694a80b', lineage).
narrative_ontology:cs_interpretation_layer_present('cb066d64-bfc9-4dae-843b-ced8b694a80b').
narrative_ontology:cs_reading_relation('cb066d64-bfc9-4dae-843b-ced8b694a80b', creed_381_pneumatology__filioque_reading, forecloses).
narrative_ontology:cs_reading_relation('cb066d64-bfc9-4dae-843b-ced8b694a80b', creed_381_pneumatology__ecumenical_reunion_reading, coexists_with).
narrative_ontology:cs_axiom('cb066d64-bfc9-4dae-843b-ced8b694a80b', foundational, monoprocession_inviolability).
narrative_ontology:cs_axiom_status(monoprocession_inviolability, holdable).
narrative_ontology:cs_axiom_grounding('cb066d64-bfc9-4dae-843b-ced8b694a80b', monoprocession_inviolability, deontological).
narrative_ontology:cs_axiom('cb066d64-bfc9-4dae-843b-ced8b694a80b', foundational, ecumenical_consent_gate).
narrative_ontology:cs_axiom_status(ecumenical_consent_gate, holdable).
narrative_ontology:cs_axiom_grounding('cb066d64-bfc9-4dae-843b-ced8b694a80b', ecumenical_consent_gate, conventional).
narrative_ontology:cs_reference_frame('cb066d64-bfc9-4dae-843b-ced8b694a80b', ecumenical_conciliar_covenant).
narrative_ontology:cs_drift_state('cb066d64-bfc9-4dae-843b-ced8b694a80b', post_filioque_adoption_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cb066d64-bfc9-4dae-843b-ced8b694a80b', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches).
narrative_ontology:constraint_victim(creed_381_pneumatology__monoprocession_reading, western_unilateral_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__monoprocession_reading, minor_eastern_churches).
narrative_ontology:constraint_victim(creed_381_pneumatology__monoprocession_reading, rome_as_unilateral_actor).
narrative_ontology:constraint_victim(creed_381_pneumatology__monoprocession_reading, reformation_and_post_reformation_western_theology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the monoprocession doctrine as canonical interpretation of the 381 creed. Benefits from the structural protection the doctrine provides: any unilateral amendment by a single see (especially Rome) requires ecumenical consent and constitutes a breach of the covenant if imposed unilaterally. The doctrine preserves the decentralized polity in which no single patriarchate can legislate doctrine for the whole Church. Collectively enforces the boundary through liturgical persistence, theological transmission, and refusal to accept Western innovations as binding.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches, beneficiary,
    organized, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches, agenda_setter).

% Bears the cost of the constraint through institutional friction: the Western addition of the Filioque (Spirit proceeds from Father AND Son) is structurally prohibited from becoming ecumenical doctrine without Eastern consent. The constraint prevents Rome (and later Western councils) from unilaterally clarifying implicit Trinitarian doctrine via papal or conciliar magisterium. The Western see is identity-locked to its own theological development and cannot exit the constraint without fracturing its claim to universal jurisdiction — yet that claim to universal authority is what the constraint denies.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, western_unilateral_innovators, payer,
    powerful, civilizational, identity_locked, global).

% The institution of the ecumenical council is the sole authorized body to amend or reinterpret the 381 creed without constituting breach. Councils are rare, consensus is hard to reach across Eastern and Western sees, and the convening authority is diffuse. The constraint traps the council itself in requiring consensus, making doctrinal innovation slow and contested.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, ecumenical_councils_as_authority, agenda_setter,
    institutional, civilizational, trapped, universal).

% Smaller Eastern traditions (Oriental Orthodox, non-Chalcedonian communities) benefit from the principle that doctrine requires ecumenical consent — their voice cannot be overridden by a more powerful see. The constraint affirms their right to object and their standing in the consent pool, even if their numerical weight is small.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, minor_eastern_churches, beneficiary,
    moderate, civilizational, constrained, global).

% Structurally bears the cost of the monoprocession constraint: Roman authority to clarify doctrine unilaterally is denied. The Filioque, adopted in Western liturgy and theology over centuries, is treated as a breach of the 381 creed's inviolability under monoprocession reading. Rome is identity-locked to its own theological tradition (the Filioque is now integral to Western Trinitarian theology) and cannot exit without internal rupture, yet the constraint prevents it from imposing that tradition on the Eastern churches without their consent.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, rome_as_unilateral_actor, payer,
    institutional, civilizational, identity_locked, global).

% Protestant and post-Reformation Catholic theological development assumes the Filioque as settled doctrine. The monoprocession reading constrains the legitimacy of this development by treating it as a unilateral breach. Reformation theologians are not parties to the original ecumenical consent structure and cannot grant consent retroactively; they carry the cost of the breach doctrine.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, reformation_and_post_reformation_western_theology, payer,
    organized, civilizational, constrained, global).

% Would argue for bilateral recognition of both monoprocession and Filioque as legitimate regional theological expressions, or for a superseding ecumenical consensus that permits both formulations in communion. Excluded from the monoprocession reading's adjudication framework: their voice calls for reframing the constraint itself, not merely interpreting it.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, ecumenical_reunion_movements, excluded,
    moderate, generational, constrained, global).

% Document the constraint's operation over centuries: how Eastern churches used it to block Western unilateral innovations, how Rome justified the Filioque despite the constraint, how the constraint's enforcement mechanisms (anathemas, excommunications, refusal of communion) functioned and evolved.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, historical_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches).
narrative_ontology:fixing_cost_class(creed_381_pneumatology__monoprocession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The 381 creed establishes monoprocession as the shared doctrinal boundary recognized by the ecumenical council. The constraint coordinates the interpretation of this boundary across space and time: no single see can reinterpret it for the whole Church without ecumenical consent. This protects the decentralized polity from doctrinal imperialism by a single seat of authority.
% TRANSFER_FUNCTION: Moves institutional legitimacy and doctrinal authority from the unilateral actor (Rome, via papal or conciliar magisterium) to the ecumenical consensus body. Any unilateral doctrinal innovation forfeits its claim to bind the Church universally and is treated as a breach of covenant. The Western sees must negotiate with Eastern sees to modify doctrine; that negotiating burden is the cost.
% ABSENT_VOICES: Reformation-era Protestant theologians who developed monoprocession-divergent theologies were not in the room of ecumenical councils; they carry the cost of the constraint without having voice in its reinterpretation. Ecumenical reunion movements that would supersede the monoprocession/Filioque binary are structurally excluded by the constraint's zero-sum framing — they would require rewriting the question, not answering it.
% DISAPPEARANCE_RATIONALE: If the constraint that monoprocession is inviolable without ecumenical consent disappeared, Rome would have unilateral authority to legislate the Filioque (or any other innovation) for the whole Church; Eastern churches would lose their veto and the decentralized polity would collapse into papal universalism. The institutional geography of Christendom would reorganize around either Roman jurisdiction or deliberate Eastern schism — no middle ground would survive.
% FOUNDING_PROBLEM: The First Council of Constantinople (381) established monoprocession as doctrine and rejected any addition to the creed without ecumenical consensus. The founding problem was the proliferation of local councils and regional bishops claiming authority to modify doctrine, which fractured Christian unity. The constraint was built to prevent any single see from legislating for the whole Church — the council itself became the sole authority for doctrinal amendment.
% FOUNDING_PROBLEM_CORROBORATION: Eastern Orthodox theological historians and the institutions of the Eastern autocephalous churches attest the founding problem remains live: unilateral Western innovations threaten ecumenical unity and must be resisted via the consent principle. Roman Catholic historians and papal authorities attest the founding problem is substantially solved by papal jurisdiction and that monoprocession can be understood as implicit doctrine compatible with the Filioque when properly interpreted. Historical evidence outside the benefiting parties (ecumenical reunion scholars, medieval historians) attests that doctrinal fragmentation did occur and that the constraint functioned to slow unilateral amendments for centuries, but also that the constraint failed to prevent the Filioque's adoption in the West, suggesting either the constraint's enforcement mechanism weakened or its inviolability claim was contested from the outset.
narrative_ontology:disappearance_verdict(creed_381_pneumatology__monoprocession_reading, world_rearranges).
narrative_ontology:founding_problem_status(creed_381_pneumatology__monoprocession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(creed_381_pneumatology__monoprocession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(creed_381_pneumatology__monoprocession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(creed_381_pneumatology__monoprocession_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creed_381_pneumatology__monoprocession_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(creed_381_pneumatology__monoprocession_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(creed_381_pneumatology__monoprocession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 at interval end) because the constraint structurally prevents a powerful see (Rome) from legislating doctrine for the whole Church, extracting institutional legitimacy from that see. The cost to Rome is high: unilateral innovations are branded as breaches; any doctrinal clarification requires negotiating with less-powerful Eastern actors, reversing the asymmetry. Suppression is high (0.72) because the constraint's persistence depends on active enforcement: anathemas, refusal of communion, liturgical non-recognition, and sustained theological argument that the Filioque is illegitimate. Alternatives (Rome imposing the Filioque unilaterally, or Eastern churches accepting it as binding) are suppressed by the combined weight of covenant language, doctrinal sanction, and institutional refusal. Theater is moderate (0.41): the doctrinal arguments for monoprocession are genuine theological claims grounded in patristic sources, but enforcement also involves performative acts (repeated anathemas, periodic re-assertion of the boundary) that maintain the constraint as much as defend the doctrine itself. The measurement series shows slow accumulation of extractiveness and suppression from the Filioque's adoption in the West (~9th century, normalized to t=10 in interval) through its hardening in post-Reformation Catholic doctrine (~t=20) and into contemporary ecumenical deadlock (t=35), with theater rising as enforcement becomes more ceremonial relative to its initial doctrinal shock.
 *
 * PERSPECTIVAL GAP:
 *   From the Eastern churches' seat, the constraint is genuine coordination that protects their voice in the Church universal: monoprocession is true doctrine, unilateral amendment is breach, and the ecumenical consensus model is the only legitimate polity. From Rome's seat, the constraint is a structural veto that prevents the Church from developing doctrine in response to new theological understanding: monoprocession can be understood as compatible with implicit Filioque doctrine, and the Eastern refusal to engage with Western theology is institutional obstinacy. The engine computes this divergence from the structural data: Eastern churches have identity-locked exit (cannot renounce their own theology to placate Rome without internal schism) and are organized collectively (multiple autocephalous sees forming a bloc), whereas Rome has institutional power but sees its authority constrained by the veto. The effective extraction Rome experiences is different from what the Eastern churches experience—Rome must negotiate and accept delays; the Eastern churches must resist pressure to capitulate or face institutional isolation.
 *
 * DIRECTIONALITY LOGIC:
 *   Eastern autocephalous churches are the beneficiaries (d near 0.0–0.2): they benefit from the consent requirement, which preserves their veto over doctrinal innovation. Rome and Western unilateral actors are the targets (d near 0.8–1.0): they bear the cost of the constraint through institutional friction, delayed doctrinal clarity, and the stigma of breach. The constraint amplifies extraction against Rome because Rome is institutional (high power) but identity-locked (cannot exit Filioque doctrine without internal crisis), and because the constraint's scope is universal (affects all Western theology, not just Roman innovation). The Eastern churches' organized power and collective identity-locking (they cannot renounce monoprocession without theological contradiction) offset their lower individual power atoms; collectively, they maintain the veto through liturgical persistence and theological transmission.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—preventing unilateral doctrinal legislation by a single see—was live at t=0 (ecumenical councils convened regularly, doctrinal disputes required negotiation). By t=20 (post-Reformation), the founding problem is contested: Rome's magisterium is self-sustaining within the West, and the Filioque is settled Catholic doctrine, making the founding problem moot for Western Catholicism. For Eastern churches, the founding problem remains live: unilateral Western innovation continues to threaten unity. The constraint persists because the Eastern churches enforce it ceremonially and doctrinally, and because reunion remains a stated goal (however distant). This creates a mandatrophy tension: the constraint's coordination function (preventing unilateral innovation) is no longer performed—innovation happened, the Filioque is real, the veto failed. What persists is the constraint's extraction function: it blocks ecumenical communion and stigmatizes Western doctrine as breach. The mandatrophy is not fully resolved (the churches have not formally renounced the constraint or admitted bilateral legitimacy of both doctrines), but it is acknowledged: many contemporary ecumenical scholars treat the Filioque/monoprocession split as a historical accident layered with institutional politics, not a live doctrinal dispute. The measurement series shows the theater_ratio rising faster than extractiveness after t=20, suggesting that enforcement becomes more performative as the founding problem's urgency fades.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    monoprocession_vs_filioque_empirical_reconcilability,
    'Are monoprocession and Filioque logically reconcilable within a single Trinitarian framework, or do they represent genuinely incompatible metaphysical claims about the processions?',
    'Systematic theology constructing a unified framework that permits both formulations as equivalent expressions, or demonstration that both formulations generate contradictory entailments about the Holy Spirit''s nature and relationship to the Godhead.',
    'If reconcilable, the constraint''s treatment of the Filioque as breach becomes negotiable, and the boundary between monoprocession reading and filioque reading shifts from doctrinal incompatibility to institutional politics. If incompatible, the constraint represents a genuine divide in Trinitarian metaphysics, not merely polity. The mandatrophy status depends on this answer: if reconcilable, the constraint is zombie (the founding problem is solvable but the constraint persists); if incompatible, the constraint is mandated (the divide is real and the constraint is the only mechanism preserving unity across it).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(monoprocession_vs_filioque_empirical_reconcilability, conceptual, 'Whether monoprocession and Filioque are metaphysically compatible or represent incompatible theological ontologies.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Does Rome''s acceptance of the veto on unilateral doctrine rest on internalized theological conviction (Rome genuinely believes unilateral amendment is breach), or on structural barriers (Rome lacks the power to impose the Filioque without Eastern acceptance, so the veto is effective coercion)?',
    'Post-constraint counterfactual: if the structural barriers (Eastern bloc refusal, anathema threat, schism risk) were removed and Rome possessed unilateral enforcement capacity, would it continue to respect the consent requirement? Alternatively, analysis of Roman theological argumentation: does Rome argue the Filioque is compatible with monoprocession (internalized conviction), or does it argue the Eastern veto is illegitimate (structural resistance framed as doctrinal)?',
    'If internalized, the suppression measure (0.72) overstates the coercive force required; the constraint operates with lower active enforcement than the metric suggests. If structural, the suppression is accurate and the constraint depends entirely on the power-balance that gives Eastern churches their veto. If structural suppression persists even post-constraint-removal (Rome carries internalized monoprocession conviction after centuries), that suggests identity fusion and would shift the exit_options for Rome from identity_locked to trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether the suppression is internalized theological conviction or structural coercion by the Eastern bloc.').

omega_variable(
    reading_identity_and_kernel_boundary,
    'Is the monoprocession reading itself a commitment-system constraint (a reading of the creed-381 kernel), or is it a natural property of the creed that the reading merely reports? In other words, does monoprocession-as-inviolable exist independently of readers who claim it, or is it an artifact of the monoprocession reading''s institutional assertion?',
    'Genealogical analysis: what would the creed say and how would it be interpreted if no Eastern Orthodox tradition existed to assert the monoprocession reading? Would inviolability be a feature of the creed itself, or a reading-dependent claim? Alternatively, textual analysis of the creed-381 documents: does the creed itself assert its own inviolability, or is inviolability a subsequent interpretive layer added by the monoprocession tradition?',
    'If monoprocession-as-inviolable is reading-dependent (artifacts), then the constraint''s ε and classification depend on the reading''s persistence—if the reading dies, the constraint dematerializes. If it is kernel-inherent, the constraint exists independently of the reading''s assertion (though the reading may be its only living voice). This affects the classification: if reading-dependent, the constraint''s extractiveness partly reflects the institutional power needed to maintain the reading (higher ε reflects reading-maintenance cost, not pure doctrinal extraction); if kernel-inherent, the ε reflects genuine doctrinal asymmetry. The omega itself represents the committer-axis under-determination: what appears to be a doctrinal claim might be a reading-identity claim layered above.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_and_kernel_boundary, conceptual, 'Whether monoprocession-as-inviolable is a reading-dependent artifact or a kernel-inherent property of the 381 creed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__monoprocession_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cree_tr_t0, creed_381_pneumatology__monoprocession_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(cree_tr_t0, observed).
narrative_ontology:measurement(cree_tr_t5, creed_381_pneumatology__monoprocession_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(cree_tr_t5, observed).
narrative_ontology:measurement(cree_tr_t10, creed_381_pneumatology__monoprocession_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(cree_tr_t10, observed).
narrative_ontology:measurement(cree_tr_t15, creed_381_pneumatology__monoprocession_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(cree_tr_t15, observed).
narrative_ontology:measurement(cree_tr_t20, creed_381_pneumatology__monoprocession_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(cree_tr_t20, observed).
narrative_ontology:measurement(cree_tr_t25, creed_381_pneumatology__monoprocession_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(cree_tr_t25, observed).
narrative_ontology:measurement(cree_tr_t30, creed_381_pneumatology__monoprocession_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(cree_tr_t30, observed).
narrative_ontology:measurement(cree_tr_t35, creed_381_pneumatology__monoprocession_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(cree_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(cree_be_t0, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(cree_be_t0, observed).
narrative_ontology:measurement(cree_be_t5, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement_basis(cree_be_t5, observed).
narrative_ontology:measurement(cree_be_t10, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(cree_be_t10, observed).
narrative_ontology:measurement(cree_be_t15, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(cree_be_t15, observed).
narrative_ontology:measurement(cree_be_t20, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(cree_be_t20, observed).
narrative_ontology:measurement(cree_be_t25, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(cree_be_t25, observed).
narrative_ontology:measurement(cree_be_t30, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(cree_be_t30, observed).
narrative_ontology:measurement(cree_be_t35, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(cree_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(cree_su_t0, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(cree_su_t0, observed).
narrative_ontology:measurement(cree_su_t5, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(cree_su_t5, observed).
narrative_ontology:measurement(cree_su_t10, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(cree_su_t10, observed).
narrative_ontology:measurement(cree_su_t15, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(cree_su_t15, observed).
narrative_ontology:measurement(cree_su_t20, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(cree_su_t20, observed).
narrative_ontology:measurement(cree_su_t25, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(cree_su_t25, observed).
narrative_ontology:measurement(cree_su_t30, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(cree_su_t30, observed).
narrative_ontology:measurement(cree_su_t35, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(cree_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__monoprocession_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(creed_381_pneumatology__monoprocession_reading, 0.16).
narrative_ontology:affects_constraint(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology__filioque_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology__ecumenical_reunion_reading).

% DUAL FORMULATION NOTE:
% This story instantiates one reading (monoprocession) of the contested kernel creed_381_pneumatology. The sibling readings—filioque_reading and ecumenical_reunion_reading—generate different structural claims (magisterial authority vs. bilateral equivalence) and different ε values (low extraction vs. high coordination) from the same kernel text. The kernel contest is inherent to the commitment-system architecture: a fixed text (the 381 creed) is subject to multiple readings that produce incompatible constraints. See cs_structure.reading_relations for the structural relationships between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(creed_381_pneumatology__monoprocession_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
