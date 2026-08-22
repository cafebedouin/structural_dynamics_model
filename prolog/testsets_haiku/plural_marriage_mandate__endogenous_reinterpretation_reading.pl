% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: Prophetic Reinterpretation of Plural Marriage Mandate (Endogenous Reading)
 *   domain: religious/institutional/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto issued by LDS church president Wilford Woodruff
 *   officially suspended the practice of plural marriage in response to
 *   federal anti-polygamy legislation and the threat of institutional
 *   collapse. This constraint story instantiates the ENDOGENOUS
 *   REINTERPRETATION READING: the Manifesto is interpreted as a legitimate
 *   prophetic reinterpretation in which God revealed the temporal suspension
 *   of plural marriage to preserve the church's salvific mission. Under this
 *   reading, the suspension is a coordinated institutional adaptation around
 *   a new prophetic directive, not capitulation to external force. The
 *   beneficiary is the institutional church (survival, temple operation,
 *   missionary legitimacy); the victim is fundamentalist dissidents who are
 *   excommunicated for maintaining the original reading. This reading claims
 *   the constraint is a ROPE (genuine coordination around new revelation)
 *   while acknowledging that suppression increased post-1890 (enforcement of
 *   the new interpretation against dissenters). The measurement series show
 *   extractiveness rising sharply from 1880–1890 (peak at Manifesto), then
 *   declining as the reinterpretation becomes normalized; suppression peaks
 *   post-1890 and gradually declines as resistance is consolidated into
 *   separated communities; theater rises through 1890–1900 then stabilizes,
 *   reflecting the diminishing need to actively defend reinterpretation
 *   against internal dissent.
 *
 * KEY AGENTS:
 *   - institutional_lds_church: agenda-setter (power=institutional), frames reinterpretation as prophetic authority
 *   - faithful_mainstream_membership: beneficiary (power=organized, exit=constrained), gain temple access and community coherence by accepting suspension
 *   - fundamentalist_dissidents: payer (power=moderate, exit=identity_locked), excommunicated and excluded from temples for maintaining original reading
 *   - federal_government: observer (power=institutional), created the coercive environment, now observes institutional compliance
 *   - american_protestant_mainstream: beneficiary (power=organized, exit=mobile), gains social legitimacy for the church once plural marriage is abandoned
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.42).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.38).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "Prophetic Reinterpretation of Plural Marriage Mandate (Endogenous Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious/institutional/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, '737cebda-8d49-4a71-bb20-c347b368275a').
narrative_ontology:cs_kernel_codification('737cebda-8d49-4a71-bb20-c347b368275a', fixed_text).
narrative_ontology:cs_authority_grounding('737cebda-8d49-4a71-bb20-c347b368275a', lineage).
narrative_ontology:cs_interpretation_layer_present('737cebda-8d49-4a71-bb20-c347b368275a').
narrative_ontology:cs_reading_relation('737cebda-8d49-4a71-bb20-c347b368275a', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('737cebda-8d49-4a71-bb20-c347b368275a', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('737cebda-8d49-4a71-bb20-c347b368275a', foundational, prophetic_reinterpretation_legitimacy).
narrative_ontology:cs_axiom_status(prophetic_reinterpretation_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('737cebda-8d49-4a71-bb20-c347b368275a', prophetic_reinterpretation_legitimacy, theological).
narrative_ontology:cs_axiom('737cebda-8d49-4a71-bb20-c347b368275a', foundational, divine_temporal_suspension_principle).
narrative_ontology:cs_axiom_status(divine_temporal_suspension_principle, holdable).
narrative_ontology:cs_axiom_grounding('737cebda-8d49-4a71-bb20-c347b368275a', divine_temporal_suspension_principle, deontological).
narrative_ontology:cs_reference_frame('737cebda-8d49-4a71-bb20-c347b368275a', eternal_plural_marriage_doctrine).
narrative_ontology:cs_drift_state('737cebda-8d49-4a71-bb20-c347b368275a', post_manifesto_suspension_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('737cebda-8d49-4a71-bb20-c347b368275a', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, institutional_lds_church).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, faithful_mainstream_membership).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, american_protestant_mainstream).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_dissidents).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, prophetic_authority_doctrine).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, divine_temporal_suspension_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and enforces the Manifesto as a legitimate prophetic reinterpretation. Controls temple access, excommunication proceedings, and the narrative frame for understanding the reinterpretation. Can reinterpret again if circumstances change (has arbitrage-level exit). Frames institutional survival as a divine good requiring the suspension.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, institutional_lds_church, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Accept the Manifesto's reinterpretation and retain temple access, community participation, and marriage legitimacy. Must internalize the teaching that plural marriage is eternally true but currently suspended. Can exit by leaving the church entirely, but that means losing family ties, community identity, and the institutional structure of religious practice.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, faithful_mainstream_membership, beneficiary,
    organized, biographical, constrained, national).

% Reject the Manifesto's reinterpretation and maintain that plural marriage is a permanently binding doctrine. Are excommunicated, lose temple access, and are pushed into separate communities. Exit from the practice means abandoning their theological conviction and their understanding of eternal family structure — the identity fusion makes exit costly and structural constraints limit alternatives.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_dissidents, payer,
    moderate, biographical, identity_locked, national).

% Enforced anti-polygamy legislation and property seizure against the church through the 1880s. Observes the Manifesto as compliance with federal law. External to the constraint's internal coordination logic but the causal source of the pressure that makes reinterpretation necessary (under the exogenous readings).
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, federal_government, observer,
    institutional, biographical, analytical, national).

% Gains social and religious legitimacy for the LDS church once plural marriage is abandoned. The Manifesto's success depends on the mainstream accepting the narrative that reinterpretation (not coercion) occurred. Can withdraw this legitimacy if future practice undermines the frame.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, american_protestant_mainstream, beneficiary,
    organized, biographical, mobile, national).

% Assess the Manifesto's epistemic and causal status: was it prophetic reinterpretation, federal coercion, or institutional pragmatism? See the full structure of what is at stake in the reading contest. Can evaluate the three readings' coherence and evidential support.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, historical_record_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__endogenous_reinterpretation_reading, institutional_lds_church).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates church membership around a unified understanding of divine will: suspends plural marriage practice while retaining the doctrine, allowing the institutional church to survive federal prosecution, operate temples, and maintain missionary access while claiming continuity with founding revelation.
% TRANSFER_FUNCTION: Transfers temple access, community legitimacy, and family recognition to those who accept the reinterpretation; transfers excommunication, temple loss, and family rupture to those who reject it. Transfers institutional legitimacy from the federal government's coercive frame to the church's prophetic authority frame.
% ABSENT_VOICES: Plural marriage practitioners not affiliated with the institutional church have no seat; women whose family structures dissolved under the suspension lack formal representation in the coordination process; the federal government's causal role is externalized from the legitimacy narrative; future generations who will inherit the doctrinal ambiguity are not present to voice the long-term sustainability question.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its enforcement vanished overnight, fundamentalist communities would expand and claim continuity with the original revelation, the church would face renewed federal prosecution, mainstream members would face internal coherence crisis (forced to choose: recover practice or fully abandon doctrine), and the institutional church's legitimacy narrative would collapse. The constraint enables a specific institutional form to persist.
% FOUNDING_PROBLEM: The church was founded on plural marriage as a binding divine principle; federal anti-polygamy legislation in the 1880s threatened institutional extinction through property seizure and leadership imprisonment. The founding problem (under this reading) is: how to remain faithful to divine principles while ensuring the church's institutional survival and salvific mission.
% FOUNDING_PROBLEM_CORROBORATION: The institutional church attests the founding problem remains live: institutional survival requires continuing revelation-responsive adaptation. Independent historians attest that federal coercion was the immediate historical cause of the Manifesto. Fundamentalist dissidents attest the founding problem is NOT institutional survival but doctrinal fidelity, which (they claim) the Manifesto violated. The corroboration splits along reading-lines: supporters of this reading see institutional survival as validated; critics see it as code for capitulation.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).
:- end_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at the 1890 peak) because this reading asserts the coordination function is genuine: the suspension preserves a real institutional good (temple access, missionary work, community coherence) for those who accept the reinterpretation. But the constraint requires active enforcement (suppression=0.38) against fundamentalist dissidents who reject the reading — enforcement is not self-evident or naturalized. Theater is moderate-low (0.28): the prophetic narrative is central to the coordination (not purely performative), but its necessity as a legitimating frame for the institutional decision is itself what theater measures — how much of the enforcement work is carried by the reinterpretation narrative versus by institutional power. The measurement series capture the intensity dynamics: extractiveness and suppression both rise sharply 1888–1890 as the Manifesto is issued and enforced (peak coercive moment), then decline as dissidents are separated into distinct communities and the new interpretation becomes normalized. Theater rises during the same peak period (highest institutional justification effort) then stabilizes as the reinterpretation is internalized. The metric trajectory reflects a constraint that is MOST extractive and theatrical at the moment of institutional transition (the Manifesto's issuance), then settles into a lower, more stable regime as separation reduces internal resistance.
 *
 * PERSPECTIVAL GAP:
 *   The institutional church and faithful mainstream membership experience this constraint as beneficial coordination: a reinterpretation that saves the church and allows continued ritual practice (temple work) under revised understanding. Fundamentalist dissidents experience the same structure as extractive: their excommunication, temple loss, and family rupture are the costs of enforcing a reading they reject. The engine should compute different per-seat types from this structural asymmetry. From the church's agenda-setting seat with arbitrage exit, the constraint computes as rope (genuine coordination benefit). From the fundamentalist's identity-locked seat, the same structural constraint computes as snare or tangled_rope (forced excommunication under a reinterpretation they deny legitimacy to). The divergence is what the measurement should detect: the beneficiary seat sees rope; the victim seat sees extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional church is a clear beneficiary (d approaches 0.0): it survives, operates temples, gains missionary legitimacy, and has arbitrage-level exit options (can reinterpret again if conditions change). Mainstream membership is near-symmetric (d near 0.5): they gain temple access but must internalize cognitive dissonance (doctrine eternally true but practice temporally suspended). Fundamentalist dissidents are clear targets (d approaches 1.0): they lose community, are excommunicated, and have identity-locked exit (leaving means abandoning their understanding of divine law). The federal government is external to the directionality measure (observer seat). American Protestantism benefits (d low) by accepting the church as legitimate. The suppression metric (0.38) is high relative to extractiveness (0.42) because the constraint's persistence requires active enforcement: without ongoing excommunication of plural practitioners and institutional suppression of the original reading's legitimacy, fundamentalist communities would expand and claim continuity with the founding revelation. The constraint is held in place by organized force, not by participant preference alone.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading faces a critical mandatrophy question: the founding problem stated by the church is 'preserve the institution and its salvific mission.' Under this reading, the Manifesto solves that problem — institutional survival is the observable outcome. However, the alternative reading (exogenous_override) states the founding problem is 'maintain fidelity to divine command regarding plural marriage,' which this Manifesto violates by suspending practice. The mandatrophy depends on which founding problem is legitimate — the church's institutional-survival framing or the fundamentalist's doctrinal-fidelity framing. This reading asserts that institutional survival IS a divine requirement, making the reinterpretation legitimate. That assertion is what routes to omega variables (the irreducible uncertainty about whether the founding problem is correctly stated by the church). The classification does not collapse the uncertainty — it names it. This constraint should compute as rope (genuine coordination) UNDER the assumption that prophetic authority is the correct framework for understanding the Manifesto, and as tangled_rope or snare UNDER the assumption that federal coercion is the actual cause. The reading itself is the assumption; different readings of the same kernel produce different computed types.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prophetic_authority_legitimacy,
    'Is the Manifesto a genuine expression of prophetic reinterpretation (the reading''s core claim), or is the revelation-narrative a frame applied retroactively to justify institutional capitulation under federal coercion?',
    'Theological hermeneutics: examine whether the doctrine of temporal suspension is internally coherent with the church''s core truth claims (eternal celestial marriage plus temporal suspension). Historical evidence: determine whether the prophetic framing appears in internal church documents before the federal ultimatum or only after. Comparative religious analysis: whether other faith traditions show similar patterns of reinterpretation under institutional pressure.',
    'If the reinterpretation is genuinely prophetic (doctrine coherently suspends practice), the constraint is a rope (legitimate coordination around a new divine directive). If the framing is applied retroactively, the constraint is tangled_rope or snare (institutional survival buying legitimacy through revelation language). This omega resolves whether the reading''s core claim is true.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prophetic_authority_legitimacy, conceptual, 'Whether the Manifesto instantiates genuine prophetic reinterpretation or institutional capitulation dressed in revelation-language.').

omega_variable(
    identity_lock_mechanism_dissidents,
    'For fundamentalist dissidents, is their identity-lock to plural marriage practice driven by genuine theological conviction (belief the doctrine is divine), institutional identity fusion (community and family structure built on plural practice), or internalized suppression (decades of messaging that plural marriage is eternally correct)?',
    'Post-exit trajectory of fundamentalists who leave the practice: do identity-locked beliefs persist after exit, or do they dissolve when the institutional enforcement and relational structure are removed? Generational shift: do second-generation fundamentalists show the same identity-lock or reduced commitment?',
    'If identity-lock is primarily theological conviction, the fundamentalists are making an independent choice to maintain the original reading (they have the logical option to reinterpret as mainstream members do). If identity-lock is relational or internalized, the exit is genuinely blocked and the suppression metric understates the constraint''s effective force on that seat. Affects classification: if exit is more genuinely constrained than the authored ''identity_locked'' atom suggests, the constraint computes as more extractive from that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_dissidents, empirical, 'Whether fundamentalist identity-lock is primarily conviction-driven or structurally/relationally enforced.').

omega_variable(
    reinterpretation_sustainability,
    'Does the church''s doctrine-retained-practice-suspended framework coherently solve the founding problem (institutional survival), or does the cognitive dissonance of holding both the doctrine and its suspension create long-term institutional pressure that will eventually resolve toward one or the other?',
    'Longitudinal institutional dynamics: track whether the church gradually migrates toward fully abandoning plural marriage as doctrine (as the LDS church has moved) or toward recovering practice under a new prophet''s reinterpretation. Survey membership: measure whether mainstream members genuinely hold the doctrine as eternally true or whether it has devolved into ''historical doctrine no longer operative.''',
    'If the framework is unstable and generates structural pressure toward either full abandonment or practice recovery, the constraint is SCAFFOLD (temporary coordination around a transition state) rather than ROPE (stable coordination). The measurement series show extractiveness declining after 1890, which could reflect stabilization OR could reflect the beginning of a long-term drift toward full doctrinal abandonment. This omega disambiguates the trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reinterpretation_sustainability, empirical, 'Whether the doctrine-suspended-practice framework is a sustainable coordination or an unstable transition.').

omega_variable(
    reading_committer_contest,
    'Which reading of the plural_marriage_mandate kernel is the correct interpretation of what the Manifesto instantiated: endogenous prophetic reinterpretation, exogenous federal override, or pragmatic institutional adaptation?',
    'This omega routes the committer-axis contest into the omega structure per Rule 2 (Kernels and Readings). The resolution requires evaluating the three sibling readings'' structural claims: does the Manifesto''s authority come from God (endogenous), from federal coercion (exogenous), or from institutional strategy (pragmatic)? The readings coexist in actual LDS and fundamentalist discourse; no single fact can resolve which reading is ''correct'' because correctness is reading-indexed (OQ-26). What can be resolved is the empirical status of the coercive environment and the chronology of the revelation-framing.',
    'The computed type of this constraint depends on which reading is true: if endogenous_reinterpretation, type is rope; if exogenous_override, type is snare; if institutional_pragmatism, type is tangled_rope. The three constraints (three readings, one kernel) have different ε values and different types by construction. This omega documents that the classification outcome is reading-dependent, which is the point of the decomposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_committer_contest, conceptual, 'The irreducible kernel-reading contest: which reading of the Manifesto''s authority and legitimacy is instantiated here?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 1880, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1880, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement_basis(plur_tr_t1880, observed).
narrative_ontology:measurement(plur_tr_t1888, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1888, 0.15).
narrative_ontology:measurement_basis(plur_tr_t1888, observed).
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.28).
narrative_ontology:measurement_basis(plur_tr_t1890, observed).
narrative_ontology:measurement(plur_tr_t1900, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1900, 0.32).
narrative_ontology:measurement_basis(plur_tr_t1900, observed).
narrative_ontology:measurement(plur_tr_t1910, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.3).
narrative_ontology:measurement_basis(plur_tr_t1910, observed).
narrative_ontology:measurement(plur_tr_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1920, 0.26).
narrative_ontology:measurement_basis(plur_tr_t1920, observed).

% Extraction over time
narrative_ontology:measurement(plur_be_t1880, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1880, 0.18).
narrative_ontology:measurement_basis(plur_be_t1880, observed).
narrative_ontology:measurement(plur_be_t1888, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1888, 0.35).
narrative_ontology:measurement_basis(plur_be_t1888, observed).
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.42).
narrative_ontology:measurement_basis(plur_be_t1890, observed).
narrative_ontology:measurement(plur_be_t1900, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement_basis(plur_be_t1900, observed).
narrative_ontology:measurement(plur_be_t1910, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1910, 0.35).
narrative_ontology:measurement_basis(plur_be_t1910, observed).
narrative_ontology:measurement(plur_be_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1920, 0.28).
narrative_ontology:measurement_basis(plur_be_t1920, observed).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1880, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1880, 0.15).
narrative_ontology:measurement_basis(plur_su_t1880, observed).
narrative_ontology:measurement(plur_su_t1888, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1888, 0.28).
narrative_ontology:measurement_basis(plur_su_t1888, observed).
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.38).
narrative_ontology:measurement_basis(plur_su_t1890, observed).
narrative_ontology:measurement(plur_su_t1900, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1900, 0.42).
narrative_ontology:measurement_basis(plur_su_t1900, observed).
narrative_ontology:measurement(plur_su_t1910, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.4).
narrative_ontology:measurement_basis(plur_su_t1910, observed).
narrative_ontology:measurement(plur_su_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1920, 0.36).
narrative_ontology:measurement_basis(plur_su_t1920, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.1).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% The plural_marriage_mandate kernel decomposes into three constraint stories corresponding to three distinct readings of the 1890 Manifesto. Each reading instantiates a different constraint with a different ε, different beneficiary/victim structure, and different computed type. ENDOGENOUS REINTERPRETATION (this story): Manifesto is legitimate prophetic reinterpretation; type is rope. EXOGENOUS OVERRIDE (sibling): Manifesto is federal coercion forcing abandonment of divine requirement; type is snare. INSTITUTIONAL PRAGMATISM (sibling): Manifesto is strategic adaptation where revelation-narrative legitimates survival-driven capitulation; type is tangled_rope. All three share the same kernel (plural marriage mandate), the same historical event (the Manifesto), and the same structural participants (church, mainstream membership, fundamentalists). They differ in HOW they account for the Manifesto's legitimacy and authority. The reading_relations edges (forecloses, coexists_with, influences) model the logical structure of the contest. Per the ε-invariance principle (DP-001), the readings are separate constraints, not measurements of a single constraint; they appear as distinct constraint_id entries in the corpus, linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
