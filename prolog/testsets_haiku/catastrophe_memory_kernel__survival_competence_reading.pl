% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Ritual Transmission of Persecution-Survival Competence
 *   domain: religious/social/adaptive
 *
 * SUMMARY:
 *   Among persecuted communities with endemic or recurring threat of
 *   persecution, ritual practice encodes and transmits adaptive capacity for
 *   survival under hostile conditions. The constraint structures how survival
 *   knowledge — safe-house networks, warning signals, family regrouping
 *   protocols, concealment techniques — is preserved and transmitted across
 *   generations when formal education channels are forbidden, monitored, or
 *   controlled by hostile authority. The ritual appears to the outside
 *   observer as religious or commemorative practice; its operational content
 *   (the knowledge it encodes) is hidden. The community experiences the
 *   constraint as essential: the ritual is how the group stays alive by
 *   rehearsing and transmitting what to do when persecution comes. The
 *   constraint imposes costs: time investment, the burden of maintaining
 *   distinctiveness despite assimilation pressure, and the enforcement of
 *   participation (especially on younger members who may prefer
 *   assimilation). The claim is tangled_rope — genuine coordination (survival
 *   knowledge transmission) coupled with asymmetric extraction (suppression
 *   of assimilation choices, boundary-maintenance burden on those who would
 *   prefer to blend in). The metrics reflect moderate extractiveness (0.48
 *   endpoint) because the coordination function is real but so is the
 *   extraction; suppression is substantial (0.62) because the constraint
 *   persists partly by actively excluding assimilationist exit.
 *
 * KEY AGENTS:
 *   - persecuted_community: powerless (structurally vulnerable to persecution), identity-locked (cannot leave without ceasing to exist as the group), generational time-horizon. Experiences the ritual as essential survival transmission.
 *   - assimilationist_pressure_bearers: moderate power (individuals with some agency), identity-locked (exit from ritual means exit from community), biographical time-horizon. Experience cost of maintaining distinctiveness and resisting assimilation.
 *   - ritual_specialists: moderate power (gatekeeping authority over ritual content and transmission), constrained exit (their role is constituted by the ritual), generational time-horizon. Agenda-setters who determine what knowledge gets encoded.
 *   - hostile_majority: institutional power (controls formal institutions, legal system, economic opportunity), analytical exit options (external observer position), generational time-horizon. Structurally excluded from ritual; would suppress it if they understood its operational content.
 *   - secular_ethnographer: analytical power, analytical exit, biographical time-horizon. Observer position; infers function from structure; cannot access the operational knowledge being encoded.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.48).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Ritual Transmission of Persecution-Survival Competence").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious/social/adaptive").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, '8e8db31a-f763-4e31-b5e4-e861b5d54f81').
narrative_ontology:cs_kernel_codification('8e8db31a-f763-4e31-b5e4-e861b5d54f81', implicit).
narrative_ontology:cs_authority_grounding('8e8db31a-f763-4e31-b5e4-e861b5d54f81', practice).
narrative_ontology:cs_interpretation_layer_present('8e8db31a-f763-4e31-b5e4-e861b5d54f81').
narrative_ontology:cs_reading_relation('8e8db31a-f763-4e31-b5e4-e861b5d54f81', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e8db31a-f763-4e31-b5e4-e861b5d54f81', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e8db31a-f763-4e31-b5e4-e861b5d54f81', catastrophe_memory_kernel__boundary_maintenance_reading, influences).
narrative_ontology:cs_axiom('8e8db31a-f763-4e31-b5e4-e861b5d54f81', foundational, ritual_encodes_actionable_survival_knowledge).
narrative_ontology:cs_axiom_status(ritual_encodes_actionable_survival_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('8e8db31a-f763-4e31-b5e4-e861b5d54f81', ritual_encodes_actionable_survival_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('8e8db31a-f763-4e31-b5e4-e861b5d54f81', secondary, persecution_threat_requires_hidden_transmission).
narrative_ontology:cs_axiom_status(persecution_threat_requires_hidden_transmission, holdable).
narrative_ontology:cs_axiom_grounding('8e8db31a-f763-4e31-b5e4-e861b5d54f81', persecution_threat_requires_hidden_transmission, empirically_contingent).
narrative_ontology:cs_reference_frame('8e8db31a-f763-4e31-b5e4-e861b5d54f81', persecution_survival_through_encoded_knowledge).
narrative_ontology:cs_drift_state('8e8db31a-f763-4e31-b5e4-e861b5d54f81', contemporary_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8e8db31a-f763-4e31-b5e4-e861b5d54f81', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, persecuted_community).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, assimilationist_pressure_bearers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness starts at 0.35 (early period: threat is high, ritual is functional, community consensus on necessity is strong, less theater) and rises to 0.48 by interval end (later period: threat perception may be declining or shifting, more ritual content is ornamental relative to operational, increased internal questioning of necessity). The rise is gentle (0.35→0.48 over 120 units) rather than steep, suggesting the constraint stabilizes around moderate extractiveness — the coordination function is real enough to justify the cost for most of the community most of the time. Theater ratio rises from 0.25 to 0.41 over the same interval, indicating that as external threat may decline or become less immediate, a larger share of ritual activity is devoted to identity-maintenance and group-bonding rather than operational rehearsal. Suppression requirement rises from 0.55 to 0.64 (peaking at t=60) and then stabilizes, suggesting that enforcement intensity increased as external threat perception changed or as internal resistance to the constraint's burden increased, then stabilized as new equilibrium was reached. Accessibility collapse at 0.72 is high: once a community member understands what the ritual encodes and why, the 'alternatives' (assimilate, abandon the knowledge, trust external institutions for survival) become much less attractive — the knowledge horizon collapses because the threat is real. Resistance at 0.55 is moderate: the community does resist (some want to assimilate, some question the necessity, some experience the burden as excessive), but the resistance doesn't grow into exit because identity-lock holds. The measurements are all authored on a single time grid so every metric is present at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The persecuted_community and ritual_specialists experience this constraint as coordination: it is how we stay alive together, how the knowledge survives, how we maintain the capacity to respond. From their seats, the suppression and extraction are the price of that survival, not an unjust burden. The assimilationist_pressure_bearers (especially younger members) may experience it differently: as a constraint on their individual freedom, a requirement they didn't choose, a burden of distinctiveness they want to shed. The hostile_majority, if they understood the operational content, would experience it as seditious: clandestine organization for resistance. The secular observer sees a system that genuinely encodes knowledge but can't determine whether that knowledge is essential (empirically necessary for survival) or redundant (survival knowledge is accessible through other channels, and the ritual persists for reasons of identity and group-bonding rather than operational necessity). The engine computes seat-specific classifications from the structural data; the perspectival gap is inherent in the constraint's structure, not an error in measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The persecuted_community and ritual_specialists are beneficiaries (they gain survival capacity, they control the transmission) with low directionality toward extraction — they are the seats the coordination function serves. The assimilationist_pressure_bearers are victims of the constraint's suppression — they bear the cost of forced distinctiveness and enforced participation. The constraint's base extraction (0.48) is scaled upward for targets (assimilationist pressure-bearers) by their high suppression and low exit options (identity-locked: they cannot leave without losing community and identity). The hostile_majority is excluded by design; their absence is part of the constraint's structure (secrecy is a survival mechanism). The beneficiary/victim split is what drives the tangled_rope classification: the same constraint that provides genuine coordination (survival knowledge) also enforces asymmetry (suppression of assimilation, boundary-maintenance burden).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to transmit survival knowledge when formal institutions are controlled by hostile authority) is live for communities under actual persecution threat. The constraint would resolve to piton (inertial survival of tradition despite atrophied function) only if and when the persecution threat substantially declines but the ritual persists. The measurement series shows extractiveness stabilizing around 0.48 by interval end, and theater ratio rising to 0.41, which is consistent with a constraint entering a period where the founding problem's acuteness may be declining but the ritual persists. If the interval spans a period of decreasing persecution threat, the constraint would drift toward piton or boundary_maintenance_reading (ritual as group-identity marker rather than survival-knowledge transmission). The mandatrophy resolution depends on empirical determination of threat persistence (omega: persecution_threat_persistence). If threat is live, the constraint remains tangled_rope. If threat has substantially declined, the constraint has likely drifted toward a different reading (boundary_maintenance or symbol_continuity) or toward piton (theater ratio rising, operational content declining, ritual persists as tradition).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_vs_symbolic_content,
    'Does the ritual genuinely encode operationally useful survival knowledge (safe-house networks, warning signals, evasion routes), or is the operational knowledge preserved through other channels (family instruction, written codes, clandestine education) while the ritual serves primarily symbolic and community-bonding functions?',
    'Detailed ethnographic access to ritual specialists and community members who have used ritual-encoded knowledge in actual persecution scenarios; comparison of survival outcomes between communities that maintain strong ritual practice and those that have abandoned it in favor of explicit instruction; analysis of whether ritual performance correlates with demonstrable survival competence.',
    'If operational: the extraction component (suppression, time burden, assimilation pressure) is the price of genuine survival capacity, supporting a tangled_rope classification. If symbolic: the constraint becomes more purely extractive (snare-like), with less genuine coordination function offsetting the boundary-maintenance burden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(operational_vs_symbolic_content, empirical, 'Whether survival knowledge is genuinely encoded in ritual or primarily preserved through other channels.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of ritual abandonment (the cost of leaving the community, or the internal enforcement of participation) structurally enforced (legal penalties, economic exclusion, social shunning by organized group enforcement) or internalized (identity fusion, spiritual conviction, fear of damnation, belief in supernatural consequence)?',
    'Post-exit trajectory analysis: if members who leave the community and the ritual maintain the suppression (identity damage, psychological guilt, fear), suppression is substantially internalized; if suppression drops markedly post-exit, it was primarily structural. Interviews with lapsed practitioners and apostates; analysis of exit narratives.',
    'If internalized: the constraint''s effective suppression exceeds the structural measure; the target carries the suppression with them after exit, making re-entry or re-assimilation difficult. This supports a higher effective extraction. If structural: suppression is confined to active membership and drops at exit boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Suppression mechanism: structural (external barriers) vs. internalized (cognitive/identity fusion).').

omega_variable(
    survival_competence_vs_trauma_processing,
    'Is the ritual''s primary function to encode actionable survival responses (operational knowledge), or to process and transmit intergenerational trauma as a warning system (emotional/psychological meaning-making)?',
    'This is a reading-identity question (kernel decomposition). The survival_competence_reading privileges operational knowledge transmission; the trauma_encoding_reading privileges emotional/psychological integration. Ethnographic documentation of how ritual specialists describe the function; outcome analysis of whether communities using the ritual show better survival outcomes (competence) or better trauma recovery (processing). These may not be mutually exclusive, but the weighting differs between readings.',
    'The survival_competence_reading (this one) emphasizes actionable knowledge, moderate extraction justified by genuine resilience-building. The trauma_encoding_reading would emphasize emotional survival and group healing, with extraction reframed as the cost of processing collective suffering. The readings coexist; the question is which function drives the constraint''s persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_competence_vs_trauma_processing, conceptual, 'The core distinction between survival_competence_reading and trauma_encoding_reading: is the ritual''s primary function actionable knowledge or emotional processing?').

omega_variable(
    persecution_threat_persistence,
    'Does the founding problem (persecution threat requiring hidden knowledge transmission) remain live, or has the persecution environment changed enough that the survival knowledge becomes vestigial while the ritual persists as tradition and group-identity marker?',
    'Historical and contemporary analysis of actual persecution threat levels; testimony from community members about whether they fear persecution; comparison of ritual practice intensity to threat assessment; analysis of whether newly-adapted survival protocols (digital communication, international networks, legal advocacy) have replaced ritual-encoded knowledge.',
    'If the threat is live: the constraint remains a tangled_rope, with suppression/extraction justified by genuine survival need. If the threat is dead or substantially reduced: the constraint drifts toward piton (theatrical maintenance of tradition) or toward symbol_continuity_reading (ritual as identity continuity rather than survival training). The measurement series shows extractiveness peaking at t=60 and then stabilizing; if threat assessment drops over the interval, this may indicate drift toward piton or reading-shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(persecution_threat_persistence, empirical, 'Whether the persecution threat that justifies the constraint persists or has substantially declined.').

omega_variable(
    kernel_reading_coherence,
    'The catastrophe_memory_kernel has four readings: survival_competence (this one), trauma_encoding, symbol_continuity, and boundary_maintenance. To what extent do these readings describe genuinely different constraints (different ε, different beneficiary structures, different persistence mechanisms) versus different interpretations of the same constraint''s function?',
    'Structural analysis: do the readings yield different ε estimates? Different beneficiary/victim distributions? Different persistence mechanisms? If yes, they are different constraints (per ε-invariance principle); if no, they are readings of the same constraint. The network.affects_constraints entries will link them; the resolution mechanism is analytical unpacking of whether the sibling readings'' metrics and beneficiary structures are genuinely distinct.',
    'If the readings are genuinely distinct constraints: each has its own story file, own metrics, own type classification. If they are interpretations of the same constraint: the constraint should be analyzed from all four readings'' perspectives, and the type classification may reflect multiple simultaneous readings (rare but possible). This affects how the corpus treats the constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coherence, conceptual, 'Whether the sibling readings are separate constraints or interpretations of a single constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 60, 0.43).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 100, 0.41).
narrative_ontology:measurement(cata_tr_t120, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 120, 0.41).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 60, 0.51).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 80, 0.49).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 100, 0.48).
narrative_ontology:measurement(cata_be_t120, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 120, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 60, 0.64).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 80, 0.63).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 100, 0.62).
narrative_ontology:measurement(cata_su_t120, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 120, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__survival_competence_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_kernel has four readings instantiating four structurally distinct constraints over a shared persecuted-community ritual practice. The survival_competence_reading (this file) emphasizes operational knowledge transmission and genuine survival coordination. The trauma_encoding_reading emphasizes emotional processing of collective suffering. The symbol_continuity_reading emphasizes identity and cultural continuity across time. The boundary_maintenance_reading emphasizes group boundary enforcement through shared practice. Each reading has different ε, different beneficiary/victim structure, and different persistence mechanisms. They coexist as live interpretations and may all be partially true — the ritual may simultaneously encode survival knowledge, process trauma, preserve identity, and enforce boundaries. The readings are linked via network.affects_constraints to enable downstream analysis of how readings interact, which reading dominates under different threat conditions, and how the constraint family transforms as the persecution environment changes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__survival_competence_reading, powerless, 0.28).
constraint_indexing:directionality_override(catastrophe_memory_kernel__survival_competence_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
