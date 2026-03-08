% ============================================================================
% CONSTRAINT STORY: witness_obligation_without_recipient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_witness_obligation_without_recipient, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: witness_obligation_without_recipient
 *   human_readable: Witness Obligation Without Recipient
 *   domain: epistemology/systems_theory/labor_studies
 *
 * SUMMARY:
 *   The witness obligation without recipient constraint emerges when
 *   institutional architecture creates a structural gap between observation
 *   capacity and reception capacity. An observer possesses a structurally
 *   complete account — full documentation, verified data, coherent narrative
 *   — but exists within an institutional system where no entity can receive
 *   or act on that completeness. The administrative apparatus can acknowledge
 *   receipt and file the account, but its instruments cannot process the
 *   content. The institutional cores already possess the information through
 *   direct channels and do not need the formal archive. The witness faces a
 *   choice: transmit to those who already know (cores) or retain in an unread
 *   administrative queue. Both options preserve the witness obligation (the
 *   professional duty to document) while nullifying its epistemic function
 *   (the account produces no institutional learning or action). This
 *   constraint is downstream of two structural features: measurement
 *   apparatus bidirectionality (the act of observation changes the observer,
 *   creating identity fusion with the witnessing role) and categorical
 *   instrument blindness (institutional instruments are optimized for their
 *   native categories and cannot process observations that cross category
 *   boundaries). The constraint exhibits tangled rope structure: genuine
 *   coordination function (liability management, institutional legitimacy
 *   through documented process) coexists with asymmetric extraction (witness
 *   labor produces theater rather than epistemic integration).
 *
 * KEY AGENTS:
 *   - Observer as Witness: Primary victim (powerless/identity_locked) — professional identity constituted through documentation obligation; structurally mobile but cannot exit without abandoning professional self-concept
 *   - Administrative Apparatus: Primary beneficiary (institutional/arbitrage) — captures coordination value (liability shield, process documentation) without bearing cost of witness labor
 *   - Institutional Cores: Secondary beneficiary (institutional/arbitrage) — already possess information through direct channels; formal archive legitimates their knowledge claims
 *   - Compliance Officer: Mixed position (moderate/constrained) — benefits from coordination function (audit trail) while bearing extraction (maintaining unread logs)
 *   - Archival Profession: Organized agents (organized/mobile) — maintain infrastructure but see own function as degraded (piton perspective)
 *   - Epistemic Commons: Secondary victim (powerless/trapped) — abstract collective good; complete accounts exist but are not integrated into institutional knowledge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(witness_obligation_without_recipient, 0.48).
domain_priors:suppression_score(witness_obligation_without_recipient, 0.62).
domain_priors:theater_ratio(witness_obligation_without_recipient, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(witness_obligation_without_recipient, extractiveness, 0.48).
narrative_ontology:constraint_metric(witness_obligation_without_recipient, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(witness_obligation_without_recipient, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(witness_obligation_without_recipient, tangled_rope).
narrative_ontology:human_readable(witness_obligation_without_recipient, "Witness Obligation Without Recipient").
narrative_ontology:topic_domain(witness_obligation_without_recipient, "epistemology/systems_theory/labor_studies").

domain_priors:requires_active_enforcement(witness_obligation_without_recipient).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(witness_obligation_without_recipient, administrative_apparatus).
narrative_ontology:constraint_beneficiary(witness_obligation_without_recipient, institutional_cores).
narrative_ontology:constraint_victim(witness_obligation_without_recipient, observer_as_witness).
narrative_ontology:constraint_victim(witness_obligation_without_recipient, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBSERVER AS WITNESS (SNARE) — Identity-locked by professional obligation to document and report. Structurally mobile (could leave the role) but identity is constituted through the witnessing function. Cannot stop documenting without ceasing to be the kind of professional they are. Experiences maximum extraction: labor of complete documentation yields no epistemic function, only administrative theater.
constraint_indexing:constraint_classification(witness_obligation_without_recipient, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPLIANCE OFFICER (TANGLED ROPE) — Constrained by institutional position but benefits from the coordination function: the archive serves as liability shield and process documentation. Experiences mixed extraction: genuine coordination value (audit trail, legal protection) alongside extraction (labor of maintaining unread logs, theater of completeness).
constraint_indexing:constraint_classification(witness_obligation_without_recipient, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ADMINISTRATIVE APPARATUS (ROPE) — Primary beneficiary. The constraint coordinates liability management and institutional legitimacy. The archive's existence (not its content) provides legal cover and demonstrates due diligence. Experiences as pure coordination: the witness's labor produces the institutional good of documented process.
constraint_indexing:constraint_classification(witness_obligation_without_recipient, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL CORES (ROPE) — Secondary beneficiary. Already possess the information through direct channels. The formal archive serves as confirmation and legitimation of what they already know. Low extraction: the constraint coordinates their knowledge claims with institutional authority without imposing cost.
constraint_indexing:constraint_classification(witness_obligation_without_recipient, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ARCHIVAL PROFESSION (PITON) — Organized agents maintaining the infrastructure of witness documentation. See their own function as degraded: the archive persists through institutional inertia, not because it serves epistemic function. High theater ratio: maintaining unread logs is performative compliance, not knowledge work.
constraint_indexing:constraint_classification(witness_obligation_without_recipient, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both coordination function (liability management, institutional legitimacy) and extraction mechanism (witness labor produces theater rather than epistemic function). The constraint genuinely coordinates institutional risk management while extracting from the witness whose complete account has no recipient capable of processing it.
constraint_indexing:constraint_classification(witness_obligation_without_recipient, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(witness_obligation_without_recipient_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(witness_obligation_without_recipient, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(witness_obligation_without_recipient, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(witness_obligation_without_recipient, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(witness_obligation_without_recipient, TR),
    TR >= 0.70.

:- end_tests(witness_obligation_without_recipient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The witness's labor of complete documentation produces institutional goods (liability management, legitimacy) but not epistemic goods (learning, action, integration). The extraction is not maximal because some coordination value exists — the archive genuinely serves legal and compliance functions. But the extraction is substantial because the witness's epistemic labor (producing a complete account) is converted into administrative theater (maintaining an unread log). Suppression (0.62): Moderate-high. The witness cannot exit the obligation without abandoning professional identity. Structural barriers include professional licensing requirements, employment contracts, and institutional norms that define competent practice as complete documentation. Internalized barriers include identity fusion: the witness's self-concept is constituted through the witnessing role. The suppression is not total — some witnesses do exit, and some institutions create genuine reception capacity — but it is substantial. Theater ratio (0.68): High. The archive's primary function is performative: demonstrating institutional due diligence, providing litigation defense, satisfying compliance requirements. The archive's epistemic function (informing decisions, enabling learning) is minimal. The theater ratio has increased over the interval as documentation requirements have expanded faster than reception capacity.
 *
 * PERSPECTIVAL GAP:
 *   The witness experiences a snare: identity-locked into documentation labor that produces no epistemic function. The administrative apparatus experiences a rope: the constraint coordinates liability management and institutional legitimacy. The compliance officer experiences a tangled rope: genuine coordination value (audit trail) coexists with extraction (maintaining unread logs). The institutional cores experience a rope: the archive legitimates their knowledge claims without imposing cost. The archival profession experiences a piton: their function persists through institutional inertia despite degraded epistemic value. The analytical observer sees the tangled rope structure: genuine coordination (liability management) coexists with asymmetric extraction (witness labor produces theater). The perspectival gap reveals that 'witness obligation' is not a single phenomenon but a presheaf over observation sites: the same structural data (complete account in unread archive) appears as professional duty (witness), institutional asset (apparatus), degraded ritual (archival profession), or extractive mechanism (analytical observer) depending on the observer's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The observer as witness is identity-locked: professional identity is constituted through the documentation obligation. Exit would require not just leaving the role but abandoning the self-concept of competent professional. This produces high directionality (d ≈ 0.89) — the witness is structurally a victim with constrained exit, and the identity lock raises d further. The administrative apparatus is a beneficiary with arbitrage exit: it captures coordination value (liability shield) without bearing witness labor cost, producing low directionality (d ≈ 0.05). The compliance officer is a mixed case: constrained exit (cannot easily leave institutional role) but benefits from coordination function (audit trail protects them too), producing moderate directionality (d ≈ 0.55). The institutional cores are beneficiaries with arbitrage exit: they already know the information and gain legitimation without cost, producing low directionality (d ≈ 0.15). The archival profession is organized with mobile exit: they can leave the field, and they see their own function as degraded, producing moderate directionality (d ≈ 0.40) — not victims but not pure beneficiaries either.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled rope classification requires BOTH coordination function AND asymmetric extraction, not merely the appearance of both. The administrative apparatus genuinely benefits from the archive's existence (liability shield, compliance documentation) — this is not theater masking pure extraction. The witness genuinely bears costs (labor of complete documentation, identity lock preventing exit) — this is not voluntary coordination. The constraint coordinates institutional risk management while extracting from the witness whose complete account has no recipient capable of processing it. The tangled rope classification prevents two errors: (1) classifying as rope (pure coordination) would erase the witness's extraction and identity lock; (2) classifying as snare (pure extraction) would erase the genuine coordination function that benefits the administrative apparatus. The constraint is genuinely hybrid: it solves a real coordination problem (institutional liability management) through a mechanism that imposes asymmetric costs (witness labor producing unread archives).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recipient_capability_threshold,
    'What constitutes a ''capable recipient'' — one who can process the complete account vs one who merely acknowledges receipt?',
    'Operational definition: track whether archived accounts lead to institutional action, policy change, or epistemic integration. If archive is consulted only during litigation or audit, recipient capability is administrative (theater). If archive informs decision-making, recipient capability is epistemic (coordination).',
    'If threshold is low (acknowledgment suffices): constraint is rope from more perspectives. If threshold is high (processing required): constraint is snare from more perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recipient_capability_threshold, conceptual, 'Definition of recipient capability for witness accounts').

omega_variable(
    witness_identity_fusion_mechanism,
    'Is the witness''s identity lock structural (professional obligation enforced by licensing/employment) or internalized (self-concept constituted through witnessing role)?',
    'Post-exit trajectory analysis: if witnesses who leave the role continue documenting in personal capacity, identity fusion is internalized. If documentation ceases immediately upon role exit, identity lock was structural.',
    'If internalized: identity_locked classification confirmed, suppression is higher than structural measure suggests. If structural: constrained or trapped classification more accurate, suppression is measurable external barrier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(witness_identity_fusion_mechanism, empirical, 'Mechanism of witness identity lock').

omega_variable(
    archive_latency_value,
    'Does the archive''s epistemic value emerge over time (future historians, delayed institutional learning) or is it permanently theater?',
    'Historical analysis: track whether archived witness accounts from previous generations are consulted for current decision-making. If archives older than 10 years are never accessed except for litigation, latency value is zero. If older archives inform policy, latency value is real.',
    'If latency value is real: scaffold perspective emerges (temporary coordination failure with sunset as institutional learning matures). If latency value is zero: snare perspective strengthened (extraction is permanent, not transitional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_latency_value, empirical, 'Whether archive has delayed epistemic value').

omega_variable(
    transmission_to_cores_alternative,
    'If the witness transmits directly to institutional cores (bypassing the archive), does this resolve the obligation or create additional extraction (dual documentation burden)?',
    'Comparative analysis: witnesses who transmit to cores vs those who file only to archive. Measure career outcomes, institutional response rates, and documentation labor hours. If direct transmission reduces total labor and increases response, it resolves the constraint. If it creates dual burden with no increased response, it compounds extraction.',
    'If direct transmission resolves: the constraint is coordination failure (rope/scaffold from more perspectives). If it compounds: the constraint is structural extraction (snare from more perspectives) — the archive is not a failed coordination mechanism but a deliberate labor sink.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transmission_to_cores_alternative, empirical, 'Whether direct transmission to cores resolves witness obligation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(witness_obligation_without_recipient, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(witness_tr_t0, witness_obligation_without_recipient, theater_ratio, 0, 0.45).
narrative_ontology:measurement(witness_tr_t3, witness_obligation_without_recipient, theater_ratio, 3, 0.55).
narrative_ontology:measurement(witness_tr_t6, witness_obligation_without_recipient, theater_ratio, 6, 0.62).
narrative_ontology:measurement(witness_tr_t10, witness_obligation_without_recipient, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(witness_be_t0, witness_obligation_without_recipient, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(witness_be_t3, witness_obligation_without_recipient, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(witness_be_t6, witness_obligation_without_recipient, base_extractiveness, 6, 0.43).
narrative_ontology:measurement(witness_be_t10, witness_obligation_without_recipient, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(witness_obligation_without_recipient, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of measurement_apparatus_bidirectionality (mountain — observation changes observer, creating identity fusion) and categorical_instrument_blindness (rope — institutional instruments optimized for native categories cannot process cross-boundary observations). The witness obligation emerges from the interaction: the observer is identity-locked into witnessing (bidirectionality) while the institution lacks reception capacity (instrument blindness). The upstream constraints are structurally distinct: bidirectionality has ε ≈ 0.08 (mountain), instrument blindness has ε ≈ 0.15 (rope), witness obligation has ε = 0.48 (tangled rope). The ε values differ because they measure different structural features: bidirectionality measures the inevitability of observer transformation, instrument blindness measures the coordination cost of categorical specialization, witness obligation measures the extraction from identity-locked documentation labor.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
