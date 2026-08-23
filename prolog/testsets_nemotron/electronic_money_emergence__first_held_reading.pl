% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__first_held_reading, []).

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
 *   constraint_id: electronic_money_emergence__first_held_reading
 *   human_readable: First Institutional Holding of Dematerialized Currency
 *   domain: economic/monetary/technological
 *
 * SUMMARY:
 *   This constraint story instantiates the 'first_held_reading' of the
 *   electronic_money_emergence kernel: the claim that digital money emerged
 *   as a discrete institutional event when the first institutional bearer (a
 *   central bank or designated commercial bank) held dematerialized currency
 *   in a form legally and operationally distinguishable from physical notes.
 *   The reading anchors emergence in a measurable, regulatory threshold — the
 *   moment electronic reserves or digital ledger entries became the legal
 *   equivalent of physical currency for settlement purposes. This reading
 *   competes with two sibling readings: became_thinkable_reading (emergence
 *   as conceptual possibility) and m4_m5_collapse_reading (emergence as
 *   statistical measurement artifact). The metrics describe a constraint with
 *   very low extractiveness and suppression, high accessibility_collapse
 *   (alternatives genuinely disappear once the legal threshold is crossed),
 *   and low resistance — consistent with a mountain. However, the declared
 *   beneficiaries (central_banks, commercial_banking_sector,
 *   payment_infrastructure_providers) trigger the FSM candidate pathway: if
 *   the 'natural boundary' is actually a constructed category that benefits
 *   these agents, the constraint is a false summit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.12).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.08).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, mountain).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "First Institutional Holding of Dematerialized Currency").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic/monetary/technological").

domain_priors:emerges_naturally(electronic_money_emergence__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, '519280b4-bf9a-48a8-bfea-e7887f0ac330').
narrative_ontology:cs_kernel_codification('519280b4-bf9a-48a8-bfea-e7887f0ac330', formalized).
narrative_ontology:cs_authority_grounding('519280b4-bf9a-48a8-bfea-e7887f0ac330', lineage).
narrative_ontology:cs_interpretation_layer_present('519280b4-bf9a-48a8-bfea-e7887f0ac330').
narrative_ontology:cs_reading_relation('519280b4-bf9a-48a8-bfea-e7887f0ac330', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('519280b4-bf9a-48a8-bfea-e7887f0ac330', electronic_money_emergence__m4_m5_collapse_reading, influences).
narrative_ontology:cs_axiom('519280b4-bf9a-48a8-bfea-e7887f0ac330', foundational, legal_recognition_creates_monetary_ontology).
narrative_ontology:cs_axiom_status(legal_recognition_creates_monetary_ontology, holdable).
narrative_ontology:cs_axiom_grounding('519280b4-bf9a-48a8-bfea-e7887f0ac330', legal_recognition_creates_monetary_ontology, conventional).
narrative_ontology:cs_axiom('519280b4-bf9a-48a8-bfea-e7887f0ac330', secondary, institutional_bearer_is_necessary_for_settlement_finality).
narrative_ontology:cs_axiom_status(institutional_bearer_is_necessary_for_settlement_finality, holdable).
narrative_ontology:cs_axiom_grounding('519280b4-bf9a-48a8-bfea-e7887f0ac330', institutional_bearer_is_necessary_for_settlement_finality, instrumental).
narrative_ontology:cs_reference_frame('519280b4-bf9a-48a8-bfea-e7887f0ac330', central_bank_ledger_sovereignty).
narrative_ontology:cs_drift_state('519280b4-bf9a-48a8-bfea-e7887f0ac330', post_crypto_challenge_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('519280b4-bf9a-48a8-bfea-e7887f0ac330', '2026-08-15T14:32:17Z').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, central_banks).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, commercial_banking_sector).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, payment_infrastructure_providers).
narrative_ontology:constraint_vindicates(electronic_money_emergence__first_held_reading, monetary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(electronic_money_emergence__first_held_reading, legal_tender_continuity_principle).
narrative_ontology:constraint_vindicates(electronic_money_emergence__first_held_reading, settlement_finality_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain seigniorage, monetary policy transmission control, and regulatory authority from being the recognized first institutional holders of dematerialized currency. Their position is legally entrenched; exit would mean abandoning monetary sovereignty. They administer the constraint by defining what counts as legal electronic money.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, central_banks, beneficiary,
    institutional, generational, arbitrage, national).

% Gain settlement efficiency, balance sheet expansion capacity, and a franchised role in the electronic payment system. They are licensed participants in the central bank's ledger system; exit means losing access to the payment infrastructure and regulatory franchise. They benefit from the constraint but do not set its terms.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, commercial_banking_sector, beneficiary,
    organized, biographical, constrained, national).

% Gain a mandated market for electronic settlement services (SWIFT, ACH, RTGS systems, card networks). They operate the technical infrastructure that makes the 'first held' threshold operational. They have more exit mobility than banks — they could pivot to private settlement networks — but the regulatory franchise is lucrative.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, payment_infrastructure_providers, beneficiary,
    organized, biographical, mobile, global).

% Would offer alternative electronic money forms (stablecoins, CBDC competitors, private digital currencies) but are excluded from the 'first institutional bearer' status by the same legal threshold that defines the constraint. They argue the threshold is arbitrary and protects incumbents; their exclusion is the enforcement surface of the constraint.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, fintech_innovators, excluded,
    moderate, immediate, constrained, global).

% Analyze the emergence of electronic money across different readings. They see the first_held_reading as one legitimate framing among others, and document how the legal threshold interacts with conceptual and statistical framings. They neither collect from nor pay into the constraint.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, monetary_historians, observer,
    analytical, civilizational, analytical, universal).

% Are structurally excluded from the electronic money system defined by the 'first institutional bearer' threshold because they lack access to the licensed banking infrastructure. The constraint's legal recognition framework does not accommodate them; their exclusion is a systemic consequence of the institutional bearer requirement, not an active targeting.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, unbanked_populations, excluded,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__first_held_reading, central_banks).
narrative_ontology:fixing_cost_class(electronic_money_emergence__first_held_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, legally recognized threshold for what counts as 'electronic money' in systemic settlement — solving the coordination problem of multiple incompatible digital representations by anchoring finality in the central bank's ledger.
% TRANSFER_FUNCTION: Moves seigniorage, regulatory authority, and settlement franchise from the public domain (where multiple private issuers could compete) to the designated institutional bearers (central banks, licensed commercial banks, authorized payment infrastructure). The transfer is not a direct payment but a legal monopoly grant.
% ABSENT_VOICES: Fintech innovators and unbanked populations are structurally excluded. Fintech innovators would argue for open competition in electronic money issuance; unbanked populations would argue for inclusion in the settlement system. Both are kept out by the legal threshold that defines the 'first institutional bearer.'
% DISAPPEARANCE_RATIONALE: If the legal threshold of 'first institutional bearer holding dematerialized currency' vanished overnight, the settlement finality anchor for electronic payments would disappear. Multiple competing digital currency systems would emerge (stablecoins, private bank tokens, crypto assets), the central bank's monetary policy transmission would fracture, and the payment system would reorganize around a multiplicity of finality layers rather than a single ledger.
% FOUNDING_PROBLEM: The transition from physical to dematerialized currency required a legally certain point at which electronic entries became 'money' for settlement finality purposes — without which the payment system could not migrate from paper to electronic rails without fragmentation and trust collapse.
% FOUNDING_PROBLEM_CORROBORATION: Central banks attest the problem remains live (citing stablecoin fragmentation and CBDC necessity). Fintech advocates and monetary historians outside the beneficiary set (e.g., Selgin, White, and the Free Banking school; Bank for International Settlements working papers on payment system pluralism) attest the founding problem was substantially solved by the 1990s RTGS rollout and the arrangement persists as regulatory perimeter protection.
narrative_ontology:disappearance_verdict(electronic_money_emergence__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__first_held_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(electronic_money_emergence__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__first_held_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__first_held_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, ExtMetricName, E),
    domain_priors:suppression_score(electronic_money_emergence__first_held_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(electronic_money_emergence__first_held_reading),
    narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(electronic_money_emergence__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint primarily defines a legal/ontological boundary rather than transferring resources — the 'extraction' is the seigniorage and regulatory privilege that accrue to the beneficiaries from being the recognized holders of electronic money. Suppression is very low (0.08) because no active coercion maintains the threshold; it is a recognition event. Theater_ratio (0.15) reflects the ceremonial/performative aspects of central bank announcements and regulatory frameworks that frame the transition as managed rather than inevitable. Accessibility_collapse (0.88) is high because once an institutional bearer holds dematerialized currency with legal recognition, alternative forms (private scrip, unregulated digital tokens) lose settlement finality for systemic payments. Resistance (0.18) is low because the transition was largely welcomed by the financial system for efficiency. The measurement series show a slow accumulation of extractiveness and theater as the electronic money system matured and layered additional regulatory and commercial structures onto the original threshold.
 *
 * DIRECTIONALITY LOGIC:
 *   Central_banks sit at d ≈ 0.05 (full beneficiary: they gain seigniorage, monetary policy transmission, and regulatory authority). Commercial_banking_sector sits at d ≈ 0.15 (beneficiary: they gain settlement efficiency, balance sheet expansion capacity, and payment franchise). Payment_infrastructure_providers sit at d ≈ 0.25 (beneficiary: they gain a mandated market for electronic settlement services). The analytical observer seat (this reading's authoring position) sits at d ≈ 0.5. No victim seats are declared because the constraint as defined is a recognition threshold, not an extraction mechanism — though the FSM omega questions whether the beneficiaries' gains constitute hidden extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (settlement finality and monetary sovereignty in a dematerializing world) is contested: central banks attest it remains live (ongoing threats from private digital currencies), while fintech advocates and some monetary historians attest it is substantially solved and the arrangement persists as regulatory capture. The mandate has not clearly outlived its function — electronic settlement infrastructure continues to require a finality anchor — but the regulatory perimeter has expanded beyond the original problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ontology,
    'Is the threshold ''first institutional bearer holds dematerialized currency'' a genuine natural boundary in monetary ontology, or a constructed category that benefits identifiable agents?',
    'Cross-historical comparison: if the same legal/regulatory recognition pattern appears across independent monetary systems (e.g., different central banks adopting electronic reserves at different times), the boundary is more natural; if the timing and form correlate with specific institutional interests, it is more constructed.',
    'If constructed, the constraint is a false summit candidate — the engine''s FSM signature would reclassify it as tangled_rope, revealing the extraction of seigniorage and regulatory capture layered onto the ontological claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ontology, conceptual, 'Whether the first-held threshold is a natural monetary boundary or an institutional construction').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does this reading''s structural disagreement with its sibling readings locate — in the kernel''s codification, the authority''s grounding, or the drift measurement?',
    'Structural mapping of each reading''s cs_structure declarations: the reading that declares formalized+lineage disagrees with the reading that declares distributed+practice on both kernel_codification and authority_grounding; the reading that declares fixed_text+extraction disagrees on authority_grounding. The precise delta is the committer structure itself.',
    'If the disagreement is in kernel_codification, the readings are different constraint families; if only in authority_grounding, they share a kernel but contest its adjudication. This determines whether FSM applies to one reading or the whole family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Structural locus of disagreement between first_held_reading, became_thinkable_reading, and m4_m5_collapse_reading').

omega_variable(
    regulatory_capture_of_emergence_narrative,
    'Does the ''first held'' narrative serve to legitimize central bank control over the electronic money transition, making the emergence appear inevitable and regulator-led rather than market-driven?',
    'Trace the genealogical usage: when central banks or regulators invoke ''first institutional holding'' as the emergence criterion, do they simultaneously claim exclusive authority over the resulting electronic money regime? Correlate with the founding_problem_status and corroboration.',
    'If the narrative functions as legitimation, the beneficiaries declared (central_banks, commercial_banking_sector) are not incidental — the mountain claim masks a tangled_rope structure where regulatory authority extracts control over the monetary transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_of_emergence_narrative, empirical, 'Whether the first_held emergence narrative functions as regulatory legitimation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 1960, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eme_fhr_tr_t1960, electronic_money_emergence__first_held_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(eme_fhr_tr_t1970, electronic_money_emergence__first_held_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(eme_fhr_tr_t1980, electronic_money_emergence__first_held_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(eme_fhr_tr_t1990, electronic_money_emergence__first_held_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(eme_fhr_tr_t2000, electronic_money_emergence__first_held_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(eme_fhr_tr_t2010, electronic_money_emergence__first_held_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(eme_fhr_tr_t2020, electronic_money_emergence__first_held_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(eme_fhr_be_t1960, electronic_money_emergence__first_held_reading, base_extractiveness, 1960, 0.05).
narrative_ontology:measurement(eme_fhr_be_t1970, electronic_money_emergence__first_held_reading, base_extractiveness, 1970, 0.06).
narrative_ontology:measurement(eme_fhr_be_t1980, electronic_money_emergence__first_held_reading, base_extractiveness, 1980, 0.08).
narrative_ontology:measurement(eme_fhr_be_t1990, electronic_money_emergence__first_held_reading, base_extractiveness, 1990, 0.1).
narrative_ontology:measurement(eme_fhr_be_t2000, electronic_money_emergence__first_held_reading, base_extractiveness, 2000, 0.11).
narrative_ontology:measurement(eme_fhr_be_t2010, electronic_money_emergence__first_held_reading, base_extractiveness, 2010, 0.12).
narrative_ontology:measurement(eme_fhr_be_t2020, electronic_money_emergence__first_held_reading, base_extractiveness, 2020, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(eme_fhr_su_t1960, electronic_money_emergence__first_held_reading, suppression_requirement, 1960, 0.02).
narrative_ontology:measurement(eme_fhr_su_t1970, electronic_money_emergence__first_held_reading, suppression_requirement, 1970, 0.03).
narrative_ontology:measurement(eme_fhr_su_t1980, electronic_money_emergence__first_held_reading, suppression_requirement, 1980, 0.05).
narrative_ontology:measurement(eme_fhr_su_t1990, electronic_money_emergence__first_held_reading, suppression_requirement, 1990, 0.07).
narrative_ontology:measurement(eme_fhr_su_t2000, electronic_money_emergence__first_held_reading, suppression_requirement, 2000, 0.08).
narrative_ontology:measurement(eme_fhr_su_t2010, electronic_money_emergence__first_held_reading, suppression_requirement, 2010, 0.08).
narrative_ontology:measurement(eme_fhr_su_t2020, electronic_money_emergence__first_held_reading, suppression_requirement, 2020, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__first_held_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__first_held_reading, 0.2).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__m4_m5_collapse_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, central_bank_digital_currency_mandate).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, payment_system_finality_law).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, commercial_bank_reserve_requirements).

% DUAL FORMULATION NOTE:
% This constraint family (electronic_money_emergence) decomposes the single natural-language concept 'electronic money emergence' into three structurally distinct constraints with different ε values, beneficiary structures, and type classifications. The first_held_reading claims mountain (ε=0.12); became_thinkable_reading likely claims mountain with even lower ε (conceptual boundary); m4_m5_collapse_reading likely claims tangled_rope or snare (measurement artifact with regulatory extraction). They are linked via affects_constraints because the legal threshold this reading identifies is often cited as evidence for the statistical category the m4_m5_collapse_reading treats as constructed, and the conceptual possibility the became_thinkable_reading identifies is the precondition both later readings presuppose.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(electronic_money_emergence__first_held_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
