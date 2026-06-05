% ============================================================================
% CONSTRAINT STORY: party_ban_instrument__chilling_critique_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_party_ban_chilling_critique, []).

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
 *   constraint_id: party_ban_instrument__chilling_critique_reading
 *   human_readable: Party Ban Instrument: Chilling Critique Reading
 *   domain: legal/political/constitutional
 *
 * SUMMARY:
 *   The party ban instrument in German constitutional law operates through a
 *   dual mechanism: formal dissolution authority (rare, requires Federal
 *   Constitutional Court judgment) and anticipatory chilling (omnipresent,
 *   requires only the standing possibility of dissolution). The
 *   chilling_critique reading focuses on the second mechanism — how the
 *   instrument disciplines political edges without active judgment.
 *   Surveillance by domestic security services intensifies when organizations
 *   approach the spectrum boundaries; funding sources dry up; organizational
 *   formation becomes legally risky. The extractiveness operates through
 *   uncertainty: the radical-but-legal opposition cannot know in advance
 *   which organizational forms, funding sources, or rhetorical positions will
 *   trigger dissolution proceedings. The instrument also enables funding
 *   exclusion mechanisms (political party funding transparency laws, denial
 *   of public broadcasting access) that tax dissent without requiring a ban
 *   judgment. This reading contrasts with the founding_precedents_reading
 *   (which emphasizes the SRP/KPD bans as precedents establishing settled
 *   doctrine) and the potentiality_threshold_reading (which emphasizes the
 *   NPD judgment's refinement: hostility to the order is insufficient without
 *   capacity to achieve it). The chilling_critique reading identifies a
 *   structural feature: the ban instrument extracts compliance through the
 *   disciplinary effect of its mere existence, independent of actual use.
 *
 * KEY AGENTS:
 *   - Radical-but-legal opposition: Primary victim (powerless/trapped) — bears the chilling effect; cannot exit because dissent is their constitutive activity; suppression operates through anticipation of possible dissolution
 *   - Civil society at spectrum edges: Secondary victim (moderate/constrained) — organizations, movements, funding networks operating at constitutional boundaries; experience suppression as funding exclusion, surveillance intensification, and organizational formation costs
 *   - Constitutional order (bounded political spectrum): Primary beneficiary (institutional/arbitrage) — maintains the boundary between protected and unprotected political forms; experiences the ban instrument as coordination of constitutional limits
 *   - Federal Constitutional Court: Authority structure (institutional/extraction) — holds the formal ban power; benefits from the instrument's deterrent effect (fewer cases reach judgment because organizations self-censor); extract flows from the threat of dissolution
 *   - Domestic security services (BfV): Enforcement apparatus (organized/constrained) — conduct surveillance at spectrum edges; monitor for triggering conditions; contribute to the chilling effect through visibility and documentation
 *   - Analytical observer: Observational position (analytical/analytical) — sees the structure: suppression through anticipation, extraction through uncertainty, coordination maintained without judgments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(party_ban_instrument__chilling_critique_reading, 0.58).
domain_priors:suppression_score(party_ban_instrument__chilling_critique_reading, 0.68).
domain_priors:theater_ratio(party_ban_instrument__chilling_critique_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(party_ban_instrument__chilling_critique_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(party_ban_instrument__chilling_critique_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(party_ban_instrument__chilling_critique_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(party_ban_instrument__chilling_critique_reading, tangled_rope).
narrative_ontology:human_readable(party_ban_instrument__chilling_critique_reading, "Party Ban Instrument: Chilling Critique Reading").
narrative_ontology:topic_domain(party_ban_instrument__chilling_critique_reading, "legal/political/constitutional").

domain_priors:requires_active_enforcement(party_ban_instrument__chilling_critique_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(party_ban_instrument__chilling_critique_reading, '69ba9740-4390-4f53-b678-a5f9dbf8a2e3').
narrative_ontology:cs_kernel_codification('69ba9740-4390-4f53-b678-a5f9dbf8a2e3', formalized).
narrative_ontology:cs_authority_grounding('69ba9740-4390-4f53-b678-a5f9dbf8a2e3', lineage).
narrative_ontology:cs_interpretation_layer_present('69ba9740-4390-4f53-b678-a5f9dbf8a2e3').
narrative_ontology:cs_reading_relation('69ba9740-4390-4f53-b678-a5f9dbf8a2e3', party_ban_instrument__founding_precedents_reading, coexists_with).
narrative_ontology:cs_reading_relation('69ba9740-4390-4f53-b678-a5f9dbf8a2e3', party_ban_instrument__potentiality_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('69ba9740-4390-4f53-b678-a5f9dbf8a2e3', foundational, suppression_through_anticipatory_possibility).
narrative_ontology:cs_axiom_status(suppression_through_anticipatory_possibility, holdable).
narrative_ontology:cs_axiom_grounding('69ba9740-4390-4f53-b678-a5f9dbf8a2e3', suppression_through_anticipatory_possibility, empirically_contingent).
narrative_ontology:cs_axiom('69ba9740-4390-4f53-b678-a5f9dbf8a2e3', secondary, dissent_monitoring_tax_mechanism).
narrative_ontology:cs_axiom_status(dissent_monitoring_tax_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('69ba9740-4390-4f53-b678-a5f9dbf8a2e3', dissent_monitoring_tax_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('69ba9740-4390-4f53-b678-a5f9dbf8a2e3', constitutional_order_with_formal_ban_authority).
narrative_ontology:cs_drift_state('69ba9740-4390-4f53-b678-a5f9dbf8a2e3', contemporary_surveillance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('69ba9740-4390-4f53-b678-a5f9dbf8a2e3', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(party_ban_instrument__chilling_critique_reading, party_ban_instrument).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(party_ban_instrument__chilling_critique_reading, bounded_political_spectrum).
narrative_ontology:constraint_beneficiary(party_ban_instrument__chilling_critique_reading, constitutional_order_defenders).
narrative_ontology:constraint_victim(party_ban_instrument__chilling_critique_reading, radical_legal_opposition).
narrative_ontology:constraint_victim(party_ban_instrument__chilling_critique_reading, dissent_at_spectrum_edges).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RADICAL-BUT-LEGAL OPPOSITION (SNARE) — Structurally trapped by the ban instrument's chilling effect. Cannot exit: dissent is the agent's constitutive activity. Suppression operates through anticipation — the standing possibility of dissolution discipline creates a monitoring tax on all edge-of-spectrum political activity. No explicit judgment required; the instrument's mere existence (and founding precedents of SRP/KPD dissolution) creates the extract. Maximum extraction because the agent bears the full cost of uncertainty: is this organizational form acceptable? Will this funding source trigger scrutiny? Does this rhetoric cross the threshold? The agent cannot know until after dissolution.
constraint_indexing:constraint_classification(party_ban_instrument__chilling_critique_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL SOCIETY AT SPECTRUM EDGE (TANGLED ROPE) — Moderate power, constrained exit. Benefits from the coordination function: the ban instrument establishes a constitutional floor — what forms of political organization are protected. Also bears costs: funding sources dry up; organizational formation requires legal caution; public discourse self-censors to avoid association with banned categories. Extraction is real but not maximal; some legal pathways remain open. The agent experiences suppression as the cost of operating in the shadow of the instrument.
constraint_indexing:constraint_classification(party_ban_instrument__chilling_critique_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL ORDER DEFENDERS / BOUNDED SPECTRUM (ROPE) — Institutional beneficiary with arbitrage options. Experiences the ban instrument as pure coordination: it maintains the boundary between protected and unprotected political speech. The instrument enables pluralism within constitutional limits. For this perspective, the chilling effect is not extraction but coordination benefit — it communicates the constitutional floor to potential entrants. The agent can exit (amend the constitution) or reframe (change which parties are banned) but sees these options as arbitrage within a stable institutional framework, not as exits from the constraint itself.
constraint_indexing:constraint_classification(party_ban_instrument__chilling_critique_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, the ban threshold appears immutable: any constitutional order must distinguish protected from unprotected political forms. The analytical observer risks naturalizing what this reading reveals as contingent institutional choice. The chilling_critique_reading identifies this perspective as a false summit — the threshold is not a natural law but a constructed boundary maintained through institutional enforcement.
constraint_indexing:constraint_classification(party_ban_instrument__chilling_critique_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(party_ban_instrument__chilling_critique_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(party_ban_instrument__chilling_critique_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(party_ban_instrument__chilling_critique_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(party_ban_instrument__chilling_critique_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(party_ban_instrument__chilling_critique_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The chilling_critique reading identifies extraction through anticipatory suppression. Organizations at spectrum edges pay a monitoring tax — surveillance intensifies, funding sources require caution, organizational formation incurs legal risks. This extraction is enforced not through explicit judgments but through the standing possibility of dissolution. The baseline extractiveness reflects that: (a) the actual ban rate is low (only ~10 formal dissolutions in post-WWII German history), so the extraction is not from actual enforcement; (b) the chilling effect is substantial — behavioral changes are observable across a wide range of edge-spectrum activity; (c) the uncertainty premium is significant — organizations cannot know in advance what will trigger scrutiny. The rising trajectory (0.35 → 0.58 across the interval) reflects the instrument's increasing effectiveness as a chilling mechanism as security services developed surveillance capacity and as transparency laws enabled funding exclusion without formal bans. Suppression (0.68): High. The standing possibility of dissolution creates a suppressive mechanism that operates independent of judgment. Alternatives to the banned activity are constrained: dissent cannot exit; organizations cannot reorganize without legal risk; funding sources are systematically restricted. The suppression is structural (not merely coercive) — the boundaries are internalized as self-censorship. Theater ratio (0.45): Moderate-low. The ban instrument's actual mechanism is relatively straightforward: formal Constitutional Court dissolution judgments are rare and highly legible (high theater would be if the instrument were mostly symbolic or performative). However, the instrument's power operates primarily through anticipation rather than judgment, which introduces some performative elements — the visibility of the threat matters more than the actual frequency of bans. The theater ratio reflects that the instrument's core mechanism (chilling through threat) is not fully transparent in public discourse.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a dramatic perspectival gap between the victim and beneficiary perspectives. The radical-but-legal opposition sees a snare — trapped, bearing extraction through uncertainty, with no exit. The constitutional order sees rope — a coordination mechanism that maintains the boundary between protected and unprotected forms. The moderate actors (civil society at edges) see tangled_rope — genuine coordination benefits (the boundary is useful) alongside real extraction costs (the monitoring tax). The analytical observer risks seeing a natural law (mountain) — any constitutional order must distinguish protected from unprotected forms — but the chilling_critique reading reveals this as a false summit: the extraction through anticipatory suppression is not a natural law but a contingent institutional arrangement. The gap between perspectives reflects the gap between experiencing the instrument as a threat (victim) and experiencing it as a protective boundary (beneficiary).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural position relative to the chilling mechanism. The radical-but-legal opposition bears full extraction (high d ~0.88): they are the primary target, have no exit options (trapped), and their dissent is the object of the instrument. Civil society at spectrum edges has moderate-high d (~0.62): they experience both benefits (coordination) and costs (suppression), with some constrained exit options. The constitutional order has low d (~0.12): they are the primary beneficiary, have high arbitrage options (can amend the constitution, reinterpret precedents), and experience the instrument as coordination rather than extraction. The analytical observer has high d (~0.75): in a civilizational timescale, the observer is analyzing the mechanisms but not exempt from them. The security services have moderate d (~0.55): they enforce the instrument (beneficiary positioning) but face constraints from constitutional limits on surveillance (constrained exit). The derived directionalities produce the perspectival gap: beneficiaries see rope (low chi from their position), victims see snare (high chi from their position).
 *
 * MANDATROPHY ANALYSIS:
 *   The chilling_critique reading resolves the mandatrophy by specifying the extraction mechanism precisely: suppression operates through anticipation of dissolution, not through actual use of the ban power. This avoids the ambiguity of treating the instrument as either pure coordination (rope) or pure extraction (snare) by showing it as a genuine hybrid (tangled_rope). The coordination function is real — the instrument establishes a constitutional boundary. The extraction function is also real — it taxes dissent through the monitoring cost of operating near the boundary. The classification prevents the false summit error (naturalizing the threshold as immutable law) while preserving the recognition that the threshold does serve a coordination function for the constitutional order. The mandatrophy resolution hinges on the empirical question (omega_1) of whether the chilling effect is caused by the instrument or merely correlated with pre-existing constitutional commitments — if caused, the tangled_rope classification is robust; if merely correlated, the classification shifts toward rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chilling_effect_measurement_ambiguity,
    'Is the observed suppression of radical-spectrum political activity caused by the ban instrument''s chilling effect, or by pre-existing constitutional commitments that the ban merely formalizes?',
    'Comparative analysis: periods before/after ban instrument codification; cross-national comparison with jurisdictions lacking explicit ban provisions but similar constitutional orders; measurement of organizational formation rates and funding flows at spectrum edges pre- and post-formalization',
    'If chilling effect is instrumentally caused by the ban: extractiveness remains high (0.58+). If the ban merely formalizes pre-existing suppression: extractiveness drops to ~0.35 (it is coordination, not extraction). Classification could shift from tangled_rope to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_measurement_ambiguity, empirical, 'Whether ban instrument causes chilling or formalizes pre-existing suppression').

omega_variable(
    anticipatory_suppression_vs_formal_judgment,
    'Does suppression operate primarily through the standing possibility of dissolution (anticipatory/preemptive) or through actual bans?',
    'Analysis of ban frequency and organizational behavior: count of actual dissolutions vs. self-censoring organizations; tracking of funding behavior and speech changes in relation to threat-of-ban statements vs. actual bans; interview/documentary evidence of organizational decision-making at spectrum edges',
    'If primarily anticipatory: the instrument extracts through uncertainty without use — this is the chilling_critique reading''s core claim, extractiveness ~0.58. If primarily through actual judgments: extractiveness is lower (~0.42) and classification shifts toward scaffold (each individual ban is a judgment, not systemic chilling).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anticipatory_suppression_vs_formal_judgment, empirical, 'Whether suppression is anticipatory or judgment-based').

omega_variable(
    reading_contest_kernel_stability,
    'Is the party ban instrument a stable constitutional commitment or a contested kernel where the three readings (founding_precedents, potentiality_threshold, chilling_critique) represent genuinely incompatible interpretive frameworks?',
    'Doctrinal analysis: examine Federal Constitutional Court opinions to determine whether they adopt one reading consistently or oscillate between readings; track shifts in emphasis across landmark cases (SRP/KPD precedents vs. NPD threshold refinements vs. contemporary surveillance/funding decisions); assess whether judges acknowledge the reading contest or treat one reading as settled',
    'If readings are genuinely coexistent (different parties hold them simultaneously): this reading is legitimate and the constraint classification is robust. If one reading is increasingly foreclosed by doctrine: this reading''s status as holdable vs. overridden changes. If the kernel is destabilizing (readings converging/diverging over time): drift_state and reference_frame become critical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_kernel_stability, conceptual, 'Whether party ban readings are coexistent or one is foreclosing others').

omega_variable(
    committer_frame_reading_identity,
    'Does the chilling_critique reading (suppression through anticipatory discipline of dissent-at-the-edges) represent a coherent doctrinal position or an external analytical frame imposed on the instrument?',
    'Doctrinal archaeology: trace whether German courts, legal scholars, or political actors have explicitly named and adopted this frame; if so, identify in which decisions and legal traditions. If not: determine whether this reading is a reconstruction from observable suppression patterns or an imposition of external critique onto the doctrine.',
    'If internally coherent doctrinal position: the reading''s axioms are holdable and the constraint classification is robust. If external analytical reconstruction: the reading instantiates a critique of the doctrine rather than a legitimate internal reading. Classification remains tangled_rope but with higher omega uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_reading_identity, conceptual, 'Whether chilling_critique reading is internal doctrine or external analysis').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(party_ban_instrument__chilling_critique_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pban_chill_theater_t0, party_ban_instrument__chilling_critique_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(pban_chill_theater_t30, party_ban_instrument__chilling_critique_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(pban_chill_theater_t60, party_ban_instrument__chilling_critique_reading, theater_ratio, 60, 0.45).

% Extraction over time
narrative_ontology:measurement(pban_chill_extract_t0, party_ban_instrument__chilling_critique_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pban_chill_extract_t30, party_ban_instrument__chilling_critique_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(pban_chill_extract_t60, party_ban_instrument__chilling_critique_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(pban_chill_suppress_t0, party_ban_instrument__chilling_critique_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(pban_chill_suppress_t30, party_ban_instrument__chilling_critique_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(pban_chill_suppress_t60, party_ban_instrument__chilling_critique_reading, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(party_ban_instrument__chilling_critique_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(party_ban_instrument__chilling_critique_reading, party_ban_instrument__founding_precedents_reading).
narrative_ontology:affects_constraint(party_ban_instrument__chilling_critique_reading, party_ban_instrument__potentiality_threshold_reading).

% DUAL FORMULATION NOTE:
% The party_ban_instrument kernel decomposes into three constraint stories corresponding to three readings. The chilling_critique reading focuses on the anticipatory suppression mechanism and is downstream of the founding_precedents reading (which establishes the precedential force of SRP/KPD dissolutions) and upstream of the potentiality_threshold reading (which refines the conditions for actual bans, making anticipatory chilling the primary mechanism). Each reading has its own ε value and classification: founding_precedents emphasizes the historical precedents as settled doctrine; potentiality_threshold emphasizes the threshold refinement; chilling_critique emphasizes the operation of the instrument through anticipation independent of judgment. All three are legitimate readings of the same kernel and coexist in contemporary doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(party_ban_instrument__chilling_critique_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
