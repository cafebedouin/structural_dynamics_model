% ============================================================================
% CONSTRAINT STORY: messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_messianic_suspension, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: messianic_suspension
 *   human_readable: Messianic Suspension of Sacrifice Obligation
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   The messianic suspension of sacrifice obligation represents a textual
 *   authority's resolution of a material impossibility: covenantal obligation
 *   requires sacrifice, historical circumstances prohibit sacrifice (exile,
 *   diaspora, absence of Temple), and eschatological expectation projects
 *   restoration. The suspension doctrine—obligation remains binding but
 *   inactive, study substitutes for performance, readiness is maintained
 *   through textual engagement—creates a constraint that exhibits
 *   tangled_rope structure: genuine coordination function (study community,
 *   textual tradition transmission) plus asymmetric burden distribution
 *   (practitioners bear readiness cost; interpretive authority maintains
 *   monopoly over suspension conditions). This is ONE READING of the
 *   contested kernel 'sacrifice_obligation_continuity.' Sibling readings
 *   (study_as_performance, performance_only, archival_preservation)
 *   instantiate different structural relationships to the same foundational
 *   texts but with different beneficiary/victim declarations and
 *   extractiveness values. The messianic_suspension reading specifically
 *   instantiates the logic that the obligation persists in principle,
 *   suspension is contingent on unfulfilled eschatological conditions, and
 *   study maintains the community in a state of readiness for eventual
 *   reactivation. Theater ratio has increased monotonically over 1500 years:
 *   as messianic expectation has deferred without fulfillment, the suspension
 *   regime has become increasingly performative—the 'readiness' narrative now
 *   substantially sustains institutional function independent of genuine
 *   reactivation possibility. The constraint demonstrates how a coordination
 *   solution (study practice) can become an extraction mechanism once the
 *   original justifying condition (imminent restoration) ceases to be
 *   credible.
 *
 * KEY AGENTS:
 *   - Committed practitioners (powerless/identity_locked): Bear the readiness burden indefinitely; identity fused with 'one who waits'; experience maximum extraction as deferral without discharge
 *   - Study community (moderate/constrained): Coordinated benefit from study practice and community cohesion; constrained by study requirement and avoidance of alternative settlement practices; mixed extraction/coordination
 *   - Interpretive authority class (institutional/arbitrage): Sustains monopoly over suspension doctrine interpretation; low experienced extraction as beneficiary; arbitrage exit option (can reinterpret conditions)
 *   - Sectarian alternatives (organized/constrained): Organized dissenters from suspension doctrine; experience institutional constraint on alternative readings; suppression of performance_only or archival_preservation interpretations
 *   - Historical witness (institutional/arbitrage): Observes the constraint from civilizational timescale as degraded institutional performance; theater ratio reveals function atrophy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(messianic_suspension, 0.35).
domain_priors:suppression_score(messianic_suspension, 0.48).
domain_priors:theater_ratio(messianic_suspension, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(messianic_suspension, extractiveness, 0.35).
narrative_ontology:constraint_metric(messianic_suspension, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(messianic_suspension, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(messianic_suspension, tangled_rope).
narrative_ontology:human_readable(messianic_suspension, "Messianic Suspension of Sacrifice Obligation").
narrative_ontology:topic_domain(messianic_suspension, "religious_law/ritual_studies/textual_tradition").

domain_priors:requires_active_enforcement(messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(messianic_suspension, '2e1f0837-2e01-4c03-a58c-087cda22f509').
narrative_ontology:cs_kernel_codification('2e1f0837-2e01-4c03-a58c-087cda22f509', fixed_text).
narrative_ontology:cs_authority_grounding('2e1f0837-2e01-4c03-a58c-087cda22f509', lineage).
narrative_ontology:cs_interpretation_layer_present('2e1f0837-2e01-4c03-a58c-087cda22f509').
narrative_ontology:cs_reading_relation('2e1f0837-2e01-4c03-a58c-087cda22f509', messianic_suspension__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('2e1f0837-2e01-4c03-a58c-087cda22f509', messianic_suspension__performance_only, forecloses).
narrative_ontology:cs_reading_relation('2e1f0837-2e01-4c03-a58c-087cda22f509', messianic_suspension__archival_preservation, coexists_with).
narrative_ontology:cs_axiom('2e1f0837-2e01-4c03-a58c-087cda22f509', foundational, covenant_obligation_persists).
narrative_ontology:cs_axiom_status(covenant_obligation_persists, holdable).
narrative_ontology:cs_axiom_grounding('2e1f0837-2e01-4c03-a58c-087cda22f509', covenant_obligation_persists, deontological).
narrative_ontology:cs_axiom('2e1f0837-2e01-4c03-a58c-087cda22f509', foundational, suspension_awaits_restoration).
narrative_ontology:cs_axiom_status(suspension_awaits_restoration, holdable).
narrative_ontology:cs_axiom_grounding('2e1f0837-2e01-4c03-a58c-087cda22f509', suspension_awaits_restoration, empirically_contingent).
narrative_ontology:cs_reference_frame('2e1f0837-2e01-4c03-a58c-087cda22f509', suspended_obligation_awaiting_restoration).
narrative_ontology:cs_drift_state('2e1f0837-2e01-4c03-a58c-087cda22f509', contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2e1f0837-2e01-4c03-a58c-087cda22f509', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(messianic_suspension, interpretive_authority_class).
narrative_ontology:constraint_beneficiary(messianic_suspension, study_institution).
narrative_ontology:constraint_victim(messianic_suspension, committed_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(messianic_suspension, sectarian_alternatives).
narrative_ontology:constraint_vindicates(messianic_suspension, messianic_eschatology_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Those bound by the covenant obligation who accept the suspension doctrine. They practice study, maintain ritual readiness (avoidance of substitute acts like sacrificial equivalence or resumption of secular settlement practices), and wait for restoration. They bear the existential cost of indefinite deferral—commitment without discharge, readiness without consummation. Exit would require abandoning the identity constituted through covenant participation and messianic expectation. The burden is framed as obligation-in-suspension rather than as failure or guilt, but the deferral itself is the extraction mechanism.
narrative_ontology:constraint_stakeholder(messianic_suspension, committed_practitioners, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(messianic_suspension, committed_practitioners, observer).

% The formal interpretive authority that maintains and enforces the suspension doctrine. Administers the study requirement, certifies readiness status, manages the boundary between suspension (obligation remains but inactive) and supersession (obligation is permanently replaced). Benefits from the coordination function of study practice and the interpretive monopoly over what constitutes 'readiness' and what conditions would trigger reactivation. Can exit or reinterpret the suspension doctrine without institutional loss—the doctrine is a tool of authority maintenance, not a constraint on the authority itself.
narrative_ontology:constraint_stakeholder(messianic_suspension, study_institution, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(messianic_suspension, study_institution, beneficiary).

% Organized factions that reject or reinterpret the suspension doctrine (those holding performance_only reading, those maintaining sacrificial equivalence through other means, those accepting archival_preservation). They bear the cost of institutional suppression—their readings are actively suppressed, their practice is sanctioned, their interpretive authority is delegitimized. The enforcement of suspension doctrine conformity constrains their alternative readings. They have structural capacity to exit (can establish separate communities) but face high social cost of schism and institutional barrier to legitimacy claims.
narrative_ontology:constraint_stakeholder(messianic_suspension, sectarian_alternatives, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(messianic_suspension, sectarian_alternatives, excluded).

% The normative claim that covenantal obligations persist across time and circumstance—that the obligation to sacrifice does not disappear merely because circumstances make it impossible. This is a proposition vindicating the suspension doctrine's internal logic, not an actor collecting from the constraint. The doctrine is itself archived in the tradition and validated by the textual authority's endorsement of suspension framework.
narrative_ontology:constraint_stakeholder(messianic_suspension, historical_continuity_doctrine, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(messianic_suspension, historical_continuity_doctrine).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintenance of textual tradition and community identity during exile/diaspora. The study practice keeps the tradition alive, transmits hermeneutic knowledge across generations, and coordinates expectation for eschatological restoration. Genuine coordination problem: how to preserve covenant identity and textual transmission when the covenantal act itself is materially impossible.
% TRANSFER_FUNCTION: Readiness burden flows from practitioners to study institution (interpretive authority). Practitioners invest time, attention, and identity commitment in study and ritual avoidance; the institution maintains monopoly over interpretation of suspension conditions and reactivation triggers. The transfer is asymmetric: practitioners bear a diffuse existential burden (indefinite deferral); authority collects concentrated institutional benefit (monopoly, coherence, legitimacy). No material goods transfer, but status, interpretive authority, and institutional continuity flow toward the interpretive authority class.
% ABSENT_VOICES: Those who would reject the messianic eschatology entirely (secular practitioners, those for whom restoration is not a credible expectation); those who would prefer supersession doctrine (permanent replacement of obligation); those from outside the tradition who do not recognize the covenant's binding force. These voices are absent from the interpretive consensus because the suspension framework is enforced by the institutional authority and embedded in training and socialization.
% DISAPPEARANCE_RATIONALE: If the suspension doctrine disappeared, the tradition would face an immediate fork: (1) Accept supersession—the obligation is permanently replaced by study equivalence or other substitution. (2) Reactivate—restore sacrificial practice where materially possible (diaspora communities to Temple sites, theoretical advocacy for Temple reconstruction). (3) Preserve archivally—acknowledge the obligation as obsolete but historically binding. Communities holding the suspension reading would experience this as the dissolution of the binding framework that coordinates their identity. Communities holding alternative readings would experience this as liberation from suppression. The world does not rearrange in a simple way; different parties would rearrange in opposite directions.
% FOUNDING_PROBLEM: The exile produced material impossibility: covenantal obligation requires sacrifice at the Temple, but the Temple is destroyed and sacrifice is prohibited outside it. Diaspora communities cannot fulfill the obligation. Eschatological expectation projects restoration as solving the impossibility by returning the community to the Temple. The suspension doctrine bridges the gap: obligation persists in principle (honoring the covenant's binding force), suspension removes guilt for non-fulfillment (the condition is beyond the practitioner's power), and study maintenance preserves readiness for the future state when restoration makes reactivation possible.
% FOUNDING_PROBLEM_CORROBORATION: Attestation from rabbinic authorities: Talmudic sources (especially Avodah Zarah, Menachot) document the suspension reasoning and its eschatological anchor. Attestation from historians: exile/diaspora conditions are well-documented; Temple destruction is established fact. Attestation from skeptics: contemporary scholars of religion (outside the tradition's authority structure) largely accept that messianic restoration has not occurred and that the suspension doctrine is now maintained through institutional continuity rather than through active fulfillment of the restoration condition. The tradition's own authorities do not fully acknowledge the dead-status finding—they maintain the suspension framework as operative and the restoration expectation as live, even though neither is practically operative.
narrative_ontology:disappearance_verdict(messianic_suspension, contested).
narrative_ontology:founding_problem_status(messianic_suspension, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMITTED PRACTITIONER (SNARE) — Identity fused with covenant obligation; structurally mobile (could physically cease participation) but identity constituted through the role of 'one who waits in readiness.' The suspension is experienced as indefinite deferral without terminal date or clear reactivation condition. Cannot exit without renouncing identity. Bears the burden of readiness without cathartic fulfillment or definitive violation. Experiences maximum extraction — perpetual obligation without discharge.
constraint_indexing:constraint_classification(messianic_suspension, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: STUDY COMMUNITY (TANGLED ROPE) — Coordinated benefit: study practice creates community cohesion, transmits tradition, provides meaningful intellectual work. Extraction: study substitutes for the forbidden sacrificial act and becomes the mechanism through which readiness is maintained — a coordination function embedded in extraction. The study requirement constrains other practices (commercial activities, leisure pursuits that might constitute 'settling' rather than waiting). Mixed structure: genuine coordination plus asymmetric burden distribution.
constraint_indexing:constraint_classification(messianic_suspension, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERPRETIVE AUTHORITY (ROPE) — Sustains interpretive monopoly over the suspension doctrine itself. Arbitrage exit: can shift interpretation (declare reactivation, declare perpetual suspension, declare performance equivalent) without loss of institutional position. Benefits from the coordination function: study practice maintains community cohesion and transmits authority's hermeneutic tradition. Low experienced extraction from the beneficiary perspective — the constraint is experienced as enabling institutional continuity and interpretive legitimacy.
constraint_indexing:constraint_classification(messianic_suspension, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: HISTORICAL WITNESS (PITON) — From a civilizational/archival perspective, the suspension doctrine is now largely maintained as historical performance: the messianic condition that originally justified suspension (expectation of imminent restoration) has not materialized for 2000+ years. The constraint persists through institutional inertia and textual authority rather than through active functional justification. Theater ratio is high: study performance maintains the 'readiness' narrative even though the reactivation conditions are effectively dormant. The piton classification reflects degraded function preserved through performance.
constraint_indexing:constraint_classification(messianic_suspension, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / TEXTUAL IMMUTABILITY VIEW (MOUNTAIN) — From the civilizational analytical position, the suspension doctrine appears as a logical derivative of foundational textual commitments: if the covenant obligates sacrifice AND sacrifice is forbidden in exile AND restoration will reactivate obligation, then suspension follows necessarily from the axiomatic premises. This perspective risks naturalizing a contingent institutional arrangement (the fusion of covenant obligation with messianic eschatology) as a structural law of religious logic. Engine will compute this as a false summit: the beneficiary structure (interpretive authority maintains monopoly) reveals extraction masked by logical necessity.
constraint_indexing:constraint_classification(messianic_suspension, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: SECTARIAN ALTERNATIVE (TANGLED ROPE) — Organized factions that reject the suspension doctrine (performance-only readers, those who maintain sacrificial equivalence through other means, those who accept perpetual supersession) experience the official suspension doctrine as a constraint on their own interpretive freedom. For organized dissenters, the constraint is enforcement of the interpretive monopoly: study conformity is required; alternative readings are suppressed. Extraction runs toward institutional authority; coordination function (textual inheritance) exists but is experienced as coercive. Split classification: coordinated for conformists, extractive for dissenters.
constraint_indexing:constraint_classification(messianic_suspension, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(messianic_suspension_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(messianic_suspension, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(messianic_suspension, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(messianic_suspension, TR),
    TR >= 0.70.

:- end_tests(messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The messianic suspension doctrine creates asymmetric burden (committed practitioners bear readiness cost) balanced against genuine coordination function (study maintains community and textual transmission). The extractiveness is not maximal because the coordination component is real and substantial—study genuinely serves tradition maintenance. But extraction occurs because the burden and benefit are asymmetrically distributed: practitioners bear the deferred-obligation cost; interpretive authority sustains the monopoly over what constitutes 'readiness.' Measurement trajectory shows extractiveness increasing from 0.28 to 0.38 over 1500 years as the justifying eschatological condition (imminent restoration) becomes chronologically implausible, and the extraction mechanism (deferral obligation) becomes increasingly unmoored from its original functional justification. Suppression (0.48): Moderate. Practitioners can physically cease participation, but identity fusion with the waiting role makes exit functionally unavailable—internalized suppression outweighs structural barriers. Alternative readings (performance_only, sectarian practices) are suppressed by institutional enforcement and social sanction. Theater ratio (0.68): High and increasing. The study performance maintains the 'readiness' narrative despite the effective dormancy of reactivation conditions. The trajectory from 0.42 to 0.73 reflects the degradation of functional justification over time: early in the suspension period, waiting and readiness had credible near-term meaning; by the 1500-year mark, the performance is increasingly theatrical—it persists because the institutional structure persists, not because reactivation is expected soon.
 *
 * PERSPECTIVAL GAP:
 *   The messianic suspension exemplifies how a single constraint structure produces radically different classifications from different observer positions. The committed practitioner (identity_locked) sees pure extraction (Snare)—indefinite deferral without resolution. The study community sees mixed coordination and extraction (Tangled Rope)—real community benefit embedded in asymmetric burden. The interpretive authority sees enabling coordination (Rope)—the suspension doctrine sustains institutional continuity and hermeneutic monopoly. The sectarian alternative sees enforcement of interpretive suppression (Tangled Rope from the dissenters' side). The historical witness sees degraded institutional performance (Piton)—the 'readiness' narrative persists through institutional inertia rather than functional justification. The analytical observer risks seeing logical necessity (Mountain)—suspension follows from textual axioms. These are not different opinions about a single fact; they are accurate readings of structurally distinct positions within the constraint. The presheaf of perspectives IS the structural fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from: (1) beneficiary/victim status—practitioners are victims, authority is beneficiary; (2) exit options—committed practitioners are identity_locked (cannot exercise structural mobility due to identity fusion), interpretive authority has arbitrage options; (3) power level—powerless practitioners experience maximum d toward 1.0 (full target), institutional authority experiences d toward 0.0 (full beneficiary). The engine computes effective extraction chi from these d values: powerless/identity_locked victims receive high chi amplification; institutional/arbitrage beneficiaries receive damping or inversion. The study community occupies intermediate position: moderate power, constrained exit, mixed beneficiary/victim status produces moderate d and moderate chi. The perceptual gap between 'Snare' (practitioners' experience, high chi) and 'Rope' (authority's experience, low/negative chi) is the direct output of directionality differentiation.
 *
 * MANDATROPHY ANALYSIS:
 *   The suspension doctrine resolves mandatrophy by maintaining the formal obligation ('the covenant still binds') while indefinitely deferring enforcement ('reactivation awaits restoration'). The mandate persists in principle but is inoperative in practice, creating the piton-trajectory signature: institutional performance (study, readiness language) maintains the mandate's nominal presence while functional purpose has atrophied. The theater_ratio trajectory (0.42 → 0.73) directly tracks this mandatrophy as performance substitutes for function. The constraint avoids simple reclassification to pure performance by anchoring the suspension to an external eschatological claim (messianic restoration) that the interpretive authority does not control. This creates the asymmetric burden structure: practitioners bear the cost of the deferral; the authority maintains the doctrine but is not personally subject to its constraints. Mandatrophy resolution would require either (a) reactivation of the eschatological condition (restoration claim renewed with credible timeline) or (b) explicit supersession doctrine (obligation acknowledged as permanently replaced). The current regime sustains the fiction of suspension precisely to avoid the doctrinal rupture that either resolution would require.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_supersession_ambiguity,
    'Is the obligation genuinely suspended (dormant, to be reactivated) or has it been superseded (permanently replaced by alternative fulfillment mechanism)?',
    'Textual analysis of authoritative sources across tradition; examination of whether messianic conditions are stated as prerequisites for reactivation or as permanently deferred eschatological claims; historical documentation of interpretive shifts regarding permanence of the suspension regime.',
    'If suspension: constraint is tangled_rope (coordination + deferral burden). If supersession: constraint is piton (historical performance masking permanent substitution). If contested: omega remains unresolved and classification reflects the contested structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suspension_vs_supersession_ambiguity, conceptual, 'Ambiguity between suspension (temporary, reversible) and supersession (permanent replacement)').

omega_variable(
    readiness_burden_allocation,
    'Does the readiness burden fall equally on all practitioners or is it asymmetrically distributed toward interpretive authority/scholars?',
    'Ethnographic documentation of who bears the actual constraint (time investment in study, opportunity cost, identity maintenance); comparison of enforcement intensity across social strata within the tradition.',
    'If equal: constraint approaches pure coordination (higher Rope classification probability). If asymmetric: constraint''s extraction component is higher; Snare classification justified for powerless agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(readiness_burden_allocation, empirical, 'Distribution of readiness maintenance burden across practitioner classes').

omega_variable(
    eschatological_timeline_collapse,
    'As the actual historical timeline of messianic expectation extends (now 2000+ years with no restoration), does the constraint''s functional classification degrade from tangled_rope toward piton?',
    'Longitudinal analysis of interpretive texts across centuries; measurement of theater_ratio increase over time; documentation of shift from ''soon'' to ''indefinite waiting''; ethnographic comparison of commitment intensity across generations.',
    'If timeline collapse occurs: T17 abductive trigger (mountain_extraction_accumulation) fires on expanding theater_ratio; constraint may require reclassification from tangled_rope toward piton. If timeline is re-interpreted (messianic imminent claim renewed), constraint reverts to activation logic and victim status changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eschatological_timeline_collapse, empirical, 'Functional degradation as messianic timeline extends without fulfillment').

omega_variable(
    kernel_reading_contest,
    'Which reading of the sacrifice_obligation_continuity kernel is structurally operative in this community at this moment: messianic_suspension (this reading), study_as_performance, performance_only, or archival_preservation?',
    'Ethnographic interview regarding practitioner''s own understanding of why study and abstention from sacrifice are maintained; analysis of hermeneutic authority''s published position on the suspension doctrine; examination of whether dissenters are suppressed or accommodated.',
    'Different reading instantiations have different ε values and victim sets. Identifying which reading is locally operative determines the true structural constraint and its proper classification. A community that consciously reads the obligation as perpetually superseded has a different constraint (performance_only) than one reading it as suspended pending restoration (this reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which kernel reading is the operative interpretive framework in this community').

omega_variable(
    identity_lock_mechanism,
    'Is the identity lock experienced by committed practitioners a feature of the suspension doctrine itself (waiting-as-identity) or of broader communal identity construction (tribal/ethnic affiliation)?',
    'Comparative ethnography of exit narratives (practitioners who leave the tradition); examination of whether identity loss is attributed to cosmological obligation (suspension doctrine) or to social/communal severing (not specific to sacrifice obligation).',
    'If lock is specific to suspension: the identity_locked exit classification is precise and Snare classification is justified. If lock is broader communal identity: the suspension constraint is less isolate and the exit_options should be re-evaluated upward to constrained (high communal cost but not identity-constituting).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Specificity of identity lock to suspension obligation vs broader communal identity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(messianic_suspension, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mess_susp_theater_t0, messianic_suspension, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mess_susp_theater_t500, messianic_suspension, theater_ratio, 500, 0.55).
narrative_ontology:measurement(mess_susp_theater_t1000, messianic_suspension, theater_ratio, 1000, 0.68).
narrative_ontology:measurement(mess_susp_theater_t1500, messianic_suspension, theater_ratio, 1500, 0.73).

% Extraction over time
narrative_ontology:measurement(mess_susp_extract_t0, messianic_suspension, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mess_susp_extract_t500, messianic_suspension, base_extractiveness, 500, 0.31).
narrative_ontology:measurement(mess_susp_extract_t1000, messianic_suspension, base_extractiveness, 1000, 0.35).
narrative_ontology:measurement(mess_susp_extract_t1500, messianic_suspension, base_extractiveness, 1500, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(mess_susp_suppress_t0, messianic_suspension, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(mess_susp_suppress_t500, messianic_suspension, suppression_requirement, 500, 0.5).
narrative_ontology:measurement(mess_susp_suppress_t1000, messianic_suspension, suppression_requirement, 1000, 0.48).
narrative_ontology:measurement(mess_susp_suppress_t1500, messianic_suspension, suppression_requirement, 1500, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(messianic_suspension, identity_coordination).
narrative_ontology:affects_constraint(messianic_suspension, study_as_performance).
narrative_ontology:affects_constraint(messianic_suspension, performance_only).
narrative_ontology:affects_constraint(messianic_suspension, archival_preservation).
narrative_ontology:affects_constraint(messianic_suspension, exile_adaptation_doctrine).

% DUAL FORMULATION NOTE:
% The messianic_suspension reading is part of a constraint family decomposing the 'sacrifice_obligation_continuity' kernel. Four structurally distinct constraints instantiate four readings of the same foundational texts, with different ε values reflecting different interpretive solutions: suspension (moderate extraction via deferral), performance equivalence (lower extraction via substitution), supersession (higher extraction via exclusion), and archival removal (zero extraction via exemption). These are not the same constraint viewed from different angles; they have different beneficiary/victim declarations and different victim presence. Network links establish the family structure and enable measurement of cross-reading contamination (how a shift in interpretive dominance from one reading to another affects the constraint's metrics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
