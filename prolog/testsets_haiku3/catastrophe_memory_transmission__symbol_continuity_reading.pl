% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__symbol_continuity_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__symbol_continuity_reading
 *   human_readable: Catastrophe Memory Transmission—Symbol Continuity Reading
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   In communities facing catastrophe—genocide, forced displacement,
 *   environmental collapse—ritual practice encodes and transmits communal
 *   identity. The symbol-continuity reading holds that the preservation of
 *   the canonical symbolic form IS the survival mechanism: identity persists
 *   because the form persists, transmitted faithfully across generations even
 *   when material conditions, environment, or diaspora scatter the community.
 *   This reading instantiates ONE constraint within the contested kernel of
 *   catastrophe memory transmission. Its counterparts—the
 *   operational-competence reading (ritual encodes survival knowledge through
 *   pattern recognition) and the hybrid-embedded reading (survival competence
 *   is inseparable from symbolic form)—are structurally different constraints
 *   with different victim sets, beneficiary structures, and ε values. This
 *   story models only the symbol-continuity reading: the constraint that
 *   ritual fidelity to canonical form is the identity preservation mechanism,
 *   benefiting communal identity bearers while extracting adaptive capacity
 *   from resource-constrained practitioners.
 *
 * KEY AGENTS:
 *   - communal_identity_bearers: identity-locked beneficiaries whose continued existence (as a community) depends on the constraint's preservation of symbolic form
 *   - adaptive_practitioners: moderate-power payers constrained by fidelity enforcement; they see adaptive modification as a coherent transmission strategy
 *   - resource_constrained_communities: powerless payers bearing extraction costs during subsistence crisis (time diverted to ritual when survival resources are scarce)
 *   - ritual_authority_lineage: institutional agenda-setter that sets fidelity standards and suppresses alternative interpretations
 *   - displaced_diaspora: excluded voices that would testify adaptation preserves identity; structurally blocked from full participation
 *   - external_observers: analytical seat documenting whether the constraint serves flourishing or masks identity erosion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_transmission__symbol_continuity_reading, 0.72).
domain_priors:theater_ratio(catastrophe_memory_transmission__symbol_continuity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__symbol_continuity_reading, "Catastrophe Memory Transmission—Symbol Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__symbol_continuity_reading, "religious_studies/collective_memory/ritual_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__symbol_continuity_reading, '6bc23071-69d7-4dbf-b9e4-6f29c00c50ca').
narrative_ontology:cs_kernel_codification('6bc23071-69d7-4dbf-b9e4-6f29c00c50ca', distributed).
narrative_ontology:cs_authority_grounding('6bc23071-69d7-4dbf-b9e4-6f29c00c50ca', lineage).
narrative_ontology:cs_interpretation_layer_present('6bc23071-69d7-4dbf-b9e4-6f29c00c50ca').
narrative_ontology:cs_reading_relation('6bc23071-69d7-4dbf-b9e4-6f29c00c50ca', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('6bc23071-69d7-4dbf-b9e4-6f29c00c50ca', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_axiom('6bc23071-69d7-4dbf-b9e4-6f29c00c50ca', foundational, communal_identity_form_constitutive).
narrative_ontology:cs_axiom_status(communal_identity_form_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('6bc23071-69d7-4dbf-b9e4-6f29c00c50ca', communal_identity_form_constitutive, deontological).
narrative_ontology:cs_axiom('6bc23071-69d7-4dbf-b9e4-6f29c00c50ca', foundational, meaning_transmits_through_form_replication).
narrative_ontology:cs_axiom_status(meaning_transmits_through_form_replication, holdable).
narrative_ontology:cs_axiom_grounding('6bc23071-69d7-4dbf-b9e4-6f29c00c50ca', meaning_transmits_through_form_replication, empirically_contingent).
narrative_ontology:cs_reference_frame('6bc23071-69d7-4dbf-b9e4-6f29c00c50ca', inherited_form_fidelity_mandate).
narrative_ontology:cs_drift_state('6bc23071-69d7-4dbf-b9e4-6f29c00c50ca', contemporary_diaspora_resource_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6bc23071-69d7-4dbf-b9e4-6f29c00c50ca', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_bearers).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_practitioners).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, resource_constrained_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Community members for whom ritual continuity IS the survival mechanism—mourning practice, symbolic transmission, and collective identity are fused. They benefit from the constraint's preservation of symbolic form and communal coherence across generations. Exit would mean abandoning the identity frame itself and the relational bonds it constitutes. The constraint validates their continuity claim: 'we are still ourselves because we still do this.'
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_bearers, beneficiary,
    organized, generational, identity_locked, local).

% Community members—often younger, more mobile, or environmentally displaced—who understand the ritual's symbolic meaning but see operational adaptations as necessary for transmission itself to survive (modified timing, compressed form, accessible location, translated language). They bear the cost of enforced fidelity: time diverted from subsistence or adaptation, exclusion from modified forms they see as coherent transmissions, internal guilt or external shaming for proposed changes. Exit means either renouncing community identity or practicing the ritual in violation of the fidelity constraint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_practitioners, payer,
    moderate, biographical, constrained, local).

% Communities facing acute resource scarcity (displacement, poverty, environmental degradation, conflict) for whom the time, materials, or geographic stability required by the canonical ritual form become prohibitive. They pay in missed subsistence work, foregone adaptation, or the crisis of non-compliance with community standards during survival emergencies. No exit available: abandoning the ritual abandons community membership; modifying it violates the fidelity constraint enforced by identity-locked agents.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, resource_constrained_communities, payer,
    powerless, immediate, trapped, local).

% The inherited authority structure—elders, clergy, traditional knowledge keepers—that stewards the symbolic form and enforces its transmission. They set the boundary between 'valid adaptation' and 'loss of identity.' Their authority rests on the claim that they possess the tradition's true meaning and that deviation from the canonical form is degradation. They actively suppress alternative interpretations and modified practices, framing them as loss rather than evolution.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, ritual_authority_lineage, agenda_setter,
    institutional, civilizational, analytical, local).

% Anthropologists, historians, human-rights monitors, and policy analysts who document whether ritual fidelity constraints serve communal flourishing or calcify adaptation capacity, whether they preserve identity through authentic continuity or through enforced performance that masks erosion of actual transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, external_observers, observer,
    analytical, generational, analytical, global).

% Community members scattered by forced displacement, migration, or diaspora who cannot access the canonical ritual site, timing, or assembled community. They are structurally excluded from full participation in the fidelity constraint and often develop modified or hybrid practices. Their voices—that adaptation can preserve meaning, that diaspora communities maintain authentic identity—are suppressed by the fidelity doctrine.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, displaced_diaspora, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__symbol_continuity_reading, ritual_authority_lineage).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__symbol_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits communal identity and collective memory across generations by encoding them in symbolic form—mourning practice, ritual markers, linguistic utterances, material objects. The symbolic form is the survival mechanism: identity persists through fidelity to the form because the form IS the identity claim.
% TRANSFER_FUNCTION: Moves time, resources, mobility, and adaptive capacity FROM resource-constrained and environmentally displaced practitioners TO the preservation of the canonical symbolic form itself. The constraint extracts compliance labor (time spent in ritual even during subsistence crisis), foregoes adaptive modifications (that would reduce fidelity), and suppresses alternative forms that might serve the same identity function with lower resource cost.
% ABSENT_VOICES: Displaced diaspora members, adaptive practitioners who see evolution as transmission rather than loss, communities for whom survival itself depends on modification. They would testify that identity can be maintained through adapted forms, that the fidelity doctrine is a performance by the authority lineage to maintain its interpretive power, not a structural requirement of continuity.
% DISAPPEARANCE_RATIONALE: If the symbol-continuity fidelity constraint vanished, communities would reorganize ritual practice around modified forms that preserved meaning while accommodating displacement, resource scarcity, and evolution. The authority lineage would lose its structural justification for suppressing adaptation. Communal identity would continue—possibly strengthened by authentic transmission—but through different symbolic forms and transmitted through different channels.
% FOUNDING_PROBLEM: Catastrophe—genocide, forced displacement, diaspora, environmental collapse—threatens to sever the symbolic transmission chain and thus the continuity of communal identity. The constraint was built to prevent that severance: encoding the identity in a precise, transmissible, enforceable symbolic form that can be preserved even when all other community structures are destroyed.
% FOUNDING_PROBLEM_CORROBORATION: Historians of genocide and diaspora attest that symbolic transmission was often the ONLY survival mechanism when communities were scattered and material structures destroyed. However, adaptive practitioners and diaspora communities attest that the problem is partially solved: identity persists through modified and hybrid forms; the fidelity doctrine is now a choice by the authority lineage, not a structural necessity. The constraint persists not because the founding problem demands it but because the authority lineage benefits from maintaining interpretive monopoly.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) and rising because the constraint sacrifices adaptive capacity to preserve form. The measurement series shows systematic increase: the rising extractiveness curve reflects the reality that as environmental conditions worsen (displacement, resource scarcity, diaspora), the fidelity constraint's cost rises but its enforcement strengthens. Theater ratio is moderate (0.41) because a portion of enforcement energy now defends the form itself rather than transmitting the meaning—visible in the authority lineage's intensifying suppression of modified practices. Suppression requirement rises in parallel (0.58 to 0.72) because adaptive practitioners and resource-constrained communities increasingly resist or violate the fidelity constraint in real time, requiring active enforcement. The coercion grid captures the level-differentiated picture: individual resistance decays (as internalized identity-lock strengthens and adaptive practitioners face isolation), but organizational resistance (diaspora communities, reformist factions) remains live and must be actively suppressed. Class-level suppression rises most sharply because the poorest communities face the highest cost of compliance during crisis.
 *
 * PERSPECTIVAL GAP:
 *   The authority lineage and the identity-locked beneficiaries experience the constraint as non-negotiable: the form IS the identity, and its preservation is intrinsically good. Adaptive practitioners experience it as extractive: they see the meaning surviving through modified forms and experience the fidelity enforcement as suppressing evolution rather than enabling survival. Resource-constrained communities experience it as purely extractive during crisis: they bear the survival cost (time to ritual when time is subsistence) while gaining no benefit to adaptive capacity. The engine computes these divergent directionalities from the structural data—beneficiary vs. payer status, exit options (identity-locked vs. constrained), power differentials. The authored claim (tangled rope) reflects the structure: genuine coordination (identity transmission) coupled with asymmetric extraction (adaptive capacity sacrificed for form fidelity).
 *
 * DIRECTIONALITY LOGIC:
 *   Identity-locked beneficiaries (d ≈ 0.15): their entire identity is constituted by participation in the constraint; they benefit from its enforcement; exit is unthinkable because it means ceasing to be themselves. Adaptive practitioners (d ≈ 0.65): moderate power but constrained exit (abandon community or comply with fidelity) and bearing real costs (time, forgone adaptation). Resource-constrained communities (d ≈ 0.85): powerless, trapped (no exit), bearing crisis-level extraction costs (survival time diverted). The authority lineage's directionality (d ≈ 0.20) is derived as beneficiary-like: they set the rules, collect interpretive authority, and face no material cost. The constraint's effective extraction is amplified by scope (local but dense) and by directionality asymmetry—high target d combined with high suppression means the trapped agents bear the full extractive force.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question for symbol-continuity reading: Is the founding problem (preserving identity across catastrophic severance) still structurally live, or has it been partially solved? The reading's own account suggests partial resolution: diaspora communities maintain identity through modified practices, adaptive transmission preserves meaning, and the fidelity doctrine is increasingly a choice by the authority lineage rather than a necessity. However, the extinction risk (that the community's symbolic form could be completely severed) remains real in contexts of ongoing displacement or suppression. The constraint's mandate is contested but not dead. The tangled-rope classification prevents false-summit misclassification (treating identity preservation as a natural law) while allowing for the genuine coordination function the constraint serves—identity transmission IS a coordination problem in communities facing existential severance. The rising extractiveness and theater metrics suggest the constraint is increasingly performing identity rather than transmitting it, which is a mandatrophy signal: the form persists theatrically while the actual transmission of meaning erodes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_constitution_mechanism,
    'Is communal identity genuinely constituted by symbolic form fidelity, or does identity persist through adapted/modified forms and the fidelity doctrine is a choice by the authority lineage?',
    'Ethnographic study of diaspora communities: if identity and belonging persist through modified or hybrid ritual forms without fidelity to canonical symbols, the identity is not form-dependent. If canonical-form members deny modified-form practitioners as ''authentic'' community, the fidelity claim is maintained by exclusion, not by identity necessity.',
    'If identity is form-independent, the constraint reclassifies from tangled-rope (genuine coordination + extraction) to snare (pure extraction with coordination cover). The omegas of identity_locked exit would shift from internalized (identity boundary = community boundary) to structural (authority-enforced exclusion). Remedies would shift from accommodation to transformation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_constitution_mechanism, empirical, 'Whether identity is intrinsically form-dependent or form-independent.').

omega_variable(
    transmission_mode_efficacy,
    'Does canonical symbolic form transmission preserve meaning-carrying capacity through generations, or do adapted/hybrid forms transmit meaning equally well at lower cost?',
    'Longitudinal study comparing meaning-retention and practice-continuity across canonical-form and adapted-form communities over 2–3 generations. Measure: (a) participant reported meaning preservation; (b) coherence of transmitted knowledge; (c) community continuity markers; (d) adaptive capacity in changed environments.',
    'If adapted forms transmit meaning equally, the fidelity constraint is extractive without coordination justification—the constraint reclassifies to snare, and mandatrophy is resolved (founding problem solved; constraint persists as institutional inertia and authority rent-seeking). If canonical form transmits meaning better, the coordination function is real and the extraction is a genuine tangled-rope trade-off.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transmission_mode_efficacy, empirical, 'Whether form fidelity is necessary for meaning transmission or whether adaptation preserves transmission efficacy.').

omega_variable(
    resource_crisis_suppression_mechanism,
    'During resource crisis, is the suppression of adapted ritual forms structural (community members internalize identity-fidelity fusion) or internalized-but-removable (the authority lineage actively enforces exclusion of adapters, but the suppression would lift if enforcement ceased)?',
    'Post-crisis ethnography: in communities where enforcement was lifted or disrupted (through diaspora separation, authority collapse, or policy change), do adaptive practitioners resume adapted forms and maintain community identity, or do they return to fidelity compliance? Measure baseline return-to-compliance rates in post-enforcement contexts.',
    'If suppression is internalized (identity-locked fusion persists after enforcement lifts), the trapped directionality is real and the extraction is genuine. If suppression is structural-but-removable (adapters would continue modified forms if authority ceased enforcing), the fidelity constraint is maintained by coercion, not by identity necessity, strengthening the snare classification and weakening the tangled-rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_crisis_suppression_mechanism, empirical, 'Whether suppression is internalized identity-lock or structural authority enforcement.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the symbol-continuity reading''s core axiom (identity=form-fidelity) logically foreclose the operational-competence reading''s axiom (ritual encodes survival knowledge), or do both readings coexist as live interpretations of the same kernel?',
    'Textual and ethnographic analysis: can a single community or tradition hold BOTH that the form preserves identity AND that the form encodes survival knowledge, or does commitment to one reading require rejecting the other? Are there communities that frame the form as simultaneously identity-preserving and operationally-competent?',
    'If readings foreclose each other, the network relationship is forecloses and the CS structure''s reading_relations is forecloses (rare). If both coexist, the relationship is coexists_with (more common): different communities adopt different readings without logical contradiction. If one reading influences the other''s viability, the relationship is influences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether sibling readings are logically exclusive or compatible live positions.').

omega_variable(
    authority_lineage_capture,
    'Is the ritual-authority lineage''s defense of fidelity a genuine stewardship of the identity transmission mechanism, or has the authority lineage become captured: the form''s fidelity now serves the lineage''s power and interpretive monopoly rather than the community''s identity survival?',
    'Institutional analysis: (a) does authority lineage benefit materially or institutionally from fidelity enforcement (status, control, resources)? (b) would removing fidelity enforcement change the authority structure? (c) are alternative interpreters excluded from recognized authority? (d) do authority figures themselves practice adaptive forms (private adaptation vs. public enforcement)? A pattern of private adaptation + public enforcement + interpretive monopoly + authority benefit is capture.',
    'If authority capture is high, the constraint reclassifies from tangled-rope to snare: the coordination function is real but secondary; the primary function is extracting interpretive authority. Effective extraction χ would be higher than base ε suggests. Remedies would target authority decentralization rather than accommodation of adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_lineage_capture, empirical, 'Whether authority lineage stewards genuine coordination or has become captured by the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__symbol_continuity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(cata_su_t8, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(cata_su_t32, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 40, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(cata_grid_01, catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse(class), 0, 0.85).
narrative_ontology:measurement(cata_grid_02, catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse(class), 40, 0.89).
narrative_ontology:measurement(cata_grid_03, catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse(individual), 0, 0.72).
narrative_ontology:measurement(cata_grid_04, catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse(individual), 40, 0.81).
narrative_ontology:measurement(cata_grid_05, catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse(organizational), 0, 0.68).
narrative_ontology:measurement(cata_grid_06, catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse(organizational), 40, 0.76).
narrative_ontology:measurement(cata_grid_07, catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse(structural), 0, 0.78).
narrative_ontology:measurement(cata_grid_08, catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse(structural), 40, 0.82).
narrative_ontology:measurement(cata_grid_09, catastrophe_memory_transmission__symbol_continuity_reading, resistance(class), 0, 0.58).
narrative_ontology:measurement(cata_grid_10, catastrophe_memory_transmission__symbol_continuity_reading, resistance(class), 40, 0.48).
narrative_ontology:measurement(cata_grid_11, catastrophe_memory_transmission__symbol_continuity_reading, resistance(individual), 0, 0.48).
narrative_ontology:measurement(cata_grid_12, catastrophe_memory_transmission__symbol_continuity_reading, resistance(individual), 40, 0.42).
narrative_ontology:measurement(cata_grid_13, catastrophe_memory_transmission__symbol_continuity_reading, resistance(organizational), 0, 0.61).
narrative_ontology:measurement(cata_grid_14, catastrophe_memory_transmission__symbol_continuity_reading, resistance(organizational), 40, 0.52).
narrative_ontology:measurement(cata_grid_15, catastrophe_memory_transmission__symbol_continuity_reading, resistance(structural), 0, 0.52).
narrative_ontology:measurement(cata_grid_16, catastrophe_memory_transmission__symbol_continuity_reading, resistance(structural), 40, 0.43).
narrative_ontology:measurement(cata_grid_17, catastrophe_memory_transmission__symbol_continuity_reading, stakes_inflation(class), 0, 0.71).
narrative_ontology:measurement(cata_grid_18, catastrophe_memory_transmission__symbol_continuity_reading, stakes_inflation(class), 40, 0.81).
narrative_ontology:measurement(cata_grid_19, catastrophe_memory_transmission__symbol_continuity_reading, stakes_inflation(individual), 0, 0.62).
narrative_ontology:measurement(cata_grid_20, catastrophe_memory_transmission__symbol_continuity_reading, stakes_inflation(individual), 40, 0.74).
narrative_ontology:measurement(cata_grid_21, catastrophe_memory_transmission__symbol_continuity_reading, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(cata_grid_22, catastrophe_memory_transmission__symbol_continuity_reading, stakes_inflation(organizational), 40, 0.68).
narrative_ontology:measurement(cata_grid_23, catastrophe_memory_transmission__symbol_continuity_reading, stakes_inflation(structural), 0, 0.64).
narrative_ontology:measurement(cata_grid_24, catastrophe_memory_transmission__symbol_continuity_reading, stakes_inflation(structural), 40, 0.72).
narrative_ontology:measurement(cata_grid_25, catastrophe_memory_transmission__symbol_continuity_reading, suppression(class), 0, 0.58).
narrative_ontology:measurement(cata_grid_26, catastrophe_memory_transmission__symbol_continuity_reading, suppression(class), 40, 0.75).
narrative_ontology:measurement(cata_grid_27, catastrophe_memory_transmission__symbol_continuity_reading, suppression(individual), 0, 0.54).
narrative_ontology:measurement(cata_grid_28, catastrophe_memory_transmission__symbol_continuity_reading, suppression(individual), 40, 0.68).
narrative_ontology:measurement(cata_grid_29, catastrophe_memory_transmission__symbol_continuity_reading, suppression(organizational), 0, 0.61).
narrative_ontology:measurement(cata_grid_30, catastrophe_memory_transmission__symbol_continuity_reading, suppression(organizational), 40, 0.73).
narrative_ontology:measurement(cata_grid_31, catastrophe_memory_transmission__symbol_continuity_reading, suppression(structural), 0, 0.62).
narrative_ontology:measurement(cata_grid_32, catastrophe_memory_transmission__symbol_continuity_reading, suppression(structural), 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__symbol_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__symbol_continuity_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe-memory-transmission kernel. The symbol-continuity reading models the constraint as enforcing canonical symbolic form for identity preservation (type: tangled_rope, ε=0.68, victims: adaptive capacity). Sibling readings—operational-competence (ritual as survival knowledge encoding, type: rope, ε≈0.45) and hybrid-embedded (form and competence inseparable, type: tangled_rope, ε≈0.55, victims: operational flexibility)—model the same kernel under different interpretive framings. They are NOT parts of this constraint; they are separate constraints linked by affects_constraints because the readings compete in the same community's decision space and because evidence about one reading (e.g., whether form actually preserves meaning) affects the others' empirical grounding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_transmission__symbol_continuity_reading, organized, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
