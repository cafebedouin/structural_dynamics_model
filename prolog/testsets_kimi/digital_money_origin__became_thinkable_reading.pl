% ============================================================================
% CONSTRAINT STORY: digital_money_origin__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__became_thinkable_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: digital_money_origin__became_thinkable_reading
 *   human_readable: Digital Money Origin: Became Thinkable Reading
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'became_thinkable' reading of the
 *   contested digital_money_origin kernel. The reading locates the emergence
 *   of digital money at the moment the concept became technically and
 *   institutionally conceivableâprior to widespread
 *   implementationâthereby privileging conceptual architects over
 *   implementation-era builders. It operates as a tangled rope: it genuinely
 *   coordinates scholarly and institutional understanding around a coherent
 *   origin narrative, while asymmetrically extracting historical credit and
 *   legitimacy from excluded populations and operational innovators. Sibling
 *   readings include the 'first_held' reading (privileging practical
 *   possession) and the 'regulatory_recognition' reading (privileging formal
 *   state incorporation).
 *
 * KEY AGENTS:
 *   - Early conceptual architects (cryptographers, cypherpunks, early monetary theorists): Primary agenda-setters and beneficiariesâset the origin narrative and collect historical priority.
 *   - Monetary authorities: Secondary beneficiariesâborrow legitimacy from the conceptual lineage for current digital currency programs.
 *   - Implementation-era entrepreneurs: Primary payersâbear the cost of historical erasure as their operational work is framed as execution rather than origin.
 *   - Technically excluded populations: Full targetsâpowerless, trapped in systems designed by others, excluded from the conceptual framing.
 *   - Academic monetary historians: Observersâanalytical seat from which the contested kernel and its drift are visible.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.55).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.6).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Digital Money Origin: Became Thinkable Reading").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, '77ea24aa-70e5-45d9-9366-f9cd5cdd5076').
narrative_ontology:cs_kernel_codification('77ea24aa-70e5-45d9-9366-f9cd5cdd5076', distributed).
narrative_ontology:cs_authority_grounding('77ea24aa-70e5-45d9-9366-f9cd5cdd5076', expertise).
narrative_ontology:cs_interpretation_layer_present('77ea24aa-70e5-45d9-9366-f9cd5cdd5076').
narrative_ontology:cs_reading_relation('77ea24aa-70e5-45d9-9366-f9cd5cdd5076', digital_money_origin__first_held_reading, coexists_with).
narrative_ontology:cs_reading_relation('77ea24aa-70e5-45d9-9366-f9cd5cdd5076', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('77ea24aa-70e5-45d9-9366-f9cd5cdd5076', foundational, conceptual_priority_defines_origin).
narrative_ontology:cs_axiom_status(conceptual_priority_defines_origin, holdable).
narrative_ontology:cs_axiom_grounding('77ea24aa-70e5-45d9-9366-f9cd5cdd5076', conceptual_priority_defines_origin, conventional).
narrative_ontology:cs_axiom('77ea24aa-70e5-45d9-9366-f9cd5cdd5076', foundational, institutional_imagination_prefigures_implementation).
narrative_ontology:cs_axiom_status(institutional_imagination_prefigures_implementation, holdable).
narrative_ontology:cs_axiom_grounding('77ea24aa-70e5-45d9-9366-f9cd5cdd5076', institutional_imagination_prefigures_implementation, instrumental).
narrative_ontology:cs_reference_frame('77ea24aa-70e5-45d9-9366-f9cd5cdd5076', technical_conceivability_threshold).
narrative_ontology:cs_drift_state('77ea24aa-70e5-45d9-9366-f9cd5cdd5076', post_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('77ea24aa-70e5-45d9-9366-f9cd5cdd5076', '').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_conceptual_architects).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, monetary_authorities).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, implementation_era_entrepreneurs).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, technically_excluded_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cryptographers, cypherpunks, and early monetary theorists whose conceptual work is treated as the origin point of digital money. They benefit from historical priority claims and institutional recognition, and they actively maintain the narrative through publications, conferences, and curriculum influence.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_conceptual_architects, agenda_setter,
    institutional, generational, mobile, global).

% Central banks and regulatory bodies that benefit from a conceptual lineage linking their current digital currency projects to foundational technical work, lending institutional legitimacy to their programs.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, monetary_authorities, beneficiary,
    institutional, generational, constrained, global).

% Builders of the first widely deployed digital payment systems and cryptocurrencies who find their practical implementation work treated as secondary to earlier conceptual breakthroughs. Their operational innovations are framed as execution rather than origin.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, implementation_era_entrepreneurs, payer,
    moderate, biographical, constrained, global).

% Populations without access to the technical and institutional discourses in which digital money was first conceived. They are excluded from the conceptual framing of money's future and bear the costs of systems designed without their input.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, technically_excluded_populations, payer,
    powerless, immediate, trapped, global).

% Scholars who trace the intellectual lineage of digital money. Some reinforce the conceivability-origin narrative; others challenge it. They occupy an analytical seat from which the contested kernel is visible.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, academic_monetary_historians, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__became_thinkable_reading, early_conceptual_architects).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates scholarly and institutional understanding around a specific origin point for digital money, establishing intellectual priority and a coherent pre-history that links conceptual breakthroughs to contemporary systems.
% TRANSFER_FUNCTION: Moves historical credit, institutional legitimacy, and resource allocation from implementation-era actors and populations excluded from technical discourse to the early conceptual architects and monetary authorities who claim the originating insight.
% ABSENT_VOICES: Implementation-focused entrepreneurs who built the first working systems, retail users who adopted digital money before regulatory recognition, and populations without technical literacy who would locate the origin in practical access rather than abstract conceivability.
% DISAPPEARANCE_RATIONALE: If the conceivability-origin framework vanished, historical narratives would shift to privilege implementation and use, institutional legitimacy would flow to operational innovators rather than conceptual pioneers, and the current distribution of credit and research funding in monetary history would reorganize.
% FOUNDING_PROBLEM: The need to establish intellectual priority and a coherent historical lineage for digital monetary systems before they achieved widespread technical deployment, in order to secure legitimacy and research continuity.
% FOUNDING_PROBLEM_CORROBORATION: Early conceptual architects attest the problem was the absence of a recognized theoretical foundation. Implementation-era entrepreneurs and some historians of technology attest the problem was always practical adoption and user access, not conceptualization; they corroborate that the framing persists to serve the priority claims of the architects rather than the historical record.
narrative_ontology:disappearance_verdict(digital_money_origin__became_thinkable_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__became_thinkable_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__became_thinkable_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__became_thinkable_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.18 to 0.55 over the interval as the conceivability-origin narrative hardens from speculative historiography into institutionalized orthodoxy. Suppression rises to 0.60 because maintaining the reading against competing origin stories requires active gatekeeping in curricula, citation practices, and funding allocation. Theater reaches 0.42 as an increasing share of maintenance activity becomes performative repetition of founding-figure mythology rather than genuine historical inquiry. Accessibility collapse at 0.50 reflects that once the framing is accepted, alternatives remain conceptually available but are structurally marginalized. Resistance at 0.45 captures growing pushback from implementation-focused historians and marginalized communities.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of the early conceptual architects, the constraint is a ropeâa necessary historiographical framework that prevents teleological confusion by distinguishing conception from execution. From the implementation-era entrepreneurs and technically excluded populations, the same structure reads as a snare: an origin story that suppresses operational innovation and non-technical participation to concentrate credit in a small technical priesthood. The engine resolves this divergence from the structural data: the coordination function is real but the extraction is asymmetric and enforced through institutionalized citation and curriculum.
 *
 * DIRECTIONALITY LOGIC:
 *   Early conceptual architects are structural beneficiaries with mobile exit and high power; their directionality sits near the beneficiary end. Monetary authorities benefit from borrowed legitimacy but are constrained by institutional inertia. Implementation-era entrepreneurs bear the cost of historical erasure with only moderate power and constrained exit. Technically excluded populations are full targets: powerless, trapped, and carrying the highest directionality. Academic historians sit at analytical distance.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling the narrative as a pure mountain (inevitable historiography) or a pure snare (fabricated mythology). The coordination functionâestablishing a coherent lineage for a complex technical historyâis genuine and would need to be replaced by some other ordering principle if removed. However, the asymmetric extraction of credit and the active enforcement against competing origin stories make it more than a rope. The absence of a sunset clause and the absence of a single implementation target distinguish it from scaffold and snare respectively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historiographic_priority_ambiguity,
    'Is the prioritization of conceptual over implementation history a necessary methodological choice for coherent historiography, or an extractive allocation of credit that suppresses operational innovation?',
    'Comparative analysis of historiographic practices in other technical fields (e.g., aviation, computing) to determine whether privileging conceptual breakthroughs over deployment is standard or anomalous.',
    'If methodological, the extraction metric should be revised downward; if extractive, the suppression and theater metrics are likely understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historiographic_priority_ambiguity, conceptual, 'Whether conceptual priority is historiographically necessary or extractive').

omega_variable(
    excluded_voice_suppression_mechanism,
    'Is the exclusion of non-technical populations from the origin narrative structural (no access to archives or discourse) or internalized (they accept the technical framing as natural)?',
    'Ethnographic and interview-based study of how non-technical users and marginalized communities conceptualize the origin and legitimacy of digital money.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, and directionality for excluded populations approaches full target.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_voice_suppression_mechanism, empirical, 'Structural vs internalized suppression of excluded origin voices').

omega_variable(
    regulatory_influence_direction,
    'Does the became_thinkable reading genuinely influence regulatory frameworks, or merely coexist with them while regulators independently construct their own origin stories?',
    'Citation and discourse analysis of central bank digital currency white papers and regulatory documents to trace their reliance on conceptual-origin vs regulatory-origin narratives.',
    'Would clarify whether the influences relation to regulatory_recognition_reading should be upgraded to coexists_with or downgraded to a weaker structural tie.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_influence_direction, empirical, 'Whether conceptual-origin narrative structurally shapes regulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__became_thinkable_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(digi_tr_t8, digital_money_origin__became_thinkable_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(digi_tr_t16, digital_money_origin__became_thinkable_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(digi_tr_t24, digital_money_origin__became_thinkable_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(digi_tr_t32, digital_money_origin__became_thinkable_reading, theater_ratio, 32, 0.36).
narrative_ontology:measurement(digi_tr_t40, digital_money_origin__became_thinkable_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__became_thinkable_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(digi_be_t8, digital_money_origin__became_thinkable_reading, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(digi_be_t16, digital_money_origin__became_thinkable_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(digi_be_t24, digital_money_origin__became_thinkable_reading, base_extractiveness, 24, 0.42).
narrative_ontology:measurement(digi_be_t32, digital_money_origin__became_thinkable_reading, base_extractiveness, 32, 0.48).
narrative_ontology:measurement(digi_be_t40, digital_money_origin__became_thinkable_reading, base_extractiveness, 40, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__became_thinkable_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(digi_su_t8, digital_money_origin__became_thinkable_reading, suppression_requirement, 8, 0.22).
narrative_ontology:measurement(digi_su_t16, digital_money_origin__became_thinkable_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(digi_su_t24, digital_money_origin__became_thinkable_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(digi_su_t32, digital_money_origin__became_thinkable_reading, suppression_requirement, 32, 0.53).
narrative_ontology:measurement(digi_su_t40, digital_money_origin__became_thinkable_reading, suppression_requirement, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the digital_money_origin kernel, which decomposes into three structurally distinct claims about monetary origins: the conceptual-conceivability reading (this file), the first-held reading, and the regulatory-recognition reading. Each reading carries a different epsilon, stakeholder set, and temporal origin point.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
