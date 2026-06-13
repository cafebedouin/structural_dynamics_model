% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__adaptive_fiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__adaptive_fiction_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lycurgan_laws__adaptive_fiction_reading
 *   human_readable: Lycurgan Laws: Adaptive Fiction Reading
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'adaptive fiction' reading of the Lycurgan
 *   laws, where the rhetoric of immutability served as a 'noble lie' to
 *   maintain social order, while Spartan elites (ephors, kings, gerousia)
 *   covertly adapted the laws through interpretation and selective
 *   enforcement. The system was not truly rigid but maintained a facade of
 *   unchangeability, allowing for flexibility without explicit revision. This
 *   reading contrasts with views of the laws as genuinely immutable or as a
 *   brittle system leading to collapse.
 *
 * KEY AGENTS:
 *   - spartan_ephors: Agenda-setter (institutional/generational) — interpreted and adapted laws.
 *   - spartan_kings: Agenda-setter (institutional/generational) — held religious and military authority, influenced interpretation.
 *   - spartan_gerousia: Agenda-setter (institutional/generational) — council of elders, advised and interpreted laws.
 *   - spartan_citizens_seeking_reform: Payer (powerless/biographical) — bore the costs of perceived rigidity, lacked formal channels for explicit reform.
 *   - spartan_populace: Beneficiary (moderate/biographical) — benefited from social stability and identity, but also subject to the laws' demands.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.25).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.4).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, rope).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Laws: Adaptive Fiction Reading").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:requires_active_enforcement(lycurgan_laws__adaptive_fiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, 'cdfedb05-b8c5-4810-a5cf-0ab1bd9f7583').
narrative_ontology:cs_kernel_codification('cdfedb05-b8c5-4810-a5cf-0ab1bd9f7583', formalized).
narrative_ontology:cs_authority_grounding('cdfedb05-b8c5-4810-a5cf-0ab1bd9f7583', lineage).
narrative_ontology:cs_interpretation_layer_present('cdfedb05-b8c5-4810-a5cf-0ab1bd9f7583').
narrative_ontology:cs_reading_relation('cdfedb05-b8c5-4810-a5cf-0ab1bd9f7583', lycurgan_laws__sacral_fidelity_reading, coexists_with).
narrative_ontology:cs_reading_relation('cdfedb05-b8c5-4810-a5cf-0ab1bd9f7583', lycurgan_laws__demographic_trap_reading, coexists_with).
narrative_ontology:cs_axiom('cdfedb05-b8c5-4810-a5cf-0ab1bd9f7583', foundational, immutability_as_rhetorical_device).
narrative_ontology:cs_axiom_status(immutability_as_rhetorical_device, holdable).
narrative_ontology:cs_axiom_grounding('cdfedb05-b8c5-4810-a5cf-0ab1bd9f7583', immutability_as_rhetorical_device, conventional).
narrative_ontology:cs_axiom('cdfedb05-b8c5-4810-a5cf-0ab1bd9f7583', foundational, elite_interpretive_flexibility).
narrative_ontology:cs_axiom_status(elite_interpretive_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('cdfedb05-b8c5-4810-a5cf-0ab1bd9f7583', elite_interpretive_flexibility, empirically_contingent).
narrative_ontology:cs_reference_frame('cdfedb05-b8c5-4810-a5cf-0ab1bd9f7583', spartan_elite_adaptive_governance).
narrative_ontology:cs_drift_state('cdfedb05-b8c5-4810-a5cf-0ab1bd9f7583', late_spartan_period, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('cdfedb05-b8c5-4810-a5cf-0ab1bd9f7583', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_ephors).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_kings).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_gerousia).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, spartan_citizens_seeking_reform).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_populace).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected magistrates with significant power, including judicial and executive functions. They were key in interpreting and applying the Lycurgan laws, often adapting them to contemporary needs while publicly upholding their immutability. They benefited from the stability and authority derived from the laws' perceived divine origin.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_ephors, agenda_setter,
    institutional, generational, constrained, national).

% Two hereditary kings who held religious and military authority. While their direct legislative power was limited, their influence and interpretation of tradition played a role in the covert adaptation of the laws. They benefited from the legitimacy and continuity provided by the Lycurgan framework.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_kings, agenda_setter,
    institutional, generational, constrained, national).

% Council of 28 elders (over 60 years old) who served for life. They prepared legislation for the assembly and acted as a high court. Their wisdom and experience were crucial in interpreting the laws, allowing for subtle adjustments over time without formal amendment. They benefited from their elevated status and influence within the system.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_gerousia, agenda_setter,
    institutional, generational, constrained, national).

% Citizens who perceived the laws as rigid and sought changes to address evolving social or economic conditions. Their options for explicit reform were virtually non-existent due to the immutability myth, leading to frustration and a sense of being trapped by an unyielding system. Their identity as Spartans was deeply tied to adherence to these laws.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_citizens_seeking_reform, payer,
    powerless, biographical, identity_locked, national).

% The broader body of Spartan citizens who largely accepted the Lycurgan system as foundational to their identity and stability. They benefited from the social cohesion and military strength it fostered, but also bore the costs of its strict demands and the lack of overt political agency. Their identity was fused with the Lycurgan way of life.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_populace, beneficiary,
    moderate, biographical, identity_locked, national).

% The enslaved population of Laconia and Messenia, who were fundamentally excluded from the Lycurgan system's benefits and bore its most extreme costs. Their labor supported the Spartan citizen class, and their suppression was a core function of the Spartan state, though not directly addressed by the 'immutability' aspect of the laws themselves.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, helots, excluded,
    powerless, generational, trapped, regional).

% Modern and ancient scholars who analyze the Lycurgan laws, their historical application, and their impact on Spartan society. They are outside the system, seeking to understand its true nature, including the interplay between declared immutability and practical adaptation.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, historians_and_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__adaptive_fiction_reading, spartan_ephors).
narrative_ontology:fixing_cost_class(lycurgan_laws__adaptive_fiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable, coherent framework for Spartan society, coordinating military training, social structure, and political authority under the guise of ancient, divinely inspired laws, thereby fostering a strong collective identity and purpose.
% TRANSFER_FUNCTION: Transferred political and interpretive flexibility to the Spartan elite (ephors, kings, gerousia) by allowing them to adapt the laws covertly, while transferring the burden of perceived rigidity and lack of explicit reform channels to the general citizenry.
% ABSENT_VOICES: Spartan citizens who desired explicit reform or a more transparent legislative process were effectively silenced by the rhetoric of immutability and the lack of formal mechanisms for amendment. Their voices were absent from the public discourse on the laws' nature.
% DISAPPEARANCE_RATIONALE: If the Lycurgan laws and the myth of their immutability vanished overnight, the entire Spartan social, political, and military structure would collapse. The system of governance, land distribution, military training, and citizen identity was so deeply intertwined with these laws that their disappearance would necessitate a complete societal reorganization.
% FOUNDING_PROBLEM: The Lycurgan laws were purportedly established to address internal strife, social inequality, and military weakness in early Sparta, aiming to create a stable, egalitarian, and militarily dominant society.
% FOUNDING_PROBLEM_CORROBORATION: Ancient historians like Plutarch and Xenophon, while often romanticizing Lycurgus, corroborate the intent to solve these problems. Modern historians, however, contest the extent of their success and the true authorship/dating, often viewing the 'founding problem' as a retrospective justification for an evolving system. The Spartan elite themselves would have attested to the ongoing 'live' status of these problems to justify their continued authority, but external historical analysis suggests the original problems were either solved or transformed, while the laws persisted through adaptation.
narrative_ontology:disappearance_verdict(lycurgan_laws__adaptive_fiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__adaptive_fiction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__adaptive_fiction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(lycurgan_laws__adaptive_fiction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__adaptive_fiction_reading_tests).
:- end_tests(lycurgan_laws__adaptive_fiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.25) is moderate, reflecting the costs of maintaining the 'noble lie' and the lack of explicit reform channels, but not as high as a truly rigid system. Suppression (0.4) is present to maintain the facade of immutability and discourage open dissent, but not absolute. The high theater ratio (0.6) is central to this reading: a significant portion of the constraint's operation was performative maintenance of the 'unchangeable' myth, while actual adaptation occurred covertly. Accessibility collapse is low (0.3) because the system, while rhetorically rigid, was functionally adaptable through elite interpretation, meaning alternatives (covert adaptation) were not entirely collapsed. Resistance is low (0.1) because open resistance was suppressed, and covert adaptation by elites diffused pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Spartan elites (ephors, kings, gerousia), the laws were a flexible tool for governance, allowing for adaptation while preserving the myth of stability. For ordinary citizens, the system appeared immutable, and their ability to explicitly challenge or reform it was severely constrained. The engine's per-seat classification should reflect this divergence, with elites experiencing a more 'rope-like' constraint and citizens a more 'tangled rope' or 'snare-like' one due to the perceived lack of agency.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spartan ephors, kings, and gerousia are beneficiaries and agenda-setters (d near 0.0-0.2) as they wielded the laws' interpretive flexibility to maintain power and adapt the state. Spartan citizens seeking reform are victims (d near 0.8-1.0) as they faced the perceived rigidity and lacked formal channels for change. The broader Spartan populace is a diffuse beneficiary (d near 0.4-0.6) of the stability the laws provided, but also bore the costs of its demands.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading suggests the Lycurgan laws did not suffer from mandatrophy in the sense of functional obsolescence due to rigidity, but rather adapted covertly. The 'noble lie' allowed the mandate to persist by shifting its operational reality away from its declared form. The classification as a 'rope' (albeit with high theater) prevents mislabeling it as a 'piton' (inertial decay) or 'snare' (pure extraction from rigidity), by highlighting the functional, albeit hidden, adaptation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    noble_lie_vs_genuine_belief,
    'Was the claim of Lycurgan immutability a deliberate ''noble lie'' by the Spartan elite, or a genuine, widely held belief in its divine origin?',
    'Archaeological and textual analysis of non-elite Spartan writings (if any exist) and contemporary foreign accounts to gauge popular belief vs. elite rhetoric.',
    'If a noble lie, the constraint is more extractive (deliberate manipulation); if genuine belief, it''s closer to a cultural mountain (internalized constraint).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(noble_lie_vs_genuine_belief, empirical, 'Distinguishing elite manipulation from popular belief in Lycurgan immutability.').

omega_variable(
    covert_adaptation_extent,
    'To what extent did Spartan institutions (ephors, kings, gerousia) actually adapt the Lycurgan laws through interpretation, rather than merely maintaining a facade of immutability?',
    'Detailed historical analysis of specific legal and political decisions over time, tracing how ''unchanging'' laws were applied to novel situations or used to justify new policies.',
    'Greater evidence of covert adaptation strengthens the ''rope'' classification by showing functional flexibility; less adaptation would push it towards ''piton'' (inertial rigidity) or ''snare'' (pure extraction through rigidity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covert_adaptation_extent, empirical, 'Quantifying the degree of covert adaptation in Lycurgan laws.').

omega_variable(
    kernel_reading_distinction,
    'This constraint is the ''adaptive_fiction_reading'' of the ''lycurgan_laws'' kernel. How does this reading''s emphasis on covert adaptation and elite interpretation structurally differ from the ''sacral_fidelity_reading'' (absolute adherence) and ''demographic_trap_reading'' (brittle rigidity)?',
    'Comparing the core axioms and proposed mechanisms of change/stasis across all three readings, noting how each explains the historical trajectory of Sparta.',
    'The ''adaptive_fiction_reading'' posits a more flexible, albeit opaque, system (rope-like), while the ''sacral_fidelity_reading'' would be a mountain (unchangeable) and the ''demographic_trap_reading'' a snare (rigid extraction leading to collapse).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Clarifying the structural differences between the Lycurgan Laws kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(lycu_tr_t50, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(lycu_tr_t100, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 100, 0.55).
narrative_ontology:measurement(lycu_tr_t150, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 150, 0.6).
narrative_ontology:measurement(lycu_tr_t200, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 200, 0.58).
narrative_ontology:measurement(lycu_tr_t250, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 250, 0.6).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(lycu_be_t50, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement(lycu_be_t100, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 100, 0.25).
narrative_ontology:measurement(lycu_be_t150, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 150, 0.24).
narrative_ontology:measurement(lycu_be_t200, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 200, 0.23).
narrative_ontology:measurement(lycu_be_t250, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 250, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(lycu_su_t50, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 50, 0.32).
narrative_ontology:measurement(lycu_su_t100, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 100, 0.35).
narrative_ontology:measurement(lycu_su_t150, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 150, 0.38).
narrative_ontology:measurement(lycu_su_t200, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 200, 0.39).
narrative_ontology:measurement(lycu_su_t250, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 250, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, identity_coordination).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__demographic_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Lycurgan Laws' kernel. This 'adaptive fiction' reading posits covert elite adaptation, contrasting with the 'sacral fidelity' reading (genuine immutability) and the 'demographic trap' reading (brittle rigidity leading to collapse). Each reading represents a distinct constraint with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
