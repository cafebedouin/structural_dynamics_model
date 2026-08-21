% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__prophetic_override_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__prophetic_override_reading
 *   human_readable: Eternal Marriage Covenant: Prophetic Override Reading
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint describes the 'prophetic override' reading of the eternal
 *   marriage covenant, where the doctrine of continuing revelation allows a
 *   living prophet to supersede prior divine commandments, particularly in
 *   response to external pressures like federal anti-polygamy laws. This
 *   reading prioritizes the survival and adaptability of the church
 *   institution over the literal, immutable adherence to past revelations. It
 *   is claimed as a 'rope' by its proponents (a necessary coordination
 *   mechanism for institutional survival) but operates with significant
 *   extraction and suppression, making it a 'tangled_rope' from an analytical
 *   perspective.
 *
 * KEY AGENTS:
 *   - church_leadership: Agenda setter (institutional/constrained)
 *   - church_institution: Beneficiary (institutional/constrained)
 *   - members_adhering_to_prior_revelation: Payer (powerless/identity_locked)
 *   - dissenting_factions: Excluded (powerless/trapped)
 *   - federal_government: Observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.65).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.78).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Eternal Marriage Covenant: Prophetic Override Reading").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, '76e24b11-8b69-4e1a-8292-2b63c0d385a6').
narrative_ontology:cs_kernel_codification('76e24b11-8b69-4e1a-8292-2b63c0d385a6', formalized).
narrative_ontology:cs_authority_grounding('76e24b11-8b69-4e1a-8292-2b63c0d385a6', lineage).
narrative_ontology:cs_interpretation_layer_present('76e24b11-8b69-4e1a-8292-2b63c0d385a6').
narrative_ontology:cs_reading_relation('76e24b11-8b69-4e1a-8292-2b63c0d385a6', eternal_marriage_covenant__immutable_commandment_reading, influences).
narrative_ontology:cs_reading_relation('76e24b11-8b69-4e1a-8292-2b63c0d385a6', eternal_marriage_covenant__temporal_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('76e24b11-8b69-4e1a-8292-2b63c0d385a6', foundational, living_prophet_supersedes_prior_revelation).
narrative_ontology:cs_axiom_status(living_prophet_supersedes_prior_revelation, holdable).
narrative_ontology:cs_axiom_grounding('76e24b11-8b69-4e1a-8292-2b63c0d385a6', living_prophet_supersedes_prior_revelation, theological).
narrative_ontology:cs_axiom('76e24b11-8b69-4e1a-8292-2b63c0d385a6', secondary, institutional_survival_is_paramount).
narrative_ontology:cs_axiom_status(institutional_survival_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('76e24b11-8b69-4e1a-8292-2b63c0d385a6', institutional_survival_is_paramount, instrumental).
narrative_ontology:cs_reference_frame('76e24b11-8b69-4e1a-8292-2b63c0d385a6', prophetic_adaptability_framework).
narrative_ontology:cs_drift_state('76e24b11-8b69-4e1a-8292-2b63c0d385a6', contemporary_secular_society, gap(stable, minor, true)).
narrative_ontology:cs_created_at('76e24b11-8b69-4e1a-8292-2b63c0d385a6', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_institution).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, members_adhering_to_prior_revelation).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, dissenting_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the authority to receive and declare new revelation, which can supersede prior commandments. This power allows the institution to adapt to external pressures while maintaining internal coherence and survival. They benefit from the flexibility to navigate legal and social challenges.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from the doctrine of continuing revelation by being able to adapt its practices to ensure its survival and growth in changing legal and social environments. This flexibility prevents existential threats from external authorities.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_institution, beneficiary,
    institutional, civilizational, constrained, global).

% Are required to abandon practices or beliefs based on prior revelation when a new, superseding revelation is announced. This can cause significant personal and spiritual distress, as their identity is often deeply tied to the superseded practices. They bear the cost of doctrinal shifts.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, members_adhering_to_prior_revelation, payer,
    powerless, biographical, identity_locked, local).

% Are those who refuse to accept the superseding revelation, often leading to excommunication or marginalization. They are structurally excluded from the main body of the church and face social and spiritual isolation for adhering to what they believe is the 'true' or 'eternal' doctrine.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, dissenting_factions, excluded,
    powerless, generational, trapped, regional).

% Exerted legal and political pressure that historically led to the 'new revelation' superseding the practice of plural marriage. While not a direct participant in the religious doctrine, its actions are a key external constraint that activates the prophetic override mechanism.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, federal_government, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows the church to maintain a unified body of doctrine and practice by providing a mechanism for authoritative adaptation to new circumstances, preventing schism over external pressures.
% TRANSFER_FUNCTION: Transfers the authority to define and redefine divine law from historical texts or past prophets to the living prophet, ensuring the institution's continuity and adaptability. It also transfers the burden of adapting to new revelations onto individual members.
% ABSENT_VOICES: Those who believe in the absolute immutability of prior revelation, particularly regarding eternal covenants, are effectively silenced or marginalized. They would argue that divine law cannot be changed by human (even prophetic) decree.
% DISAPPEARANCE_RATIONALE: If the doctrine of continuing revelation and prophetic override vanished, the church would face immediate and severe internal conflict when confronted with external pressures that contradict prior revelations. It would likely fracture into multiple factions, unable to adapt or maintain a unified identity, leading to significant institutional reorganization or collapse.
% FOUNDING_PROBLEM: The church faced existential threats from the federal government due to its practice of plural marriage, which was deemed illegal and immoral by secular law.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, government documents, and independent academic analyses corroborate the federal pressure and the church's response. While the specific legal threat of plural marriage is 'dead', the underlying problem of institutional survival in a secular society remains 'live', making the status 'contested' from different perspectives.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__prophetic_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__prophetic_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eternal_marriage_covenant__prophetic_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__prophetic_override_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the cost imposed on members who must abandon deeply held practices and beliefs. Suppression (0.78) is high due to the severe consequences for dissent (excommunication, social ostracization), which effectively eliminates alternatives to compliance. The theater ratio (0.20) is low, indicating that the 'new revelation' is genuinely functional in resolving the institutional crisis, though it also serves to maintain the authority structure. The claimed type is 'rope' because the church frames it as a necessary coordination for survival, but the metrics reveal significant extraction and coercion, leading to an analytical classification of 'tangled_rope'.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of church leadership, the prophetic override is a vital mechanism for divine guidance and institutional preservation, a 'rope' that navigates existential threats. For members whose eternal salvation was tied to the superseded practice, it is a deeply extractive and coercive 'snare' that demands profound personal sacrifice and reorientation of their spiritual identity. The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership and the institution itself are clear beneficiaries (d near 0.0) as the doctrine ensures their survival and continued authority. Members adhering to prior revelation are targets (d near 1.0) as they bear the direct costs of doctrinal shifts. Dissenting factions are also targets, facing exclusion. The federal government acts as an external force, not directly benefiting from the religious constraint but influencing its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as 'tangled_rope' prevents mislabeling this as pure coordination (rope) by highlighting the asymmetric extraction and active enforcement required. It also avoids mislabeling it as pure extraction (snare) by acknowledging the genuine coordination function of institutional survival. The 'dead' status of the founding problem (plural marriage) combined with the 'world_rearranges' disappearance verdict signals a potential for mandatrophy, where the mechanism persists even after the initial crisis, but the 'contested' status of the problem indicates ongoing debate about its current function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prophetic_authority_grounding,
    'Is the prophetic authority to supersede prior revelation grounded in divine mandate or institutional necessity?',
    'Theological analysis of scriptural interpretation and historical precedent, alongside sociological analysis of institutional power dynamics during crises.',
    'If primarily divine, the constraint is closer to a ''mountain'' (divinely ordained). If primarily institutional, it is a ''tangled_rope'' (human construct for survival).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prophetic_authority_grounding, conceptual, 'Ambiguity in the source of prophetic authority for doctrinal change.').

omega_variable(
    identity_lock_severity,
    'How deeply is the identity of members tied to specific, superseded revelations, and what is the psychological cost of reorientation?',
    'Qualitative sociological studies and psychological assessments of former and current members affected by doctrinal shifts.',
    'Higher identity lock severity increases the effective extraction and suppression for affected members, pushing their seat classification closer to ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_severity, empirical, 'The degree to which members'' identities are fused with superseded doctrines.').

omega_variable(
    founding_problem_status_ambiguity,
    'Is the founding problem (federal pressure on plural marriage) truly ''dead'', or has it merely transformed into a ''live'' problem of institutional adaptation to broader secular norms?',
    'Longitudinal historical analysis of church-state relations and comparative studies of religious institutions adapting to modern secular societies.',
    'If the problem is truly dead, the constraint''s persistence is more indicative of a ''piton'' or ''snare'' (inertia/pure extraction). If it has transformed, it supports the ''tangled_rope'' classification (ongoing coordination function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_status_ambiguity, empirical, 'Whether the original justification for the prophetic override remains relevant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(eter_tr_t1894, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1894, 0.15).
narrative_ontology:measurement(eter_tr_t1898, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1898, 0.18).
narrative_ontology:measurement(eter_tr_t1904, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1904, 0.2).

% Extraction over time
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1890, 0.5).
narrative_ontology:measurement(eter_be_t1894, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1894, 0.55).
narrative_ontology:measurement(eter_be_t1898, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1898, 0.6).
narrative_ontology:measurement(eter_be_t1904, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1904, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1890, 0.65).
narrative_ontology:measurement(eter_su_t1894, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1894, 0.7).
narrative_ontology:measurement(eter_su_t1898, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1898, 0.75).
narrative_ontology:measurement(eter_su_t1904, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1904, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'eternal_marriage_covenant' kernel. It focuses on the prophetic authority to supersede prior revelation for institutional survival, distinct from readings emphasizing immutability or mere temporal accommodation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
