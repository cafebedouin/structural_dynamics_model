% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__universal_rights_reading, []).

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
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Magna Carta Universal Due Process Precedent
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint is one reading of the Magna Carta 1215 kernel,
 *   specifically the 'universal_rights_reading'. It interprets Clause 39 as
 *   establishing a transhistorical principle of universal due process,
 *   applying to all persons and limiting arbitrary state power. Sibling
 *   readings include the 'baronial_privilege_reading' (Magna Carta as a
 *   feudal contract for specific elites) and the 'living_document_reading'
 *   (Magna Carta as an adaptable constitutional substrate whose meaning
 *   evolves).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.4).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.15).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Magna Carta Universal Due Process Precedent").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, '1210f349-6b28-4497-853f-31a20993dec9').
narrative_ontology:cs_kernel_codification('1210f349-6b28-4497-853f-31a20993dec9', fixed_text).
narrative_ontology:cs_authority_grounding('1210f349-6b28-4497-853f-31a20993dec9', lineage).
narrative_ontology:cs_interpretation_layer_present('1210f349-6b28-4497-853f-31a20993dec9').
narrative_ontology:cs_reading_relation('1210f349-6b28-4497-853f-31a20993dec9', magna_carta_1215__baronial_privilege_reading, forecloses).
narrative_ontology:cs_reading_relation('1210f349-6b28-4497-853f-31a20993dec9', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('1210f349-6b28-4497-853f-31a20993dec9', foundational, universal_human_dignity).
narrative_ontology:cs_axiom_status(universal_human_dignity, holdable).
narrative_ontology:cs_axiom_grounding('1210f349-6b28-4497-853f-31a20993dec9', universal_human_dignity, deontological).
narrative_ontology:cs_axiom('1210f349-6b28-4497-853f-31a20993dec9', foundational, state_power_is_limited_by_law).
narrative_ontology:cs_axiom_status(state_power_is_limited_by_law, holdable).
narrative_ontology:cs_axiom_grounding('1210f349-6b28-4497-853f-31a20993dec9', state_power_is_limited_by_law, conventional).
narrative_ontology:cs_reference_frame('1210f349-6b28-4497-853f-31a20993dec9', enlightenment_rights_tradition).
narrative_ontology:cs_drift_state('1210f349-6b28-4497-853f-31a20993dec9', contemporary_human_rights_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('1210f349-6b28-4497-853f-31a20993dec9', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, all_persons).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, legal_profession).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, state_actors_seeking_arbitrary_power).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, executive_branch_overreach).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from protection against arbitrary state action, ensuring fundamental rights to life, liberty, and property. Bears the diffuse cost of maintaining a legal system, but is a net beneficiary of the constraint's existence.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, all_persons, beneficiary,
    powerless, biographical, trapped, universal).

% Interprets and enforces the principles of due process, upholding the constraint against state overreach. Benefits from the legitimacy and stability provided by the rule of law.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Operates within the framework of due process, advocating for individuals and ensuring state compliance. Benefits from the structured legal environment and the demand for their expertise.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, legal_profession, beneficiary,
    organized, biographical, constrained, national).

% Bears the cost of being constrained by due process, unable to act arbitrarily or bypass legal procedures. Experiences the constraint as an impediment to immediate action or political objectives.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, state_actors_seeking_arbitrary_power, payer,
    powerful, immediate, constrained, national).

% Represents instances where the executive attempts to bypass due process for perceived efficiency or security. Is forced to comply with legal procedures, incurring costs in time and resources.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, executive_branch_overreach, payer,
    institutional, immediate, constrained, national).

% Can pass laws that define or refine due process, or attempt to limit its scope. Benefits from the stability of a lawful system but may also seek to expand state power, creating tension with the constraint.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, legislature, agenda_setter,
    institutional, generational, mobile, national).

% Analyze the historical evolution and contemporary application of Magna Carta and due process. Their work informs legal interpretation and public understanding, but they do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, historians_legal_scholars, observer,
    analytical, generational, analytical, universal).

% Represent the original, narrower interpretation of Magna Carta as a feudal contract for specific elites. Their historical perspective is acknowledged but structurally excluded from the 'universal rights' reading's application.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, baronial_descendants_feudal_historians, excluded,
    moderate, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common, predictable standard for legal process and state action, ensuring fair treatment and limiting arbitrary power across diverse individuals and state entities, thereby coordinating societal expectations of justice.
% TRANSFER_FUNCTION: Transfers the right to arbitrary action from the state to individuals (as protection), and transfers the burden of proof and lawful process onto the state, requiring it to justify its actions according to established law.
% ABSENT_VOICES: Those who advocate for unchecked executive power or national security exemptions would object to the constraint's universal application; they are often present in political discourse but structurally excluded from the *application* of due process. The original 'free men' (barons) are also absent from this universalist interpretation.
% DISAPPEARANCE_RATIONALE: If the principle of universal due process vanished overnight, state power would become arbitrary, individual liberties would collapse, and the legal system would lose its legitimacy, leading to widespread social and political instability as fundamental protections are removed.
% FOUNDING_PROBLEM: To limit the arbitrary power of the monarch (King John) and establish a framework for lawful governance and protection of subjects from unlawful seizure or punishment, particularly in the context of feudal obligations.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, human rights organizations, and international bodies consistently attest to the ongoing relevance of due process in limiting state power and protecting individual rights, citing its continuous invocation in constitutional and human rights law globally. This corroboration comes from outside the immediate beneficiaries of state power.
narrative_ontology:disappearance_verdict(magna_carta_1215__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__universal_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(magna_carta_1215__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__universal_rights_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__universal_rights_reading_tests).
:- end_tests(magna_carta_1215__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (establishing a common standard for justice and limiting state power) but also involves asymmetric extraction: it extracts compliance from state actors who would otherwise act arbitrarily, making them 'victims' of its enforcement. Its base extractiveness is moderate (0.4) reflecting the ongoing cost of state compliance and the severity of violations when it fails. Suppression is low (0.15) because the constraint itself *reduces* state suppression, though it requires active enforcement against state attempts to bypass it. Resistance is high (0.7) due to continuous challenges from state actors seeking to expand their power. The historical measurements reflect a gradual increase in the constraint's 'extractiveness' (cost of compliance for the state) and 'suppression_requirement' (effort needed to enforce it) as its scope broadened and state power grew, alongside a moderate, but present, 'theater_ratio' as it became a rhetorical symbol.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individuals and the judiciary, this constraint is a vital protection and a source of legitimacy. From the perspective of state actors seeking to bypass it, it is an impediment and a cost. The engine computes this divergence from the structural data, showing how the same constraint is experienced differently across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   All persons, the judiciary, and the legal profession are structural beneficiaries, gaining protection and legitimacy from the constraint. State actors seeking arbitrary power and executive branch overreach are the primary targets/payers, as the constraint directly limits their actions. The legislature holds a dual role, both upholding and potentially challenging the constraint's scope. Historians and scholars are analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_free_men,
    'Is the term ''free men'' in Magna Carta''s Clause 39 to be interpreted strictly as feudal barons, or expansively as all persons?',
    'Analysis of historical legal reception, subsequent constitutional documents (e.g., US Bill of Rights, UDHR), and contemporary judicial precedent that explicitly extends its principles.',
    'If strictly interpreted, the constraint''s beneficiary set shrinks dramatically, and its classification might shift towards a Piton or even a historical artifact. If expansively interpreted (as in this reading), its universal scope and protective function are affirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_free_men, conceptual, 'Ambiguity regarding the original and evolving scope of ''free men'' in Magna Carta.').

omega_variable(
    magna_carta_nature,
    'Is Magna Carta fundamentally a specific feudal contract, or a foundational constitutional document embodying enduring principles?',
    'Comparative constitutional law analysis, examination of its influence on subsequent legal systems, and philosophical inquiry into the nature of constitutionalism.',
    'If primarily a feudal contract, its transhistorical relevance diminishes, potentially weakening its classification as an active constraint. If a foundational constitutional document, its status as a living precedent is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magna_carta_nature, conceptual, 'Debate over Magna Carta''s primary nature: specific contract vs. constitutional foundation.').

omega_variable(
    source_of_authority,
    'Does the authority of Magna Carta''s principles derive from its original intent (originalism) or from evolving interpretive tradition and precedential accumulation (living constitutionalism)?',
    'Judicial philosophy debates, legal scholarship on constitutional interpretation, and the actual practice of courts in applying historical texts.',
    'This reading emphasizes a transhistorical principle, which aligns more with an evolving interpretation that finds universalism in its core. A strict originalist view might limit its application to its historical context, reducing its contemporary scope and impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(source_of_authority, preference, 'Original intent vs. evolving interpretation as the source of Magna Carta''s authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 1215, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__universal_rights_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1688, magna_carta_1215__universal_rights_reading, theater_ratio, 1688, 0.2).
narrative_ontology:measurement(magn_tr_t1789, magna_carta_1215__universal_rights_reading, theater_ratio, 1789, 0.25).
narrative_ontology:measurement(magn_tr_t1948, magna_carta_1215__universal_rights_reading, theater_ratio, 1948, 0.28).
narrative_ontology:measurement(magn_tr_t2023, magna_carta_1215__universal_rights_reading, theater_ratio, 2023, 0.3).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__universal_rights_reading, base_extractiveness, 1215, 0.1).
narrative_ontology:measurement(magn_be_t1688, magna_carta_1215__universal_rights_reading, base_extractiveness, 1688, 0.2).
narrative_ontology:measurement(magn_be_t1789, magna_carta_1215__universal_rights_reading, base_extractiveness, 1789, 0.3).
narrative_ontology:measurement(magn_be_t1948, magna_carta_1215__universal_rights_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(magn_be_t2023, magna_carta_1215__universal_rights_reading, base_extractiveness, 2023, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__universal_rights_reading, suppression_requirement, 1215, 0.05).
narrative_ontology:measurement(magn_su_t1688, magna_carta_1215__universal_rights_reading, suppression_requirement, 1688, 0.1).
narrative_ontology:measurement(magn_su_t1789, magna_carta_1215__universal_rights_reading, suppression_requirement, 1789, 0.12).
narrative_ontology:measurement(magn_su_t1948, magna_carta_1215__universal_rights_reading, suppression_requirement, 1948, 0.14).
narrative_ontology:measurement(magn_su_t2023, magna_carta_1215__universal_rights_reading, suppression_requirement, 2023, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
