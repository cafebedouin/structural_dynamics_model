% ============================================================================
% CONSTRAINT STORY: preparedness_retention__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__husk_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: preparedness_retention__husk_reading
 *   human_readable: Preparedness as Memorial Performance (Husk Reading)
 *   domain: governance/disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   The Dutch disaster preparedness system has evolved from a Cold War civil
 *   defense apparatus into a national compliance regime. The 'husk reading'
 *   describes the current state: drills, inspections, and reporting rituals
 *   consume resources and produce a sense of readiness, but the tacit
 *   competence required for actual D5 (catastrophic, society-disrupting)
 *   events has atrophied. The constraint is the nationally mandated
 *   drill/inspection cycle — not preparedness per se, but the specific
 *   institutional form it has taken. Beneficiaries are the agencies that
 *   administer it and the political executives who consume its visibility.
 *   Victims are the frontline responders whose skill maintenance is crowded
 *   out, the populations who bear the failure cost, and the specialized
 *   institutions (water boards, Rijkswaterstaat) whose operational competence
 *   is diluted by performative compliance demands.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__husk_reading, 0.78).
domain_priors:suppression_score(preparedness_retention__husk_reading, 0.72).
domain_priors:theater_ratio(preparedness_retention__husk_reading, 0.84).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, theater_ratio, 0.84).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__husk_reading, piton).
narrative_ontology:human_readable(preparedness_retention__husk_reading, "Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_retention__husk_reading, "governance/disaster_preparedness/institutional_memory").

domain_priors:requires_active_enforcement(preparedness_retention__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__husk_reading, 'e962ac22-268b-4e12-a10b-b51b60ed3ded').
narrative_ontology:cs_kernel_codification('e962ac22-268b-4e12-a10b-b51b60ed3ded', formalized).
narrative_ontology:cs_authority_grounding('e962ac22-268b-4e12-a10b-b51b60ed3ded', extraction).
narrative_ontology:cs_interpretation_layer_present('e962ac22-268b-4e12-a10b-b51b60ed3ded').
narrative_ontology:cs_reading_relation('e962ac22-268b-4e12-a10b-b51b60ed3ded', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('e962ac22-268b-4e12-a10b-b51b60ed3ded', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('e962ac22-268b-4e12-a10b-b51b60ed3ded', foundational, drill_compliance_is_not_competence).
narrative_ontology:cs_axiom_status(drill_compliance_is_not_competence, holdable).
narrative_ontology:cs_axiom_grounding('e962ac22-268b-4e12-a10b-b51b60ed3ded', drill_compliance_is_not_competence, empirically_contingent).
narrative_ontology:cs_axiom('e962ac22-268b-4e12-a10b-b51b60ed3ded', foundational, institutional_legitimacy_extracts_from_operational_capacity).
narrative_ontology:cs_axiom_status(institutional_legitimacy_extracts_from_operational_capacity, holdable).
narrative_ontology:cs_axiom_grounding('e962ac22-268b-4e12-a10b-b51b60ed3ded', institutional_legitimacy_extracts_from_operational_capacity, instrumental).
narrative_ontology:cs_reference_frame('e962ac22-268b-4e12-a10b-b51b60ed3ded', post_1953_national_interoperability_mandate).
narrative_ontology:cs_drift_state('e962ac22-268b-4e12-a10b-b51b60ed3ded', post_2021_limburg_floods, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e962ac22-268b-4e12-a10b-b51b60ed3ded', '').
narrative_ontology:cs_kernel_id(preparedness_retention__husk_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, political_executives).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, audit_inspection_bodies).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, affected_populations).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, specialized_technical_institutions).
narrative_ontology:constraint_vindicates(preparedness_retention__husk_reading, institutional_legitimacy_through_visibility).
narrative_ontology:constraint_vindicates(preparedness_retention__husk_reading, compliance_as_substitute_for_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and mandate drill schedules, inspection regimes, and reporting formats. Their budgets and institutional standing depend on demonstrable compliance activity — after-action reports, exercise participation rates, checklist completion. They control the definition of what counts as 'prepared' and direct resources toward visible outputs rather than tacit skill maintenance. When a D5 event occurs, they coordinate the response but do not execute frontline operations.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__husk_reading, emergency_management_agencies, beneficiary).

% Receive public credit for 'investing in preparedness' through announced drill cycles, new inspection protocols, and funding allocations that produce photo-ops and press releases. The electoral cycle rewards visible action over latent competence. They face no direct consequence when exercises prove hollow — the performance itself is the political product. Exit is irrelevant; they are the audience the performance is staged for.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, political_executives, beneficiary,
    institutional, biographical, arbitrage, national).

% Conduct compliance inspections against standardized checklists. Their mandate, funding, and professional relevance derive from the existence of inspectable artifacts — plans, logs, certificates, exercise records. They have no mandate or capability to assess whether the inspected organization can actually perform under D5 conditions. Their professional identity is bound to the inspection ritual itself.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, audit_inspection_bodies, beneficiary,
    organized, biographical, mobile, national).

% Participate in mandated drills that consume training time without building the judgment, coordination, or physical conditioning needed for actual events. They know the drills are performative but cannot refuse without career penalty. Their tacit knowledge — radio discipline under stress, triage intuition, improvisation when plans fail — atrophies because the system rewards checklist compliance, not skill retention. Exit means leaving the profession; identity is fused with the role.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, frontline_responders, payer,
    organized, biographical, identity_locked, regional).

% Bear the consequence when the performance fails — delayed evacuation, collapsed communications, absent logistics, improvisation by untrained personnel. They have no voice in drill design, no visibility into the gap between exercise and reality, and no exit during the event. Their trust in institutions is the resource the performance consumes.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, affected_populations, payer,
    powerless, immediate, trapped, local).

% Entities like Rijkswaterstaat and water boards that retain genuine technical competence through continuous operational demand (flood defense, infrastructure management). They are compelled to participate in the national drill/inspection theater, diverting scarce expert time to performative compliance. Their actual competence is not improved by the rituals; their distinct operational culture is flattened into the national compliance framework. They cannot fully exit the national system but maintain parallel competence-preserving practices.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, specialized_technical_institutions, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__husk_reading, specialized_technical_institutions, excluded).

% Study the gap between exercise performance and event outcomes. They document the atrophy of tacit knowledge, the misallocation of resources toward visibility, and the systematic overconfidence produced by successful drills. Their work is cited in after-action reports but rarely changes the incentive structure that produces the husk.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, disaster_researchers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a nationally legible framework for allocating preparedness resources, assigning responsibilities, and establishing a common language for inter-agency coordination during events. The drill cycle creates scheduled touchpoints where organizations must communicate.
% TRANSFER_FUNCTION: Moves budget, personnel time, and political attention from frontline skill maintenance (scenario-based training, equipment familiarity, cross-team rehearsal under stress) to the production of inspectable artifacts (plans, logs, certificates, after-action reports). Transfers legitimacy from demonstrated competence to demonstrated compliance.
% ABSENT_VOICES: Frontline responders and affected populations are structurally excluded from drill design and inspection criteria. Specialized technical institutions (water boards, Rijkswaterstaat) are present in the compliance theater but their operational wisdom is not integrated into the national framework — they are compelled to perform compliance rather than teach competence.
% DISAPPEARANCE_RATIONALE: If the national drill/inspection mandate vanished overnight, emergency management agencies would lose their primary legitimacy instrument and budget justification. Political executives would lose a visible 'preparedness' product. Frontline responders would reclaim training time for actual skill work. Specialized institutions would continue their operational practices unchanged. The coordination vocabulary would persist informally. The ritual shell would collapse; the competence substrate (where it exists) would survive.
% FOUNDING_PROBLEM: After the 1953 North Sea flood and subsequent Cold War civil defense imperatives, the Netherlands needed a nationally coordinated system to ensure that disparate water boards, municipalities, and response organizations could operate together during catastrophic flooding. The drill/inspection regime was built to create shared procedures, interoperable communications, and verified readiness across a fragmented institutional landscape.
% FOUNDING_PROBLEM_CORROBORATION: Water board historians and Rijkswaterstaat institutional memory attest that the original interoperability problem was substantially solved by the 1990s through standardized communications protocols, joint command structures, and integrated hydraulic modeling — all driven by operational necessity, not drill compliance. The 1953 founding problem is dead; the regime persists as its own justification. The Dutch Safety Board (Onderzoeksraad voor Veiligheid) has repeatedly documented the ceremony-competence gap in post-event investigations (e.g., 2021 Limburg floods, 2022 Rotterdam harbor exercise series) — corroboration from outside the beneficiary set.
narrative_ontology:disappearance_verdict(preparedness_retention__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(preparedness_retention__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__husk_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   High theater_ratio (0.84) reflects that the overwhelming majority of drill/inspection activity is performative — designed to produce inspectable artifacts, not stress-tested competence. Extractiveness (0.78) is high because the regime extracts frontline training time, expert attention, and budget toward compliance production while delivering diminishing actual readiness. Suppression (0.72) is substantial: the mandate is enforced through budget conditionality, legal liability frameworks, and professional discipline — frontline responders cannot opt out, specialized institutions cannot decline participation. Accessibility_collapse (0.38) is moderate: alternative preparedness models exist (the water boards' operational culture, community-based resilience initiatives) but they are marginalized by the national framework's resource capture. Resistance (0.55) is significant but fragmented: frontline unions critique drill quality, researchers publish the gap, water boards negotiate exemptions — but no coalition has forced structural reform.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (emergency management agencies), the constraint looks like a coordination scaffold — it creates national interoperability, shared terminology, and scheduled rehearsal. From the payer seats (frontline responders, affected populations), it operates as a piton — a degraded coordination mechanism maintained by institutional inertia, extracting resources while delivering declining functional value. The specialized institutions occupy a unique dual position: they experience the national regime as extraction (payer) while their own operational practices constitute a genuine coordination rope (excluded from the national frame). The engine computes this divergence from the declared power/exit/role structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Emergency management agencies and political executives sit at the beneficiary end (d ~ 0.15-0.25): they control the constraint, collect its legitimacy rents, and face no competence consequences. Audit bodies are beneficiaries (d ~ 0.2) — their professional existence depends on the inspectable regime. Frontline responders are identity-locked targets (d ~ 0.85): they bear the time cost, know the rituals are hollow, but cannot exit without abandoning professional identity. Affected populations are trapped targets (d ~ 0.95): zero voice, zero exit, full consequence. Specialized technical institutions are constrained payers with partial exit (d ~ 0.65): they maintain parallel competence but must divert resources to the national theater. The engine will compute per-seat types from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (national interoperability for catastrophic flood response) was substantially solved by the 1990s through operational integration, not drill compliance. The drill/inspection regime solved a 1950s-1980s problem; it now persists as a piton — its original coordination function atrophied, maintained by the legitimacy rents it generates for administering agencies and political executives. The mandate has outlived its function: base_properties.mandatrophy_resolved should be true (the mandate is acknowledged as misaligned), yet the constraint persists with rising theater_ratio and extractiveness. This is the mandatrophy signature: a dead founding problem, a living constraint, beneficiaries who defend the shell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_identity,
    'This constraint is one reading (husk_reading) of the contested kernel ''preparedness_retention''. What structural elements distinguish this reading from its siblings (competence_reading, hybrid_reading)?',
    'Decompose the kernel into its constituent constraints per the ε-invariance principle. Each reading gets its own ε, beneficiaries, victims, and type. The husk_reading declares high ceremony-to-competence ratio, compliance-favoring resource allocation, institutional legitimacy as beneficiary, D5 response capacity as victim. The competence_reading would declare low theater_ratio, skill-preserving drills, frontline responders as beneficiaries. The hybrid_reading would declare stratified beneficiaries/victims across institutional tiers.',
    'If the kernel is treated as a single constraint with variable measurement, ε becomes observer-relative and classification becomes unstable. Decomposition into three constraint stories linked by network.affects_constraints preserves ε-invariance and makes the structural disagreement explicit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_identity, conceptual, 'Kernel/reading decomposition: this story is one reading of a contested kernel, not a standalone constraint').

omega_variable(
    ceremony_competence_boundary,
    'At what point does a drill cross from competence-preserving to purely performative? Is there a measurable threshold in stress fidelity, decision autonomy, or consequence realism?',
    'Comparative analysis of drill designs that produce measurable skill retention vs. those that produce only checklist compliance. Track skill decay curves for responders exposed to different drill types. The Dutch Safety Board''s exercise evaluations (e.g., 2022 Rotterdam harbor series) provide a starting dataset.',
    'If a structural boundary exists, the constraint could be partially reformed — high-fidelity drills preserved, low-fidelity rituals eliminated. If the boundary is continuous or context-dependent, the entire national regime may be structurally extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremony_competence_boundary, empirical, 'Whether the ceremony/competence distinction is a sharp structural boundary or a gradient').

omega_variable(
    specialized_institution_dual_position,
    'Do specialized technical institutions (water boards, Rijkswaterstaat) genuinely retain competence through operational demand, or is their ''competence'' also partly performative — maintained by the same institutional incentives that produce the national theater?',
    'Compare their event performance (1995, 1998, 2021 floods) against their drill performance. Assess whether their operational culture is sustained by continuous hydraulic management (genuine coordination) or by the same compliance logic that drives the national regime. Interview retired senior engineers on skill transmission pathways.',
    'If their competence is also partly performative, the hybrid_reading collapses toward the husk_reading — the stratification is theater all the way down. If genuinely operational, the hybrid_reading identifies a real competence reservoir that the national regime parasitizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specialized_institution_dual_position, empirical, 'Whether the specialized institutions'' competence is genuine operational retention or also partly performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__husk_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_ret_husk_tr_t1995, preparedness_retention__husk_reading, theater_ratio, 1995, 0.42).
narrative_ontology:measurement(prep_ret_husk_tr_t2000, preparedness_retention__husk_reading, theater_ratio, 2000, 0.51).
narrative_ontology:measurement(prep_ret_husk_tr_t2005, preparedness_retention__husk_reading, theater_ratio, 2005, 0.58).
narrative_ontology:measurement(prep_ret_husk_tr_t2010, preparedness_retention__husk_reading, theater_ratio, 2010, 0.66).
narrative_ontology:measurement(prep_ret_husk_tr_t2015, preparedness_retention__husk_reading, theater_ratio, 2015, 0.73).
narrative_ontology:measurement(prep_ret_husk_tr_t2020, preparedness_retention__husk_reading, theater_ratio, 2020, 0.8).
narrative_ontology:measurement(prep_ret_husk_tr_t2025, preparedness_retention__husk_reading, theater_ratio, 2025, 0.84).

% Extraction over time
narrative_ontology:measurement(prep_ret_husk_be_t1995, preparedness_retention__husk_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(prep_ret_husk_be_t2000, preparedness_retention__husk_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(prep_ret_husk_be_t2005, preparedness_retention__husk_reading, base_extractiveness, 2005, 0.59).
narrative_ontology:measurement(prep_ret_husk_be_t2010, preparedness_retention__husk_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(prep_ret_husk_be_t2015, preparedness_retention__husk_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement(prep_ret_husk_be_t2020, preparedness_retention__husk_reading, base_extractiveness, 2020, 0.75).
narrative_ontology:measurement(prep_ret_husk_be_t2025, preparedness_retention__husk_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(prep_ret_husk_su_t1995, preparedness_retention__husk_reading, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement(prep_ret_husk_su_t2000, preparedness_retention__husk_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(prep_ret_husk_su_t2005, preparedness_retention__husk_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(prep_ret_husk_su_t2010, preparedness_retention__husk_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(prep_ret_husk_su_t2015, preparedness_retention__husk_reading, suppression_requirement, 2015, 0.64).
narrative_ontology:measurement(prep_ret_husk_su_t2020, preparedness_retention__husk_reading, suppression_requirement, 2020, 0.69).
narrative_ontology:measurement(prep_ret_husk_su_t2025, preparedness_retention__husk_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__hybrid_reading).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, national_crisis_management_framework).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, water_board_operational_autonomy).

% DUAL FORMULATION NOTE:
% This constraint (husk_reading) and its siblings (competence_reading, hybrid_reading) form the preparedness_retention constraint family. Each reading instantiates a different constraint from the same kernel: competence_reading claims the drill regime is a rope (genuine coordination), husk_reading claims it is a piton (atrophied coordination maintained by legitimacy rents), hybrid_reading claims a stratified system with rope-like specialized institutions and piton-like national regime. The ε values differ substantially: competence_reading ε ≈ 0.25, husk_reading ε ≈ 0.78, hybrid_reading ε stratified. They are linked by network.affects_constraints and share the kernel_id in their provenance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_retention__husk_reading, organized, 0.85).
constraint_indexing:directionality_override(preparedness_retention__husk_reading, powerless, 0.95).
constraint_indexing:directionality_override(preparedness_retention__husk_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
