% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__principle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__principle_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gelassenheit_separation__principle_reading
 *   human_readable: Amish Separation Principle — Structural Entanglement Avoidance Reading
 *   domain: religious/technology_governance/commitment_systems
 *
 * SUMMARY:
 *   The principle_reading of Amish separation (Gelassenheit) treats the
 *   avoidance of structural entanglement in worldly systems as the core
 *   criterion. Technology is evaluated by whether it creates functional
 *   dependency on outside infrastructure — solar panels and pneumatic tools
 *   are permitted when genuinely off-grid because they do not tie the
 *   household to utility grids, financial systems, or state services.
 *   Internet access and insurance are categorically forbidden regardless of
 *   technical isolation because they are seen as inherently connective to
 *   worldly systems. This reading produces lower base extractiveness (ε≈0.42)
 *   and moderate suppression (0.38) compared to the artifact_reading, because
 *   the functional criterion allows genuine technological adaptation.
 *   However, the absolute bans on internet and insurance create a
 *   non-negotiable core that prevents the constraint from being a pure rope.
 *   The constraint coordinates community boundary maintenance while
 *   extracting compliance from marginal members and youth — a tangled_rope
 *   structure.
 *
 * KEY AGENTS:
 *   - ordained_ministry: agenda_setter (institutional/biographical/identity_locked/regional) — interprets and enforces the Ordnung
 *   - community_elders: beneficiary (organized/generational/identity_locked/local) — maintain social position through boundary enforcement
 *   - baptized_members: beneficiary (organized/biographical/identity_locked/local) — receive community mutual aid and identity
 *   - marginal_members: payer (moderate/biographical/constrained/local) — bear compliance costs without full voice
 *   - youth_in_rumspringa: payer (powerless/immediate/trapped/local) — face baptism decision under constraint pressure
 *   - external_dependents: excluded (powerless/biographical/trapped/regional) — non-members dependent on community (e.g., non-Amish spouses, children)
 *   - scholar_observer: observer (analytical/civilizational/analytical/universal) — studies the constraint from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, 0.42).
domain_priors:suppression_score(gelassenheit_separation__principle_reading, 0.38).
domain_priors:theater_ratio(gelassenheit_separation__principle_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__principle_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__principle_reading, "Amish Separation Principle — Structural Entanglement Avoidance Reading").
narrative_ontology:topic_domain(gelassenheit_separation__principle_reading, "religious/technology_governance/commitment_systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__principle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__principle_reading, 'ee6e784b-929b-469f-803d-16094d7c68d0').
narrative_ontology:cs_kernel_codification('ee6e784b-929b-469f-803d-16094d7c68d0', distributed).
narrative_ontology:cs_authority_grounding('ee6e784b-929b-469f-803d-16094d7c68d0', practice).
narrative_ontology:cs_interpretation_layer_present('ee6e784b-929b-469f-803d-16094d7c68d0').
narrative_ontology:cs_reading_relation('ee6e784b-929b-469f-803d-16094d7c68d0', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('ee6e784b-929b-469f-803d-16094d7c68d0', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('ee6e784b-929b-469f-803d-16094d7c68d0', foundational, structural_nonentanglement_as_separation_criterion).
narrative_ontology:cs_axiom_status(structural_nonentanglement_as_separation_criterion, holdable).
narrative_ontology:cs_axiom_grounding('ee6e784b-929b-469f-803d-16094d7c68d0', structural_nonentanglement_as_separation_criterion, deontological).
narrative_ontology:cs_axiom('ee6e784b-929b-469f-803d-16094d7c68d0', foundational, functional_isolation_suffices_for_permissibility).
narrative_ontology:cs_axiom_status(functional_isolation_suffices_for_permissibility, holdable).
narrative_ontology:cs_axiom_grounding('ee6e784b-929b-469f-803d-16094d7c68d0', functional_isolation_suffices_for_permissibility, instrumental).
narrative_ontology:cs_axiom('ee6e784b-929b-469f-803d-16094d7c68d0', secondary, internet_insurance_inherently_entangle).
narrative_ontology:cs_axiom_status(internet_insurance_inherently_entangle, holdable).
narrative_ontology:cs_axiom_grounding('ee6e784b-929b-469f-803d-16094d7c68d0', internet_insurance_inherently_entangle, deontological).
narrative_ontology:cs_reference_frame('ee6e784b-929b-469f-803d-16094d7c68d0', gelassenheit_ordnung_1950).
narrative_ontology:cs_drift_state('ee6e784b-929b-469f-803d-16094d7c68d0', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ee6e784b-929b-469f-803d-16094d7c68d0', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__principle_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, ordained_ministry).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, community_elders).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, baptized_members).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, marginal_members).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, youth_in_rumspringa).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, external_dependents).
narrative_ontology:constraint_vindicates(gelassenheit_separation__principle_reading, gelassenheit_as_structural_nonentanglement).
narrative_ontology:constraint_vindicates(gelassenheit_separation__principle_reading, functional_isolation_as_sufficient_criterion).
narrative_ontology:constraint_vindicates(gelassenheit_separation__principle_reading, off_grid_energy_permissibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The bishop, ministers, and deacons collectively interpret the Ordnung for their district. They rule on technology requests (solar installations, pneumatic tools, phone shanties) by assessing whether the technology creates structural entanglement. Their authority derives from ordination lottery and communal recognition. They do not personally profit but their role's legitimacy depends on the separation constraint's vitality. Exit is identity-locked — leaving ministry means leaving the community's epistemic framework.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, ordained_ministry, agenda_setter,
    institutional, generational, identity_locked, regional).

% Senior baptized members (often former ministers) whose social authority and family cohesion depend on the community's distinctiveness. They benefit from the mutual aid system (barn raisings, medical cost sharing, elder care) that the separation boundary protects. They advocate for strictness in edge cases to preserve the boundary. Their exit is identity-locked — their life narrative is constituted by the community.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, community_elders, beneficiary,
    organized, generational, identity_locked, local).

% Adult members who have chosen baptism. They receive the full mutual aid safety net, shared labor pools, and a coherent lifeworld. They accept the technology restrictions as the price of membership. Most experience the constraint as net beneficial — the functional isolation criterion allows enough adaptation (solar for refrigeration, pneumatics for workshops) to avoid material crisis. Exit is identity-locked: leaving means losing the entire social world.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, baptized_members, beneficiary,
    organized, biographical, identity_locked, local).

% Members who need forbidden technologies for livelihood or health — e.g., a carpenter needing internet for CAD files, a family needing insurance for a chronic condition. They petition the ministry for exceptions, often denied under principle_reading's absolute bans. They comply but experience the constraint as extraction: they pay the cost of technological abstinence without the full benefit of mutual aid (which may not cover their specific needs). Exit is constrained — they could leave but face economic and relational rupture.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, marginal_members, payer,
    moderate, biographical, constrained, local).

% Adolescents (typically 16–22) in the pre-baptism exploration period. They experience the constraint's full force without having consented to it. The principle_reading's internet ban is acutely felt — they cannot develop digital skills for outside employment. The baptism decision is structurally pressured: return and submit, or leave with minimal education, no credentials, and severed family ties. Exit is trapped during rumspringa; becomes identity_locked if they baptize, constrained if they leave.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, youth_in_rumspringa, payer,
    powerless, immediate, trapped, local).

% Non-members whose lives are structured by the constraint: non-Amish spouses of marginal members, children of leavers, business partners. They have no voice in Ordnung revisions but bear spillover costs (e.g., a spouse cannot obtain family health insurance because the member refuses it). They would object to the absolute internet/insurance bans if consulted. Exit is trapped — they cannot change the constraint and leaving the relationship is often impractical.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, external_dependents, excluded,
    powerless, biographical, trapped, regional).

% Academic researchers of Amish technology governance, religious studies scholars, legal analysts of religious exemption regimes. They observe the constraint from outside, tracking its adaptation, enforcement, and effects on members and non-members. They do not bear costs or collect benefits from the constraint's operation.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, scholar_observer, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(gelassenheit_separation__principle_reading, scholar_observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a coherent communal lifeworld (Gelassenheit) by preventing structural dependencies on state, market, and technological systems that would fracture yieldedness to the community. Solves the coordination problem of collective boundary maintenance without centralized policing — each household's technology choices are visible and mutually accountable.
% TRANSFER_FUNCTION: Moves technological autonomy and risk-bearing capacity from individual members (especially marginal members and youth) to the collective boundary-maintenance system. The community gains coherence and mutual aid viability; members forgo specific capabilities (internet, insurance, grid power) that would create outside entanglements.
% ABSENT_VOICES: Youth before baptism (no vote on Ordnung), marginal members whose petitions are denied, external dependents (non-member spouses, children of leavers), and non-Amish institutions (hospitals, insurers, employers) that interact with the community on constrained terms. They are absent because the constraint's authority structure recognizes only baptized members as legitimate participants.
% DISAPPEARANCE_RATIONALE: If the principle_reading vanished overnight, districts would immediately adopt internet for commerce and education, insurance for medical risk, and grid connections for convenience. The mutual aid system would collapse within a generation as the material basis for distinctiveness erodes. Baptism rates would plummet. The community would reorganize into a mainstream Anabaptist denomination or dissolve.
% FOUNDING_PROBLEM: Preserving Gelassenheit (yieldedness to God's will as mediated through the community) against the fragmenting forces of industrial modernity — wage labor, state bureaucracy, market dependency, and technological mediation of daily life.
% FOUNDING_PROBLEM_CORROBORATION: The ordained ministry attests the problem is live — modernity's fragmentation has accelerated (smartphones, gig economy, surveillance capitalism). Sociologists of religion (Kraybill, Nolt, Johnson-Weiner) attest the founding problem is substantially solved in its original form (the community has survived) but the principle_reading's functional criterion is a novel adaptation, not the original solution. Former members attest the problem is dead for them — they experienced the constraint as extraction, not coordination.
narrative_ontology:disappearance_verdict(gelassenheit_separation__principle_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__principle_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__principle_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gelassenheit_separation__principle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__principle_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__principle_reading_tests).
:- end_tests(gelassenheit_separation__principle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the functional isolation criterion permits real technological adoption (solar, pneumatics) that reduces material hardship — the constraint is not purely extractive. Suppression is moderate (0.38) because enforcement operates through shunning and baptism pressure rather than physical coercion, but identity_locked exit makes resistance costly. Theater ratio is low (0.18) — the separation practice is genuinely lived, not performative. Accessibility collapse (0.62) reflects that alternatives (leaving the community) exist but are existentially costly due to identity fusion. Resistance (0.45) is present but channeled through rumspringa and quiet non-compliance rather than open challenge.
 *
 * PERSPECTIVAL GAP:
 *   The ordained ministry (agenda_setter) experiences this as coordination — they maintain the community's distinct witness. Baptized members (beneficiary) experience it as net positive — mutual aid, identity, reduced modernity anxiety. Marginal members and youth (payers) experience it as extraction — they bear the costs of forbidden technologies (no internet for education/business, no insurance for risk pooling) without full consent. The engine computes per-seat χ from these structural positions; the divergence between agenda_setter (low χ) and payer (high χ) is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: ordained_ministry (controls interpretation, status from enforcement), community_elders (social capital from boundary maintenance), baptized_members (mutual aid, identity, reduced decision burden). Victims: marginal_members (compliance without voice), youth_in_rumspringa (facing baptism under structural pressure), external_dependents (subject to community rules without membership). The absolute internet/insurance ban raises directionality for payers — these are not negotiable even when functionally isolatable, making the constraint more extractive for those who would use them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving Gelassenheit — yieldedness to God/community — against modernity's fragmentation) remains live (contested status). The principle_reading adapts the means (functional isolation criterion) while holding the end. This prevents mandatrophy: the constraint still solves a live coordination problem (boundary maintenance without total technological freeze). The artifact_reading shows stronger mandatrophy signals (frozen artifact criteria, rising theater).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the principle_reading instantiate a distinct constraint from artifact_reading and consequence_reading, or are they measurement perspectives on one constraint?',
    'Compare ε values and beneficiary/victim structures across the three readings. If ε differs by >0.15 or beneficiary sets diverge, they are distinct constraints per ε-invariance.',
    'If distinct, each reading gets its own classification and the kernel is a family. If not, the kernel contest is perspectival, not structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three separation readings are structurally distinct constraints or perspectival variants').

omega_variable(
    functional_isolation_boundary,
    'Where does ''functional isolation'' end and ''structural entanglement'' begin for off-grid technologies?',
    'Case-level analysis of edge technologies: solar with grid-tie capability, pneumatic tools with electric compressors, battery systems with utility charging. Track ordinal rulings over time.',
    'A sharp boundary supports low ε (coordination); a porous boundary with case-by-case negotiation raises ε and suppression (extraction via interpretive discretion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_isolation_boundary, empirical, 'Precision of the functional isolation criterion in practice').

omega_variable(
    internet_insurance_absolute_ban,
    'Why does the principle_reading forbid internet and insurance regardless of isolation possibility, when its own criterion is functional isolation?',
    'Trace the doctrinal genealogy: is this an axiom (separation_from_worldly_systems) that overrides the functional criterion, or a pragmatic judgment about those technologies'' inherent connectivity?',
    'If axiomatic, the constraint has a non-negotiable core (higher suppression). If pragmatic, the ban could lift with technical change (lower suppression, scaffold-like).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(internet_insurance_absolute_ban, conceptual, 'Structural basis for the internet/insurance prohibition under the principle_reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__principle_reading, 1950, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gel_sep_princ_tr_t1950, gelassenheit_separation__principle_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(gel_sep_princ_tr_t1970, gelassenheit_separation__principle_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(gel_sep_princ_tr_t1990, gelassenheit_separation__principle_reading, theater_ratio, 1990, 0.13).
narrative_ontology:measurement(gel_sep_princ_tr_t2010, gelassenheit_separation__principle_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(gel_sep_princ_tr_t2020, gelassenheit_separation__principle_reading, theater_ratio, 2020, 0.17).
narrative_ontology:measurement(gel_sep_princ_tr_t2030, gelassenheit_separation__principle_reading, theater_ratio, 2030, 0.18).

% Extraction over time
narrative_ontology:measurement(gel_sep_princ_be_t1950, gelassenheit_separation__principle_reading, base_extractiveness, 1950, 0.28).
narrative_ontology:measurement(gel_sep_princ_be_t1970, gelassenheit_separation__principle_reading, base_extractiveness, 1970, 0.31).
narrative_ontology:measurement(gel_sep_princ_be_t1990, gelassenheit_separation__principle_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(gel_sep_princ_be_t2010, gelassenheit_separation__principle_reading, base_extractiveness, 2010, 0.39).
narrative_ontology:measurement(gel_sep_princ_be_t2020, gelassenheit_separation__principle_reading, base_extractiveness, 2020, 0.41).
narrative_ontology:measurement(gel_sep_princ_be_t2030, gelassenheit_separation__principle_reading, base_extractiveness, 2030, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gel_sep_princ_su_t1950, gelassenheit_separation__principle_reading, suppression_requirement, 1950, 0.25).
narrative_ontology:measurement(gel_sep_princ_su_t1970, gelassenheit_separation__principle_reading, suppression_requirement, 1970, 0.28).
narrative_ontology:measurement(gel_sep_princ_su_t1990, gelassenheit_separation__principle_reading, suppression_requirement, 1990, 0.32).
narrative_ontology:measurement(gel_sep_princ_su_t2010, gelassenheit_separation__principle_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(gel_sep_princ_su_t2020, gelassenheit_separation__principle_reading, suppression_requirement, 2020, 0.37).
narrative_ontology:measurement(gel_sep_princ_su_t2030, gelassenheit_separation__principle_reading, suppression_requirement, 2030, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__principle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__principle_reading, 0.08).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__consequence_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, amish_baptism_requirement).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, amish_shunning_practice).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, amish_mutual_aid_system).

% DUAL FORMULATION NOTE:
% The gelassenheit_separation kernel decomposes into three structurally distinct constraint stories (principle, artifact, consequence readings) linked by affects_constraints. They share the kernel_id but have divergent ε, beneficiary/victim structures, and suppression profiles. The principle_reading has the lowest ε (0.42) and most permissive technology criterion; artifact_reading has higher ε and suppression due to visual conformity enforcement; consequence_reading sits between with practice-preservation criterion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gelassenheit_separation__principle_reading, organized, 0.15).
constraint_indexing:directionality_override(gelassenheit_separation__principle_reading, moderate, 0.75).
constraint_indexing:directionality_override(gelassenheit_separation__principle_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
