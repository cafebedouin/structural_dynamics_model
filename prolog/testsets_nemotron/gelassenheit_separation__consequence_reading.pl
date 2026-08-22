% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__consequence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__consequence_reading, []).

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
 *   constraint_id: gelassenheit_separation__consequence_reading
 *   human_readable: Gelassenheit Separation — Consequence Reading: Technology Evaluated by Effect on Visiting, Mutual Aid, Geographic Rootedness
 *   domain: religious_studies/technology_governance/commitment_systems
 *
 * SUMMARY:
 *   The consequence reading of Gelassenheit separation evaluates technology
 *   by its effect on the community's core practices: visiting (daily
 *   face-to-face interaction), mutual aid (neighbors helping neighbors with
 *   harvests, building, crises), and geographic rootedness (staying on the
 *   land, living near kin). This reading permits telephones in barns for
 *   coordinating work and emergencies but forbids them in homes where they
 *   would displace visiting. It allows tractors for stationary belt power
 *   (threshing, sawing) but not for transportation. The constraint is a
 *   fine-grained, context-sensitive discernment framework administered by the
 *   bishop council — low extraction overall but with concentrated costs on
 *   specific member subgroups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__consequence_reading, 0.22).
domain_priors:suppression_score(gelassenheit_separation__consequence_reading, 0.45).
domain_priors:theater_ratio(gelassenheit_separation__consequence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__consequence_reading, rope).
narrative_ontology:human_readable(gelassenheit_separation__consequence_reading, "Gelassenheit Separation — Consequence Reading: Technology Evaluated by Effect on Visiting, Mutual Aid, Geographic Rootedness").
narrative_ontology:topic_domain(gelassenheit_separation__consequence_reading, "religious_studies/technology_governance/commitment_systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__consequence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__consequence_reading, '7440df02-3308-47f6-8438-911f2fe85a08').
narrative_ontology:cs_kernel_codification('7440df02-3308-47f6-8438-911f2fe85a08', implicit).
narrative_ontology:cs_authority_grounding('7440df02-3308-47f6-8438-911f2fe85a08', lineage).
narrative_ontology:cs_interpretation_layer_present('7440df02-3308-47f6-8438-911f2fe85a08').
narrative_ontology:cs_reading_relation('7440df02-3308-47f6-8438-911f2fe85a08', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('7440df02-3308-47f6-8438-911f2fe85a08', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_axiom('7440df02-3308-47f6-8438-911f2fe85a08', foundational, technology_evaluated_by_effect_on_core_practices).
narrative_ontology:cs_axiom_status(technology_evaluated_by_effect_on_core_practices, holdable).
narrative_ontology:cs_axiom_grounding('7440df02-3308-47f6-8438-911f2fe85a08', technology_evaluated_by_effect_on_core_practices, conventional).
narrative_ontology:cs_axiom('7440df02-3308-47f6-8438-911f2fe85a08', foundational, visiting_mutual_aid_rootedness_are_separation_criteria).
narrative_ontology:cs_axiom_status(visiting_mutual_aid_rootedness_are_separation_criteria, holdable).
narrative_ontology:cs_axiom_grounding('7440df02-3308-47f6-8438-911f2fe85a08', visiting_mutual_aid_rootedness_are_separation_criteria, conventional).
narrative_ontology:cs_reference_frame('7440df02-3308-47f6-8438-911f2fe85a08', gemeinschaft_practice_preservation).
narrative_ontology:cs_drift_state('7440df02-3308-47f6-8438-911f2fe85a08', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7440df02-3308-47f6-8438-911f2fe85a08', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__consequence_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, ordnung_community_members).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, bishop_council).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, elderly_members).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, families_with_young_children).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, young_adults_seeking_education).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, members_needing_remote_medical_access).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, households_requiring_off_farm_income).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, families_with_young_children).
narrative_ontology:constraint_vindicates(gelassenheit_separation__consequence_reading, community_practices_preservation_as_separation_criterion).
narrative_ontology:constraint_vindicates(gelassenheit_separation__consequence_reading, visiting_and_mutual_aid_as_core_practices).
narrative_ontology:constraint_vindicates(gelassenheit_separation__consequence_reading, geographic_rootedness_as_identity_anchor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The bishop council interprets the Ordnung for each district, ruling on specific technologies case by case — permitting telephones in barns for emergency coordination while forbidding them in homes to preserve visiting. Their authority derives from the church's lineage and the community's acceptance of their interpretive role. They bear the burden of discernment but hold the power to shape daily life.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, bishop_council, agenda_setter,
    institutional, generational, identity_locked, local).

% The broader community benefits from preserved visiting patterns, strong mutual aid networks, and geographic stability. Members experience the technology rulings as protective of the practices that constitute their shared life — the telephone in the barn coordinates harvest help; its absence from the home keeps evenings open for neighborly visits.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, ordnung_community_members, beneficiary,
    organized, biographical, identity_locked, local).

% Elders rely most heavily on the visiting and mutual aid practices the rulings protect. Their care networks, knowledge transmission, and social standing are woven into the geographic rootedness the constraint preserves. They have no meaningful exit and do not seek one — their identity is constituted by the community.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, elderly_members, beneficiary,
    moderate, biographical, identity_locked, local).

% Parents benefit from the strong intergenerational support and safe, rooted environment for raising children. They also bear costs: limited access to certain medical specialists, educational resources, and off-farm income opportunities that would require technologies ruled out. Their constrained exit reflects deep community ties and the practical difficulty of relocating with children.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, families_with_young_children, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__consequence_reading, families_with_young_children, payer).

% Young adults who wish to pursue higher education or specialized trades face significant barriers: the constraint forbids technologies (internet, automobiles, certain communication tools) that would enable distance learning or commuting. Their exit is constrained by family ties, identity formation within the community, and the economic penalty of leaving without credentials.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, young_adults_seeking_education, payer,
    moderate, biographical, constrained, local).

% Members with chronic conditions or acute needs requiring specialist care unavailable locally bear the heaviest costs. The constraint's prohibition on home telephones and internet access means telemedicine is impossible; travel to distant clinics requires hiring a driver (permitted) but coordination is slow. They are trapped by health needs and community membership simultaneously.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, members_needing_remote_medical_access, payer,
    powerless, immediate, trapped, local).

% Households facing land scarcity or economic pressure need off-farm income but the constraint limits technologies that enable remote work or commuting to factory jobs. Tractors are permitted for belt power (stationary farm work) but not for transportation. Their constrained exit reflects the tension between economic necessity and community membership.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, households_requiring_off_farm_income, payer,
    moderate, biographical, constrained, local).

% Academic observers study the Ordnung as a living case of technology governance by communal discernment. They document the rulings, their rationales, and their effects on community cohesion and individual flourishing. Their analytical seat sees the full structural pattern across districts and generations.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, outside_sociologists_anthropologists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the practices of visiting, mutual aid, and geographic rootedness that constitute the community's shared life by evaluating each technology against its effect on those practices — permitting what supports them (telephone in barn for coordinating harvest help), restricting what erodes them (telephone in home displacing evening visits).
% TRANSFER_FUNCTION: Moves convenience, economic opportunity, and access to external resources (specialized medicine, higher education, remote work) from the constrained member subgroups (young adults, medically dependent, economically pressured households) to the community as a whole, which retains its cohesive practices and intergenerational stability.
% ABSENT_VOICES: Former members who left because the technology rulings made their life paths impossible — particularly those who needed medical access or educational pathways the constraint foreclosed. They are absent because leaving the community means losing the very relationships that would give them voice in its governance. Also absent: young children who will inherit the constraint's effects but cannot yet articulate dissent.
% DISAPPEARANCE_RATIONALE: If the consequence-reading Ordnung vanished overnight, the community's visiting patterns would erode as home telephones and internet entered; mutual aid networks would weaken as geographic rootedness declined; young adults would pursue education and off-farm income in greater numbers; the community would reorganize around different practices and likely fragment or assimilate.
% FOUNDING_PROBLEM: The community faced assimilation into English society and loss of distinct practices — visiting, mutual aid, and geographic rootedness were eroding as members adopted technologies that connected them to the outside world more than to each other. The consequence reading emerged as a discernment framework: evaluate each technology by whether it preserves or undermines the practices that hold the community together.
% FOUNDING_PROBLEM_CORROBORATION: The bishop council and community elders attest the founding problem remains live — they see assimilation pressure increasing with each new technology. Outside scholars (Kraybill, Hostetler, Nolt) corroborate that the consequence-reading framework continues to function as a live discernment practice across districts, not merely a historical artifact. No attestation from outside the benefiting parties claims the problem is dead.
narrative_ontology:disappearance_verdict(gelassenheit_separation__consequence_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__consequence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__consequence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gelassenheit_separation__consequence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__consequence_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__consequence_reading_tests).
:- end_tests(gelassenheit_separation__consequence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the constraint primarily coordinates community practices rather than extracting resources for a beneficiary class. The bishop council collects no rents; the coordination function is genuine. Suppression is moderate (0.45) because the constraint actively restricts specific technologies and enforces compliance through shunning, but alternatives exist within the framework (barn telephone, hired drivers). Theater ratio is very low (0.12) — the discernment process is substantive, not performative. Accessibility collapse is moderate (0.38) — members can and do leave, but at high identity cost. Resistance is low-moderate (0.28) — most members accept the framework; dissent is channeled through the bishop council's case-by-case process.
 *
 * PERSPECTIVAL GAP:
 *   From the bishop council's seat, the constraint is a genuine coordination mechanism preserving the community's life — a rope. From the trapped payer seats (medically dependent members), the same structure operates as extraction without reciprocity — approaching snare. The engine computes this divergence from the structural data; the authored claim (rope) reflects the constraint's dominant coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   The bishop council sits near the beneficiary end (d ~ 0.15): they administer the constraint and their authority is sustained by it, but they bear the discernment burden. Community members, elderly, and families are beneficiaries (d ~ 0.2-0.3) — they receive the coordination benefits of preserved practices. Young adults, medically dependent members, and economically pressured households are payers (d ~ 0.7-0.9) — they bear concentrated costs in foregone opportunities and access. Their exit options range from constrained to trapped, amplifying effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (assimilation pressure eroding core practices) remains live per both internal and external attestation. The constraint has not become a piton — its discernment function is actively exercised (new technologies ruled on regularly) and its coordination benefits are actively experienced. No mandatrophy resolution is declared; the arrangement continues to serve its founding purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the gelassenheit_separation kernel, or does it represent a local application of a single unified constraint?',
    'Compare the structural delta across districts: if the consequence-reading districts consistently permit barn telephones and forbid home telephones while artifact-reading districts forbid both, the readings are structurally distinct constraints with different ε values and beneficiary/victim structures.',
    'If distinct, each reading gets its own constraint story with its own classification. If unified, the variation is within-constraint application noise and should be modeled as a single story with measurement variance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings are structurally distinct constraints.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (shunning, church discipline) or internalized (members believe the rulings are right and self-enforce)?',
    'Post-exit suppression trajectory: interview former members about whether they continued to self-restrict technology use after leaving. If suppression persists after the extractive mechanism is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, which would increase measured extraction for identity-locked payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in interpersonal/communal constraints.').

omega_variable(
    coordination_extraction_boundary,
    'Where does the genuine coordination function (preserving visiting, mutual aid, rootedness) end and extraction (concentrating costs on the young, sick, and poor) begin?',
    'Counterfactual analysis: if the bishop council adopted the principle-reading framework (functional isolation test) instead, would the coordination benefits hold while the concentrated costs decrease? Compare outcomes across districts using different readings.',
    'If the coordination benefits can be achieved with lower concentrated costs, the current constraint is a tangled_rope — genuine coordination plus asymmetric extraction. If the consequence-reading''s specific rules are necessary for the coordination benefits, it remains a rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__consequence_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t1970, gelassenheit_separation__consequence_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(gela_tr_t1985, gelassenheit_separation__consequence_reading, theater_ratio, 1985, 0.09).
narrative_ontology:measurement(gela_tr_t2000, gelassenheit_separation__consequence_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(gela_tr_t2010, gelassenheit_separation__consequence_reading, theater_ratio, 2010, 0.11).
narrative_ontology:measurement(gela_tr_t2020, gelassenheit_separation__consequence_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(gela_tr_t2025, gelassenheit_separation__consequence_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(gela_be_t1970, gelassenheit_separation__consequence_reading, base_extractiveness, 1970, 0.12).
narrative_ontology:measurement(gela_be_t1985, gelassenheit_separation__consequence_reading, base_extractiveness, 1985, 0.15).
narrative_ontology:measurement(gela_be_t2000, gelassenheit_separation__consequence_reading, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement(gela_be_t2010, gelassenheit_separation__consequence_reading, base_extractiveness, 2010, 0.2).
narrative_ontology:measurement(gela_be_t2020, gelassenheit_separation__consequence_reading, base_extractiveness, 2020, 0.21).
narrative_ontology:measurement(gela_be_t2025, gelassenheit_separation__consequence_reading, base_extractiveness, 2025, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t1970, gelassenheit_separation__consequence_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(gela_su_t1985, gelassenheit_separation__consequence_reading, suppression_requirement, 1985, 0.38).
narrative_ontology:measurement(gela_su_t2000, gelassenheit_separation__consequence_reading, suppression_requirement, 2000, 0.41).
narrative_ontology:measurement(gela_su_t2010, gelassenheit_separation__consequence_reading, suppression_requirement, 2010, 0.43).
narrative_ontology:measurement(gela_su_t2020, gelassenheit_separation__consequence_reading, suppression_requirement, 2020, 0.44).
narrative_ontology:measurement(gela_su_t2025, gelassenheit_separation__consequence_reading, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__consequence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__consequence_reading, 0.08).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__principle_reading).

% DUAL FORMULATION NOTE:
% This constraint (consequence_reading) and its siblings (artifact_reading, principle_reading) form a constraint family decomposed from the gelassenheit_separation kernel. They share a kernel but instantiate different ε values, beneficiary/victim structures, and claimed types. The consequence reading has the lowest ε (fine-grained contextual rules); the artifact reading has higher ε (blanket prohibitions); the principle reading sits between (functional isolation test).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gelassenheit_separation__consequence_reading, institutional, 0.15).
constraint_indexing:directionality_override(gelassenheit_separation__consequence_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
