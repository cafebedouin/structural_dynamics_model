% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__subsidiarity_balance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__subsidiarity_balance, []).

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
 *   constraint_id: federation_membership_treaty__subsidiarity_balance
 *   human_readable: Subsidiarity-Balanced Free Movement Within Federation Treaty
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   A federation treaty establishes free movement of persons as a core right,
 *   but subjects it to a proportionality test: member states may restrict
 *   mobility where necessary to protect legitimate national interests (labor
 *   market stability, welfare sustainability, public order), provided the
 *   restriction is proportionate to the aim. This reading — the subsidiarity
 *   balance — treats mobility as a qualified right whose boundaries are
 *   negotiated case-by-case through judicial review and intergovernmental
 *   coordination, rather than as an absolute market freedom
 *   (integration_primary) or a state-granted privilege (sovereignty_primary).
 *   The constraint operates as a graduated filter: some domains (high-skill
 *   labor, intra-corporate transfers) face minimal restriction; others
 *   (low-skill sectors, welfare-access-linked migration) face quotas, waiting
 *   periods, or safeguard clauses. The same arrangement coordinates
 *   cross-border labor allocation (rope function) while extracting compliance
 *   costs from aspiring migrants and fiscal costs from welfare systems (snare
 *   function) — hence tangled rope. Enforcement is active: courts strike down
 *   disproportionate restrictions; states maintain administrative machinery
 *   for quotas and safeguards; the commission monitors proportionality
 *   compliance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.42).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.35).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.42).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "Subsidiarity-Balanced Free Movement Within Federation Treaty").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, '2a9b200f-9e91-4b2a-8b4c-88af3ebbbcbd').
narrative_ontology:cs_kernel_codification('2a9b200f-9e91-4b2a-8b4c-88af3ebbbcbd', formalized).
narrative_ontology:cs_authority_grounding('2a9b200f-9e91-4b2a-8b4c-88af3ebbbcbd', lineage).
narrative_ontology:cs_interpretation_layer_present('2a9b200f-9e91-4b2a-8b4c-88af3ebbbcbd').
narrative_ontology:cs_reading_relation('2a9b200f-9e91-4b2a-8b4c-88af3ebbbcbd', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('2a9b200f-9e91-4b2a-8b4c-88af3ebbbcbd', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('2a9b200f-9e91-4b2a-8b4c-88af3ebbbcbd', foundational, mobility_right_qualified_by_proportionality).
narrative_ontology:cs_axiom_status(mobility_right_qualified_by_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('2a9b200f-9e91-4b2a-8b4c-88af3ebbbcbd', mobility_right_qualified_by_proportionality, conventional).
narrative_ontology:cs_axiom('2a9b200f-9e91-4b2a-8b4c-88af3ebbbcbd', foundational, legitimate_national_interests_constrain_but_not_eliminate_mobility).
narrative_ontology:cs_axiom_status(legitimate_national_interests_constrain_but_not_eliminate_mobility, holdable).
narrative_ontology:cs_axiom_grounding('2a9b200f-9e91-4b2a-8b4c-88af3ebbbcbd', legitimate_national_interests_constrain_but_not_eliminate_mobility, conventional).
narrative_ontology:cs_axiom('2a9b200f-9e91-4b2a-8b4c-88af3ebbbcbd', secondary, subsidiarity_as_legitimacy_condition_for_restrictions).
narrative_ontology:cs_axiom_status(subsidiarity_as_legitimacy_condition_for_restrictions, holdable).
narrative_ontology:cs_axiom_grounding('2a9b200f-9e91-4b2a-8b4c-88af3ebbbcbd', subsidiarity_as_legitimacy_condition_for_restrictions, conventional).
narrative_ontology:cs_reference_frame('2a9b200f-9e91-4b2a-8b4c-88af3ebbbcbd', post_war_labor_mobility_compromise).
narrative_ontology:cs_drift_state('2a9b200f-9e91-4b2a-8b4c-88af3ebbbcbd', contemporary_demographic_integration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2a9b200f-9e91-4b2a-8b4c-88af3ebbbcbd', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, federation_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, member_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, cross_border_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, cross_border_employers).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, aspiring_migrants_blocked_by_national_quotas).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, welfare_administrators_bearing_portability_costs).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, labor_market_insiders_facing_competition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, member_state_governments).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, cross_border_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, cross_border_employers).
narrative_ontology:constraint_vindicates(federation_membership_treaty__subsidiarity_balance, proportionality_principle_in_federal_law).
narrative_ontology:constraint_vindicates(federation_membership_treaty__subsidiarity_balance, subsidiarity_as_legitimacy_condition).
narrative_ontology:constraint_vindicates(federation_membership_treaty__subsidiarity_balance, mobility_rights_as_qualified_not_absolute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Commission proposes legislation, monitors compliance, and initiates infringement proceedings; the Court interprets proportionality and strikes down disproportionate restrictions. They author the legal framework and collect legitimacy from balancing mobility and state autonomy. Their exit is analytical — they observe and adjudicate rather than bear the constraint's costs.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federation_institutions, agenda_setter,
    institutional, generational, analytical, continental).

% Set restriction policy in the Council (quotas, safeguard triggers, welfare access conditions) and implement Court rulings. They bear welfare portability costs and political costs when mobility triggers backlash. Their exit is constrained — treaty withdrawal is legally possible but politically catastrophic; they negotiate within the framework.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, member_state_governments, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, member_state_governments, payer).

% Access employment across member states under the treaty right. They benefit from labor market integration but pay compliance costs (recognition of qualifications, social security coordination, language requirements) and face uncertainty when states invoke safeguard clauses. Their exit is constrained — returning to origin labor market means losing accumulated rights and earnings.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, cross_border_workers, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, cross_border_workers, payer).

% Recruit from an enlarged labor pool and deploy staff across establishments. They benefit from reduced hiring friction but pay compliance costs (posting directives, wage coordination, administrative burden). Their exit is mobile — they can relocate production or shift recruitment strategies, though sunk investments create friction.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, cross_border_employers, beneficiary,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, cross_border_employers, payer).

% Would migrate for work but are blocked by sectoral quotas, waiting periods, or safeguard measures that the proportionality test permits. They bear the full cost of exclusion (foregone earnings, stalled careers) with no effective exit — they cannot access the federation labor market and cannot change the quota system. Their situation is structurally extractive: the constraint's coordination function for others operates through their exclusion.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, aspiring_migrants_blocked_by_national_quotas, payer,
    powerless, biographical, trapped, national).

% Administer cross-border social security coordination (pension aggregation, healthcare reimbursement, unemployment benefit export). They bear rising administrative costs as mobility increases, with limited ability to reduce caseload or simplify rules — the treaty mandates portability. Their exit is constrained — they implement laws they cannot change, though they influence domestic implementation.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, welfare_administrators_bearing_portability_costs, payer,
    organized, biographical, constrained, national).

% Incumbent workers in sectors exposed to cross-border competition (construction, hospitality, care work). They experience wage pressure and job displacement from mobile workers. They organize politically (unions, sectoral associations) to demand safeguard clauses. Their exit is constrained — retraining or sector change is costly; they fight within the political system to shape restriction policy.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, labor_market_insiders_facing_competition, payer,
    moderate, biographical, constrained, regional).

% Adjudicates proportionality challenges: reviews whether national restrictions are suitable, necessary, and proportionate to legitimate aims. Its rulings define the operational boundary of the constraint. It neither collects rents nor bears extraction — it structures the game. Its exit is analytical.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federation_court, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates labor across a federation's internal borders while allowing member states to protect legitimate national interests (labor market stability, welfare sustainability, public order) through proportionate restrictions. Solves the problem of how to integrate labor markets without disabling democratic responsiveness to localized shocks.
% TRANSFER_FUNCTION: Moves compliance costs and restriction risk onto mobile workers and aspiring migrants; moves fiscal administration costs onto welfare systems; moves discretionary restriction power to member states; moves legitimacy rents to federation institutions that author and adjudicate the proportionality standard.
% ABSENT_VOICES: Third-country nationals outside the federation (no mobility right at all); undocumented migrants within the federation (excluded from portability protections); future generations who inherit the fiscal and demographic consequences of current mobility/restriction choices. They would object to both unrestricted movement (fiscal sustainability) and blanket restrictions (human development) but are not represented in the proportionality calculus.
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished overnight, member states would immediately impose unrestricted restrictions (integration_primary fear) or the federation would impose unrestricted mobility (sovereignty_primary fear). The labor market, welfare systems, and political coalitions would reorganize around one pole or the other — the graduated compromise is what holds the federation together on this issue.
% FOUNDING_PROBLEM: Post-war reconstruction required labor mobility across the federation to match workers to rebuilding needs, but member states feared welfare tourism and labor market disruption. The treaty embedded free movement as a qualified right with a proportionality safeguard.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (outside the benefiting parties) document that the post-war labor allocation problem was solved by the 1970s; the treaty's mobility provisions persisted and expanded into service liberalization and citizenship rights. The Commission and Court attest the problem is live (ongoing integration needs); member state governments attest it is dead (original rationale obsolete). The corroboration from independent scholarship supports 'dead'.
narrative_ontology:disappearance_verdict(federation_membership_treaty__subsidiarity_balance, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__subsidiarity_balance, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(federation_membership_treaty__subsidiarity_balance, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__subsidiarity_balance, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__subsidiarity_balance_tests).
:- end_tests(federation_membership_treaty__subsidiarity_balance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects that the arrangement transfers compliance burden onto mobile workers and fiscal burden onto welfare administrators, while granting states discretionary restriction power that can be used protectively. Suppression (0.35) is moderate: alternatives (unrestricted movement, blanket bans) are legally foreclosed but practically contested — states test boundaries, courts push back. Theater ratio (0.28) reflects that proportionality review has become partly performative: the legal ritual of balancing is real, but outcomes increasingly track political bargaining rather than doctrinal coherence. Accessibility collapse (0.55) is mid-range: the proportionality standard is learnable, but its application is fact-intensive and jurisdictionally variable, so alternatives don't fully collapse for informed actors. Resistance (0.48) is substantial: member states resist Commission oversight; courts face legitimacy challenges; migrant advocates litigate restrictions; employer groups lobby for liberalization.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute divergent seat types: federation institutions and mobile workers see coordination (rope/tangled_rope); blocked aspirants and welfare administrators see extraction (snare/tangled_rope); member state governments see both depending on policy domain (labor market policy vs. welfare policy). The structural asymmetry comes from domain-varying beneficiary/victim sets: in high-skill domains, the constraint is nearly rope; in low-skill/welfare-linked domains, it approaches snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Federation institutions (Commission, Court) are agenda_setters with institutional power and analytical exit — they author and enforce the proportionality standard, collecting legitimacy rents. Member state governments are dual: agenda_setters in the Council (setting restriction policy) and payers (bearing welfare portability costs, political costs of mobility backlash). Cross-border workers and employers are beneficiaries (access to larger labor markets) but also payers (compliance costs, uncertainty from shifting restriction regimes). Aspiring migrants blocked by quotas are payers with constrained exit (trapped in origin labor markets). Welfare administrators are payers bearing portability administration costs. Labor market insiders facing competition are payers with constrained exit (cannot easily leave the sector). All victim groups experience the constraint as extraction; beneficiary groups experience it as coordination with extractive overhead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war labor mobility for reconstruction) is dead; the arrangement persists because it now serves integrationist and protectionist coalitions simultaneously. The proportionality framework prevents mandatrophy resolution by making every restriction contestable — no coalition can fully capture the arrangement, but no coalition can abolish it either. The constraint is a living compromise, not a zombie.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_operationalization_ambiguity,
    'Does the proportionality test have a stable, predictable operational meaning across policy domains, or is it an interpretive vessel that legitimizes whatever restriction the Court or Council tolerates?',
    'Longitudinal study of Court rulings across domains (labor market safeguards, welfare access, public order) coding for doctrinal consistency vs. outcome-driven balancing. Compare predictability of outcomes for repeat litigants.',
    'If proportionality is a stable doctrinal standard, the constraint is a genuine coordination mechanism with extractive overhead. If it is an interpretive vessel, the coordination function is theatrical and the constraint is a snare dressed as tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_operationalization_ambiguity, conceptual, 'Whether the proportionality standard is a real coordination mechanism or a legitimizing performance.').

omega_variable(
    domain_boundary_legitimacy,
    'Are the policy domains where restrictions are permitted (labor market, welfare, public order) structurally justified by the treaty''s logic, or are they political compromises that could be expanded or contracted without changing the constraint''s nature?',
    'Trace the treaty drafting history and subsequent amendment practice: were domain boundaries derived from a theory of legitimate state interest, or negotiated as package deals? Test whether new domains (climate migration, digital nomads) fit the existing categories or require new exceptions.',
    'If domains are structurally justified, the graduated structure is a genuine coordination solution. If they are contingent political compromises, the constraint''s beneficiary/victim sets are unstable and the tangled rope classification may mask domain-specific snares.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domain_boundary_legitimacy, empirical, 'Whether the domain-varying beneficiary/victim structure reflects structural logic or political contingency.').

omega_variable(
    kernel_reading_fork_structure,
    'Do the three sibling readings (integration_primary, sovereignty_primary, subsidiarity_balance) represent distinct constraints with different ε and beneficiary/victim structures, or are they rhetorical framings of a single constraint whose ε is invariant?',
    'Compare the three readings'' extractiveness, suppression, and beneficiary/victim declarations as authored in their respective constraint stories. If ε differs by >0.15 or beneficiary/victim sets are non-overlapping, they are distinct constraints. If ε is stable and sets overlap heavily, they are framings of one constraint.',
    'If distinct constraints, the kernel decomposition is valid and each reading should be analyzed separately with network links. If one constraint, the kernel frame is a category error and the readings should be merged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_fork_structure, conceptual, 'Whether the kernel''s sibling readings are structurally distinct constraints or rhetorical variants.').

omega_variable(
    mandatrophy_vs_living_compromise,
    'Is the constraint a case of mandatrophy (founding problem dead, arrangement persists via inertia) or a living compromise (founding problem dead, but new problems sustain the arrangement)?',
    'Identify the active coalitions that would defend the constraint today vs. those that would abolish it. If defenders are the same as original beneficiaries, it''s mandatrophy. If new problems (demographic aging, skill mismatches, climate migration) generate new defender coalitions, it''s a living compromise.',
    'If mandatrophy, the constraint is a piton candidate (theater ratio should rise, extractiveness should accumulate). If living compromise, the tangled rope classification is stable and the constraint adapts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_vs_living_compromise, empirical, 'Whether the constraint''s persistence is inertial or adaptive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__subsidiarity_balance, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fede_tr_t8, federation_membership_treaty__subsidiarity_balance, theater_ratio, 8, 0.18).
narrative_ontology:measurement(fede_tr_t16, federation_membership_treaty__subsidiarity_balance, theater_ratio, 16, 0.24).
narrative_ontology:measurement(fede_tr_t24, federation_membership_treaty__subsidiarity_balance, theater_ratio, 24, 0.27).
narrative_ontology:measurement(fede_tr_t32, federation_membership_treaty__subsidiarity_balance, theater_ratio, 32, 0.28).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__subsidiarity_balance, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fede_be_t8, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(fede_be_t16, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(fede_be_t24, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(fede_be_t32, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 32, 0.42).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(fede_su_t8, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(fede_su_t16, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 16, 0.32).
narrative_ontology:measurement(fede_su_t24, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(fede_su_t32, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 32, 0.35).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 40, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__subsidiarity_balance, 0.15).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_welfare_portability_regime).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_labor_market_safeguard_clauses).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_citizenship_rights_directive).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the federation_membership_treaty kernel. The integration_primary reading treats free movement as near-absolute (lower extractiveness, lower suppression); the sovereignty_primary reading treats it as state-discretionary (higher extractiveness for mobile workers, higher suppression of mobility). This subsidiarity_balance reading sits structurally between them: it coordinates labor allocation across borders while extracting compliance costs via proportionality review. All three readings share the same treaty text but instantiate different constraints with different ε, different beneficiary/victim structures, and different enforcement logics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__subsidiarity_balance, institutional, 0.15).
constraint_indexing:directionality_override(federation_membership_treaty__subsidiarity_balance, organized, 0.45).
constraint_indexing:directionality_override(federation_membership_treaty__subsidiarity_balance, moderate, 0.7).
constraint_indexing:directionality_override(federation_membership_treaty__subsidiarity_balance, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
