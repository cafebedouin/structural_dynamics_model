% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__sovereignty_reading, []).

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
 *   constraint_id: border_legitimacy__sovereignty_reading
 *   human_readable: Border Exclusion Authority from Territorial Sovereignty
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   The sovereignty reading of border legitimacy holds that states possess a
 *   legitimate right to exclude non-members derived from territorial
 *   sovereignty — the exclusive authority of a state within its recognized
 *   borders. This reading underpins the contemporary migration control
 *   regime: states determine who enters, who stays, and who is removed. The
 *   constraint is real and actively enforced (detention, deportation, border
 *   walls, visa regimes, carrier sanctions). It coordinates the Westphalian
 *   state system and domestic welfare democracies, but extracts severely from
 *   excluded migrants who bear the costs of their exclusion with no political
 *   voice. The reading coexists with, but is contested by, freedom of
 *   movement and humanitarian obligation readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, 0.82).
domain_priors:suppression_score(border_legitimacy__sovereignty_reading, 0.88).
domain_priors:theater_ratio(border_legitimacy__sovereignty_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "Border Exclusion Authority from Territorial Sovereignty").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, 'ec977f01-749a-47cf-8cb6-3f4c242a4eb1').
narrative_ontology:cs_kernel_codification('ec977f01-749a-47cf-8cb6-3f4c242a4eb1', formalized).
narrative_ontology:cs_authority_grounding('ec977f01-749a-47cf-8cb6-3f4c242a4eb1', lineage).
narrative_ontology:cs_interpretation_layer_present('ec977f01-749a-47cf-8cb6-3f4c242a4eb1').
narrative_ontology:cs_reading_relation('ec977f01-749a-47cf-8cb6-3f4c242a4eb1', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec977f01-749a-47cf-8cb6-3f4c242a4eb1', border_legitimacy__humanitarian_obligation_reading, coexists_with).
narrative_ontology:cs_axiom('ec977f01-749a-47cf-8cb6-3f4c242a4eb1', foundational, state_exclusionary_authority).
narrative_ontology:cs_axiom_status(state_exclusionary_authority, holdable).
narrative_ontology:cs_axiom_grounding('ec977f01-749a-47cf-8cb6-3f4c242a4eb1', state_exclusionary_authority, conventional).
narrative_ontology:cs_axiom('ec977f01-749a-47cf-8cb6-3f4c242a4eb1', foundational, territorial_integrity_primacy).
narrative_ontology:cs_axiom_status(territorial_integrity_primacy, holdable).
narrative_ontology:cs_axiom_grounding('ec977f01-749a-47cf-8cb6-3f4c242a4eb1', territorial_integrity_primacy, conventional).
narrative_ontology:cs_axiom('ec977f01-749a-47cf-8cb6-3f4c242a4eb1', secondary, migrant_claims_are_discretionary).
narrative_ontology:cs_axiom_status(migrant_claims_are_discretionary, holdable).
narrative_ontology:cs_axiom_grounding('ec977f01-749a-47cf-8cb6-3f4c242a4eb1', migrant_claims_are_discretionary, conventional).
narrative_ontology:cs_reference_frame('ec977f01-749a-47cf-8cb6-3f4c242a4eb1', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('ec977f01-749a-47cf-8cb6-3f4c242a4eb1', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ec977f01-749a-47cf-8cb6-3f4c242a4eb1', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, state).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, citizens).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, asylum_seekers).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, irregular_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, stateless_persons).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, territorial_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, westphalian_state_system).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, democratic_self_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims exclusive authority to determine admission and exclusion within recognized territory. Enforces borders through immigration law, detention, deportation, and border militarization. Justifies exclusion as necessary for public order, welfare state sustainability, labor market protection, and cultural cohesion. Collects legitimacy rents from controlling membership.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, state, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from exclusionary border policy through protected labor markets, sustained welfare provisions, cultural continuity, and democratic self-governance within bounded polity. Exit is mobile (can emigrate) but rarely exercised; primary political voice is through nationalist parties that reinforce exclusion.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, citizens, beneficiary,
    organized, biographical, mobile, national).

% Denied entry, detained, or deported. Bear the full cost of exclusion: lost life chances, family separation, exposure to violence in transit or return, precarious legal status. No effective exit from the constraint — irregular migration routes are lethal and criminalized; return often means persecution or destitution.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Present at borders claiming protection under refugee law but processed through exclusionary frameworks (detention, accelerated removal, third-country agreements). The sovereignty reading treats their claims as discretionary exceptions rather than rights, making their admission contingent on state discretion.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Reside without authorization after crossing borders outside state channels. Subject to permanent deportability, labor exploitation, denial of services, and social marginalization. Exit is constrained — voluntary return programs exist but often lead to same conditions; regularization pathways are narrow and discretionary.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, irregular_migrants, payer,
    powerless, biographical, constrained, global).

% Fall entirely outside the sovereign membership system — no state claims them, no state admits them. The sovereignty reading has no mechanism for their inclusion; they are the constraint's structural remainder, permanently excluded from the only system that confers rights.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, stateless_persons, payer,
    powerless, generational, trapped, global).

% UN treaty bodies, special rapporteurs, regional courts (ECtHR, IACtHR, AfCHPR) monitor state compliance with non-refoulement and human rights obligations. Issue findings and judgments that constrain but cannot override sovereign exclusion decisions; their authority is persuasive, not enforcement-grade.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, universal).

% UNHCR, IOM, MSF, NGOs provide protection and assistance to excluded populations. Their operational access depends on state consent; they are structurally excluded from decision-making on admission/exclusion. Would advocate for open borders or expanded protection but have no vote in sovereign forums.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, humanitarian_organizations, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Territorial sovereignty provides the jurisdictional container for stable governance: monopoly on violence, public goods provision, welfare redistribution, democratic accountability, and legal order. Borders define the polity within which these coordination functions operate.
% TRANSFER_FUNCTION: Transfers life chances, labor market access, welfare eligibility, political membership, and physical security from excluded migrants to citizens and the state. The state extracts legitimacy and control; citizens extract economic and cultural rents; migrants bear the costs of their exclusion.
% ABSENT_VOICES: Excluded migrants themselves (cannot vote, cannot speak in sovereign forums), future generations of would-be migrants (not yet born, not represented), stateless persons (no state speaks for them), and the global poor whose mobility is constrained by the sovereign order they had no role in creating.
% DISAPPEARANCE_RATIONALE: If sovereign exclusion authority vanished overnight, borders would become administratively permeable. Massive migration flows would reorganize global labor markets, strain welfare states, transform national electorates, and potentially collapse the Westphalian state system. The world would rearrange fundamentally — not because borders are natural, but because the entire modern political-economy is built on them.
% FOUNDING_PROBLEM: The Westphalian settlement (1648) established territorial sovereignty as the organizing principle of international order to end religious wars by granting princes exclusive authority within defined territories. The founding problem was: how to constitute political authority that is both internally supreme and externally non-interfering.
% FOUNDING_PROBLEM_CORROBORATION: Realist IR scholars (Morgenthau, Waltz) and state practice attest sovereignty remains the organizing principle. Cosmopolitan philosophers (Carens, Benhabib), human rights lawyers, and critical migration scholars attest the founding problem is superseded by a human rights regime that limits sovereign exclusion. The 1951 Refugee Convention and subsequent human rights treaties are the corroborating instruments — they were negotiated by states but constrain the sovereignty reading's absolute claim.
narrative_ontology:disappearance_verdict(border_legitimacy__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_legitimacy__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__sovereignty_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because exclusion transfers enormous life-chance value from migrants to citizens/state. Suppression is very high (0.88) because the constraint's persistence depends on active, militarized enforcement against people who would otherwise move. Theater is low (0.22) — enforcement is genuine, not performative; the state actually stops people. Accessibility collapse is high (0.78) because irregular alternatives are lethal and criminalized. Resistance is moderate (0.52) — migrants resist through irregular movement, legal challenges, advocacy, but structural power asymmetry limits effectiveness. Claimed type is tangled_rope: genuine coordination (state capacity, public goods) AND asymmetric extraction (migrant exclusion).
 *
 * PERSPECTIVAL GAP:
 *   From the state/citizen seat, the constraint appears as legitimate coordination — the foundation of democratic self-government and welfare provision. From the migrant seats, the same structure appears as violent extraction — borders as global apartheid. The engine computes this divergence from structural data; the sovereignty reading's claim of legitimacy is its own fram, not the engine's verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   State (agenda_setter) and citizens (beneficiary) sit at low directionality (d ~ 0.1-0.2) — the constraint subsidizes their position. Excluded migrants, asylum seekers, irregular migrants, stateless persons (payers) sit at high directionality (d ~ 0.9-1.0) — they are the extraction targets, trapped by the constraint. International human rights bodies (observer) sit at analytical (d = 0.5). Humanitarian organizations (excluded) sit at constrained — they operate in the shadow of sovereignty but cannot shape it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ending interstate war via territorial sovereignty) is contested as live vs. superseded. The arrangement persists because it benefits powerful agents (states, citizens) and the excluded have no political voice. Mandatrophy is not resolved — the constraint has outlived its original justification (interstate war prevention) but captures new rents (labor market protection, cultural homogeneity) that sustain it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_natural_vs_constructed,
    'Is territorial sovereignty a natural fact of political organization or a constructed claim that serves identifiable beneficiaries?',
    'Historical analysis of state formation: if sovereignty emerges wherever humans organize politically, it may be natural; if it appears historically contingent (post-1648 Europe, spread via colonialism), it is constructed.',
    'If natural, the constraint approaches mountain (low ε). If constructed, high ε reflects distributive choice, not necessity — supporting reclassification from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_natural_vs_constructed, conceptual, 'Natural-law vs. constructed status of territorial sovereignty').

omega_variable(
    exclusion_necessity_for_coordination,
    'Is the exclusion of non-members structurally necessary for the coordination functions sovereignty enables (welfare, democracy, public order), or is exclusion extractive overhead?',
    'Counterfactual analysis: polities with high immigration (e.g., Gulf states, Singapore, historical US) maintain coordination functions with permeable borders. If coordination persists without exclusion, exclusion is not necessary.',
    'If exclusion is unnecessary for coordination, the constraint''s extraction is not the price of coordination but pure rent — shifting classification toward snare. If necessary, tangled_rope holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_necessity_for_coordination, empirical, 'Whether exclusion is a necessary condition for sovereign coordination functions').

omega_variable(
    migrant_agency_vs_victimhood,
    'Are excluded migrants purely victims of the constraint, or do they exercise agency that complicates the victim designation?',
    'Migration decision-making studies: if migrants calculate risks and choose irregular routes despite known dangers, agency is present. But agency under coercion (no safe legal pathway) does not negate victimhood.',
    'If agency is significant, the payer role is complicated — migrants are not passive objects but strategic actors navigating a hostile structure. This affects directionality calculation (exit_options may be constrained rather than trapped).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(migrant_agency_vs_victimhood, conceptual, 'Whether migrant agency modifies the structural victim designation').

omega_variable(
    reading_relations_structure,
    'Does the sovereignty reading foreclose, coexist with, or influence the freedom of movement and humanitarian obligation readings?',
    'Legal-philosophical analysis: Can a single legal framework hold both absolute sovereign exclusion and a human right to free movement? The Refugee Convention attempts both — sovereignty over admission + non-refoulement obligation. If they can coexist in one framework, relation is coexists_with; if non-refoulement logically limits sovereignty, relation is influences.',
    'Determines cs_structure.reading_relations classification. Affects whether kernel drift is modeled as contradiction or tension.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relations_structure, conceptual, 'Structural relationship between sovereignty reading and sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blsr_tr_t1945, border_legitimacy__sovereignty_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(blsr_tr_t1960, border_legitimacy__sovereignty_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement(blsr_tr_t1975, border_legitimacy__sovereignty_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(blsr_tr_t1990, border_legitimacy__sovereignty_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(blsr_tr_t2005, border_legitimacy__sovereignty_reading, theater_ratio, 2005, 0.23).
narrative_ontology:measurement(blsr_tr_t2025, border_legitimacy__sovereignty_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(blsr_be_t1945, border_legitimacy__sovereignty_reading, base_extractiveness, 1945, 0.65).
narrative_ontology:measurement(blsr_be_t1960, border_legitimacy__sovereignty_reading, base_extractiveness, 1960, 0.68).
narrative_ontology:measurement(blsr_be_t1975, border_legitimacy__sovereignty_reading, base_extractiveness, 1975, 0.72).
narrative_ontology:measurement(blsr_be_t1990, border_legitimacy__sovereignty_reading, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(blsr_be_t2005, border_legitimacy__sovereignty_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(blsr_be_t2025, border_legitimacy__sovereignty_reading, base_extractiveness, 2025, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(blsr_su_t1945, border_legitimacy__sovereignty_reading, suppression_requirement, 1945, 0.55).
narrative_ontology:measurement(blsr_su_t1960, border_legitimacy__sovereignty_reading, suppression_requirement, 1960, 0.62).
narrative_ontology:measurement(blsr_su_t1975, border_legitimacy__sovereignty_reading, suppression_requirement, 1975, 0.7).
narrative_ontology:measurement(blsr_su_t1990, border_legitimacy__sovereignty_reading, suppression_requirement, 1990, 0.78).
narrative_ontology:measurement(blsr_su_t2005, border_legitimacy__sovereignty_reading, suppression_requirement, 2005, 0.84).
narrative_ontology:measurement(blsr_su_t2025, border_legitimacy__sovereignty_reading, suppression_requirement, 2025, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_legitimacy__sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% The border_legitimacy kernel decomposes into three readings with distinct ε values and victim sets. Sovereignty reading: high ε (0.82), victims = excluded migrants. Freedom of movement reading: low ε (~0.15), victims = would-be migrants constrained by borders. Humanitarian obligation reading: moderate ε (~0.45), victims = asylum seekers denied protection. The sovereignty reading is upstream — its legitimacy claim is cited to limit the humanitarian obligation reading's scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_legitimacy__sovereignty_reading, institutional, 0.15).
constraint_indexing:directionality_override(border_legitimacy__sovereignty_reading, organized, 0.2).
constraint_indexing:directionality_override(border_legitimacy__sovereignty_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
