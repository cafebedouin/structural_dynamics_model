% ============================================================================
% CONSTRAINT STORY: border_legitimacy__humanitarian_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__humanitarian_obligation_reading, []).

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
 *   constraint_id: border_legitimacy__humanitarian_obligation_reading
 *   human_readable: Humanitarian Obligation Border Regime (Refugee/Non-Refugee Distinction)
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This constraint instantiates the humanitarian obligation reading of the
 *   border_legitimacy kernel: states have a legal obligation to admit those
 *   fleeing persecution or disaster, but not general economic migrants. The
 *   constraint operates through the refugee/non-refugee binary — a legal
 *   category distinction that sorts mobile humanity into 'deserving' and
 *   'undeserving' of protection. The reading claims this distinction solves a
 *   coordination problem (bounded obligation enabling state participation)
 *   while the metrics reveal substantial extraction: the category's
 *   enforcement extracts survival chances from economic and mixed-motive
 *   migrants, requires active suppression of alternative mobility, and has
 *   accumulated extractive overhead through deterrence infrastructure. The
 *   constraint is a tangled rope because it genuinely coordinates (states
 *   participate because the obligation is bounded) AND extracts
 *   asymmetrically (the boundedness is enforced on the most vulnerable
 *   non-refugees).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.42).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.68).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Humanitarian Obligation Border Regime (Refugee/Non-Refugee Distinction)").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, '2bc8c637-967c-4263-941c-1bc8ff0e5ad1').
narrative_ontology:cs_kernel_codification('2bc8c637-967c-4263-941c-1bc8ff0e5ad1', formalized).
narrative_ontology:cs_authority_grounding('2bc8c637-967c-4263-941c-1bc8ff0e5ad1', lineage).
narrative_ontology:cs_interpretation_layer_present('2bc8c637-967c-4263-941c-1bc8ff0e5ad1').
narrative_ontology:cs_reading_relation('2bc8c637-967c-4263-941c-1bc8ff0e5ad1', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('2bc8c637-967c-4263-941c-1bc8ff0e5ad1', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_axiom('2bc8c637-967c-4263-941c-1bc8ff0e5ad1', foundational, persecution_threshold_binds_state_obligation).
narrative_ontology:cs_axiom_status(persecution_threshold_binds_state_obligation, holdable).
narrative_ontology:cs_axiom_grounding('2bc8c637-967c-4263-941c-1bc8ff0e5ad1', persecution_threshold_binds_state_obligation, conventional).
narrative_ontology:cs_axiom('2bc8c637-967c-4263-941c-1bc8ff0e5ad1', foundational, economic_migration_excluded_from_protection_obligation).
narrative_ontology:cs_axiom_status(economic_migration_excluded_from_protection_obligation, holdable).
narrative_ontology:cs_axiom_grounding('2bc8c637-967c-4263-941c-1bc8ff0e5ad1', economic_migration_excluded_from_protection_obligation, conventional).
narrative_ontology:cs_reference_frame('2bc8c637-967c-4263-941c-1bc8ff0e5ad1', postwar_refugee_regime_founding).
narrative_ontology:cs_drift_state('2bc8c637-967c-4263-941c-1bc8ff0e5ad1', contemporary_deterrence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2bc8c637-967c-4263-941c-1bc8ff0e5ad1', '').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, refugees_asylum_seekers).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, state_sovereignty_claim).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, international_refugee_regime).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, economic_migrants_excluded).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, mixed_motive_migrants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, destination_states).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, unhcr_ngo_network).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, migration_control_industry).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, origin_transit_states).
narrative_ontology:constraint_vindicates(border_legitimacy__humanitarian_obligation_reading, non_refoulement_principle).
narrative_ontology:constraint_vindicates(border_legitimacy__humanitarian_obligation_reading, persecution_based_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals fleeing persecution, conflict, or disaster who meet the legal definition of refugee. They gain admission rights under the constraint but face high barriers to proving eligibility, dangerous journeys, and prolonged detention during determination. Their exit options are near-zero — they cannot return to persecution and have no alternative legal pathways.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, refugees_asylum_seekers, beneficiary,
    powerless, biographical, trapped, global).

% Individuals migrating for survival-level economic reasons (extreme poverty, climate degradation, lack of livelihood) who do not meet the persecution threshold. They bear the full force of border enforcement — detention, deportation, death at borders — without access to the protection regime. The constraint extracts their mobility and survival chances to maintain the refugee/non-refugee distinction.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, economic_migrants_excluded, payer,
    powerless, biographical, trapped, global).

% Individuals whose migration is driven by overlapping persecution and economic desperation (e.g., conflict zones with collapsed economies, climate-displaced from persecuted minorities). They fall into determination gaps — too economic for refugee status, too endangered for safe return. The constraint's binary category extracts them most severely.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, mixed_motive_migrants, payer,
    powerless, biographical, trapped, global).

% States that administer the asylum system and enforce the refugee/non-refugee distinction. They benefit from the constraint's legitimating function — it converts border enforcement from arbitrary exclusion into lawful distinction — while retaining sovereign discretion over recognition rates, detention policy, and burden-sharing. They can exit the regime's stricter interpretations through restrictive domestic implementation.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, destination_states, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__humanitarian_obligation_reading, destination_states, beneficiary).

% States that produce or host displaced populations. They bear disproportionate costs of the regime — hosting refugees without adequate burden-sharing, managing transit migration, absorbing deportees — while having minimal influence over the constraint's interpretation. Their exit is constrained by geography and international pressure.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, origin_transit_states, payer,
    moderate, biographical, constrained, regional).

% The institutional architecture of refugee protection (UNHCR, IOM, major NGOs). They benefit from the constraint as it funds and legitimizes their operational mandate. They have mobile exit options — they operate across jurisdictions and can shift advocacy focus — but their institutional identity is fused to the refugee category.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, unhcr_ngo_network, beneficiary,
    organized, generational, mobile, global).

% Private contractors, technology firms, and security companies that build and operate border enforcement infrastructure (detention centers, biometric systems, deportation logistics). They extract rents from the constraint's enforcement requirements. Their exit is arbitrage-grade — they serve multiple states and adapt to policy shifts.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, migration_control_industry, beneficiary,
    organized, biographical, arbitrage, global).

% Interpretive authorities (international courts, regional human rights bodies, academic discourse) that adjudicate the boundary between refugee and non-refugee. They neither collect nor pay directly but shape the constraint's operational meaning through precedent and doctrine.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, legal_scholars_courts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally administrable distinction between those whom states are obligated to protect (persecution/disaster flee-ers) and those they may lawfully exclude, enabling a functional asylum system without open borders.
% TRANSFER_FUNCTION: Transfers admission rights and protection resources from states to recognized refugees; transfers enforcement costs and exclusion harms to economic migrants and mixed-motive migrants; transfers legitimating authority to states and operational mandates to the humanitarian apparatus.
% ABSENT_VOICES: Would-be migrants from collapsed states who do not fit the persecution paradigm (e.g., pure climate displacement, generalized violence without individualized persecution, structural poverty). They are structurally excluded from the conversation because the constraint's binary category has no slot for them — they appear only as 'bogus claimants' in enforcement discourse.
% DISAPPEARANCE_RATIONALE: If the refugee/non-refugee distinction vanished overnight, states would face either open admission (freedom of movement reading) or unrestricted exclusion (sovereignty reading). The entire institutional architecture of asylum — determination procedures, UNHCR mandate, non-refoulement jurisprudence, burden-sharing negotiations — would collapse or require reconstruction.
% FOUNDING_PROBLEM: Post-WWII displacement crisis revealed that states would not protect persecution flee-ers without a legal obligation, but states would not accept an obligation without a bounded category that preserved sovereign discretion over 'ordinary' migration.
% FOUNDING_PROBLEM_CORROBORATION: The 1951 Convention drafters' records (travaux préparatoires) confirm the deliberate exclusion of economic migrants. Contemporary historians (e.g., Hathaway, Goodwin-Gill) corroborate the founding bargain. Refugee advocates argue the founding problem is live because persecution persists; restrictionists argue it is dead because the category has been weaponized for migration control; legal scholars document the contested status.
narrative_ontology:disappearance_verdict(border_legitimacy__humanitarian_obligation_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__humanitarian_obligation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__humanitarian_obligation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(border_legitimacy__humanitarian_obligation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__humanitarian_obligation_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__humanitarian_obligation_reading_tests).
:- end_tests(border_legitimacy__humanitarian_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate but rising — the constraint coordinates genuine protection for refugees (coordination function) while the enforcement apparatus increasingly targets non-refugees (extraction). Suppression (0.68) is high because the distinction requires active prevention of unauthorized entry, detention, deportation, and extraterritorial interdiction — alternatives (regional free movement, complementary protection pathways) are systematically suppressed. Theater ratio (0.28) reflects growing performative compliance: states maintain asylum systems while hollowing them out through deterrence. Accessibility collapse (0.45) is moderate — the binary category leaves some alternatives (complementary protection, humanitarian visas) but they are discretionary and shrinking. Resistance (0.55) is significant from migrant justice movements, courts, and some states but has not shifted the structural logic.
 *
 * PERSPECTIVAL GAP:
 *   From the refugee seat, the constraint is a life-saving rope (coordination function dominant). From the economic migrant seat, it is a snare (pure extraction, no coordination benefit). From the destination state seat, it is a managed compromise (tangled rope — they get sovereign cover for enforcement). From the mixed-motive migrant seat, it is a categorical trap — the constraint's binary logic has no place for them. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Refugees are beneficiaries (d ~ 0.2) — they receive protection but face determination barriers. Economic migrants and mixed-motive migrants are full targets (d ~ 0.9) — they bear enforcement costs with no offsetting benefit. Destination states are agenda-setters with beneficiary overlap (d ~ 0.15) — they extract legitimating authority and sovereign discretion. Origin/transit states are payers (d ~ 0.7) — they bear disproportionate costs. The humanitarian apparatus and migration industry are beneficiaries (d ~ 0.2) — they collect mandates and contracts. Legal observers are analytical (d ~ 0.5). The refugee/non-refugee binary creates a bifurcated victim set: both excluded groups are powerless and trapped, but mixed-motive migrants suffer the category's internal contradiction most severely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (bounded obligation to enable state participation) remains contested. The constraint has not resolved its mandatrophy: the original bargain (states protect refugees in exchange for bounded category) has layered extraction — deterrence infrastructure, externalization, criminalization — that serves migration control rather than protection. The constraint persists because no coalition can displace it: refugees need it, states rely on its legitimating function, the humanitarian apparatus depends on its mandate. This is tangled rope, not scaffold (no sunset) and not piton (the coordination function remains live).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_boundary_naturalness,
    'Is the refugee/non-refugee distinction a genuine natural joint in the structure of human displacement, or a constructed category that serves state interests?',
    'Empirical analysis of displacement drivers: if persecution and economic desperation are empirically separable populations with distinct protection needs, the category tracks nature; if they are overlapping and co-constitutive, the category is constructed.',
    'If natural, the constraint approaches mountain (low extraction from true economic migrants). If constructed, the extraction is structural — the category itself produces the victim set it excludes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_boundary_naturalness, conceptual, 'Whether the binary category reflects reality or constructs it').

omega_variable(
    deterrence_as_extraction,
    'Is the deterrence infrastructure (detention, interdiction, externalization) a necessary enforcement cost of the refugee distinction, or has it become an independent extraction mechanism?',
    'Cost-benefit analysis of deterrence measures against protection outcomes: if deterrence reduces irregular migration without reducing refugee protection access, it is enforcement; if it reduces both, it is extraction.',
    'If extraction, the constraint''s ε is higher than the coordination function justifies — the theater ratio understates the divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_as_extraction, empirical, 'Whether enforcement has become self-justifying extraction').

omega_variable(
    reading_relations_stability,
    'Does the humanitarian obligation reading structurally coexist with the sovereignty reading, or does the sovereignty reading''s growing dominance foreclose the humanitarian reading''s operational space?',
    'Track state practice: if states increasingly invoke sovereignty to override non-refoulement (pushbacks, safe third country, extraterritorial processing), the coexistence is destabilizing toward foreclosure.',
    'If foreclosure, the constraint family is collapsing into a single reading (sovereignty), reclassifying the humanitarian obligation as a vestigial piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relations_stability, empirical, 'Whether sibling readings remain in stable coexistence or are shifting toward foreclosure').

omega_variable(
    mixed_motive_category_gap,
    'Is the mixed-motive migrant category a genuine structural gap in the constraint, or a marginal edge case?',
    'Quantitative analysis of asylum claims: proportion involving mixed persecution/economic drivers; outcomes for this group vs. pure persecution claims.',
    'If structural gap, the constraint''s extraction is systematically higher than its binary logic acknowledges — the bifurcated victim set is actually tripartite.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mixed_motive_category_gap, empirical, 'Whether the binary category''s internal contradiction produces a distinct victim population').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1951, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1951, 0.1).
narrative_ontology:measurement(bord_tr_t1967, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(bord_tr_t1980, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(bord_tr_t1990, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(bord_tr_t2000, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(bord_tr_t2010, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(bord_tr_t2024, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(bord_be_t1951, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1951, 0.25).
narrative_ontology:measurement(bord_be_t1967, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement(bord_be_t1980, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(bord_be_t1990, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(bord_be_t2000, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(bord_be_t2010, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(bord_be_t2024, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1951, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1951, 0.4).
narrative_ontology:measurement(bord_su_t1967, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1967, 0.45).
narrative_ontology:measurement(bord_su_t1980, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(bord_su_t1990, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(bord_su_t2000, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(bord_su_t2010, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(bord_su_t2024, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__humanitarian_obligation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_legitimacy__humanitarian_obligation_reading, 0.12).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, asylum_determination_procedures).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, externalization_policies).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, complementary_protection_regimes).

% DUAL FORMULATION NOTE:
% This constraint is one member of the border_legitimacy kernel family. The three readings (humanitarian_obligation, sovereignty, freedom_of_movement) share the kernel's committer structure but instantiate different constraints with different ε, beneficiaries, victims, and types. The humanitarian reading's ε (0.42) is moderate — lower than the sovereignty reading's enforcement extraction but higher than the freedom of movement reading's near-zero. The family is linked through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_legitimacy__humanitarian_obligation_reading, institutional, 0.15).
constraint_indexing:directionality_override(border_legitimacy__humanitarian_obligation_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
