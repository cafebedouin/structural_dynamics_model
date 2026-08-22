% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Domain-Partitioned Practice Legitimacy (Dual Equilibrium Reading)
 *   domain: political_history/institutional_change/modernization_studies
 *
 * SUMMARY:
 *   In societies transitioning toward state-centered administration and
 *   international integration, practice legitimacy often settles into a
 *   dual-domain equilibrium: state authority governs public/administrative
 *   domains (calendar systems for taxation, dress codes for official
 *   functions, standardized weights/measures for commerce), while traditional
 *   authority retains jurisdiction over private/ritual domains (lunar
 *   calendars for agriculture and festivals, traditional dress for
 *   ceremonies, vernacular language for intimate community). This reading
 *   treats the partition not as temporary or transitional, but as a stable
 *   coordinated arrangement. The state consolidates its reach in domains
 *   essential for administration; traditional authorities retain cultural
 *   legitimacy and community authority in domains the state leaves untouched.
 *   Practitioners navigate by switching contexts: Gregorian for contracts,
 *   lunar for planting; Western formal wear for work, traditional dress for
 *   home. The arrangement prevents convergence—it actively suppresses
 *   synthesis and integration—and extracts a cost from those caught between
 *   domains and those attempting cultural fusion.
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus: enforcer of public-domain practice standardization, benefits from uniform administration and tax collection
 *   - traditional_authority_holders: custodians of ritual-domain practice, benefit from state recognition of a protected cultural sphere
 *   - practitioners_caught_between_domains: forced to maintain dual compliance, strategic context-switching, identity fragmentation
 *   - those_attempting_cultural_synthesis: face resistance from both state and traditional authorities, payers of the suppression of integration
 *   - younger_generation_in_transition: structurally excluded from legitimacy partition negotiation, potential carriers of alternative readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.58).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.62).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Domain-Partitioned Practice Legitimacy (Dual Equilibrium Reading)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political_history/institutional_change/modernization_studies").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '5b58e455-6f82-45e1-b369-f1f462b183fb').
narrative_ontology:cs_kernel_codification('5b58e455-6f82-45e1-b369-f1f462b183fb', distributed).
narrative_ontology:cs_authority_grounding('5b58e455-6f82-45e1-b369-f1f462b183fb', extraction).
narrative_ontology:cs_interpretation_layer_present('5b58e455-6f82-45e1-b369-f1f462b183fb').
narrative_ontology:cs_reading_relation('5b58e455-6f82-45e1-b369-f1f462b183fb', legitimacy_of_practice_standardization__endogenous_displacement_reading, forecloses).
narrative_ontology:cs_reading_relation('5b58e455-6f82-45e1-b369-f1f462b183fb', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('5b58e455-6f82-45e1-b369-f1f462b183fb', foundational, dual_authority_legitimacy).
narrative_ontology:cs_axiom_status(dual_authority_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('5b58e455-6f82-45e1-b369-f1f462b183fb', dual_authority_legitimacy, conventional).
narrative_ontology:cs_axiom('5b58e455-6f82-45e1-b369-f1f462b183fb', foundational, permanent_domain_partition).
narrative_ontology:cs_axiom_status(permanent_domain_partition, holdable).
narrative_ontology:cs_axiom_grounding('5b58e455-6f82-45e1-b369-f1f462b183fb', permanent_domain_partition, conventional).
narrative_ontology:cs_reference_frame('5b58e455-6f82-45e1-b369-f1f462b183fb', negotiated_domain_partition).
narrative_ontology:cs_drift_state('5b58e455-6f82-45e1-b369-f1f462b183fb', contemporary_globalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5b58e455-6f82-45e1-b369-f1f462b183fb', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authority_holders).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, practitioners_caught_between_domains).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, those_attempting_cultural_synthesis).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexistence_without_convergence).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, strategic_compliance_as_equilibrium).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes standardized practice regimes for public domains: Gregorian calendar for taxation and bureaucratic record-keeping, metric systems for commerce, Western dress codes for official functions. Enforces compliance through licensing, permits, and documentation requirements. Benefits from uniform administration and international alignment. Controls the boundary between public and private domains through regulation.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Maintain customary practice legitimacy in private/ritual domains: lunar calendars for agricultural cycles and festivals, traditional dress for ceremonies and home life, vernacular language for intimate community. Derive authority from lineage, cultural continuity, and spiritual grounding. Benefit from state recognition of a separate ritual domain where traditional authority is unchallenged. Cannot exit without cultural identity dissolution.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authority_holders, agenda_setter,
    organized, civilizational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authority_holders, beneficiary).

% Navigate dual compliance: maintain traditional practices for family/community legitimacy (lunar calendar for planting, traditional dress for ceremonies) while adopting state-mandated practices for economic participation (Gregorian calendar for contracts, Western formal wear for workplace). The partition prevents synthesis—they must adopt and abandon practices based on domain, not integrate them. Strategic switching between contexts is exhausting and creates identity fragmentation.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, practitioners_caught_between_domains, payer,
    moderate, biographical, constrained, regional).

% Seek to blend traditional and modern practices (wearing traditional dress to state functions, using lunar calendar alongside Gregorian, adapting ritual elements into public space). Face resistance from both state authorities (who interpret synthesis as incomplete modernization) and traditional authorities (who interpret it as degradation of sacred forms). The partition actively prevents their preferred equilibrium of integrated practice.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, those_attempting_cultural_synthesis, payer,
    moderate, biographical, identity_locked, regional).

% Are shaped by dual socialization but increasingly question the partition's naturalness. They experience the boundary between domains as artificial constraint rather than organic separation. Would advocate for either convergence toward state norms or genuine pluralism in public domains, but are typically excluded from legitimacy negotiations between state apparatus and traditional authorities.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, younger_generation_in_transition, excluded,
    moderate, biographical, identity_locked, national).

% Study the partition as evidence for managed modernization or as failure of incomplete convergence. Observe outcomes: stability of dual regimes, persistence of resistance, degree of internalization vs. strategic compliance. Serve as witnesses to the constraint's functioning and sources of alternative framings.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernization_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of heterogeneous legitimacy: permits simultaneous governance under two non-identical legitimacy regimes (state/administrative and traditional/ritual) without requiring a single overarching legitimacy framework. Enables state consolidation and international alignment in public domains while preserving cultural continuity and community authority in private domains.
% TRANSFER_FUNCTION: Transfers authority recognition: state apparatus gains unchallenged jurisdiction over public/administrative domains and collects compliance and tax from that jurisdiction; traditional authorities retain jurisdiction over private/ritual domains and collect cultural authority and community compliance there. Both extract the price of dual compliance from practitioners who must maintain two distinct practice sets.
% ABSENT_VOICES: Younger generation and those attempting cultural synthesis are structurally excluded from the legitimacy-partition negotiation. Advocates for cultural pluralism in public space, or for convergence toward traditional practice, have no voice in the arrangement's maintenance.
% DISAPPEARANCE_RATIONALE: If the partition vanished, the world could reorganize in multiple directions: convergence toward state norms (exogenous_override_reading path), organic synthesis from lived practice (endogenous_displacement_reading path), or fragmentation into local heterogeneity. The partition actively prevents the synthesis path; its disappearance would open that possibility.
% FOUNDING_PROBLEM: How to consolidate state administrative authority and achieve international alignment while preserving cultural legitimacy and preventing wholesale displacement of traditional authority—without requiring either complete cultural assimilation or state authority abdication.
% FOUNDING_PROBLEM_CORROBORATION: Both state administrators (need standardized domains for fiscal and legal order) and traditional authorities (need protected ritual space for cultural continuity) attest the founding problem remains live. Practitioners and anthropologists attest from outside the benefiting parties that the dual-domain structure is maintained precisely because it solves this problem, though its human costs are substantial.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.58 reflects the partition's core structure: both the state apparatus and traditional authorities extract compliance and authority recognition from practitioners who must maintain two distinct practice sets. The extraction is substantial but not total—both authorities genuinely solve part of their respective beneficiaries' legitimacy problem. Suppression at 0.62 reflects the active enforcement preventing synthesis: state authorities reject traditional practices in public domains as 'backward' or 'nonstandard'; traditional authorities reject state practices in private domains as 'contaminating' or 'inauthentic'; together they foreclose the integration path that practitioners attempting synthesis pursue. Theater_ratio at 0.48 reflects moderate performance: the state genuinely needs administrative standardization, and traditional authorities genuinely maintain ritual functions, but a growing share of the constraint's enforcement is devoted to preventing domain boundary crossing rather than serving the original functions. The measurement series tracks slight extraction escalation over the interval (0.48→0.58) as state reach expands, while suppression holds stable then slightly decreases at the end (suggesting potential pressure toward partition renegotiation). The shared time grid ensures every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus seat, the partition is natural and necessary—genuine administrative coordination that happens to leave room for cultural practice. From the traditional authority seat, it is a negotiated compromise that protects cultural continuity against state homogenization. From the practitioner seat, it is an imposed extraction requiring constant context-switching and preventing preferred integration. From the younger-generation seat (excluded from the negotiation), it appears as an artificial constraint invented by their parents' generation, increasingly obviously so as global connectivity exposes alternative models. The engine computes these divergences from the structural data: same constraint, radically different extraction profiles per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus sits at high d (near full target of extraction from practitioners and traditional authorities; near full beneficiary from administrative standardization). Traditional authorities sit at moderate-high d, asymmetrically: they are beneficiaries of the protected ritual domain but also bear costs of partition maintenance and constrained ritual space (they cannot expand traditional practice into public domains). Practitioners caught between domains sit at very high d (they bear the full dual-compliance cost without initiating the partition). Those attempting synthesis sit at maximum d—they are the direct targets of suppression from both benefiting parties. The younger generation, though excluded from authority negotiation, is identity-locked into the partition by socialization. The partition's durability depends on keeping directionalities asymmetric: state gets high benefit with low cost (suppression is handled by traditional authorities as much as by state agents); traditional authorities get legitimacy with constrained scope; practitioners pay in both compliance and identity fragmentation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('how to consolidate state authority while preserving cultural legitimacy') is genuinely live—but the partition's solution is increasingly decoupled from its stated function. The state no longer merely coordinates public administration; it increasingly SUPPRESSES synthesis in the name of modernization. Traditional authorities no longer merely preserve ritual; they increasingly POLICE the boundary against contamination. Practitioners pay the extraction not for coordination they benefit from, but for enforcement preventing their preferred integration. The theater_ratio's trajectory suggests growing performative maintenance: both authorities spend increasing effort appearing to maintain the partition rather than achieving its original function. The constraint qualifies as tangled_rope because it has genuine coordination content (state genuinely needs administrative standardization; traditional authorities genuinely need protected ritual space; practitioners do benefit from knowing which rules apply in which context) AND asymmetric extraction (both benefiting parties extract from practitioners). Mandatrophy is approaching but not yet resolved: the founding problem is live, but the arrangement increasingly extracts more than it coordinates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_internalization_vs_strategic,
    'Is compliance with the practice partition internalized (practitioners genuinely believe the domains are separate and each requires its own practice set) or purely strategic (practitioners comply to avoid sanction but do not endorse the partition)?',
    'Ethnographic work tracking practitioners'' own justifications for domain-switching; post-partition scenarios where enforcement lapses; intergenerational transmission of partition-acceptance.',
    'If internalized, the partition is a natural-seeming equilibrium and stable indefinitely; if strategic, it is vulnerable to pressure and will degrade when enforcement costs rise or exit options appear.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_internalization_vs_strategic, empirical, 'Whether the partition is an internalized norm or an enforced constraint.').

omega_variable(
    boundary_permeability_variability,
    'Is the public/private boundary uniformly observed across all practice domains, or are some domains (e.g., language, dress, dietary practice) more permeable to cross-domain adoption than others (e.g., calendar systems, weights/measures)?',
    'Comparative analysis of different practice domains within the same partition regime; tracking which domains show most synthesis pressure and least suppression.',
    'If boundaries vary, the partition is not a unified coordination structure but a coalition of multiple constraints; the reading would need decomposition into separate stories per domain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_permeability_variability, conceptual, 'Whether the dual-equilibrium reading applies uniformly or fragments by domain.').

omega_variable(
    synthesis_suppression_mechanism,
    'Is suppression of synthesis maintained by formal state prohibition (law, regulation, licensing denial) or by internalized social enforcement (both state and traditional authorities socially delegitimize synthesis attempts)?',
    'Tracking explicit legal barriers vs. social-enforcement mechanisms; cases where formal barriers are absent but social barriers remain; jurisdictions where formal barriers are lifted.',
    'If formal, the suppression can be lifted by state decree; if internalized, lifting formal barriers does not dissolve the suppression—the constraint persists via theater and social pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthesis_suppression_mechanism, empirical, 'Whether suppression is structural or internalized.').

omega_variable(
    alternative_readings_kernel_contest,
    'Is the dual_practice_equilibrium_reading a stable equilibrium or a transitional stage between endogenous_displacement and exogenous_override? Does historical trajectory suggest convergence toward one sibling reading?',
    'Long-term historical tracking of whether partitions harden or soften; whether younger generations increasingly adopt synthesis as norm; whether state authority gradually encroaches on protected domains.',
    'If transitional, the reading''s persistence is temporary; if stable, the partition is self-maintaining. The engine''s foreclosure/coexistence logic depends on this assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_readings_kernel_contest, conceptual, 'Whether the equilibrium is stable or a transient stage in a longer legitimacy shift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 8, 0.39).
narrative_ontology:measurement_basis(legi_tr_t8, observed).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 16, 0.43).
narrative_ontology:measurement_basis(legi_tr_t16, observed).
narrative_ontology:measurement(legi_tr_t25, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 25, 0.46).
narrative_ontology:measurement_basis(legi_tr_t25, observed).
narrative_ontology:measurement(legi_tr_t38, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 38, 0.48).
narrative_ontology:measurement_basis(legi_tr_t38, observed).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement_basis(legi_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(legi_be_t8, observed).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement_basis(legi_be_t16, observed).
narrative_ontology:measurement(legi_be_t25, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 25, 0.57).
narrative_ontology:measurement_basis(legi_be_t25, observed).
narrative_ontology:measurement(legi_be_t38, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 38, 0.59).
narrative_ontology:measurement_basis(legi_be_t38, observed).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(legi_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(legi_su_t8, observed).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement_basis(legi_su_t16, observed).
narrative_ontology:measurement(legi_su_t25, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 25, 0.63).
narrative_ontology:measurement_basis(legi_su_t25, observed).
narrative_ontology:measurement(legi_su_t38, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 38, 0.65).
narrative_ontology:measurement_basis(legi_su_t38, observed).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(legi_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% Three readings of a single kernel: legitimacy_of_practice_standardization. This reading (dual_practice_equilibrium) treats legitimacy as domain-partitioned and permanent. The endogenous_displacement reading traces legitimacy to organic practice adoption. The exogenous_override reading traces legitimacy to state mandates. All three readings share the same referent (practice standardization in modernizing societies) but disagree on what authority makes standardization legitimate. The three readings are linked by network.affects_constraints: changes in how the dual equilibrium operates (e.g., partition erosion) cascade to both sibling readings' empirical plausibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
