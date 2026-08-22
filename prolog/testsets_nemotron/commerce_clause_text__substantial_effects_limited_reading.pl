% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__substantial_effects_limited_reading, []).

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
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Substantial Effects Test with Economic/Non-Economic Nexus Limit
 *   domain: constitutional_law/federalism/commerce_regulation
 *
 * SUMMARY:
 *   This constraint story captures the 'substantial effects limited reading'
 *   of the Commerce Clause — the doctrinal position that federal power
 *   reaches intrastate economic activity with substantial aggregate effects
 *   on interstate commerce, but requires a jurisdictional nexus linking the
 *   regulated activity to interstate commerce and rejects pretextual
 *   regulation of non-economic activity under the commerce power. This
 *   reading emerged from the Lopez/Morrison/Raich trilogy and represents a
 *   middle position between the expansive federal reading (Wickard/Raich
 *   majority) and the originalist narrow reading (Thomas/early Rehnquist).
 *   The constraint operates as a category-policing mechanism: the
 *   economic/non-economic distinction and the 'jurisdictional nexus'
 *   requirement function as the structural limits that distinguish legitimate
 *   federal economic regulation from impermissible federal police power.
 *
 * KEY AGENTS:
 *   - federal_economic_regulators: Primary agenda_setter (institutional/generational/arbitrage/global) — defines the regulatory perimeter, benefits from expanded authority
 *   - national_market_participants: Beneficiary (organized/biographical/mobile/national) — gains uniform federal standards, preemption of state variation
 *   - interstate_business_consortia: Beneficiary (powerful/biographical/arbitrage/national) — captures regulatory stability and scale advantages
 *   - state_police_power_authorities: Victim (institutional/generational/constrained/national) — loses regulatory autonomy over local activity classified as economic
 *   - local_economic_actors_in_ambiguous_categories: Victim (moderate/biographical/constrained/regional) — faces uncertain federal preemption depending on category classification
 *   - non_economic_civil_society_organizations: Victim (organized/biographical/trapped/local) — subject to federal preemption when their activity is reclassified as economic
 *   - federal_courts: Observer/agenda_setter (analytical/civilizational/analytical/universal) — polices the category boundary through judicial review
 *   - congress: Agenda_setter (institutional/generational/arbitrage/national) — legislates at the edge of the nexus requirement, drafts jurisdictional hooks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.35).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.45).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Substantial Effects Test with Economic/Non-Economic Nexus Limit").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional_law/federalism/commerce_regulation").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, '8acfecfb-29db-4675-b36c-d2b9be4805c5').
narrative_ontology:cs_kernel_codification('8acfecfb-29db-4675-b36c-d2b9be4805c5', formalized).
narrative_ontology:cs_authority_grounding('8acfecfb-29db-4675-b36c-d2b9be4805c5', lineage).
narrative_ontology:cs_interpretation_layer_present('8acfecfb-29db-4675-b36c-d2b9be4805c5').
narrative_ontology:cs_reading_relation('8acfecfb-29db-4675-b36c-d2b9be4805c5', commerce_clause_text__expansive_federal_reading, influences).
narrative_ontology:cs_reading_relation('8acfecfb-29db-4675-b36c-d2b9be4805c5', commerce_clause_text__originalist_narrow_reading, forecloses).
narrative_ontology:cs_axiom('8acfecfb-29db-4675-b36c-d2b9be4805c5', foundational, substantial_effects_test_with_nexus_requirement).
narrative_ontology:cs_axiom_status(substantial_effects_test_with_nexus_requirement, holdable).
narrative_ontology:cs_axiom_grounding('8acfecfb-29db-4675-b36c-d2b9be4805c5', substantial_effects_test_with_nexus_requirement, conventional).
narrative_ontology:cs_axiom('8acfecfb-29db-4675-b36c-d2b9be4805c5', foundational, economic_non_economic_distinction_as_jurisdictional_limit).
narrative_ontology:cs_axiom_status(economic_non_economic_distinction_as_jurisdictional_limit, holdable).
narrative_ontology:cs_axiom_grounding('8acfecfb-29db-4675-b36c-d2b9be4805c5', economic_non_economic_distinction_as_jurisdictional_limit, conventional).
narrative_ontology:cs_axiom('8acfecfb-29db-4675-b36c-d2b9be4805c5', secondary, non_pretextual_regulation_requirement).
narrative_ontology:cs_axiom_status(non_pretextual_regulation_requirement, holdable).
narrative_ontology:cs_axiom_grounding('8acfecfb-29db-4675-b36c-d2b9be4805c5', non_pretextual_regulation_requirement, conventional).
narrative_ontology:cs_reference_frame('8acfecfb-29db-4675-b36c-d2b9be4805c5', post_lopez_morrison_raich_trilogy).
narrative_ontology:cs_drift_state('8acfecfb-29db-4675-b36c-d2b9be4805c5', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8acfecfb-29db-4675-b36c-d2b9be4805c5', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, federal_economic_regulators).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, national_market_participants).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, interstate_business_consortia).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, state_police_power_authorities).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, local_economic_actors_in_ambiguous_categories).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, non_economic_civil_society_organizations).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, economic_regulation_distinct_from_police_power).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, jurisdictional_nexus_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, non_pretextual_federal_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and defines the regulatory perimeter of federal economic authority. Drafts regulations with jurisdictional nexus hooks, litigates to defend the economic/non-economic boundary, and benefits from expanded regulatory scope. Can forum-shop across agencies and judicial circuits; exit from constraint is not meaningful — they are the constraint's architects.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_economic_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Legislates at the edge of the nexus requirement, including jurisdictional hooks in statutes to satisfy the substantial effects test. Benefits from maximal legislative latitude; constrained only by judicial enforcement of the boundary. Can amend statutes to cure nexus defects; exit means abandoning federal regulatory objectives.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, congress, agenda_setter,
    institutional, generational, arbitrage, national).

% Large firms operating across state lines who gain uniform federal standards and preemption of inconsistent state regulations. They lobby for federal regulation that creates national markets but bear compliance costs. Can relocate operations or restructure to optimize regulatory treatment; exit is costly but feasible.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, national_market_participants, beneficiary,
    organized, biographical, mobile, national).

% Industry associations and trade groups that capture regulatory stability and scale advantages through federal preemption. They invest heavily in shaping the economic/non-economic classification of their activities. Have significant influence over congressional drafting and agency rulemaking; exit means accepting state-by-state regulation.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, interstate_business_consortia, beneficiary,
    powerful, biographical, arbitrage, national).

% State attorneys general, legislatures, and regulatory agencies that lose regulatory autonomy when local activity is classified as economic with substantial interstate effects. They litigate to defend state authority (Lopez, Morrison, NFIB) but cannot exit the federal system. Their resistance is channeled through judicial review and political safeguards of federalism.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, state_police_power_authorities, payer,
    institutional, generational, constrained, national).

% Small businesses, local producers, and regional enterprises whose activities sit near the economic/non-economic boundary (e.g., local food production, intrastate professional services, community-scale manufacturing). They face unpredictable federal preemption depending on how courts classify their activity. Exit means restructuring to avoid the classification or accepting federal regulation.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, local_economic_actors_in_ambiguous_categories, payer,
    moderate, biographical, constrained, regional).

% Religious organizations, educational institutions, family associations, and community groups whose non-commercial activities risk reclassification as economic when they have aggregate effects on interstate markets (e.g., private education affecting labor markets, religious organizations employing staff). They lack the resources to litigate classification and cannot exit the regulatory reach without abandoning their mission.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, non_economic_civil_society_organizations, payer,
    organized, biographical, trapped, local).

% Polices the category boundary through judicial review. Administers the nexus requirement and economic/non-economic distinction. Their institutional legitimacy depends on maintaining the appearance of a principled limit rather than outcome-driven federalism. They neither collect nor pay the constraint's extraction but their interpretive choices determine which seats benefit.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_courts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables federal regulation of genuinely national economic problems that states cannot solve individually — collective action problems in interstate markets (e.g., civil rights in public accommodations affecting interstate travel, national environmental standards, financial market regulation, labor standards affecting mobile workforces). The substantial effects test with nexus requirement provides a structural trigger for federal authority while the economic/non-economic limit preserves state police power over non-economic local activity.
% TRANSFER_FUNCTION: Moves regulatory authority over intrastate economic activity with substantial interstate effects from state police power to federal economic regulation. The transfer operates through the category boundary: when activity is classified as 'economic' with a 'jurisdictional nexus' to interstate commerce, federal authority displaces state authority. The extraction is regulatory sovereignty — states lose the power to regulate (or not regulate) the activity; federal regulators and national market actors gain uniformity and preemption.
% ABSENT_VOICES: Future generations who will inherit the constitutional structure shaped by current category boundary decisions; local communities whose non-economic social fabric is not represented in the economic/non-economic calculus; indigenous nations whose regulatory sovereignty operates outside the state/federal binary but is affected by Commerce Clause preemption. These voices are structurally excluded because the constraint's seats are defined by current institutional actors in the federalism dispute.
% DISAPPEARANCE_RATIONALE: If the substantial effects limited reading vanished overnight, either the expansive federal reading would prevail (federal power over all economic activity with aggregate effects, no meaningful nexus limit — dramatic expansion of federal authority) or the originalist narrow reading would prevail (federal power limited to cross-border trade — dramatic contraction). The national regulatory state for economic matters would either lose its limiting principle or its constitutional foundation. The world of federal economic regulation would fundamentally rearrange.
% FOUNDING_PROBLEM: The Articles of Confederation failed because states could not coordinate responses to national economic problems — trade barriers, currency instability, debt crises, inability to regulate commerce that spilled across state lines. The Commerce Clause was designed to give the federal government authority over genuinely interstate economic problems while preserving state police power over local non-economic life. The substantial effects doctrine (post-1937) responded to the Great Depression by recognizing that intrastate activity could have aggregate effects requiring federal response.
% FOUNDING_PROBLEM_CORROBORATION: The original founding problem (interstate trade barriers, national economic coordination) is attested by the Constitutional Convention records and Federalist Papers (outside the benefiting parties). The post-1937 expansion's founding problem (national economic crisis response) is attested by New Deal legislative history and labor history. However, whether the CURRENT configuration of the substantial effects test with nexus limits still solves a live founding problem — or has become a vehicle for regulatory capture and category manipulation — is contested between federal regulators (who attest it remains live) and state sovereignty advocates (who attest the problem is solved and the arrangement persists as extraction). No neutral arbiter exists; the corroboration split mirrors the doctrinal dispute.
narrative_ontology:disappearance_verdict(commerce_clause_text__substantial_effects_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__substantial_effects_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__substantial_effects_limited_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).
:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is a Tangled Rope because it performs genuine coordination (enabling federal regulation of national economic problems that states cannot solve individually — e.g., civil rights in public accommodations affecting interstate travel, national environmental standards for pollutants crossing state lines) while simultaneously extracting regulatory authority from states over local non-economic activity. The extraction is asymmetric: states lose police power autonomy (victims), federal regulators and national market actors gain authority and uniformity (beneficiaries). Active enforcement is required — the category boundary must be policed through judicial review (Lopez, Morrison, NFIB, Raich). Theater ratio is moderate (0.25) because the nexus requirement and economic/non-economic distinction have real doctrinal content but are increasingly deployed as outcome-calibrated tools. Accessibility collapse (0.4) reflects that alternatives (state regulation, private ordering) persist but are constrained by preemption. Resistance (0.55) is substantial from states and originalist jurisprudence.
 *
 * PERSPECTIVAL GAP:
 *   From the federal regulator seat, the constraint is genuine coordination — solving collective action problems in national markets. From the state police power seat, the same structure operates as extraction — federal authority displaces state experimentation under a manipulable category test. From the local actor seat, the constraint is experienced as unpredictable preemption risk. The engine computes this divergence from the structural data: the nexus requirement and economic/non-economic distinction create real coordination value for some seats while extracting sovereignty from others. The claimed type (Tangled Rope) reflects the authoring seat's assessment that both functions are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal economic regulators and Congress are structural beneficiaries (d near 0.0-0.2): they collect regulatory authority and legislative latitude. National market participants and interstate business consortia are beneficiaries (d ~0.1-0.3): they gain preemption and uniformity but bear compliance costs. State police power authorities are primary targets (d ~0.7-0.9): they lose regulatory jurisdiction over activities reclassified as economic with interstate effects. Local economic actors in ambiguous categories are targets (d ~0.6-0.8): they face regulatory uncertainty and potential federal preemption. Non-economic civil society organizations are trapped targets (d ~0.8-0.95): they lack exit from federal power when their activity is reclassified. Federal courts sit near analytical (d ~0.5): they administer the boundary but their institutional interests align with maintaining the policing function.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enabling federal response to national economic crises the states could not address) remains live but contested. The constraint has not resolved its mandatrophy — the economic/non-economic boundary continues to migrate, and the nexus requirement functions as a variable gate rather than a fixed limit. The constraint persists because no stable alternative coordination mechanism for national economic regulation has been institutionalized, and the extraction from state police power is diffuse enough that no single coalition can dislodge it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_non_economic_boundary_stability,
    'Is the economic/non-economic distinction a stable jurisdictional boundary or an inherently contested category that shifts with political composition of the Court?',
    'Longitudinal analysis of Commerce Clause jurisprudence tracking category migration — whether activities classified as ''economic'' expand/contract systematically with doctrinal eras.',
    'If the boundary is unstable, the constraint''s coordination function degrades into category manipulation; the substantial effects test becomes a vehicle for outcome-driven federalism rather than a structural limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_non_economic_boundary_stability, conceptual, 'Stability of the economic/non-economic category boundary as a jurisdictional limit').

omega_variable(
    substantial_effects_measurement_ambiguity,
    'Does ''substantial effects'' denote an empirically measurable threshold or a doctrinal placeholder for judicial discretion?',
    'Comparative analysis of congressional findings vs. judicial review standards across cases — whether evidentiary thresholds are consistently applied or calibrated to preferred outcomes.',
    'If purely discretionary, the nexus requirement collapses into a pretextuality test with no independent content; the constraint becomes a Snare masking outcome selection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substantial_effects_measurement_ambiguity, empirical, 'Whether substantial effects has operational content beyond judicial preference').

omega_variable(
    reading_foreclosure_structure,
    'Does this reading''s core premise (federal power over genuinely economic intrastate activity with nexus) logically foreclose the originalist narrow reading within a single constitutional framework, or do they merely coexist as competing interpretive positions?',
    'Structural analysis of whether a framework committing to ''substantial effects on interstate commerce'' as the constitutional standard can simultaneously maintain ''interstate commerce limited to cross-border trade'' as its definition — the two premises operate at different levels of the interpretive stack.',
    'If forecloses, the kernel has genuine structural incompatibility between readings; if coexists_with, the dispute is political-institutional rather than logical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Logical relationship between substantial effects limited reading and originalist narrow reading').

omega_variable(
    category_policing_as_extraction_mechanism,
    'Is the economic/non-economic category boundary policing itself an extraction mechanism — does classifying regulation as ''economic'' vs ''non-economic'' determine which regulatory constituency captures federal authority?',
    'Interest-group analysis of which regulated entities benefit from economic classification (federal preemption, uniform standards) vs. non-economic classification (state experimentation, local control).',
    'If category assignment distributes regulatory rents, the constraint''s coordination function is entangled with extraction — reinforcing Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_policing_as_extraction_mechanism, empirical, 'Whether category boundary policing allocates regulatory capture rents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(comm_tr_t1964, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1964, 0.08).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(comm_tr_t2012, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2012, 0.24).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1937, 0.2).
narrative_ontology:measurement(comm_be_t1964, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1964, 0.15).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(comm_be_t2012, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2012, 0.33).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1937, 0.3).
narrative_ontology:measurement(comm_su_t1964, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1964, 0.25).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(comm_su_t2012, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2012, 0.43).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__substantial_effects_limited_reading, 0.1).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, necessary_and_proper_clause__incidental_powers_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, tenth_amendment__state_sovereignty_residuum_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings in the commerce_clause_text constraint family. The expansive_federal_reading treats the substantial effects test as effectively unlimited (no meaningful nexus requirement). The originalist_narrow_reading rejects the substantial effects test entirely. This reading accepts the test but imposes structural limits (nexus + non-pretextual economic regulation). The three stories share the same kernel text but instantiate different constraints with different ε, beneficiaries, victims, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_text__substantial_effects_limited_reading, institutional, 0.15).
constraint_indexing:directionality_override(commerce_clause_text__substantial_effects_limited_reading, organized, 0.25).
constraint_indexing:directionality_override(commerce_clause_text__substantial_effects_limited_reading, moderate, 0.65).
constraint_indexing:directionality_override(commerce_clause_text__substantial_effects_limited_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
