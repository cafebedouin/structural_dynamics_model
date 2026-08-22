% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__compact_federalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__compact_federalism, []).

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
 *   constraint_id: provincial_sovereignty_boundary__compact_federalism
 *   human_readable: Compact Federalism — Provincial Residual Sovereignty
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the compact_federalism reading of the
 *   provincial_sovereignty_boundary kernel. It models Confederation as a
 *   compact among sovereign provinces where residual sovereignty is retained,
 *   federal authority is conditional on provincial consent, equalization is
 *   perpetually negotiable, climate policy is subject to provincial override,
 *   and exit from the federation requires negotiation rather than permission.
 *   The competing readings — constitutional_subordination (provinces as
 *   creatures of the federal constitution) and resource_sovereignty_primacy
 *   (resource control grounds absolute sovereignty) — are structurally
 *   distinct constraints with different ε values, beneficiary/victim
 *   structures, and type classifications. They are linked via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, 0.42).
domain_priors:suppression_score(provincial_sovereignty_boundary__compact_federalism, 0.35).
domain_priors:theater_ratio(provincial_sovereignty_boundary__compact_federalism, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, extractiveness, 0.42).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__compact_federalism, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__compact_federalism, "Compact Federalism — Provincial Residual Sovereignty").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__compact_federalism, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__compact_federalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, '141348cf-121e-4cac-9a07-aca4bbf66514').
narrative_ontology:cs_kernel_codification('141348cf-121e-4cac-9a07-aca4bbf66514', fixed_text).
narrative_ontology:cs_authority_grounding('141348cf-121e-4cac-9a07-aca4bbf66514', lineage).
narrative_ontology:cs_interpretation_layer_present('141348cf-121e-4cac-9a07-aca4bbf66514').
narrative_ontology:cs_reading_relation('141348cf-121e-4cac-9a07-aca4bbf66514', provincial_sovereignty_boundary__constitutional_subordination, coexists_with).
narrative_ontology:cs_reading_relation('141348cf-121e-4cac-9a07-aca4bbf66514', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('141348cf-121e-4cac-9a07-aca4bbf66514', foundational, confederation_as_compact_among_sovereigns).
narrative_ontology:cs_axiom_status(confederation_as_compact_among_sovereigns, holdable).
narrative_ontology:cs_axiom_grounding('141348cf-121e-4cac-9a07-aca4bbf66514', confederation_as_compact_among_sovereigns, conventional).
narrative_ontology:cs_axiom('141348cf-121e-4cac-9a07-aca4bbf66514', foundational, provincial_consent_required_for_federal_authority).
narrative_ontology:cs_axiom_status(provincial_consent_required_for_federal_authority, holdable).
narrative_ontology:cs_axiom_grounding('141348cf-121e-4cac-9a07-aca4bbf66514', provincial_consent_required_for_federal_authority, conventional).
narrative_ontology:cs_axiom('141348cf-121e-4cac-9a07-aca4bbf66514', secondary, exit_requires_negotiation_not_permission).
narrative_ontology:cs_axiom_status(exit_requires_negotiation_not_permission, holdable).
narrative_ontology:cs_axiom_grounding('141348cf-121e-4cac-9a07-aca4bbf66514', exit_requires_negotiation_not_permission, conventional).
narrative_ontology:cs_reference_frame('141348cf-121e-4cac-9a07-aca4bbf66514', founding_compact_1867).
narrative_ontology:cs_drift_state('141348cf-121e-4cac-9a07-aca4bbf66514', contemporary_climate_equity_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('141348cf-121e-4cac-9a07-aca4bbf66514', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, provincial_governments).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, resource_extraction_interests).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, federal_policy_implementation).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, interprovincial_equity_recipients).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, climate_action_coordination).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, federal_government).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, federal_government).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__compact_federalism, founding_compact_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__compact_federalism, residual_sovereignty_principle).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__compact_federalism, equalization_as_negotiable_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate federal-provincial agreements, set resource royalties, administer equalization negotiations, and can threaten or initiate constitutional crisis to defend residual sovereignty. Their authority is fused with the compact narrative — their legitimacy derives from being the compact's parties. Exit from the federation is structurally negotiable but politically existential.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, provincial_governments, agenda_setter,
    institutional, generational, identity_locked, regional).

% Must secure provincial consent for national programs (health, climate, trade) through spending power and intergovernmental agreements. Pays fiscal transfers and policy concessions to maintain cooperation. Benefits from a functional federation but cannot compel provincial compliance on exclusive jurisdictions. Exit from the federal role is not an option; constraint is managing fragmentation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_government, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, federal_government, beneficiary).

% Operate under provincial regulatory regimes that compete for investment through royalty rates and environmental standards. Provincial control of resources (s.92A) creates a structural subsidy: provinces underprice resource access to attract capital, and the compact narrative shields this from federal override. Can relocate capital across provincial or national borders.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, resource_extraction_interests, beneficiary,
    organized, biographical, mobile, regional).

% Residents of less-resourced provinces who depend on equalization for comparable public services. Equalization is framed as a federal obligation but its level and formula are perpetually negotiated with beneficiary provinces. Have no institutional voice in intergovernmental negotiations; exit means migration to richer provinces, which is costly and depopulates their home communities further.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, interprovincial_equity_recipients, payer,
    powerless, biographical, trapped, national).

% National and international climate commitments require coordinated carbon pricing, regulation, and transition planning. Provincial override authority on resources and energy means any federal climate policy requires provincial opt-in — producing a patchwork where some provinces meet targets and others expand extraction. The coordination failure is structural: the compact treats climate as a negotiable policy area rather than a shared existential constraint.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, climate_action_coordination, payer,
    moderate, generational, constrained, global).

% Hold inherent rights and title that predate both federal and provincial sovereignty. The compact federalism reading treats provinces as the compact parties, rendering Indigenous nation-to-nation relationships invisible in intergovernmental negotiations. Resource development decisions affecting their territories are made through provincial processes they did not consent to. Exit from the Canadian constitutional order is not available; their struggle is for recognition within it.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, indigenous_nations, excluded,
    organized, generational, identity_locked, national).

% Analyze the compact thesis against the constitutional_subordination and resource_sovereignty_primacy readings. Track how judicial interpretation, political practice, and crisis moments shift the boundary. Do not collect rents from the arrangement; their professional standing benefits from the contest remaining live.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages coexistence of distinct political communities within a shared federation by making federal authority contingent on ongoing provincial consent. Solves the coordination problem of unity without uniformity: allows policy experimentation, accommodates regional diversity, and prevents secession by giving provinces veto-equivalent leverage.
% TRANSFER_FUNCTION: Moves fiscal capacity (equalization, health transfers, resource revenues) from federal treasury and richer provinces to poorer provinces, conditional on provincial policy autonomy. Moves regulatory authority from federal to provincial level on resources, energy, and environment. Moves political risk from secession to continuous negotiation.
% ABSENT_VOICES: Indigenous nations are structurally excluded from the compact — the founding agreement was among British and French colonial entities, not the peoples whose territories were being divided. Residents of have-not provinces have no seat at the first ministers' table where equalization is negotiated. Future generations bearing climate costs of provincial resource expansion have no voice.
% DISAPPEARANCE_RATIONALE: If the compact reading vanished overnight, federal authority would no longer require provincial consent for national programs. Equalization would become a federal statutory obligation, not a negotiated transfer. Climate policy would be uniformly imposed. Resource royalties would face federal minimum standards. The federation would centralize — likely triggering constitutional crisis, western alienation, and renewed sovereignty movements. The world rearranges violently.
% FOUNDING_PROBLEM: How to unite British North American colonies with distinct economies, cultures, and religious compositions into a single federation without forcing assimilation or inviting annexation by the United States. The compact solution: a federal union where the founding provinces retained residual sovereignty and could negotiate the terms of their ongoing participation.
% FOUNDING_PROBLEM_CORROBORATION: The compact thesis is attested by the provincial governments themselves and by constitutional scholars in the 'compact school' (e.g., Kenneth McRoberts, Alain-G. Gagnon). The constitutional_subordination reading is attested by federal governments, the Supreme Court's Reference re Secession (1998) which affirmed federal paramountcy, and scholars like Peter Hogg. The resource_sovereignty_primacy reading is attested by Alberta and Saskatchewan governments, the oil and gas industry, and scholars emphasizing s.92A. No single corroboration exists outside the benefiting parties for any reading — the contest is the constitution's living structure.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__compact_federalism, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__compact_federalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(provincial_sovereignty_boundary__compact_federalism, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__compact_federalism, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__compact_federalism_tests).
:- end_tests(provincial_sovereignty_boundary__compact_federalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the compact extracts federal policy capacity and interprovincial equity in exchange for provincial autonomy, but also provides genuine coordination (preventing secession, enabling diversity). Suppression (0.35) is present but not dominant: the constraint operates through negotiation and veto, not overt coercion — though the threat of constitutional crisis functions as implicit suppression. Theater ratio (0.28) reflects that intergovernmental theater (first ministers' conferences, communiqués) partially substitutes for functional coordination. Accessibility collapse (0.45) is moderate: alternatives (unitary state, looser confederation, dissolution) are structurally imaginable but politically costly. Resistance (0.55) is high: federal governments, courts, equity advocates, and climate coordinators continuously contest the compact's scope.
 *
 * PERSPECTIVAL GAP:
 *   From the provincial agenda_setter seat, the constraint appears as a rope (genuine coordination enabling diversity and preventing breakup). From the federal payer seat, it appears as a tangled_rope (coordination function real but extraction of policy capacity substantial). From the equity_recipient and climate_coordination payer seats, it appears as a snare (extraction of equity and planetary coordination with no voice). From the Indigenous excluded seat, it appears as a snare operating through erasure. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Provincial governments are the primary beneficiaries (d ≈ 0.15): they collect autonomy, resource control, and veto power. Resource extraction interests are secondary beneficiaries (d ≈ 0.20): they capture regulatory arbitrage. The federal government is a dual-positioned payer/beneficiary (d ≈ 0.50): pays fiscal transfers and policy concessions but benefits from a functioning federation. Interprovincial equity recipients and climate coordination are payers (d ≈ 0.75-0.85): they bear the costs of fragmented policy and negotiated equity with no structural voice. Indigenous nations are excluded (d ≈ 0.90): their inherent rights are rendered invisible by the compact's bilateral framing. The identity_locked exit for provinces and Indigenous nations reflects that their political identity is constituted through this very constraint — leaving it would dissolve their standing as compact parties or rights-holders.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1867 unity without assimilation) is contested as live vs. dead. The compact function (preventing secession) remains live — Quebec referenda, western alienation, and Indigenous sovereignty movements all testify that the secession risk the compact was built to manage persists. But the equity and climate coordination functions the federation now requires are structurally incompatible with the compact's veto architecture. The arrangement is neither pure coordination nor pure extraction — it is a tangled_rope where the coordination function (unity) and extraction function (provincial autonomy as rent) are fused. Mandatrophy is unresolved: the compact's original mandate has both persisted (secession prevention) and been outpaced (equity, climate).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compact_vs_constructed_ambiguity,
    'Is the provincial residual sovereignty a genuine historical fact of the 1867 compact, or a constructed constitutional narrative that serves contemporary provincial power?',
    'Historical analysis of the Confederation debates, the London Conference, and the British North America Act''s drafting — specifically whether the Fathers of Confederation understood themselves as creating a compact among sovereigns or a federal union with divided sovereignty. Also: whether the compact narrative was dormant for decades and revived strategically (e.g., 1960s Quiet Revolution, 1980s patriation, 1990s Meech Lake).',
    'If the compact is a genuine historical fact, the constraint has stronger mountain-like features (emerges_naturally from the founding moment). If constructed, it is a tangled_rope or snare where provincial governments actively maintain a narrative that extracts federal capacity. Affects FSM evaluation for any mountain claim on this kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compact_vs_constructed_ambiguity, conceptual, 'Whether provincial residual sovereignty is a founding fact or a maintained narrative').

omega_variable(
    coordination_extraction_separability,
    'Can the compact''s coordination function (preventing secession, managing diversity) be separated from its extraction function (provincial veto over equity and climate), or are they structurally fused?',
    'Counterfactual analysis: if a federal government proposed a ''new compact'' that guaranteed secession prevention and cultural autonomy but removed provincial veto on equalization formula and carbon pricing, would provinces accept it? Historical test: the 1992 Charlottetown Accord attempted something like this and failed — but was that because the functions are fused, or because the specific bargain was rejected?',
    'If separable, the constraint could evolve toward rope (coordination without extraction) via constitutional amendment. If fused, the extraction is the price of coordination — the tangled_rope classification is stable and any reform that reduces extraction increases secession risk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction in the compact are structurally separable').

omega_variable(
    indigenous_exclusion_mechanism,
    'Is Indigenous exclusion from the compact a contingent historical artifact (correctable by adding Indigenous parties to the negotiation) or a structural necessity of the compact form (which requires bilateral Crown-province symmetry)?',
    'Analyze whether any federal-provincial compact model can accommodate nation-to-nation Indigenous-Crown relationships without dissolving the compact''s bilateral logic. Examine the 1992 Charlottetown Accord''s Indigenous self-government provisions and why they failed. Track current UNDRIP implementation and whether it creates a third sovereignty pole that breaks the compact.',
    'If contingent, Indigenous inclusion is a reform path within the constraint. If structural, the compact form itself is a snare for Indigenous nations — their exclusion is not a bug but a feature of the bilateral symmetry. Changes the excluded stakeholder''s structural position from ''accidentally omitted'' to ''structurally foreclosed''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_exclusion_mechanism, conceptual, 'Whether Indigenous exclusion is contingent or structural to the compact form').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__compact_federalism, 1867, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psb_cf_tr_t1867, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1867, 0.1).
narrative_ontology:measurement(psb_cf_tr_t1930, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1930, 0.15).
narrative_ontology:measurement(psb_cf_tr_t1957, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1957, 0.18).
narrative_ontology:measurement(psb_cf_tr_t1982, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1982, 0.22).
narrative_ontology:measurement(psb_cf_tr_t1995, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(psb_cf_tr_t2015, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2015, 0.27).
narrative_ontology:measurement(psb_cf_tr_t2025, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(psb_cf_be_t1867, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1867, 0.25).
narrative_ontology:measurement(psb_cf_be_t1930, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1930, 0.3).
narrative_ontology:measurement(psb_cf_be_t1957, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1957, 0.32).
narrative_ontology:measurement(psb_cf_be_t1982, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1982, 0.38).
narrative_ontology:measurement(psb_cf_be_t1995, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement(psb_cf_be_t2015, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(psb_cf_be_t2025, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(psb_cf_su_t1867, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1867, 0.2).
narrative_ontology:measurement(psb_cf_su_t1930, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1930, 0.25).
narrative_ontology:measurement(psb_cf_su_t1957, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1957, 0.28).
narrative_ontology:measurement(psb_cf_su_t1982, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1982, 0.32).
narrative_ontology:measurement(psb_cf_su_t1995, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1995, 0.34).
narrative_ontology:measurement(psb_cf_su_t2015, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(psb_cf_su_t2025, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__compact_federalism, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary__constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary__resource_sovereignty_primacy).

% DUAL FORMULATION NOTE:
% This constraint (compact_federalism) is one of three readings of the provincial_sovereignty_boundary kernel. The constitutional_subordination reading treats provinces as federal creatures with no inherent sovereignty. The resource_sovereignty_primacy reading treats s.92A resource ownership as grounding absolute provincial sovereignty. All three share the same referent (the federal-provincial boundary) but instantiate different constraints with different ε, beneficiary/victim structures, and classifications. The compact_federalism reading coexists_with constitutional_subordination and influences resource_sovereignty_primacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(provincial_sovereignty_boundary__compact_federalism, institutional, 0.15).
constraint_indexing:directionality_override(provincial_sovereignty_boundary__compact_federalism, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
