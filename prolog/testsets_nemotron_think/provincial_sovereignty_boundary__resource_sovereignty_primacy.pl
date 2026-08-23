% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__resource_sovereignty_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__resource_sovereignty_primacy, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: provincial_sovereignty_boundary__resource_sovereignty_primacy
 *   human_readable: Provincial Resource Sovereignty as Territorial Sovereignty (s.92A)
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   Section 92A of the Constitution Act 1982 grants provinces exclusive
 *   authority over non-renewable natural resources, forestry, and electrical
 *   energy. The resource_sovereignty_primacy reading interprets this as
 *   grounding absolute provincial sovereignty — resource control equals
 *   territorial sovereignty, federal climate and fiscal policy constitutes
 *   illegitimate extraction, and unilateral exit is a constitutional right.
 *   This reading is advanced by resource-rich provinces (Alberta,
 *   Saskatchewan, Newfoundland) and supported by resource industry
 *   associations. The claimed_type is Mountain (constitutional bedrock), but
 *   the metrics show rising extraction (federal policy capacity displaced),
 *   active suppression (court challenges to federal carbon pricing,
 *   equalization disputes), and increasing theater (federal-provincial
 *   performative negotiations). The engine will compute per-seat
 *   classifications from the structural data below.
 *
 * KEY AGENTS:
 *   - provincial_governments: Primary agenda_setter and beneficiary (institutional/arbitrage) — sets resource policy, collects royalties, challenges federal intrusion
 *   - resource_industries: Primary beneficiary (powerful/mobile) — receives favorable provincial regulation and royalty regimes
 *   - federal_government: Primary payer (institutional/constrained) — loses regulatory authority over climate, energy, equalization
 *   - equalization_recipient_provinces: Payer (organized/constrained) — receive reduced transfers when resource provinces keep more revenue
 *   - indigenous_nations: Excluded (organized/trapped) — title and consultation rights overridden by provincial resource decisions
 *   - citizens: Payer (moderate/constrained) — bear climate costs and service cuts from federal-provincial fiscal fights
 *   - constitutional_courts: Observer (institutional/analytical) — adjudicate s.92A scope, define the boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.75).
domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.8).
domain_priors:theater_ratio(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, extractiveness, 0.75).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__resource_sovereignty_primacy, mountain).
narrative_ontology:human_readable(provincial_sovereignty_boundary__resource_sovereignty_primacy, "Provincial Resource Sovereignty as Territorial Sovereignty (s.92A)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__resource_sovereignty_primacy, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__resource_sovereignty_primacy).
domain_priors:emerges_naturally(provincial_sovereignty_boundary__resource_sovereignty_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__resource_sovereignty_primacy, '2d71a496-926a-47bf-8081-3a53b26415ce').
narrative_ontology:cs_kernel_codification('2d71a496-926a-47bf-8081-3a53b26415ce', formalized).
narrative_ontology:cs_authority_grounding('2d71a496-926a-47bf-8081-3a53b26415ce', lineage).
narrative_ontology:cs_interpretation_layer_present('2d71a496-926a-47bf-8081-3a53b26415ce').
narrative_ontology:cs_reading_relation('2d71a496-926a-47bf-8081-3a53b26415ce', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('2d71a496-926a-47bf-8081-3a53b26415ce', provincial_sovereignty_boundary__compact_federalism, coexists_with).
narrative_ontology:cs_axiom('2d71a496-926a-47bf-8081-3a53b26415ce', foundational, resource_ownership_equals_sovereignty).
narrative_ontology:cs_axiom_status(resource_ownership_equals_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('2d71a496-926a-47bf-8081-3a53b26415ce', resource_ownership_equals_sovereignty, conventional).
narrative_ontology:cs_axiom('2d71a496-926a-47bf-8081-3a53b26415ce', secondary, unilateral_exit_constitutional_right).
narrative_ontology:cs_axiom_status(unilateral_exit_constitutional_right, holdable).
narrative_ontology:cs_axiom_grounding('2d71a496-926a-47bf-8081-3a53b26415ce', unilateral_exit_constitutional_right, deontological).
narrative_ontology:cs_reference_frame('2d71a496-926a-47bf-8081-3a53b26415ce', s92a_provincial_ownership_regime).
narrative_ontology:cs_drift_state('2d71a496-926a-47bf-8081-3a53b26415ce', contemporary_climate_federalism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2d71a496-926a-47bf-8081-3a53b26415ce', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_governments).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_industries).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, national_climate_coordination).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, equalization_recipient_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, citizens).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_resource_ownership_s92a).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__resource_sovereignty_primacy, territorial_sovereignty_primacy).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__resource_sovereignty_primacy, unilateral_exit_constitutional_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and enforce resource development laws, collect royalties, negotiate pipeline approvals, challenge federal carbon pricing and environmental assessment in court. Threaten unilateral exit (Alberta Sovereignty Act, Saskatchewan First Act) to extract federal concessions. Control the regulatory apparatus that implements s.92A.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_governments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_governments, beneficiary).

% Oil sands, mining, forestry, and hydro companies operate under provincial regimes. They lobby for favorable royalty rates, streamlined permitting, and provincial defense against federal regulation. They can relocate capital globally but depend on provincial political protection for existing assets. Fund think tanks and political parties advancing resource sovereignty.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_industries, beneficiary,
    powerful, biographical, mobile, national).

% Loses regulatory authority over GHG emissions, energy projects, and environmental assessment when provinces invoke s.92A. Carbon pricing backstop applies only where provinces refuse equivalent systems. Equalization payments become politically toxic when resource provinces claim fiscal independence. Must negotiate every national standard.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, payer,
    institutional, generational, constrained, national).

% Receive equalization transfers calculated partly from national average fiscal capacity. When resource provinces keep 100% of resource revenue (vs. 50% pre-2007), the national average rises, reducing transfers to have-not provinces. They support federal climate policy but lack leverage against resource provinces.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, equalization_recipient_provinces, payer,
    organized, generational, constrained, national).

% Hold constitutionally protected title and consultation rights (s.35) that provinces routinely override via resource permitting. Not parties to 1982 amendment; no veto over provincial resource laws. Court victories (Tsilhqot'in, Yahey) are implemented slowly. Provincial sovereignty claim treats Indigenous jurisdiction as subordinate to provincial ownership.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_nations, excluded,
    organized, generational, trapped, national).

% Bear costs of climate inaction (wildfires, floods, heat), service cuts from federal-provincial fiscal fights, and higher energy prices from fragmented regulation. No direct exit from federal system; provincial exit rhetoric threatens national programs (healthcare, pensions). Vote in both federal and provincial elections but cannot coordinate a national climate mandate.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, citizens, payer,
    moderate, biographical, constrained, national).

% Supreme Court of Canada adjudicates s.92A scope (Reference re Greenhouse Gas Pollution Pricing Act, References re Impact Assessment Act). Defines the boundary between provincial resource ownership and federal POGG/national concern. Their rulings determine whether the constraint operates as Mountain (hard boundary) or Tangled Rope (negotiated boundary).
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_governments).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__resource_sovereignty_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates resource development, revenue collection, and regulatory authority within provincial jurisdictions, avoiding duplication and federal-provincial conflict over resource management.
% TRANSFER_FUNCTION: Moves regulatory authority over resource development, climate policy, and fiscal capacity from federal to provincial control; moves resource rents from federal equalization pool to provincial treasuries.
% ABSENT_VOICES: Indigenous nations (excluded from 1982 amendment, no veto over provincial resource laws), future generations (bear climate costs of blocked federal policy), have-not provinces (lose equalization when resource provinces opt out), federal climate bureaucracy (dismantled by provincial challenges).
% DISAPPEARANCE_RATIONALE: If provincial resource sovereignty vanished overnight, federal government would regain exclusive authority over resource-related GHG regulation, energy infrastructure, and environmental assessment. National carbon pricing would be uniform. Equalization would be recalculated without provincial resource revenue opt-outs. Indigenous consultation would shift to federal Crown duty. The federation's fiscal and climate architecture would fundamentally reorganize.
% FOUNDING_PROBLEM: Pre-1982 federal control over resource taxation and export (National Energy Program) was perceived by producing provinces as extraction of their resource wealth without consent. Provinces needed constitutional guarantee of ownership and revenue control to secure economic self-determination.
% FOUNDING_PROBLEM_CORROBORATION: Provincial governments (Alberta, Saskatchewan, Newfoundland) attest the problem remains live — federal climate policy is 'NEP 2.0'. Federal government and legal scholars (e.g., Peter Hogg, Kerry Wilkins) attest the 1982 amendment settled ownership; current conflicts are about regulatory scope, not ownership. Indigenous leaders (AFN, ITK) attest the founding problem excluded them — s.92A entrenched provincial control over lands subject to unextinguished title.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__resource_sovereignty_primacy, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__resource_sovereignty_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, ExtMetricName, E),
    domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(provincial_sovereignty_boundary__resource_sovereignty_primacy),
    narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 (1982) to 0.75 (2024) as provincial resource revenues grow and federal climate policy expands — the constraint increasingly displaces federal capacity. Suppression rises from 0.50 to 0.80 as provinces litigate federal carbon pricing, challenge equalization formula, and assert unilateral exit rhetoric. Theater ratio grows from 0.15 to 0.40 as intergovernmental meetings become performative while real decisions shift to courts and unilateral provincial action. Accessibility_collapse at 0.85 reflects that accepting 'resource control = sovereignty' makes federal climate coordination structurally impossible. Resistance at 0.70 reflects sustained federal, judicial, and inter-provincial pushback. The claimed Mountain type asserts naturalness (emerges_naturally: true) but declared beneficiaries trigger FSM evaluation.
 *
 * PERSPECTIVAL GAP:
 *   From the provincial seat, the constraint is genuine coordination (resource development, revenue certainty) — a Rope. From the federal seat, it is extraction with suppression — a Snare. From indigenous nations' seat, it is exclusionary extraction — a Snare. From equalization-recipient provinces, it is fiscal extraction — a Tangled Rope (they benefit from equalization but lose when resource provinces opt out). The engine computes this seat divergence from power/exit/beneficiary declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Provincial governments and resource industries are structural beneficiaries: they collect rents and regulatory control (d near 0.0). Federal government, equalization-recipient provinces, indigenous nations, and citizens are payers: they lose policy capacity, fiscal transfers, consultation rights, and climate mitigation (d near 1.0). Constitutional courts sit near analytical (d ~0.5). The reading's own framing inverts this — it claims federal policy extracts from provinces — but the engine's χ computation reads the constraint's actual operation: provincial sovereignty extracts federal authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (provincial economic self-determination via resource control) is contested: provinces say it remains live; federal government says 1982 settled it. The arrangement persists with rising extraction and theater, suggesting mandatrophy — the 1982 settlement's coordination function (ending federal-provincial resource wars) has atrophied into a sovereignty claim that blocks climate coordination. The constraint now serves as a piton for provincial political identity rather than a functional resource-management tool.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_constructed_sovereignty,
    'Is provincial resource sovereignty a genuine constitutional mountain (irreducible bedrock) or a constructed claim that benefits identifiable provincial and industrial actors?',
    'Comparative constitutional analysis: if other federal systems with similar resource distributions lack equivalent sovereignty claims, the claim is constructed; if the claim persists across regime changes without enforcement, it approaches mountain status.',
    'If constructed, FSM triggers reclassification to tangled_rope (coordination of resource development + asymmetric extraction from federal capacity); if mountain, the claim is immune to extraction metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_constructed_sovereignty, conceptual, 'Natural-law vs. constructed ambiguity for a Mountain claim with declared beneficiaries').

omega_variable(
    extraction_referent_ambiguity,
    'Does the constraint''s extraction run from provinces→federal (provincial sovereignty extracts federal policy capacity) or federal→provinces (federal climate policy extracts provincial resource rents)?',
    'Measure net fiscal and regulatory flows: if provinces capture resource rents that would otherwise fund federal programs, extraction is provinces→federal; if federal carbon pricing reduces provincial royalty revenue, extraction is federal→provinces. The reading''s own framing determines which flow is ''legitimate'' vs ''extraction''.',
    'Reverses the directionality of payer/beneficiary seats and flips which stakeholders are targets vs beneficiaries in the engine''s computation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_referent_ambiguity, conceptual, 'Direction of extraction depends on which reading''s referent is adopted').

omega_variable(
    suppression_mechanism_constitutional_vs_political,
    'Is the suppression of federal climate policy structural (constitutional text, court doctrine) or political (provincial resistance, intergovernmental negotiation)?',
    'Track court outcomes: if federal climate legislation survives s.92A challenges, suppression is political; if courts consistently strike down federal measures citing s.92A, suppression is structural.',
    'Structural suppression supports Mountain classification (irreducible limit); political suppression supports Tangled Rope (active enforcement required).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_constitutional_vs_political, empirical, 'Structural vs political suppression mechanism for constitutional constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psbrsp_tr_t0, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0, 0.15).
narrative_ontology:measurement(psbrsp_tr_t10, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 10, 0.2).
narrative_ontology:measurement(psbrsp_tr_t20, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 20, 0.28).
narrative_ontology:measurement(psbrsp_tr_t30, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 30, 0.35).
narrative_ontology:measurement(psbrsp_tr_t42, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 42, 0.4).

% Extraction over time
narrative_ontology:measurement(psbrsp_be_t0, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(psbrsp_be_t10, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(psbrsp_be_t20, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(psbrsp_be_t30, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(psbrsp_be_t42, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 42, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(psbrsp_su_t0, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(psbrsp_su_t10, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(psbrsp_su_t20, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(psbrsp_su_t30, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(psbrsp_su_t42, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 42, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__resource_sovereignty_primacy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.1).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, compact_federalism).

% DUAL FORMULATION NOTE:
% BGS-pattern decomposition of provincial_sovereignty_boundary kernel: ehrenfest_barrier (constitutional_text_s92a) → spectral_universality (provincial_ownership_confirmed) → eigenvector_thermalization (resource_sovereignty_primacy). This reading is the contested downstream claim (eigenvector_thermalization analog) with substantial extraction; the upstream Mountain claims (text, ownership) have negligible extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(provincial_sovereignty_boundary__resource_sovereignty_primacy, institutional, 0.15).
constraint_indexing:directionality_override(provincial_sovereignty_boundary__resource_sovereignty_primacy, powerful, 0.1).
constraint_indexing:directionality_override(provincial_sovereignty_boundary__resource_sovereignty_primacy, organized, 0.85).
constraint_indexing:directionality_override(provincial_sovereignty_boundary__resource_sovereignty_primacy, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
