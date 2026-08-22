% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__compact_federalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Compact Federalism Reading of Provincial Sovereignty Boundary
 *   domain: political/economic/federalism
 *
 * SUMMARY:
 *   This constraint authors the compact federalism reading of the provincial
 *   sovereignty boundary kernel: the claim that confederation was a
 *   negotiated compact among sovereign provinces which retained residual
 *   sovereignty and whose exit remains a matter of negotiation rather than
 *   federal permission. Under this reading, federal authority is conditional
 *   on provincial consent, equalization formulas are negotiable rather than
 *   federally imposed, and national climate policy is subject to provincial
 *   override. This is one of three structurally distinct constraints reading
 *   the same contested kernel — the constitutional_subordination reading
 *   (provinces as creatures of federal constitution with no inherent
 *   sovereignty) and the resource_sovereignty_primacy reading (s.92A resource
 *   ownership as grounding absolute territorial sovereignty) are separate
 *   constraints with their own ε values, not alternative measurements of this
 *   one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, 0.42).
domain_priors:suppression_score(provincial_sovereignty_boundary__compact_federalism, 0.38).
domain_priors:theater_ratio(provincial_sovereignty_boundary__compact_federalism, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, extractiveness, 0.42).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__compact_federalism, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__compact_federalism, "Compact Federalism Reading of Provincial Sovereignty Boundary").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__compact_federalism, "political/economic/federalism").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__compact_federalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, '80b1fe46-d017-45db-98f9-0d15ba990826').
narrative_ontology:cs_kernel_codification('80b1fe46-d017-45db-98f9-0d15ba990826', distributed).
narrative_ontology:cs_authority_grounding('80b1fe46-d017-45db-98f9-0d15ba990826', distributed).
narrative_ontology:cs_reading_relation('80b1fe46-d017-45db-98f9-0d15ba990826', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('80b1fe46-d017-45db-98f9-0d15ba990826', provincial_sovereignty_boundary__resource_sovereignty_primacy, coexists_with).
narrative_ontology:cs_axiom('80b1fe46-d017-45db-98f9-0d15ba990826', foundational, provinces_retained_pre_existing_sovereignty).
narrative_ontology:cs_axiom_status(provinces_retained_pre_existing_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('80b1fe46-d017-45db-98f9-0d15ba990826', provinces_retained_pre_existing_sovereignty, conventional).
narrative_ontology:cs_axiom('80b1fe46-d017-45db-98f9-0d15ba990826', secondary, exit_requires_negotiation_not_federal_permission).
narrative_ontology:cs_axiom_status(exit_requires_negotiation_not_federal_permission, holdable).
narrative_ontology:cs_axiom_grounding('80b1fe46-d017-45db-98f9-0d15ba990826', exit_requires_negotiation_not_federal_permission, conventional).
narrative_ontology:cs_reference_frame('80b1fe46-d017-45db-98f9-0d15ba990826', pre_confederation_colonial_negotiation).
narrative_ontology:cs_drift_state('80b1fe46-d017-45db-98f9-0d15ba990826', post_1982_patriation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('80b1fe46-d017-45db-98f9-0d15ba990826', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, resource_rich_provincial_governments).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, provincial_political_elites).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, federal_equalization_recipient_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, national_climate_policy_constituencies).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, indigenous_nations_within_provincial_boundaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, federal_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke the compact reading to assert veto power over federal climate policy, equalization formulas, and resource royalty arrangements. Frame the confederation as a negotiated treaty among equals that federal authority cannot override without renewed consent. Administer their own resource revenues and use the compact framing to resist federal conditions attached to transfers.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, resource_rich_provincial_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, resource_rich_provincial_governments, beneficiary).

% Build durable political careers on defending provincial jurisdiction against federal 'overreach.' The compact reading supplies a legitimating vocabulary — sovereignty, consent, treaty — that converts routine intergovernmental bargaining into existential defense, generating electoral returns independent of policy outcomes.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, provincial_political_elites, beneficiary,
    powerful, biographical, mobile, regional).

% Depend on equalization transfers funded partly by federal capacity that resource-rich provinces contest under the compact reading. When wealthy provinces successfully renegotiate formulas or block federal fiscal tools as intrusions on sovereignty, redistribution shrinks and recipient provinces absorb the shortfall without having been party to the renegotiation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_equalization_recipient_provinces, payer,
    moderate, biographical, constrained, national).

% Seek uniform national carbon pricing and emissions standards. Under the compact reading, provinces can override or opt out of federal climate measures as an assertion of retained sovereignty, fragmenting policy and shifting the cost of delay onto constituencies who cannot exit the national jurisdiction they live in.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, national_climate_policy_constituencies, payer,
    organized, generational, trapped, national).

% Were never party to the provincial-federal compact narrative at all — their own treaty relationships predate and sit outside the settler federal structure being negotiated. The compact reading's two-party (province/federal) framing erases a third sovereignty claim entirely; they bear the consequences of both federal and provincial resource decisions without a seat in the compact story.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, indigenous_nations_within_provincial_boundaries, excluded,
    powerless, civilizational, trapped, regional).

% Must negotiate national programs as if provincial consent were a standing precondition rather than a constitutional given, spending political and fiscal capital on province-by-province bargaining for policies formally within federal jurisdiction. Cannot compel compliance without appearing to violate the compact's founding terms.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_government, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, federal_government, agenda_setter).

% Study the founding documents, pre-confederation negotiations, and subsequent constitutional amendments to assess whether the compact characterization survives 1867/1982 codification. Their findings are cited by all sides but bind none of them.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__compact_federalism, resource_rich_provincial_governments).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__compact_federalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely coordinates a federation of provinces with divergent economies, resource endowments, and political cultures by requiring negotiated consent rather than unilateral federal imposition — this lowers the risk of provinces exiting or defecting entirely and allows heterogeneous regional preferences to be accommodated within one federal structure.
% TRANSFER_FUNCTION: Moves fiscal capacity and policy latitude away from federal redistributive and national-standard functions (equalization, climate policy) and toward resource-rich provincial treasuries and provincial political autonomy, by converting federal initiatives into renegotiable-by-veto rather than binding-by-default.
% ABSENT_VOICES: Indigenous nations whose treaty sovereignty predates and stands outside the settler province/federal compact entirely; low-income residents of recipient provinces who bear equalization shortfalls; future generations bearing delayed climate costs. None of these parties negotiated the compact and none hold a veto within its framework.
% DISAPPEARANCE_RATIONALE: If the compact reading were abandoned overnight in favor of unconditional federal supremacy, national climate policy could be imposed uniformly, equalization formulas could be set federally without provincial veto leverage, and resource-rich provinces would lose the primary legitimating vocabulary for blocking federal fiscal and environmental initiatives — provincial governments would need new grounds to resist, and intergovernmental bargaining would shift decisively toward Ottawa.
% FOUNDING_PROBLEM: Pre-1867 negotiations among British North American colonies required a framework that would secure voluntary entry into confederation without colonies fearing loss of local control over property, resources, and local institutions — a genuine problem of assembling reluctant, economically distinct polities into one union.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and federal officials attest the founding bargaining problem was substantially resolved by patriation (1982) and subsequent case law establishing federal paramountcy in enumerated areas; resource-rich provincial governments and allied legal scholars attest the compact's residual-sovereignty logic remains live and unresolved. No corroboration from outside either the provincial or federal benefiting parties has settled the question — indigenous legal scholars note both sides omit a third sovereignty claim entirely, which is itself independent evidence the settled-vs-live dispute is conducted between two self-interested parties rather than adjudicated by a neutral outside record.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__compact_federalism, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__compact_federalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__compact_federalism, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__compact_federalism, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is moderate (0.42) and rising slowly: the compact reading genuinely does something — it lowered the risk of provincial defection at founding and continues to accommodate real regional heterogeneity — but its increasing invocation to block equalization reform and fragment climate policy has grown the extractive share over four decades without ever becoming the dominant feature. Suppression is moderate (0.38): the reading does not physically coerce recipient provinces or climate constituencies, but it does foreclose certain federal policy tools by rendering them illegitimate-by-narrative, which is a real, if soft, form of suppression. Theater ratio (0.28) reflects that much of the compact invocation is genuine constitutional argument, not pure performance, though its use has grown more strategic over time.
 *
 * PERSPECTIVAL GAP:
 *   From the resource-rich provincial seat, the compact reading is genuine, hard-won coordination protecting minority regional interests against majoritarian federal overreach — a rope. From the equalization-recipient or climate-constituency seat, the same structure computes as enforced extraction: a veto mechanism that a subset of provinces wields to externalize costs onto the rest of the federation while collecting the benefits of national market access, currency, and defense. The engine should register this as seat divergence rather than resolve it — both readings are structurally coherent from their respective positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Resource-rich provincial governments and their political elites are the structural beneficiaries: the compact framing hands them veto leverage over federal redistribution and climate policy with no reciprocal exposure. Equalization-recipient provinces and national climate constituencies are targets: they bear the cost of blocked or diluted federal initiatives without having negotiated the blocking mechanism. Indigenous nations are excluded entirely from the two-party compact story, which is a distinct harm from being an ordinary victim — they are not extracted from by this constraint so much as erased from its founding narrative, a structural exclusion that predates and outlasts either settler reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — assembling reluctant colonies into one union without triggering exit — was substantially solved by 1982 patriation and subsequent federalism jurisprudence establishing enumerated federal powers. That the compact vocabulary persists and has intensified in fiscal and climate disputes since is the signature of a founding accommodation whose original function (preventing non-entry) has been substantially achieved, while its residual invocation increasingly serves ongoing rent-seeking by resource-rich provinces against redistributive and environmental federal functions — exactly the tangled_rope signature: real coordination history, live asymmetric extraction now, sustained only by active political and legal enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compact_vs_subordination_founding_fact,
    'Was confederation historically and legally a compact among pre-existing sovereign entities, or did the provinces come into legal existence only through the federal constitutional act — i.e., is there a founding fact that adjudicates between this reading and constitutional_subordination?',
    'Close historical-legal analysis of pre-1867 colonial status, the negotiating record of the Quebec and Charlottetown conferences, and subsequent judicial treatment (e.g., Reference re Secession of Quebec) of whether provinces held sovereignty prior to confederation.',
    'If the historical record clearly supports one reading, that reading gains legitimacy independent of political convenience; if the record is genuinely ambiguous or supports competing interpretations depending on which colonies and which period are examined, both readings remain live indefinitely and the kernel stays permanently contested rather than resolvable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compact_vs_subordination_founding_fact, conceptual, 'Whether founding historical-legal fact adjudicates compact vs. subordination readings.').

omega_variable(
    resource_sovereignty_reading_compatibility,
    'Is the resource_sovereignty_primacy reading (s.92A grounds absolute sovereignty via resource control) a strengthening variant of this compact reading, or a structurally distinct and potentially incompatible claim — since s.92A is a 1982 constitutional amendment, not a founding-era compact term?',
    'Compare how each reading treats the source of sovereignty: compact_federalism grounds it in 1867 founding negotiation and consent; resource_sovereignty_primacy grounds it in 1982 constitutional property assignment. Determine whether a province could coherently hold the resource_sovereignty_primacy claim while rejecting the historical compact narrative (e.g., a province indifferent to founding history but insistent on resource control).',
    'If the readings are independent grounds that happen to produce similar political outcomes, they should remain fully separate constraints with only an influences edge; if resource_sovereignty_primacy is actually a modern doctrinal extension of the compact logic, the network relationship should be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_sovereignty_reading_compatibility, conceptual, 'Structural relationship between the compact-history ground and the 1982 resource-ownership ground for provincial sovereignty claims.').

omega_variable(
    indigenous_sovereignty_exclusion,
    'Does the two-party settler compact framing (province vs. federal) structurally require the erasure of indigenous treaty sovereignty, or could the compact reading be reformulated as a multi-party arrangement that includes indigenous nations as original signatories?',
    'Examine whether any provincial or federal actor has proposed or would accept a tripartite reformulation of the sovereignty compact including indigenous treaty nations as founding parties with equivalent standing.',
    'If the exclusion is structurally necessary to the compact reading as currently practiced (i.e., the compact''s legitimating power depends on a closed two-party frame), that exclusion is not incidental but constitutive — raising the question of whether this reading''s coordination function is itself partly built on a suppressed third claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_sovereignty_exclusion, conceptual, 'Whether indigenous exclusion from the compact narrative is incidental or structurally constitutive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__compact_federalism, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1982, 0.18).
narrative_ontology:measurement(prov_tr_t1990, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(prov_tr_t2000, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(prov_tr_t2010, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(prov_tr_t2018, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2018, 0.26).
narrative_ontology:measurement(prov_tr_t2024, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1982, 0.28).
narrative_ontology:measurement(prov_be_t1990, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1990, 0.31).
narrative_ontology:measurement(prov_be_t2000, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2000, 0.34).
narrative_ontology:measurement(prov_be_t2010, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2010, 0.37).
narrative_ontology:measurement(prov_be_t2018, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2018, 0.4).
narrative_ontology:measurement(prov_be_t2024, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1982, 0.22).
narrative_ontology:measurement(prov_su_t1990, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(prov_su_t2000, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(prov_su_t2010, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2010, 0.32).
narrative_ontology:measurement(prov_su_t2018, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2018, 0.35).
narrative_ontology:measurement(prov_su_t2024, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__compact_federalism, resource_allocation).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__compact_federalism, 0.12).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, resource_sovereignty_primacy).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings reading the provincial_sovereignty_boundary kernel. compact_federalism (this story) holds provincial sovereignty pre-exists and survives confederation by negotiated consent; constitutional_subordination holds provinces are constitutional creatures with no inherent sovereignty; resource_sovereignty_primacy grounds sovereignty in 1982 resource-ownership provisions independent of founding history. Each carries its own ε, beneficiary/victim structure, and classification. This reading forecloses constitutional_subordination directly on the exit question (negotiated exit vs. federal-consent-required exit cannot both hold in one framework) while coexisting with resource_sovereignty_primacy, which grounds sovereignty on a different, compatible basis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
