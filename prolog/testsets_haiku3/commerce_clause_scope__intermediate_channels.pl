% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__intermediate_channels
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__intermediate_channels, []).

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
 *   constraint_id: commerce_clause_scope__intermediate_channels
 *   human_readable: Commerce Clause Scope: Intermediate Channels Reading
 *   domain: constitutional/federalism/economic
 *
 * SUMMARY:
 *   The Commerce Clause grants Congress power to 'regulate Commerce among the
 *   several States.' The intermediate-channels reading interprets this as
 *   federal power over (1) channels of interstate commerce (highways,
 *   waterways), (2) instrumentalities and persons/things in interstate
 *   commerce, and (3) intrastate activities substantially affecting
 *   interstate commerce, subject to limiting principles: non-economic
 *   activity requires a jurisdictional nexus, aggregation applies only to
 *   economic activity, and the causal chain cannot be attenuated. This
 *   reading is distinguished from the narrow-originalist reading (Commerce
 *   means trade crossing state lines) and the broad-effects reading (any
 *   activity affecting interstate commerce in the aggregate). The
 *   intermediate reading claims to thread between them: federal power is
 *   extensive within commerce but checked by categorical limits. This story
 *   instantiates that reading and models its extractiveness, beneficiary
 *   structure, and instability.
 *
 * KEY AGENTS:
 *   - federal_authority: Sets and enforces the three-prong doctrine and its limiting principles through constitutional interpretation and judicial review
 *   - state_authority_reserved_domains: Retains authority over non-economic activity and family/criminal/education law but bears cost of doctrinal drift when boundaries shift
 *   - intrastate_economic_actors_boundary_cases: Navigate jurisdictional uncertainty when their activity falls on the boundary between economic/non-economic or near/far from interstate commerce
 *   - supreme_court: Authors the operative reading, draws the economic/non-economic line, interprets 'substantially affects,' validates the three-prong doctrine
 *   - congress_legislative_body: Legislates on the assumption federal jurisdiction exists; uses the three-prong doctrine as cover for comprehensive national regulation
 *   - lower_court_judges: Apply the doctrine to concrete cases; bear the burden of drawing lines that shift with Supreme Court composition
 *   - dissenting_coalitions_originalist_and_broad: Excluded from operative law but maintain alternative readings in dissents and scholarship
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, 0.62).
domain_priors:suppression_score(commerce_clause_scope__intermediate_channels, 0.48).
domain_priors:theater_ratio(commerce_clause_scope__intermediate_channels, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, extractiveness, 0.62).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__intermediate_channels, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__intermediate_channels, "Commerce Clause Scope: Intermediate Channels Reading").
narrative_ontology:topic_domain(commerce_clause_scope__intermediate_channels, "constitutional/federalism/economic").

domain_priors:requires_active_enforcement(commerce_clause_scope__intermediate_channels).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__intermediate_channels, '01e5f13f-9b3c-4953-b0c0-e66f64d0f125').
narrative_ontology:cs_kernel_codification('01e5f13f-9b3c-4953-b0c0-e66f64d0f125', fixed_text).
narrative_ontology:cs_authority_grounding('01e5f13f-9b3c-4953-b0c0-e66f64d0f125', lineage).
narrative_ontology:cs_interpretation_layer_present('01e5f13f-9b3c-4953-b0c0-e66f64d0f125').
narrative_ontology:cs_reading_relation('01e5f13f-9b3c-4953-b0c0-e66f64d0f125', commerce_clause_scope__narrow_originalist, coexists_with).
narrative_ontology:cs_reading_relation('01e5f13f-9b3c-4953-b0c0-e66f64d0f125', commerce_clause_scope__broad_effects_test, influences).
narrative_ontology:cs_axiom('01e5f13f-9b3c-4953-b0c0-e66f64d0f125', foundational, three_prong_doctrine_authoritative).
narrative_ontology:cs_axiom_status(three_prong_doctrine_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('01e5f13f-9b3c-4953-b0c0-e66f64d0f125', three_prong_doctrine_authoritative, conventional).
narrative_ontology:cs_axiom('01e5f13f-9b3c-4953-b0c0-e66f64d0f125', foundational, limiting_principles_constrain_federal_reach).
narrative_ontology:cs_axiom_status(limiting_principles_constrain_federal_reach, holdable).
narrative_ontology:cs_axiom_grounding('01e5f13f-9b3c-4953-b0c0-e66f64d0f125', limiting_principles_constrain_federal_reach, deontological).
narrative_ontology:cs_axiom('01e5f13f-9b3c-4953-b0c0-e66f64d0f125', secondary, state_reserved_authority_tenth_amendment).
narrative_ontology:cs_axiom_status(state_reserved_authority_tenth_amendment, holdable).
narrative_ontology:cs_axiom_grounding('01e5f13f-9b3c-4953-b0c0-e66f64d0f125', state_reserved_authority_tenth_amendment, conventional).
narrative_ontology:cs_reference_frame('01e5f13f-9b3c-4953-b0c0-e66f64d0f125', three_prong_doctrine_with_categorical_limits).
narrative_ontology:cs_drift_state('01e5f13f-9b3c-4953-b0c0-e66f64d0f125', contemporary_2026, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('01e5f13f-9b3c-4953-b0c0-e66f64d0f125', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__intermediate_channels, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, federal_authority).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_authority_reserved_domains).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, conceptual_coherence_doctrine).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, intrastate_economic_actors_boundary_cases).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, congress_legislative_body).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, state_authority_reserved_domains).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, lower_court_judges).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies the Commerce Clause to define the scope of federal jurisdiction over interstate economic activity. Administers the three-prong doctrine (channels, instrumentalities, substantial effects) and draws the line between economic and non-economic activity. Collects legitimacy from constitutional text and accumulated case law; enforces the boundaries through judicial review of federal legislation and state regulation.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, federal_authority, agenda_setter,
    institutional, civilizational, analytical, national).

% Retains authority over family law, criminal procedure, education, and purely intrastate non-economic conduct under the Tenth Amendment. Also bears the cost of the doctrine's instability: when the economic/non-economic boundary shifts (as with marijuana, criminal penalties on economic activity, medical licensing), state law can be invalidated mid-stream. Their exit option is constitutional amendment or violent rupture; absent either, they navigate federal supremacy within the carve-outs.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_authority_reserved_domains, beneficiary,
    institutional, civilizational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, state_authority_reserved_domains, payer).

% Conduct economic activity that may or may not trigger federal jurisdiction depending on how the 'substantially affects interstate commerce' prong is applied and whether the activity is characterized as economic. Agricultural producers, manufacturers, and service providers in commerce-adjacent zones face regulatory uncertainty: a state law might be preempted or upheld depending on whether the Court applies the aggregation principle to their activity or treats it as merely local. They cannot exit the doctrine's domain; they navigate jurisdictional uncertainty.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, intrastate_economic_actors_boundary_cases, payer,
    powerful, biographical, constrained, national).

% Legislates on the assumption that federal jurisdiction exists over channels of interstate commerce, instrumentalities of interstate commerce, and economic activity substantially affecting interstate commerce. The three-prong doctrine and its limiting principles provide cover for comprehensive national legislation while maintaining the formal appearance of constitutional constraint. Congress can exit by constitutional amendment or by self-restraint (which has not occurred); absent that, Congress legislates within the doctrine's bounds and Congress itself validates those bounds.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, congress_legislative_body, beneficiary,
    powerful, biographical, mobile, national).

% Must apply the intermediate-channels doctrine to concrete cases: Does this activity substantially affect interstate commerce? Is it economic or non-economic? If economic, does aggregation apply? Courts bear the doctrinal burden of drawing lines that shift over time. They have no exit option short of Supreme Court reversal; they apply the doctrine as best they can, often producing conflicting results that must be resolved at the appellate level.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, lower_court_judges, payer,
    powerful, biographical, constrained, regional).

% Authorizes and adjudicates the intermediate-channels reading, issuing binding interpretations of the three-prong doctrine. Acts both as agenda-setter (defines what counts as substantial effects, draws the economic/non-economic line) and as observer (reviews lower court applications, occasionally reverses its own precedents when political composition shifts, can claim to be discovering the Constitution's true meaning). The Court's power is checked by amendment and custom, but within the ordinary political process, it controls the doctrine's content and evolution.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, supreme_court, observer).

% A coalition of scholars, former justices, and lower court judges who read the same constitutional text as supporting the broad-effects-test reading. They argue the economic/non-economic distinction is unstable and that aggregation should apply to all activities affecting interstate commerce, not just economic ones. Their reading is present in dissents and academic literature but is not currently the operative law; they are excluded from the constraint's operational machinery.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, dissenting_coalition_broad_effects, excluded,
    powerful, civilizational, constrained, national).

% A coalition of originalist scholars and constitutionalists who argue for a narrower reading: commerce means trade crossing state lines, not all economic activity; regulate means make regular/facilitate, not restrict; federal power is limited to removing state barriers and harmonizing interstate trade rules. This reading is present in originalist jurisprudence and some lower court opinions but is not the operational mainstream; they are excluded from the current doctrinal consensus.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, dissenting_coalition_narrow_originalist, excluded,
    organized, civilizational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__intermediate_channels, federal_authority).
narrative_ontology:fixing_cost_class(commerce_clause_scope__intermediate_channels, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable constitutional frame for distinguishing federal and state jurisdiction over economic and non-economic activity, allowing federal government to solve multi-state economic problems while preserving state authority over family law, criminal procedure, and purely local conduct. Provides a single interpretive standard (three prongs + limiting principles) rather than ad hoc case-by-case negotiation of federalism boundaries.
% TRANSFER_FUNCTION: Transfers authority from states to federal government over channels of interstate commerce, instrumentalities/persons in interstate commerce, and intrastate economic activity substantially affecting interstate commerce. States retain authority over non-economic activity and purely intrastate non-economic conduct, but bear the cost of doctrinal instability when the boundaries are contested (e.g., gun regulation under non-economic prong, marijuana federalism, medical licensure reach).
% ABSENT_VOICES: Originalist critics excluded from the operational doctrine would argue for a narrower 'facilitate interstate trade' reading. Broad-effects-test advocates excluded from the current consensus would argue the limiting principles are incoherent and economic/non-economic is a false distinction. Neither coalition sets the agenda; they write dissents and scholarship.
% DISAPPEARANCE_RATIONALE: If the intermediate-channels reading and its doctrinal structure disappeared overnight, the federalism boundary would need immediate re-negotiation: states might reassert authority over economic activity, Congress would lose the assumption of jurisdiction under the three prongs, and the Supreme Court would face immediate cases demanding clarification of federal vs. state power in commerce. The national economy has been structured on the assumption this constraint exists; removal would force rapid institutional recalibration.
% FOUNDING_PROBLEM: The Constitution grants Congress power to regulate 'Commerce among the several States' but does not define its scope: does it reach intrastate activity? Non-economic activity? Activity that merely affects interstate commerce indirectly? The founding problem was to establish the boundary between federal commerce power and reserved state authority, allowing federal government to address multi-state economic problems (uniform commercial rules, interstate trade barriers) without federalizing all economic and social regulation.
% FOUNDING_PROBLEM_CORROBORATION: The Court attests the founding problem remains live, citing need to maintain federalism boundaries. Federal and state governments depend on the doctrine for operational certainty. However, scholars and lower courts from both originalist and broad-effects camps attest the founding problem has shifted: whether the problem is now 'maintain the original federalism boundaries' (originalist view) or 'allow federal power to reach all economic activity' (broad-effects view) is itself contested. Outside corroboration from legal academics and separation-of-powers theorists confirms the boundary is unstable and periodically re-litigated; they dispute whether the instability is a feature (judicial flexibility) or a bug (doctrinal incoherence).
narrative_ontology:disappearance_verdict(commerce_clause_scope__intermediate_channels, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__intermediate_channels, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__intermediate_channels, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__intermediate_channels, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__intermediate_channels, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__intermediate_channels_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__intermediate_channels_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because federal authority is extensive within the economic sphere and the limiting principles are unstable — the three-prong doctrine and the non-economic/aggregation/attenuated-chain limits provide cover for federal jurisdiction over nearly all economic activity. The doctrine does exclude some regulatory reach (non-economic activity without jurisdictional nexus, attenuated chains, non-aggregable effects) but those exclusions are regularly tested and sometimes circumvented. Suppression is moderate (0.48) because the doctrine does not operate primarily through coercion — it operates through interpretive authority and constitutional legitimacy. States and economic actors navigate within the doctrine; they are not prevented from advocating change via dissent or constitutional amendment. Theater is moderate (0.41) because the limiting principles perform real exclusionary work in some cases (Lopez, Morrison) but are rhetorical cover in others — the doctrine has both functional and theatrical components. The measurement series shows slow extraction drift upward (0.48→0.62 over the interval) and slight theater drift (0.32→0.41), consistent with Supreme Court doctrinal expansion over decades as the Court has applied the three prongs more expansively and narrowed the limiting principles' application. One shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   From the federal authority seat, the constraint is genuine coordination: the three-prong doctrine provides stable rules for federal and state jurisdiction, allowing predictable national economic regulation. From the state authority seat (reserved domains), the constraint is real but asymmetric: states retain authority in non-commerce domains, but the commerce domain boundary drifts toward federal authority over time. From the lower court and intrastate economic actor seats, the constraint is experienced as extractive uncertainty: they navigate rules they do not set and that change. The engine should compute different types at different seats: federal authority may compute toward rope (coordination function is real), state authority toward tangled_rope (coordination with asymmetric extraction as boundaries shift), lower courts and boundary-case actors toward snare (uncertainty and lack of exit). The claim (tangled_rope) reflects the constraint as a whole: genuine federal/state coordination function plus asymmetric extraction of state and actor authority by interpretive drift.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal authority benefits from the constraint: it gains presumed jurisdiction over economic activity in all states. State authority benefits in reserved domains (family, criminal, education) but also pays when those domains intersect commerce (e.g., state medical licensing preempted by federal pharmaceutical regulation). Intrastate economic actors pay because they face jurisdictional uncertainty — their activity may be regulated as a channel, instrumentality, or substantial effect depending on Court interpretation. The non-agent 'conceptual coherence' pays because the limiting principles are unstable. Congress benefits by legislating within the three-prong cover. Lower courts and the Supreme Court set the agenda: they interpret the doctrine and enforce its boundaries, collecting the legitimacy of constitutionalism while exercising vast interpretive power. Directionality for federal authority: near beneficiary (d~0.15). For states in reserved domains: near symmetric (d~0.45) because they benefit (retained authority) and pay (boundary instability). For boundary-case economic actors: near target (d~0.75) because jurisdictional uncertainty is a real cost they cannot exit. For conceptual coherence: near target (d~0.80) because instability in the limiting principles means the doctrine constantly redefines its own boundaries, extracting coherence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — 'define the boundary between federal commerce power and state authority' — remains live in the sense that the boundary is still contested and courts still address it. However, the problem's character has shifted: the original founding problem was 'prevent federal power from abolishing state authority,' which the intermediate reading claims to solve via categorical limits. By end of interval, the actual problem that persists is 'draw the line between economic and non-economic activity' and 'distinguish substantial effects from attenuated effects' — problems the limiting principles claim to solve but do not, because the principles themselves are unstable. The constraint has not fully resolved mandatrophy (the founding problem is not dead), but it has become partially mandatrophic (the original founding problem is addressed by mechanisms that no longer reliably work). This is why measurement series show slow extraction drift: as courts have applied the limiting principles more expansively, the original constraint's stated function (protecting state authority) has attenuated. The constraint persists because the doctrinal structure still legitimates federal jurisdiction and provides the appearance of limits; it has not been replaced because no single alternative reading has coalesced. This is the profile of a constraint approaching piton status: genuine coordination function (federal/state boundary) maintained for institutional inertia but performing less real limiting work than the doctrine claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_noneconomic_boundary_instability,
    'Is the distinction between economic and non-economic activity a stable constitutional category, or is it a manipulation point for the Court to reach preferred outcomes?',
    'Longitudinal analysis of cases where the Court has classified activity as economic or non-economic: if classification tracks the Court''s ideological composition rather than the activity''s intrinsic nature, the category is unstable. Compare cases over decades to detect drift.',
    'If unstable, the limiting principle ''non-economic activity requires jurisdictional element'' is manipulable: the Court can classify any activity as economic and reach it; alternatively, the Court can exclude activities by calling them non-economic. This would mean the victim group includes any intrastate actor whose classification is contestable. If stable, the principle functions as a genuine constitutional limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_noneconomic_boundary_instability, empirical, 'Whether economic/non-economic classification is doctrine or cover story').

omega_variable(
    aggregation_principle_scope_creep,
    'Does the aggregation principle (''substantially affects'' applies only to economic activity) actually constrain federal power, or does it permit federal regulation of all economic activity in any jurisdiction?',
    'Test cases where aggregation has been refused: have those cases involved non-economic activity, or merely economic activity the Court wished to exclude? If purely non-economic cases are the boundary, aggregation is a real limit. If economic cases are excluded, the principle has become a non-limit.',
    'If aggregation is effective only against non-economic activity, federal power extends to all economic activity in any state, substantially constraining state authority. If aggregation extends to economic activity, state authority is severely limited even within commerce. The reading''s claimed moderate extractiveness depends on aggregation being a real boundary; if it is not, extractiveness rises toward the broad-effects reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_principle_scope_creep, empirical, 'Whether aggregation functions as a binding limit on federal power').

omega_variable(
    attenuated_causal_chain_manipulation,
    'How attenuated can the causal chain be before it violates the limiting principle ''cannot regulate via attenuated causal chains''? Is there a stable test, or does the Court treat attenuation as a manipulation point?',
    'Collect cases rejecting federal jurisdiction on attenuated-chain grounds (e.g., Lopez, Morrison, NFIB); compare their causal chains to cases accepting jurisdiction on similar chains. If chains of similar length are rejected in some cases and accepted in others, the principle is manipulable.',
    'If the attenuated-chain test is stable and enforced, the limiting principle functions and constrains federal power over activities far from interstate commerce. If unstable, the principle provides rhetorical cover but not real constraint. The reading''s claimed medium extractiveness assumes the test functions; if it is manipulated, extractiveness rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attenuated_causal_chain_manipulation, empirical, 'Whether attenuated-chain limits are binding or rhetorical').

omega_variable(
    kernel_reading_alternative_forecloses,
    'Does the intermediate-channels reading logically foreclose the narrow-originalist reading, or can both readings be maintained within different interpretive frameworks?',
    'Examine whether accepting the three-prong doctrine (channels, instrumentalities, substantial effects) requires rejecting the originalist ''trade crossing state lines'' definition of Commerce. If a party can accept the intermediate reading for current law while holding that the original meaning was narrower, both readings coexist; if accepting three prongs requires denying originalism, one forecloses the other.',
    'If forecloses: the readings are in direct contradiction and cannot both be true in the same framework. If coexists: both are live positions held by different parties (originalists can accept current doctrine while arguing for amendment). This affects how the constraint models rivalry among readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_forecloses, conceptual, 'Logical relationship between intermediate-channels and narrow-originalist readings').

omega_variable(
    kernel_reading_alternative_coexists_broad,
    'Does the intermediate-channels reading coexist with the broad-effects reading, or do the limiting principles logically foreclose broad effects?',
    'Examine whether limiting principles (non-economic requires nexus, aggregation only for economic, no attenuated chains) are structurally incompatible with broad-effects doctrine. If broad-effects can accept the limiting principles as applied narrowly while arguing they should not apply at all, coexistence holds; if broad-effects requires rejecting the entire three-prong structure, foreclosure applies.',
    'If coexists: both readings are live in jurisprudence and scholarship. If forecloses: adoption of intermediate-channels would require formal overruling of broad-effects positions. This affects the model of doctrinal rivalry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_coexists_broad, conceptual, 'Logical relationship between intermediate-channels and broad-effects readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__intermediate_channels, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_scope__intermediate_channels, theater_ratio, 0, 0.32).
narrative_ontology:measurement(comm_tr_t5, commerce_clause_scope__intermediate_channels, theater_ratio, 5, 0.34).
narrative_ontology:measurement(comm_tr_t10, commerce_clause_scope__intermediate_channels, theater_ratio, 10, 0.37).
narrative_ontology:measurement(comm_tr_t15, commerce_clause_scope__intermediate_channels, theater_ratio, 15, 0.39).
narrative_ontology:measurement(comm_tr_t20, commerce_clause_scope__intermediate_channels, theater_ratio, 20, 0.4).
narrative_ontology:measurement(comm_tr_t25, commerce_clause_scope__intermediate_channels, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_scope__intermediate_channels, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(comm_be_t5, commerce_clause_scope__intermediate_channels, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(comm_be_t10, commerce_clause_scope__intermediate_channels, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(comm_be_t15, commerce_clause_scope__intermediate_channels, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(comm_be_t20, commerce_clause_scope__intermediate_channels, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(comm_be_t25, commerce_clause_scope__intermediate_channels, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_scope__intermediate_channels, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(comm_su_t5, commerce_clause_scope__intermediate_channels, suppression_requirement, 5, 0.44).
narrative_ontology:measurement(comm_su_t10, commerce_clause_scope__intermediate_channels, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(comm_su_t15, commerce_clause_scope__intermediate_channels, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(comm_su_t20, commerce_clause_scope__intermediate_channels, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(comm_su_t25, commerce_clause_scope__intermediate_channels, suppression_requirement, 25, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__intermediate_channels, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__intermediate_channels, 0.12).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, dormant_commerce_clause_state_regulatory_power).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, tenth_amendment_reserved_powers).

% DUAL FORMULATION NOTE:
% The commerce_clause_scope kernel decomposes into three constraint stories corresponding to three live judicial readings: narrow_originalist (trade crossing state lines, regulate=facilitate, minimal federal power), intermediate_channels (three prongs + limiting principles, moderate federal power), and broad_effects_test (substantially affects in aggregate, no limiting principles, maximum federal power). Each reading instantiates a different constraint with different ε, beneficiary/victim structures, and computed types. The intermediate_channels reading is the operative mainstream law; the other two are dissenting positions with varying institutional presence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__intermediate_channels, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
