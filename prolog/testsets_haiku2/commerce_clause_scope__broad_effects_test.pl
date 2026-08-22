% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__broad_effects_test
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__broad_effects_test, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: commerce_clause_scope__broad_effects_test
 *   human_readable: Commerce Clause Broad Effects Test Interpretation
 *   domain: constitutional/federalism/political
 *
 * SUMMARY:
 *   The Commerce Clause of the U.S. Constitution grants Congress power to
 *   'regulate commerce among the states.' The broad effects test reading
 *   interprets this to mean: any economic activity that substantially affects
 *   interstate commerce in the aggregate falls within federal jurisdiction;
 *   'regulate' includes prohibition and comprehensive control; federal power
 *   extends to intrastate activities based on their cumulative national
 *   economic impact. This reading is instantiated in federal courts and
 *   administrative practice, particularly since the 1930s–1970s. It stands in
 *   contest with narrower originalist readings (commerce means trade crossing
 *   state lines only) and intermediate positions (federal power extends to
 *   channels and instrumentalities, and to substantially affecting
 *   activities, but subject to limiting principles for non-economic activity
 *   and attenuated causation). This constraint story models ONLY the broad
 *   effects test reading, treating it as one reading of the contested
 *   commerce-clause-scope kernel. The claim/metric divergence is intentional:
 *   the reading is claimed as tangled_rope (genuine coordination function for
 *   national market integration + asymmetric extraction of state sovereignty)
 *   while the authored metrics describe high extractiveness and substantial
 *   suppression of state autonomy. The engine measures that structural gap.
 *
 * KEY AGENTS:
 *   - federal_regulators — institutional agenda-setter; derive authority from this reading; set jurisdictional boundaries
 *   - national_interest_groups — organized beneficiaries; depend on federal authority to impose uniform standards
 *   - civil_rights_constituencies — organized beneficiaries; depend on federal jurisdiction to enforce rights against state discrimination
 *   - state_legislatures — institutional payers; lose sovereign authority over intrastate economic regulation
 *   - state_police_powers — doctrine (non-agent); subordinated by this reading to federal commerce authority
 *   - federal_courts — institutional agenda-setter; author and enforce the reading through precedent
 *   - Congress — institutional beneficiary and agenda-setter; expands legislative authority under this reading
 *   - narrow_originalist and intermediate-channels supporters — excluded; their alternative framings are marginalized in doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, 0.81).
domain_priors:suppression_score(commerce_clause_scope__broad_effects_test, 0.68).
domain_priors:theater_ratio(commerce_clause_scope__broad_effects_test, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, extractiveness, 0.81).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Commerce Clause Broad Effects Test Interpretation").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional/federalism/political").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, '8f3f213b-c32f-41db-a8b7-f5944d252f0a').
narrative_ontology:cs_kernel_codification('8f3f213b-c32f-41db-a8b7-f5944d252f0a', fixed_text).
narrative_ontology:cs_authority_grounding('8f3f213b-c32f-41db-a8b7-f5944d252f0a', lineage).
narrative_ontology:cs_interpretation_layer_present('8f3f213b-c32f-41db-a8b7-f5944d252f0a').
narrative_ontology:cs_reading_relation('8f3f213b-c32f-41db-a8b7-f5944d252f0a', commerce_clause_scope__narrow_originalist, forecloses).
narrative_ontology:cs_reading_relation('8f3f213b-c32f-41db-a8b7-f5944d252f0a', commerce_clause_scope__intermediate_channels, influences).
narrative_ontology:cs_axiom('8f3f213b-c32f-41db-a8b7-f5944d252f0a', foundational, commerce_includes_intrastate_cumulative_effects).
narrative_ontology:cs_axiom_status(commerce_includes_intrastate_cumulative_effects, holdable).
narrative_ontology:cs_axiom_grounding('8f3f213b-c32f-41db-a8b7-f5944d252f0a', commerce_includes_intrastate_cumulative_effects, empirically_contingent).
narrative_ontology:cs_axiom('8f3f213b-c32f-41db-a8b7-f5944d252f0a', foundational, federal_authority_subsumes_state_police_powers_on_economic_effects).
narrative_ontology:cs_axiom_status(federal_authority_subsumes_state_police_powers_on_economic_effects, holdable).
narrative_ontology:cs_axiom_grounding('8f3f213b-c32f-41db-a8b7-f5944d252f0a', federal_authority_subsumes_state_police_powers_on_economic_effects, deontological).
narrative_ontology:cs_reference_frame('8f3f213b-c32f-41db-a8b7-f5944d252f0a', new_deal_federal_emergency_authority).
narrative_ontology:cs_drift_state('8f3f213b-c32f-41db-a8b7-f5944d252f0a', contemporary_social_policy_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8f3f213b-c32f-41db-a8b7-f5944d252f0a', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_regulators).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_interest_groups).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, civil_rights_enforcement_constituencies).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_legislatures).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_economic_experimentation).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_police_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, congress).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, small_businesses_localized).
narrative_ontology:constraint_vindicates(commerce_clause_scope__broad_effects_test, national_economic_integration_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__broad_effects_test, cumulative_effects_aggregation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Commerce Clause to authorize comprehensive federal regulation of economic activity claimed to substantially affect interstate commerce in the aggregate. Set the jurisdictional threshold for federal power. Under this reading, can regulate intrastate activities based on cumulative economic effects. Benefit from the expansive authority the reading confers and the uniformity it enables across state boundaries.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Seek uniform national regulatory standards (environmental, labor, consumer protection, civil rights). Benefit from federal jurisdiction to impose these standards on all economic actors regardless of state-line location. Avoid the friction and regulatory arbitrage of navigating fifty different state regimes. No direct regulatory power but strong alliance with federal agencies that adopt this reading.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_interest_groups, beneficiary,
    organized, generational, mobile, national).

% Depend on federal Commerce Clause authority to enforce civil rights protections (voting rights, public accommodations, employment discrimination, housing) against state and local actors who would otherwise control these domains. Under this reading, intrastate discrimination is reachable because it affects interstate commerce (integrated national markets). Without this jurisdiction, state-level discrimination would persist unchecked.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, civil_rights_enforcement_constituencies, beneficiary,
    organized, generational, constrained, national).

% Lose sovereign authority over intrastate economic activity within their borders. Cannot set regulatory boundaries on the grounds that an activity is local—if it substantially affects interstate commerce in the aggregate, federal authority claims preemption. Must either comply with federal standards, bring cases to overturn them, or formally secede (not a real option). Their police powers (health, safety, welfare within state boundaries) are subordinated to federal economic regulation.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_legislatures, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__broad_effects_test, state_legislatures, excluded).

% Local and state-level regulatory experimentation (alternative employment rules, local labor standards, state agricultural policy, local land use, regional environmental rules differing from national standards) becomes subject to federal preemption claims. States cannot serve as 'laboratories of democracy' in economic domains once those domains are declared to substantially affect interstate commerce.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, local_economic_experimentation, payer,
    powerless, biographical, trapped, local).

% A doctrine (not an agent): the reserve power of states to regulate for health, safety, and welfare within their borders. This reading subordinates that doctrine to federal commerce authority whenever the regulated activity has any substantial economic effect in the aggregate. The doctrine persists in name but is hollowed in scope.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_police_powers, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__broad_effects_test, state_police_powers).

% Hold the counterclaim that 'commerce among states' means only trade crossing state lines and that 'regulate' means facilitate, not restrict. They argue the broad effects test collapses federalism into a formality. Their position is not in the room when federal agencies interpret their authority under this reading; their challenge route is litigation, which faces a settled interpretive baseline.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, narrow_originalist_reading_supporters, excluded,
    analytical, generational, identity_locked, national).

% Advance a middle position: federal power extends to channels and instrumentalities of interstate commerce and to activities substantially affecting it, BUT subject to limiting principles (non-economic activity requires jurisdictional element, aggregation applies only to economic activity, no attenuated causal chains). They argue the broad effects test removes these safeguards. Their voice is present in academic constitutional law but marginalized in judicial doctrine.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, intermediate_channels_reading_supporters, excluded,
    analytical, generational, identity_locked, national).

% Adjudicate whether federal regulation is within Commerce Clause authority. Under the broad effects test reading, courts defer substantially to congressional judgment about whether an activity substantially affects interstate commerce. They enforce the reading by upholding federal regulation even when the causal chain is attenuated. They also author the precedent the reading rests on.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Enacts federal regulation claimed to address activities substantially affecting interstate commerce. Under the broad effects test, can regulate virtually any economic activity by asserting (and resting on precedent that such assertions are deferred to) that it affects interstate commerce in the aggregate. Expands federal legislative jurisdiction and centralizes authority.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, congress, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__broad_effects_test, congress, agenda_setter).

% Subject to federal regulation (labor standards, environmental rules, licensing) even if their business operates only within one state, because the cumulative effect of similar businesses in other states affects interstate commerce. Cannot invoke state autonomy as a shield; federal authority reaches them directly. Bear compliance costs from federal standards that may not reflect local conditions.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, small_businesses_localized, payer,
    moderate, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__broad_effects_test, federal_regulators).
narrative_ontology:fixing_cost_class(commerce_clause_scope__broad_effects_test, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables uniform national economic regulation: by asserting federal authority over activities with cumulative interstate effects, the reading permits Congress to impose consistent rules across all states simultaneously, avoiding regulatory arbitrage and facilitating an integrated national market. Solves the coordination problem of 50 independent regulators unable to internalize the effects of their choices on other states' commerce.
% TRANSFER_FUNCTION: Transfers sovereign authority from state legislatures to federal institutions (Congress, federal courts, federal agencies). States pay in autonomy over intrastate economic regulation; federal regulators and constituencies benefiting from uniform national standards gain authority to set and enforce those standards nationwide.
% ABSENT_VOICES: Narrow originalist and intermediate-channels reading supporters are excluded from the institutional process that settles Commerce Clause meaning. They appear as litigants and amici in constitutional cases, but the baseline interpretive framework is already established; their challenges face an entrenched precedent. State legislatures are nominally participants but are bound by the outcomes rather than partners in setting the jurisdictional boundary.
% DISAPPEARANCE_RATIONALE: If the broad effects test interpretation disappeared overnight (i.e., if courts reverted to a narrower reading of Commerce Clause authority), federal regulatory authority would shrink dramatically. Environmental, labor, and civil rights enforcement at the federal level would contract to activities directly crossing state lines or involving federal property. State experimentation would resume; regulatory authority would decentralize. The integrated national regulatory state depends structurally on this reading.
% FOUNDING_PROBLEM: The Great Depression and the economic crisis of the 1930s created pressure for federal intervention in economic activity; the Court initially resisted, striking down New Deal legislation. The founding problem was: how can the federal government respond to national economic emergencies when the Constitution grants power only over 'commerce among states'? The Court's answer (developed through the 1930s–1970s) was to expand 'commerce' to include any activity substantially affecting interstate commerce in the aggregate.
% FOUNDING_PROBLEM_CORROBORATION: Federal regulators and Congress attest the founding problem remains live: continued need for uniform national responses to economic crises, public health, and civil rights. State legislatures and federalism advocates attest the problem was solved and the reading persists as an instrument of centralized power. Academic constitutional law is divided; historical economic analysis shows that the 1930s crisis created genuine coordination problems, but contemporary enforcement (labor, environment) extends far beyond crisis response. Independent observers note the reading has expanded well beyond the original justification.
narrative_ontology:disappearance_verdict(commerce_clause_scope__broad_effects_test, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__broad_effects_test, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__broad_effects_test, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__broad_effects_test, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__broad_effects_test_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__broad_effects_test_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) because the reading transfers sovereignty from states to federal institutions. States lose the ability to set regulatory boundaries for economic activity within their borders; if an activity substantially affects interstate commerce in the aggregate, federal authority claims preemption. Suppression is moderately high (0.68) because state challenges to this interpretation face an entrenched precedent baseline; federal courts defer to congressional judgments about whether an activity substantially affects interstate commerce. The suppression is enforcement of the reading's jurisdictional claim, not brute coercion, but it works to prevent state experimentation. Theater is moderate (0.42): the coordination rhetoric (uniform national standards, integrated markets) is genuine and substantial, but a portion of enforcement activity serves to prevent state reversals rather than to achieve the original coordination goal. The measurement series trace rising extractiveness and theater over the interval, reflecting expansion of federal regulatory reach and increasing use of the reading to justify regulations addressing social policy (health, safety) beyond the original economic crisis justification. All metrics share one time grid so every measurement is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The federal institutional seat and the state legislative seat compute fundamentally differently from this constraint. From the federal seat, it is genuine coordination: solving the alignment problem of 50 independent regulators, enabling crisis response, facilitating civil rights enforcement. From the state seat, it is extraction disguised as coordination: the federal seat seized authority, narrowed state options, and now defends its domain by suppressing state challenges and alternative interpretations. Neither seat is wrong about what it observes; they occupy incommensurable positions relative to the constraint. The engine computes per-seat classification from structural data; the divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality overrides are not needed: the automatic derivation from beneficiary/victim declarations and exit_options produces accurate d values. Federal regulators are beneficiaries with high power and multiple exit options (can shift interpretation, accept new doctrine) → d low, subsidized position. State legislatures are victims with institutional power but trapped exit (cannot opt out of the federal system) → d high, target position. Civil rights constituencies are beneficiaries with constrained exit (depend on federal jurisdiction; if it shrinks, their enforcement capacity contracts) → d moderate, asymmetric position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandate is to coordinate national economic regulation and enable federal response to interstate effects that state-level regulation cannot address. The founding problem (Great Depression, economic crisis, need for federal intervention) is contested: federal regulators attest it remains live; states and federalism scholars attest it was solved decades ago and the reading now persists as a vehicle for centralizing power. The disappearance verdict is world_rearranges: if the reading were overturned (reversion to narrower Commerce Clause interpretation), federal regulatory authority would contract dramatically. The mismatch between mandatrophy_resolved (is the founding problem still live?) and the observed persistence of the reading is precisely where the tangled-rope character emerges: there is a genuine coordination function, but there is also extraction. The reading is not a pure snare; state experimentation is not completely suppressed, and federal regulation does solve authentic collective-action problems. But the reading's expansion far beyond the founding crisis, combined with federal courts' deference to congressional assertions about economic effects, suggests the coordination justification has been stretched to cover extraction. Theater_ratio rising over time (0.25 → 0.42) supports this: performance maintenance increases as the founding crisis recedes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (national economic coordination during crises) still live, or has it been substantially solved such that the reading now functions primarily as an instrument of centralized power?',
    'Empirical assessment: examine whether federal regulation under this reading continues to address genuinely interstate coordination problems (crises, market failures, spillovers) or has expanded to address intrastate social policy (health, safety, morality) decoupled from interstate economic effects. Historical analysis: when did the reading''s scope exceed the crisis-response justification?',
    'If the problem is dead and the reading persists, the constraint is a piton or snare (mandatrophy present). If the problem is live, the tangled_rope classification holds — coordination + extraction coexist. If contested, the classification sits at the boundary (snare_candidate if analysis leans toward dead; tangled_rope if toward live).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the Commerce Clause broad effects test addresses a live coordination problem or persists as extraction.').

omega_variable(
    natural_vs_constructed_federalism_boundary,
    'Is the federalism boundary (state vs. federal authority) a natural feature of constitutional structure, or is it a constructed allocation that the Commerce Clause reading actively reshapes?',
    'Originalist analysis: compare the reading''s interpretation of ''commerce'' and ''regulate'' to the enumerative Constitution''s text and original understanding. Structural analysis: examine whether the broad effects test interpretation was chosen or merely discovered in the Constitution''s text.',
    'If natural, the reading reveals a boundary the Constitution embeds; the extraction is incidental to coordination. If constructed, the reading is a false-summit candidate: it claims to discover what federal authority the Constitution grants, but it actually decides, through interpretive choice, how much authority it grants.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_federalism_boundary, conceptual, 'Whether the federalism boundary is constitutional structure or interpretive construction.').

omega_variable(
    cumulative_effects_aggregation_ambiguity,
    'Does ''cumulative effects'' justify federal regulation of ANY economic activity (because virtually any activity has some effect in aggregate), or are there principled limiting doctrines that apply only to truly national economic phenomena?',
    'Doctrinal analysis: examine judicial precedent for limiting principles on aggregation. Counterfactual: ask whether Congress could plausibly claim federal authority over local agriculture, local manufacturing, local personal services, and identify where the courts would say ''no'' — that boundary marks the operational limit.',
    'If aggregation is effectively unlimited, the reading approaches snare (federal authority covers everything; state autonomy is nil). If limiting principles are operational, the tangled_rope classification holds (genuine coordination on interstate activities; extraction limited by residual state authority over intrastate affairs).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cumulative_effects_aggregation_ambiguity, empirical, 'Whether cumulative effects aggregation has principled limits or is effectively unlimited.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression (0.68) structural (federal courts enforce doctrinal baselines that block state challenges) or internalized (state legislatures have accepted the reading as legitimate and no longer mount serious challenges)?',
    'Behavioral analysis: examine state legislative proposals to circumvent federal authority; track litigation rates challenging federal jurisdiction; assess whether state political elites still view federalism as a live constraint or have normalized federal supremacy.',
    'If suppression is structural, states are resisting but constrained by courts and precedent — removal of the suppression mechanism would restore state experimentation. If internalized, states have fused their identity with federal subordination — even removal of formal constraints might not restore state autonomy (the suppression is now inside state preferences).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression of state autonomy is structural or internalized in state preferences.').

omega_variable(
    reading_contention_kernel_level,
    'Is this constraint one reading of a contested kernel (the Commerce Clause as a commitment subject to multiple interpretations), or does it claim to represent the Constitution''s settled meaning?',
    'The presence of live alternative readings (narrow_originalist, intermediate_channels) in academic and some judicial venues indicates the kernel remains contested. The counting authority (federal courts under the broad effects test) claims settled meaning; the counting opposition claims the reading is a construction, not a discovery.',
    'If the kernel is genuinely contested, this reading''s classification can diverge from the narrow reading''s (same constitutional text, different constraints generated from different readings). If the broad effects test reading claims to have resolved the contest and foreclosed alternatives, the claim exceeds what the evidence supports.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contention_kernel_level, conceptual, 'Whether this reading claims to settle the Commerce Clause or acknowledges sibling readings as live.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_scope__broad_effects_test, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t15, commerce_clause_scope__broad_effects_test, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(comm_tr_t15, observed).
narrative_ontology:measurement(comm_tr_t30, commerce_clause_scope__broad_effects_test, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(comm_tr_t30, observed).
narrative_ontology:measurement(comm_tr_t45, commerce_clause_scope__broad_effects_test, theater_ratio, 45, 0.37).
narrative_ontology:measurement_basis(comm_tr_t45, observed).
narrative_ontology:measurement(comm_tr_t60, commerce_clause_scope__broad_effects_test, theater_ratio, 60, 0.4).
narrative_ontology:measurement_basis(comm_tr_t60, observed).
narrative_ontology:measurement(comm_tr_t75, commerce_clause_scope__broad_effects_test, theater_ratio, 75, 0.41).
narrative_ontology:measurement_basis(comm_tr_t75, observed).
narrative_ontology:measurement(comm_tr_t90, commerce_clause_scope__broad_effects_test, theater_ratio, 90, 0.42).
narrative_ontology:measurement_basis(comm_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_scope__broad_effects_test, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t15, commerce_clause_scope__broad_effects_test, base_extractiveness, 15, 0.71).
narrative_ontology:measurement_basis(comm_be_t15, observed).
narrative_ontology:measurement(comm_be_t30, commerce_clause_scope__broad_effects_test, base_extractiveness, 30, 0.74).
narrative_ontology:measurement_basis(comm_be_t30, observed).
narrative_ontology:measurement(comm_be_t45, commerce_clause_scope__broad_effects_test, base_extractiveness, 45, 0.77).
narrative_ontology:measurement_basis(comm_be_t45, observed).
narrative_ontology:measurement(comm_be_t60, commerce_clause_scope__broad_effects_test, base_extractiveness, 60, 0.79).
narrative_ontology:measurement_basis(comm_be_t60, observed).
narrative_ontology:measurement(comm_be_t75, commerce_clause_scope__broad_effects_test, base_extractiveness, 75, 0.8).
narrative_ontology:measurement_basis(comm_be_t75, observed).
narrative_ontology:measurement(comm_be_t90, commerce_clause_scope__broad_effects_test, base_extractiveness, 90, 0.81).
narrative_ontology:measurement_basis(comm_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_scope__broad_effects_test, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t15, commerce_clause_scope__broad_effects_test, suppression_requirement, 15, 0.61).
narrative_ontology:measurement_basis(comm_su_t15, observed).
narrative_ontology:measurement(comm_su_t30, commerce_clause_scope__broad_effects_test, suppression_requirement, 30, 0.63).
narrative_ontology:measurement_basis(comm_su_t30, observed).
narrative_ontology:measurement(comm_su_t45, commerce_clause_scope__broad_effects_test, suppression_requirement, 45, 0.65).
narrative_ontology:measurement_basis(comm_su_t45, observed).
narrative_ontology:measurement(comm_su_t60, commerce_clause_scope__broad_effects_test, suppression_requirement, 60, 0.66).
narrative_ontology:measurement_basis(comm_su_t60, observed).
narrative_ontology:measurement(comm_su_t75, commerce_clause_scope__broad_effects_test, suppression_requirement, 75, 0.67).
narrative_ontology:measurement_basis(comm_su_t75, observed).
narrative_ontology:measurement(comm_su_t90, commerce_clause_scope__broad_effects_test, suppression_requirement, 90, 0.68).
narrative_ontology:measurement_basis(comm_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__broad_effects_test, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__broad_effects_test, 0.18).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__intermediate_channels).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, state_police_powers_doctrine).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, federalism_reserve_powers).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, civil_rights_federal_enforcement_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the commerce_clause_scope kernel. The kernel is the Commerce Clause's scope — a contested constitutional commitment. Three structurally distinct readings generate three distinct constraints with different victim sets, beneficiary structures, and ε values: broad_effects_test (this file, high extraction from state sovereignty); narrow_originalist (federal authority limited to state-line trade); intermediate_channels (federal authority to substantially affecting activities, with limiting principles). Each reading is an independent constraint story. They are linked via network.affects_constraints because they are siblings in the same kernel family; a shift in constitutional doctrine from one reading to another would restructure state/federal authority relationships throughout the system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
