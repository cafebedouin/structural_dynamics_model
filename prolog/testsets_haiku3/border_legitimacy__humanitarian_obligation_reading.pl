% ============================================================================
% CONSTRAINT STORY: border_legitimacy__humanitarian_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: border_legitimacy__humanitarian_obligation_reading
 *   human_readable: Humanitarian Obligation Distinction: Refugees vs. Economic Migrants
 *   domain: political/international_law
 *
 * SUMMARY:
 *   This constraint instantiates the humanitarian obligation reading of the
 *   border legitimacy kernel: states have a duty to admit those fleeing
 *   persecution or disaster, but retain legitimate authority to exclude
 *   general economic migrants. The reading operationalizes a categorical
 *   distinction (refugee vs. economic migrant) that mediates between human
 *   rights claims and sovereignty claims. The constraint is CLAIMED as
 *   tangled rope (genuine coordination function + asymmetric extraction) to
 *   reflect the reading's own internal structure: it coordinates
 *   international responsibility for persecution victims while simultaneously
 *   enabling mass exclusion of those classified as economic migrants. The
 *   reading's truth is the one constraint this JSON describes; sibling
 *   readings (sovereignty, freedom of movement) are separate constraints with
 *   different beneficiary structures and ε values.
 *
 * KEY AGENTS:
 *   - States as gatekeepers: institutional power, arbitrage exit, control the categorical definition
 *   - Refugees and persecution victims: powerless, trapped exit, beneficiary status clarified by the distinction
 *   - Economic migrants: powerless, trapped exit, structural victims of the exclusion
 *   - Asylum seekers in borderline cases: powerless, trapped exit, caught in the interpretive boundary
 *   - Host countries: moderate power, constrained exit, bear capacity cost but benefit from burden-shifting
 *   - International refugee convention body: institutional power, observer, interprets categorical boundaries
 *   - NGOs: organized power, constrained exit, document ground reality, pressure boundary
 *   - Climate-displaced and disaster survivors: powerless, trapped exit, excluded from the framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.62).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.58).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Humanitarian Obligation Distinction: Refugees vs. Economic Migrants").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, '4bf2e7e4-cc42-4be0-b939-ae399ee95b6d').
narrative_ontology:cs_kernel_codification('4bf2e7e4-cc42-4be0-b939-ae399ee95b6d', fixed_text).
narrative_ontology:cs_authority_grounding('4bf2e7e4-cc42-4be0-b939-ae399ee95b6d', lineage).
narrative_ontology:cs_interpretation_layer_present('4bf2e7e4-cc42-4be0-b939-ae399ee95b6d').
narrative_ontology:cs_reading_relation('4bf2e7e4-cc42-4be0-b939-ae399ee95b6d', border_legitimacy__sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('4bf2e7e4-cc42-4be0-b939-ae399ee95b6d', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_axiom('4bf2e7e4-cc42-4be0-b939-ae399ee95b6d', foundational, persecution_creates_special_claim).
narrative_ontology:cs_axiom_status(persecution_creates_special_claim, holdable).
narrative_ontology:cs_axiom_grounding('4bf2e7e4-cc42-4be0-b939-ae399ee95b6d', persecution_creates_special_claim, deontological).
narrative_ontology:cs_axiom('4bf2e7e4-cc42-4be0-b939-ae399ee95b6d', foundational, state_discretion_over_economic_migration).
narrative_ontology:cs_axiom_status(state_discretion_over_economic_migration, holdable).
narrative_ontology:cs_axiom_grounding('4bf2e7e4-cc42-4be0-b939-ae399ee95b6d', state_discretion_over_economic_migration, deontological).
narrative_ontology:cs_reference_frame('4bf2e7e4-cc42-4be0-b939-ae399ee95b6d', post_1951_refugee_convention_framework).
narrative_ontology:cs_drift_state('4bf2e7e4-cc42-4be0-b939-ae399ee95b6d', contemporary_climate_and_conflict_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4bf2e7e4-cc42-4be0-b939-ae399ee95b6d', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, states_as_gatekeepers).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, asylum_seekers_borderline_cases).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, refugees_and_persecution_victims).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, asylum_seekers_borderline_cases).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, host_countries).
narrative_ontology:constraint_vindicates(border_legitimacy__humanitarian_obligation_reading, human_rights_doctrine_persecution_primacy).
narrative_ontology:constraint_vindicates(border_legitimacy__humanitarian_obligation_reading, territorial_sovereignty_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and enforces the refugee/economic-migrant distinction through asylum law, border control, and deportation machinery. Controls the categorical definition that determines who may enter and who must be excluded. Justifies the distinction as respecting both humanitarian obligation and legitimate sovereign control of borders. Collects the benefit of excluding the broader economic migrant population while appearing to honor humanitarian duty.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, states_as_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, national).

% Those fleeing imminent persecution, genocide, or life-threatening disaster are recognized as having a claim to admission under international refugee convention and humanitarian obligation. Their status is clarified (in theory) by the categorical distinction; their access to asylum is legitimated by the same framework that excludes others. Benefit from the moral clarity of their category even though many face years of uncertainty and precarious conditions in camps or host countries.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, refugees_and_persecution_victims, beneficiary,
    powerless, immediate, trapped, global).

% Individuals fleeing poverty, lack of economic opportunity, climate-driven livelihood collapse, or failed states where basic services are absent but no organized persecution occurs. Classified as ineligible under the humanitarian obligation framework and subject to exclusion, deportation, or confinement in informal detention. Many face conditions structurally similar to persecution (starvation, violence, family separation) but fall outside the categorical definition and thus outside legitimate admission claims.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, economic_migrants, payer,
    powerless, biographical, trapped, global).

% Those fleeing generalized violence, state collapse, or environmental catastrophe that does not fit the persecution category but is nonetheless life-threatening. Neither clearly economic migrants nor clearly persecution victims. Caught in the boundary of the distinction; outcomes depend on adjudicators' interpretation of whether their circumstances meet the categorical threshold. Many are held in limbo pending asylum determination, facing suppression through procedural delay and evidentiary burden.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, asylum_seekers_borderline_cases, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__humanitarian_obligation_reading, asylum_seekers_borderline_cases, beneficiary).

% The doctrine of humanitarian obligation and human rights protection is vindicated by the constraint's operation — the categorical distinction between refugees and economic migrants operationalizes the principle that persecution creates a special moral claim while economic deprivation alone does not. The doctrine benefits from the constraint's existence because the constraint makes the doctrine legible and actionable in law.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, humanitarian_doctrine_carriers, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(border_legitimacy__humanitarian_obligation_reading, humanitarian_doctrine_carriers).

% The doctrine of territorial sovereignty and state authority is vindicated by the constraint — the state's right to control borders and define entry categories is preserved and operationalized through the refugee/economic-migrant distinction. The constraint demonstrates that sovereignty and humanitarian obligation can coexist within a single framework.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, sovereignty_doctrine_carriers, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(border_legitimacy__humanitarian_obligation_reading, sovereignty_doctrine_carriers).

% Bear the cost of asylum systems, refugee camps, and the institutional machinery of determination. Must balance humanitarian admission with fiscal and social capacity. The distinction helps them manage volume by explicitly excluding economic migrants, though many host countries in the Global South carry massive refugee populations despite limited resources. The constraint legitimates burden-shifting by allowing wealthier states to exclude migrants labeled economic.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, host_countries, payer,
    moderate, biographical, constrained, national).

% Individuals displaced by drought, flooding, or climate-driven resource collapse are structurally absent from the humanitarian obligation framework unless they can frame displacement as persecution. Their need is acute and their exit options zero, but the categorical distinction treats them as economic migrants. They would advocate for expansion of protected categories but have no voice in border policy formation.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, climate_displaced_and_disaster_survivors, excluded,
    powerless, immediate, trapped, global).

% Interprets and adjudicates refugee status under international law. Carries the authority to clarify categorical boundaries through case law and procedural guidance. Sits between state gatekeepers and affected populations; can recommend expansion or contraction of protected categories but relies on state compliance.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, international_refugee_convention_body, observer,
    institutional, generational, analytical, universal).

% Document conditions on the ground and advocate for protection standards. See the practical limitation of the categorical distinction in real-time: individuals fleeing generalized violence that is not organized persecution, or climate disaster that is life-threatening but not persecution. Their testimony pressures the boundary between categories but does not control it.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, non_governmental_humanitarian_organizations, observer,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__humanitarian_obligation_reading, states_as_gatekeepers).
narrative_ontology:fixing_cost_class(border_legitimacy__humanitarian_obligation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operationalizes humanitarian obligation by creating a categorical distinction that clarifies which migrations trigger international legal duties and which fall under sovereign discretion. Allows states to offer asylum to those with special claims (persecution victims) while maintaining the prerogative to control general migration flow. Provides a legible framework for resource allocation and institutional design of asylum systems.
% TRANSFER_FUNCTION: Transfers the burden of accommodating desperate populations from wealthy states and low-capacity host countries to a bifurcated system: persecution victims gain formal recognition and (in theory) legal protection; economic migrants are excluded, detained, or deported, transferring their survival risk back to origin countries or fragile host states. The distinction allows resource-rich states to avoid capacity expansion by defining most desperate populations as ineligible.
% ABSENT_VOICES: Climate-displaced persons, those fleeing generalized violence that is not organized persecution, individuals facing starvation in failed states but no specific persecution, and economic migrants themselves are structurally excluded from the initial framing and agenda-setting of border policy. They can testify about their conditions but cannot define the categories that determine their legal status. Their absence is enforced by the categorical framework itself.
% DISAPPEARANCE_RATIONALE: If the humanitarian obligation distinction vanished, border control would revert to pure sovereignty (all migration subject to state discretion) or pure human rights (freedom of movement as a right). Host countries would lose the legitimating framework that allows them to claim humanitarian identity while excluding most desperate populations. Asylum systems would either collapse into unrestricted entry or shift toward explicit economic/capacity-based selection. The current compromise would dissolve.
% FOUNDING_PROBLEM: Post-WWII recognition that states have a minimal humanitarian obligation toward those fleeing organized persecution (genocide, political targeting, religious purge), but also recognition that states retain legitimate authority to control borders and manage population flow for economic and social reasons. The constraint was built to reconcile these two commitments by creating a category (refugee/persecution) that triggers obligation and a residual category (economic migrant) that does not.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR and international refugee law bodies attest that persecution-based claims remain live and require admission. Economic migrants and humanitarian organizations attest that the founding problem has shifted: climate displacement, state collapse, and generalized violence now account for more forced migration than organized persecution. Wealthier states attest the founding distinction remains valid; lower-capacity host countries attest it is operationally broken because their refugee populations now include massive cohorts of climate-displaced and economically desperate persons who do not fit the persecution category. No external consensus: the divergence itself is the signal.
narrative_ontology:disappearance_verdict(border_legitimacy__humanitarian_obligation_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__humanitarian_obligation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__humanitarian_obligation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_legitimacy__humanitarian_obligation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__humanitarian_obligation_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__humanitarian_obligation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__humanitarian_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the moderate but real extraction this reading enables: the categorical distinction allows wealthier states to exclude populations in desperate circumstances by re-labeling them economic rather than humanitarian victims. Suppression (0.58) captures the enforcement machinery required to maintain the distinction (asylum determination, deportation, detention of those deemed ineligible). Theater (0.42) reflects the gap between the humanitarian justification and the exclusionary outcome: humanitarian language legitimates exclusion. Accessibility collapse (0.48) is moderate because alternatives exist (smuggling, irregular entry, seeking asylum in neighboring countries) but are costly and risky — the distinction does not collapse alternatives entirely but makes legal entry extraordinarily narrow. Resistance (0.71) is relatively high because many parties contest the boundary: humanitarian organizations, economic migrants themselves, host countries in the Global South, and climate scientists all challenge whether the persecution category adequately captures the contemporary forced-migration landscape. The measurement series shows extractiveness climbing from 0.48 to 0.62 over 30 years as the gap between persecution-based admissions and actual forced migration causes (climate, state failure, generalized violence) widened — the constraint extracts more as its founding distinction becomes less empirically adequate. Theater rises similarly as states invest more in rhetoric (humanitarian mission) while maintaining steady-state exclusion. Suppression rises as enforcement machinery hardens (biometric systems, detention technology, procedural barriers).
 *
 * PERSPECTIVAL GAP:
 *   The state gatekeeper seat and the economic migrant seat compute dramatically differently. From the state seat, this is genuine coordination: the distinction allows orderly, principled asylum allocation. From the economic migrant seat, the same constraint is pure exclusion dressed in humanitarian language. The boundary-case seat sits between: they benefit from the existence of a favorable category (persecution) but suffer from the narrowness of its definition. Host countries have a dual relationship: they benefit from the legitimacy the distinction provides when they claim humanitarian identity, but they suffer from the capacity burden when forced migration escalates beyond the narrow persecution category. The engine computes this divergence from the structural data (beneficiaries, victims, exit options, power asymmetry).
 *
 * DIRECTIONALITY LOGIC:
 *   States hold institutional power with arbitrage exit (they can adopt different asylum standards or close borders entirely); they benefit by defining and controlling the boundary. Economic migrants and borderline cases hold powerless status with trapped exit (they cannot leave or stay legally under this reading; their only options are irregular entry, deportation, or suffering in origin countries). Persecution victims are clarified as beneficiaries by the constraint — their category is recognized, their claim is legitimated — though their actual material benefit often involves years in camps or precarious legal status. The distinction creates d-asymmetry: states sit near d=0.2 (beneficiary of the gatekeeper role, slight cost of maintaining the system), economic migrants sit near d=0.9 (nearly full targets, extracted through exclusion), and borderline cases sit near d=0.75 (mostly targets because their status is uncertain and unfavorable adjudication is common). These divergences arise directly from the structural data, not from subjective positioning.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is claimed as tangled rope because it exhibits both genuine coordination (respecting humanitarian obligation toward persecution victims) and asymmetric extraction (enabling exclusion of economic migrants). The coordination function is real: the refugee convention exists to protect a vulnerable population from death or serious harm due to identity-based persecution, and that function persists. The extraction is also real: the same categorical framework legitimates exclusion of populations facing identical severity of harm if it is labeled economic rather than persecution-based. Misclassifying this as pure rope (all coordination, no extraction) would ignore the exclusion machinery. Misclassifying as pure snare (all extraction, no real function) would ignore the genuine protection of persecution victims. The tangled-rope classification captures the internal tension: one reading's coordination function becomes another reading's justification for exclusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persecution_persecution_boundary_collapse,
    'What counts as persecution sufficient to trigger humanitarian obligation, and what counts as generalized suffering that does not? Where is the category boundary stable?',
    'Case-law accumulation, empirical classification of forced-migration causes, panel studies of adjudication consistency. At what threshold does violence or discrimination become persecution rather than generalized hardship?',
    'If persecution boundary collapses (becomes undefnable or highly context-dependent), the entire distinction becomes inoperable. If boundaries are stable, the constraint''s categorical function is preserved but the extraction question remains (is the category narrow enough to exclude masses in acute need?).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persecution_persecution_boundary_collapse, conceptual, 'Whether persecution can be defined with sufficient stability to support categorical distinction').

omega_variable(
    climate_disaster_status_contestation,
    'Are individuals displaced by climate or environmental catastrophe correctly classified as economic migrants, or do they belong in a protected category analogous to persecution victims?',
    'International recognition of climate migration as a protected category (e.g., new protocol amendment), or empirical consensus that environmental displacement is causally equivalent to persecution (both force migration, both life-threatening). Alternatively, continued classification as economic migration becomes explicitly indefensible.',
    'If climate-displaced persons are reclassified as protected, the constraint''s beneficiary set expands and extraction of climate migrants falls; if they remain classified as economic, the suppression machinery must intensify (more detention, faster deportation) to maintain exclusion as climate displacement accelerates. Either way, the current stable boundary collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_disaster_status_contestation, empirical, 'Whether climate displacement should be treated as persecution-equivalent or economic migration').

omega_variable(
    state_capacity_vs_humanitarian_obligation_mismatch,
    'What is the relationship between a state''s declared humanitarian obligation toward refugees and its actual capacity to admit and integrate them? When does capacity exhaustion release states from obligation?',
    'International agreements on minimum capacity thresholds, burden-sharing mechanisms, or explicit acknowledgment that humanitarian obligation is aspirational (states declare duty but retain practical discretion to exclude based on capacity).',
    'If capacity is explicit escape hatch, the constraint''s extraction increases (wealthier states exclude using capacity pretext). If capacity is not recognized, wealthier states face pressure to expand admission or increase resettlement. This is the operational pinch: the humanitarian reading asserts obligation without addressing how obligation scales with state capacity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_capacity_vs_humanitarian_obligation_mismatch, preference, 'Whether humanitarian obligation is absolute or capacity-scaled').

omega_variable(
    beneficiary_doctrine_vs_agent_beneficiary,
    'This reading vindicates both humanitarian rights doctrine and territorial sovereignty doctrine. Can both doctrines coexist, or does this constraint operate by making one a cover story for the other?',
    'If real cooperation between humanitarian and sovereignty principles is possible, they should operate autonomously (humanitarian duty for refugees, sovereign discretion for economic migrants, no zero-sum trade). If they coexist only through constant renegotiation and boundary policing, the constraint is covering one doctrine''s dominance with the other''s language.',
    'If the doctrines genuinely coexist, this is legitimate tangled rope. If one dominates through the other''s language, the classification shifts toward snare (doctrine-laundering).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_doctrine_vs_agent_beneficiary, conceptual, 'Whether humanitarian and sovereignty doctrines genuinely coexist or one masks the other').

omega_variable(
    sibling_reading_foreclosure,
    'Does adopting the humanitarian obligation reading logically foreclose the sovereignty reading or the freedom-of-movement reading, or can both readings coexist as live positions held by different parties?',
    'Examine whether any state or doctrine endorses both this reading and a sibling reading simultaneously. A state that asserts both humanitarian obligation to refugees AND unlimited sovereign discretion to exclude other migrants is holding coexisting readings. A philosophical tradition that asserts both right-to-migrate AND state border authority is holding coexisting readings.',
    'If sibling readings are genuinely coexistent (different parties holding different readings without logical contradiction), the relations are coexists_with. If one reading''s premises logically contradict another''s, the relation is forecloses. This shapes how the kernel contests the boundary: is it a multi-party dispute (coexist) or a logical contradiction (foreclose)?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether sibling readings logically coexist or foreclose').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.58) primarily structural (legal barriers, deportation machinery, detention conditions) or internalized (economic migrants internalize their unworthiness, accept the binary category, self-exclude from claiming asylum even when eligible)?',
    'Post-entry trajectory: if suppression persists after structural barriers are removed (rights-based asylum reform, open borders in a hypothetical jurisdiction), suppression is internalized. If suppression drops when barriers are removed, it was structural.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure because targets carry the suppression with them after exit. If structural, suppression is removable by policy reform. This shapes remediation: removing structural suppression would require dismantling asylum determination machinery; removing internalized suppression requires counter-narrative work.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of economic migrants is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t5, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(bord_tr_t5, observed).
narrative_ontology:measurement(bord_tr_t10, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(bord_tr_t10, observed).
narrative_ontology:measurement(bord_tr_t15, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(bord_tr_t15, observed).
narrative_ontology:measurement(bord_tr_t20, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(bord_tr_t20, observed).
narrative_ontology:measurement(bord_tr_t25, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(bord_tr_t25, observed).
narrative_ontology:measurement(bord_tr_t30, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(bord_tr_t30, observed).
narrative_ontology:measurement(bord_tr_t40, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(bord_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t5, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(bord_be_t5, observed).
narrative_ontology:measurement(bord_be_t10, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(bord_be_t10, observed).
narrative_ontology:measurement(bord_be_t15, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(bord_be_t15, observed).
narrative_ontology:measurement(bord_be_t20, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(bord_be_t20, observed).
narrative_ontology:measurement(bord_be_t25, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(bord_be_t25, observed).
narrative_ontology:measurement(bord_be_t30, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(bord_be_t30, observed).
narrative_ontology:measurement(bord_be_t40, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(bord_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t5, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 5, 0.49).
narrative_ontology:measurement_basis(bord_su_t5, observed).
narrative_ontology:measurement(bord_su_t10, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(bord_su_t10, observed).
narrative_ontology:measurement(bord_su_t15, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(bord_su_t15, observed).
narrative_ontology:measurement(bord_su_t20, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement_basis(bord_su_t20, observed).
narrative_ontology:measurement(bord_su_t25, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(bord_su_t25, observed).
narrative_ontology:measurement(bord_su_t30, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(bord_su_t30, observed).
narrative_ontology:measurement(bord_su_t40, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(bord_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__humanitarian_obligation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_legitimacy__humanitarian_obligation_reading, 0.12).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__freedom_of_movement_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the border_legitimacy kernel. The humanitarian_obligation_reading operationalizes the claim that states owe protection to persecution victims but retain discretion over economic migrants. The sovereignty_reading grounds border authority in state territorial control (excludes all non-citizens by default). The freedom_of_movement_reading asserts movement as a human right and treats borders as presumptively illegitimate. Each reading has a distinct ε value, distinct beneficiary/victim structure, and distinct type classification. They share the same kernel (legitimate border authority) but interpret legitimacy sources differently. Links via affects_constraints record that the humanitarian reading creates normative and institutional pressure on the sovereignty reading (humanitarian duty is incompatible with absolute closure) and is incompatible with the freedom-of-movement reading (the humanitarian reading asserts state authority; freedom-of-movement denies it).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
