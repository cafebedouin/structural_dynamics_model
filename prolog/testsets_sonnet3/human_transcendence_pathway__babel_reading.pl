% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__babel_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__babel_reading, []).

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
 *   constraint_id: human_transcendence_pathway__babel_reading
 *   human_readable: Babel Reading: Unified Power as Self-Sufficient Transcendence Substitute
 *   domain: political_theology/technology_ethics
 *
 * SUMMARY:
 *   This story instantiates the Babel reading of the
 *   human_transcendence_pathway kernel: collective human power, unified
 *   through a single language and a single technique, is claimed sufficient
 *   to secure permanence and self-sufficiency without any reference to
 *   authority beyond the collective itself. The reading treats the tower
 *   project as the paradigm case — enforced linguistic and technical
 *   uniformity purchasing large-scale coordination at the price of
 *   suppressing the plural communities absorbed into it. As enforcement
 *   intensified to hold the uniformity together, extraction and theater both
 *   rose; when the enforcing power faltered, the coordination it had produced
 *   did not survive contact with the plurality it had suppressed rather than
 *   integrated. This is a distinct constraint from the
 *   technocratic_vs_incarnational_reading (which concerns whether
 *   transcendence is achieved by eliminating human limits versus received as
 *   gift) and from the jerusalem_reading (which concerns plurality integrated
 *   into communion under blessing rather than flattened by coercion) — same
 *   kernel, three structurally different constraints with three different
 *   epsilon values.
 *
 * KEY AGENTS:
 *   - tower_architects: agenda_setter, institutional power, arbitrage exit — designs and directs the unified project, captures the enduring name and surplus
 *   - centralized_administrators: beneficiary, organized power, constrained exit — enforces uniform speech/technique, bound to the machinery they administer
 *   - linguistic_minority_populations: payer, powerless, trapped exit — absorbed into the standard, bears confusion when coordination fails
 *   - dispersed_kinship_groups: payer, powerless, trapped exit — prior self-governance subordinated to labor demands
 *   - unaffiliated_neighboring_peoples: excluded, moderate power, mobile exit — outside the project, unheard warning voice
 *   - theological_observer: observer, analytical — traces the structural fragility of unity purchased by suppression rather than integration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__babel_reading, 0.81).
domain_priors:suppression_score(human_transcendence_pathway__babel_reading, 0.78).
domain_priors:theater_ratio(human_transcendence_pathway__babel_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__babel_reading, tangled_rope).
narrative_ontology:human_readable(human_transcendence_pathway__babel_reading, "Babel Reading: Unified Power as Self-Sufficient Transcendence Substitute").
narrative_ontology:topic_domain(human_transcendence_pathway__babel_reading, "political_theology/technology_ethics").

domain_priors:requires_active_enforcement(human_transcendence_pathway__babel_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__babel_reading, '37921c04-1f72-43dc-bcca-3766ba8a234b').
narrative_ontology:cs_kernel_codification('37921c04-1f72-43dc-bcca-3766ba8a234b', distributed).
narrative_ontology:cs_authority_grounding('37921c04-1f72-43dc-bcca-3766ba8a234b', extraction).
narrative_ontology:cs_interpretation_layer_present('37921c04-1f72-43dc-bcca-3766ba8a234b').
narrative_ontology:cs_reading_relation('37921c04-1f72-43dc-bcca-3766ba8a234b', human_transcendence_pathway__technocratic_vs_incarnational_reading, coexists_with).
narrative_ontology:cs_reading_relation('37921c04-1f72-43dc-bcca-3766ba8a234b', human_transcendence_pathway__jerusalem_reading, forecloses).
narrative_ontology:cs_axiom('37921c04-1f72-43dc-bcca-3766ba8a234b', foundational, unity_requires_uniformity_not_integration).
narrative_ontology:cs_axiom_status(unity_requires_uniformity_not_integration, holdable).
narrative_ontology:cs_axiom_grounding('37921c04-1f72-43dc-bcca-3766ba8a234b', unity_requires_uniformity_not_integration, empirically_contingent).
narrative_ontology:cs_axiom('37921c04-1f72-43dc-bcca-3766ba8a234b', foundational, self_sufficient_power_needs_no_transcendent_referent).
narrative_ontology:cs_axiom_status(self_sufficient_power_needs_no_transcendent_referent, holdable).
narrative_ontology:cs_axiom_grounding('37921c04-1f72-43dc-bcca-3766ba8a234b', self_sufficient_power_needs_no_transcendent_referent, deontological).
narrative_ontology:cs_reference_frame('37921c04-1f72-43dc-bcca-3766ba8a234b', unified_project_as_guarantor_of_permanence).
narrative_ontology:cs_drift_state('37921c04-1f72-43dc-bcca-3766ba8a234b', post_dispersal_collapse, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('37921c04-1f72-43dc-bcca-3766ba8a234b', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__babel_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__babel_reading, tower_architects).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__babel_reading, centralized_administrators).
narrative_ontology:constraint_victim(human_transcendence_pathway__babel_reading, linguistic_minority_populations).
narrative_ontology:constraint_victim(human_transcendence_pathway__babel_reading, dispersed_kinship_groups).
narrative_ontology:constraint_victim(human_transcendence_pathway__babel_reading, local_cultural_traditions).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__babel_reading, human_self_sufficiency_thesis).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__babel_reading, unity_without_transcendence_is_stable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer the unified project — one language, one brick, one city, one name made great. They set the terms of participation, direct the collective labor, and frame the enterprise as security against future dispersal. They capture the prestige, the surplus labor, and the durable name; their exit option is real (they can abandon the project and retain status) even as they bind everyone else into it.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, tower_architects, agenda_setter,
    institutional, generational, arbitrage, regional).

% Middle layer that enforces uniform speech and uniform technique across the workforce — standardizing brick-making, standardizing command language, suppressing dialect variation that would slow coordination. They benefit from the efficiency the uniformity produces but are themselves bound to enforce it or lose position.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, centralized_administrators, beneficiary,
    organized, biographical, constrained, regional).

% Speakers of non-dominant tongues absorbed into the project's workforce. Their speech is flattened into the administrative standard; their distinct communicative traditions have no path to persist inside the unified structure. When the tower's coordination fails, they bear the confusion of a collapse they did not design and cannot easily undo, having already lost the internal cohesion of their own linguistic community.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, linguistic_minority_populations, payer,
    powerless, biographical, trapped, local).

% Extended family and clan structures whose internal organization is subordinated to the tower's labor demands. Their prior modes of self-governance and mutual obligation are treated as inefficiencies to be absorbed rather than plural goods to be integrated.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, dispersed_kinship_groups, payer,
    powerless, generational, trapped, local).

% The customs, oral histories, and place-based practices displaced by the single technique and single tongue the project requires. Not an actor itself, but the erased good the arrangement consumes — recorded here for completeness of what pays the cost.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, local_cultural_traditions, payer,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_non_agent(human_transcendence_pathway__babel_reading, local_cultural_traditions).

% Groups outside the project's reach who would warn that unity purchased by suppression is brittle, and that dispersal under blessing differs from dispersal under collapse. They have no seat in the tower's councils and no mechanism to be heard before construction proceeds.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, unaffiliated_neighboring_peoples, excluded,
    moderate, generational, mobile, regional).

% Reads the arrangement as the claim that collective technological and linguistic power can secure permanence and self-sufficiency without reference to any authority beyond itself — and traces what happens structurally when that claim is tested: coordination that depended entirely on enforced uniformity fragments once the enforcing power falters, because nothing beneath the uniformity was ever plural-but-integrated.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, theological_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(human_transcendence_pathway__babel_reading, tower_architects).
narrative_ontology:fixing_cost_class(human_transcendence_pathway__babel_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single language and single technique lets a large, otherwise scattered population coordinate labor, defense, and administration at a scale dispersed clans could not reach alone — genuine coordination gain exists at the level of construction throughput and collective security.
% TRANSFER_FUNCTION: Moves linguistic and cultural distinctiveness, local governance autonomy, and a share of surplus labor from dispersed kin-groups and minority speech communities to the architects and administrators who control the unified project and its enduring name.
% ABSENT_VOICES: Neighboring peoples who never joined the project, and the internal minority-language communities whose objections were absorbed into 'inefficiency' rather than heard as dissent; neither had a channel to argue that plural, blessed dispersal is not the same as coerced collapse.
% DISAPPEARANCE_RATIONALE: If the enforced unity vanished, the population would revert to (or rediscover) dispersed, plural linguistic and kinship communities; the administrators' concentrated authority and the architects' singular name would lose their basis, and coordination would have to be rebuilt on some other footing — voluntary, blessed, or otherwise.
% FOUNDING_PROBLEM: Fear of future scattering and loss of a durable, self-made name; the project was built to guarantee permanence and self-sufficiency through humanity's own unified power, without needing to trust anything beyond that power.
% FOUNDING_PROBLEM_CORROBORATION: The theological observer and the unaffiliated neighboring peoples attest that the feared scattering occurred anyway, precipitated by the very structure meant to prevent it — the architects and administrators, being the beneficiaries of the arrangement, are not credible sole witnesses to whether their founding fear was ever a sound basis for the coercion it justified; no source outside the project's own leadership attests the uniformity was ever necessary rather than merely convenient to those who administered it.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__babel_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__babel_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__babel_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(human_transcendence_pathway__babel_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__babel_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__babel_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(human_transcendence_pathway__babel_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(human_transcendence_pathway__babel_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81 by interval end) because the transfer from suppressed linguistic/cultural plurality to the architects' concentrated name and administrators' efficiency gains is substantial and widens as enforcement hardens. Suppression is authored high (0.78) and rising because holding a single language and technique across an unwilling, absorbed plurality requires escalating active enforcement — it is not a byproduct, it is close to the mechanism itself. Theater rises to 0.52 because as the underlying coordination becomes harder to sustain purely functionally, an increasing share of activity (monument-building, name-making) becomes performative demonstration of the project's permanence rather than functional coordination. accessibility_collapse (0.62) is moderate-high: once inside the project, alternate linguistic/cultural paths are largely foreclosed, though not with mountain-level completeness — the eventual scattering shows the collapse was never total. Resistance (0.55) reflects real but structurally disadvantaged pushback from absorbed communities.
 *
 * DIRECTIONALITY LOGIC:
 *   Architects sit at the beneficiary pole: institutional power, arbitrage exit, they set terms and capture the name. Administrators are beneficiaries who are also partially bound — they profit from the efficiency of uniformity but must enforce it or lose their position, which is why they carry organized power with only constrained exit rather than arbitrage. Linguistic minorities and kinship groups sit at the target pole: powerless, trapped, bearing the transfer of cultural distinctiveness into the administrative standard. Neighboring peoples are excluded rather than coordinated or extracted from directly — their exclusion from the tower's councils is what keeps the warning about brittle unity from ever reaching the architects.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fear of scattering, desire for self-secured permanence) is authored dead: the scattering happened anyway, precipitated by the enforced-uniformity structure itself, and no source outside the project's own beneficiaries attests the coercive uniformity was ever necessary to the coordination gain rather than merely convenient to those administering it. This is the mismatch the R5 consumer reads: founding_problem_status=dead crossed with disappearance_verdict=world_rearranges signals a capture/zombie pattern worth flagging — the arrangement's stated justification collapsed before its enforcement did, and enforcement (not continued function) is what kept it standing as long as it did.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_coordination_necessity,
    'Was the enforced linguistic/technical uniformity structurally necessary to achieve the coordination gain, or was uniformity a convenience to the administrators that exceeded what coordination actually required?',
    'Comparative analysis of large-scale coordination projects that integrated plural languages/techniques (translation layers, federated administration) against projects that enforced uniformity, controlling for scale and task complexity.',
    'If uniformity exceeded coordination necessity, the extraction is closer to pure administrative rent-seeking dressed as coordination; if uniformity was load-bearing for the coordination gain, some of the measured extraction is properly coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_coordination_necessity, empirical, 'Whether enforced uniformity was coordination-necessary or extraction-convenient.').

omega_variable(
    kernel_committer_framing,
    'Is the Babel reading correctly scoped as ''unified power without reference to transcendent authority,'' or does it more precisely name ''unified power that treats plurality itself as the enemy of stability'' — a related but distinguishable claim from the technocratic_vs_incarnational_reading''s limit-elimination axis?',
    'Compare structural predictions: if Babel and technocratic readings predict identical failure modes under stress, they may be one constraint mis-split; if they predict different failure modes (communication collapse vs. loss of finitude-grounded meaning), the split holds.',
    'If the readings collapse into one, the network edges and independent epsilon values authored across the family would need revision; as authored here they predict distinct failure modes (Babel: coordination collapse via enforced homogenization; technocratic: meaning/limit collapse via optimization) and the split is retained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_framing, conceptual, 'Whether the babel_reading and technocratic_vs_incarnational_reading are genuinely distinct constraints or one over-split.').

omega_variable(
    self_sufficiency_thesis_testability,
    'Is ''unity without transcendent reference can secure permanence'' an empirically testable historical claim, or a theological claim not adjudicable by the historical record alone?',
    'Track whether the specific mechanism claimed (enforced uniformity as substitute for external grounding) reliably produces the predicted fragility across independent historical cases, versus treating the claim as a theological axiom immune to disconfirmation.',
    'If empirically testable and disconfirmed repeatedly, the vindicated_propositions listed here are actively falsified rather than merely contested; if the claim is theological rather than empirical, its status is a matter of framework commitment, not evidence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_sufficiency_thesis_testability, conceptual, 'Empirical testability of the self-sufficiency thesis this reading vindicates for its beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__babel_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_transcendence_pathway__babel_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(huma_tr_t5, human_transcendence_pathway__babel_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(huma_tr_t10, human_transcendence_pathway__babel_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(huma_tr_t20, human_transcendence_pathway__babel_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(huma_tr_t30, human_transcendence_pathway__babel_reading, theater_ratio, 30, 0.47).
narrative_ontology:measurement(huma_tr_t40, human_transcendence_pathway__babel_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_transcendence_pathway__babel_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(huma_be_t5, human_transcendence_pathway__babel_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(huma_be_t10, human_transcendence_pathway__babel_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(huma_be_t20, human_transcendence_pathway__babel_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(huma_be_t30, human_transcendence_pathway__babel_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(huma_be_t40, human_transcendence_pathway__babel_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, human_transcendence_pathway__babel_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(huma_su_t5, human_transcendence_pathway__babel_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(huma_su_t10, human_transcendence_pathway__babel_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(huma_su_t20, human_transcendence_pathway__babel_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(huma_su_t30, human_transcendence_pathway__babel_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(huma_su_t40, human_transcendence_pathway__babel_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__babel_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(human_transcendence_pathway__babel_reading, 0.06).
narrative_ontology:affects_constraint(human_transcendence_pathway__babel_reading, technocratic_vs_incarnational_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__babel_reading, jerusalem_reading).

% DUAL FORMULATION NOTE:
% Three constraints share the human_transcendence_pathway kernel: babel_reading (this story — coercive homogenization claimed sufficient for self-secured permanence, epsilon high ~0.81), jerusalem_reading (plurality integrated into communion under blessing, epsilon expected low, coordination-dominant), and technocratic_vs_incarnational_reading (transcendence via limit-elimination vs. received grace, a distinct axis about the METHOD rather than the plurality-question). Babel and Jerusalem structurally mirror each other as opposed answers to 'how is a scattered humanity rebuilt' — one by suppression, one by integration — while the technocratic reading answers a different question about whether transcendence is self-achieved through optimization or received. All three are linked here so contamination/support analysis can trace how a challenge to one reading's ε or type propagates to the sibling readings' legitimacy claims (e.g., historical evidence of Babel-style projects' fragility would lend structural support to the jerusalem_reading's coordination case, without collapsing them into one constraint).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(human_transcendence_pathway__babel_reading, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
