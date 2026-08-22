% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__copyleft_counterfactual_reading, []).

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
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Permissive Licensing as Structural Enabler of Uncompensated Derivative Extraction (Copyleft-Counterfactual Reading)
 *   domain: technology/legal
 *
 * SUMMARY:
 *   This story instantiates the copyleft-counterfactual reading of the
 *   permissive_license_text kernel: the claim that copyright relaxation
 *   without a reciprocity requirement structurally enables uncompensated
 *   exploitation of maintainer labor, and that viral reciprocity (GPL-style
 *   copyleft) is the necessary structural corrective. Under this reading, the
 *   coordination function of permissive licensing (frictionless adoption) is
 *   real but is riding on an asymmetric extraction mechanism: value flows
 *   from unpaid maintainers to well-resourced commercial adopters with no
 *   compensating return flow, and that asymmetry requires the license's
 *   continued absence of reciprocity terms to persist — a structural choice
 *   actively defended by the vendors and hyperscalers who fund licensing
 *   foundations and standards discourse. This is NOT the
 *   commons_coordination_reading (which treats the same absence of
 *   reciprocity as a pure friction-minimizing good with no extraction) nor
 *   the corporate_moat_reading (which locates the harm in proprietary
 *   moat-building specifically, rather than in the missing structural remedy
 *   of reciprocity). All three readings share the same license text as their
 *   kernel but author different epsilon, different beneficiary/victim
 *   structures, and different classifications from it.
 *
 * KEY AGENTS:
 *   - unpaid_upstream_maintainers: primary target (powerless/trapped) — bears the uncompensated labor extraction
 *   - volunteer_contributor_pool: secondary target (powerless/constrained) — labor captured without return
 *   - large_proprietary_vendors: primary beneficiary (institutional/arbitrage) — captures derivative value without reciprocity
 *   - cloud_hyperscalers: primary beneficiary and agenda-setter (institutional/arbitrage) — captures value at scale and shapes licensing norms
 *   - copyleft_license_advocates: excluded voice (organized/constrained) — proposes the structural remedy this reading endorses
 *   - software_freedom_scholars: analytical observer — measures contribution asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.78).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.42).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive Licensing as Structural Enabler of Uncompensated Derivative Extraction (Copyleft-Counterfactual Reading)").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "technology/legal").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, 'a017853a-b302-4d40-b72a-781c563cf0e1').
narrative_ontology:cs_kernel_codification('a017853a-b302-4d40-b72a-781c563cf0e1', fixed_text).
narrative_ontology:cs_authority_grounding('a017853a-b302-4d40-b72a-781c563cf0e1', distributed).
narrative_ontology:cs_reading_relation('a017853a-b302-4d40-b72a-781c563cf0e1', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('a017853a-b302-4d40-b72a-781c563cf0e1', permissive_license_text__corporate_moat_reading, influences).
narrative_ontology:cs_axiom('a017853a-b302-4d40-b72a-781c563cf0e1', foundational, reciprocity_is_necessary_for_sustainable_commons).
narrative_ontology:cs_axiom_status(reciprocity_is_necessary_for_sustainable_commons, holdable).
narrative_ontology:cs_axiom_grounding('a017853a-b302-4d40-b72a-781c563cf0e1', reciprocity_is_necessary_for_sustainable_commons, empirically_contingent).
narrative_ontology:cs_axiom('a017853a-b302-4d40-b72a-781c563cf0e1', foundational, uncompensated_derivative_capture_constitutes_exploitation).
narrative_ontology:cs_axiom_status(uncompensated_derivative_capture_constitutes_exploitation, holdable).
narrative_ontology:cs_axiom_grounding('a017853a-b302-4d40-b72a-781c563cf0e1', uncompensated_derivative_capture_constitutes_exploitation, deontological).
narrative_ontology:cs_reference_frame('a017853a-b302-4d40-b72a-781c563cf0e1', goodwill_sustained_academic_commons).
narrative_ontology:cs_drift_state('a017853a-b302-4d40-b72a-781c563cf0e1', cloud_native_commercial_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a017853a-b302-4d40-b72a-781c563cf0e1', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, large_proprietary_vendors).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, cloud_hyperscalers).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, unpaid_upstream_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, volunteer_contributor_pool).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, downstream_end_users).
narrative_ontology:constraint_vindicates(permissive_license_text__copyleft_counterfactual_reading, reciprocity_is_necessary_for_sustainable_commons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and maintain the permissively licensed code that downstream commercial products depend on. Because the license carries no reciprocity requirement, large vendors can fork, harden, and resell derivative products without contributing code, money, or maintenance labor back. The maintainer cannot re-license already-released code retroactively and has no mechanism to compel contribution; their only real exit is burnout or abandonment of the project, which harms the very users they built it for.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, unpaid_upstream_maintainers, payer,
    powerless, biographical, trapped, global).

% Contribute patches and features on the understanding the project serves a shared commons. Watch corporate forks capture the commercial value of their unpaid labor with no obligation to return improvements. Can withdraw contribution effort but cannot undo the license grant already made or prevent future extraction from their existing commits.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, volunteer_contributor_pool, payer,
    powerless, biographical, constrained, global).

% Take permissively licensed code, integrate it into closed commercial products, and capture the resulting revenue while making no contribution back to the originating project. Free to choose permissive-licensed dependencies specifically because the license imposes no reciprocity, and free to switch to another permissively licensed substitute if any project attempts to impose terms after the fact.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, large_proprietary_vendors, beneficiary,
    institutional, generational, arbitrage, global).

% Operate permissively licensed software as a managed service at scale, capturing the commercial value of community-built infrastructure while contributing a small fraction of what an equivalent in-house build would cost. Actively lobby for permissive licensing norms and fund foundations that discourage reciprocal (copyleft) terms, shaping the licensing ecosystem toward arrangements they benefit from.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, cloud_hyperscalers, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, cloud_hyperscalers, agenda_setter).

% Argue that only viral reciprocity (GPL-style copyleft) can prevent the extraction observed under permissive terms, since it legally compels any distributed derivative to remain open. Largely excluded from standard-setting bodies and foundation governance, which have converged on permissive defaults under pressure from the vendors and hyperscalers who benefit from the absence of reciprocity.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, copyleft_license_advocates, excluded,
    organized, generational, constrained, global).

% Receive functioning software products, sometimes free, sometimes commercial, built atop the permissively licensed commons. Largely indifferent to the licensing terms underneath but ultimately dependent on the continued maintenance the extraction pattern erodes.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, downstream_end_users, beneficiary,
    moderate, biographical, mobile, global).

% Study licensing outcomes, maintainer burnout data, and corporate contribution ratios to evaluate which licensing regime best sustains open collaborative production without capture.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, software_freedom_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__copyleft_counterfactual_reading, cloud_hyperscalers).
narrative_ontology:fixing_cost_class(permissive_license_text__copyleft_counterfactual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permissive licensing genuinely lowers legal friction for anyone who wants to build on shared code, enabling broad adoption without negotiation overhead — that adoption-friction reduction is a real coordination function.
% TRANSFER_FUNCTION: The absence of a reciprocity requirement moves the value of unpaid maintainer and contributor labor into commercial products and managed cloud services, without any compensating flow of code, funding, or maintenance capacity back to the originating project.
% ABSENT_VOICES: Copyleft advocates and burned-out former maintainers who have left projects are structurally absent from foundation governance and standards bodies, which are disproportionately funded and staffed by the vendors and hyperscalers who benefit from permissive defaults.
% DISAPPEARANCE_RATIONALE: If permissive licensing without reciprocity disappeared and all such projects were retroactively copyleft, hyperscalers and vendors would face a binary choice on every dependency: contribute improvements back or rebuild in-house at substantially higher cost. Commercial adoption patterns, foundation funding models, and maintainer compensation structures would all reorganize around forced reciprocity.
% FOUNDING_PROBLEM: Early permissive licenses (BSD, MIT) were designed to solve academic and research code-sharing problems: let anyone use, modify, and redistribute code with minimal legal encumbrance, trusting goodwill and reputation to sustain the commons.
% FOUNDING_PROBLEM_CORROBORATION: Maintainer burnout surveys (e.g. Tidelift and Linux Foundation contributor studies) and independent academic analyses of corporate-to-community contribution ratios, produced by researchers outside both the vendor and maintainer communities, corroborate that the goodwill-sustained commons the license was built for has been substantially displaced by uncompensated commercial extraction at scale — a problem the license's original 1980s framing did not anticipate.
narrative_ontology:disappearance_verdict(permissive_license_text__copyleft_counterfactual_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__copyleft_counterfactual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__copyleft_counterfactual_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78 at interval end) because, under this reading, the standing arrangement (permissive licensing as currently practiced, without reciprocity) is assessed as substantially extractive: value capture by well-resourced commercial actors has risen steadily as cloud-native and SaaS business models matured around unpaid open-source dependencies. Suppression is moderate (0.42) — there is no direct coercion preventing maintainers from switching licenses going forward, but structural lock-in exists: switching an established project to copyleft mid-life fractures the community, alienates the corporate users the project may depend on for visibility, and cannot retroactively reclaim value already extracted from prior permissively licensed releases. Theater ratio is moderate-low (0.30) and rising, reflecting increasing 'open source sustainability' initiatives (badges, foundation pledges, sponsorship programs) that address the appearance of the problem without altering the underlying licensing structure that permits uncompensated extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (vendors, hyperscalers) the arrangement looks like genuine, low-friction coordination they are simply using as intended. From the payer seats (maintainers, contributors) the identical structure operates as an enforced asymmetry: enforced not by coercion against them directly, but by the absence of any legal mechanism compelling reciprocity, which functions as a standing permission structure for extraction. The engine should compute these as structurally different experiences of the same license text under this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (vendors, hyperscalers) sit near the full-beneficiary end: they have arbitrage-grade exit (can substitute among permissively licensed dependencies at will) and derive concentrated commercial value without offsetting cost. Victims (maintainers, contributors) sit near the full-target end: trapped or constrained exit, since the license grant is irrevocable and switching terms going forward does not recover extracted value from already-released code. Copyleft advocates are excluded rather than coordinated — under this reading, their preferred remedy (viral reciprocity) is precisely what current governance structures work to keep out of the standard-setting conversation.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than a pure snare is deliberate: this reading holds that permissive licensing DOES solve a genuine coordination problem (frictionless code reuse, broad adoption, avoidance of licensing negotiation overhead) — it is not merely extraction wearing a coordination costume. What makes it tangled rather than a clean rope is that the SAME mechanism that produces the coordination benefit (no reciprocity obligation) is also the mechanism that enables the uncompensated extraction; the two are not separable functions bolted together but one structural choice with both effects. Labeling it a pure snare would erase the real adoption-friction reduction the license provides; labeling it a pure rope would erase the asymmetric, uncompensated capture this reading holds is structurally built in.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_divergence_permissive_license_text,
    'Is the permissive_license_text kernel best read as pure coordination good (commons_coordination_reading), as extraction enabling proprietary moats specifically (corporate_moat_reading), or as extraction remediable only by viral reciprocity (this reading)? Each reading authors a different epsilon and a different victim set from the identical license text.',
    'Comparative empirical study of contribution-back ratios and commercial capture across matched permissively-licensed vs. copyleft-licensed projects of similar scale and age; if copyleft projects show materially higher contribution-back rates without materially reduced adoption, this reading''s causal claim (reciprocity requirement is the necessary fix) gains support over the commons_coordination_reading''s claim that reciprocity is unnecessary friction.',
    'If copyleft licensing does not measurably improve contribution-back ratios relative to permissive licensing, this reading''s core causal claim collapses and the constraint would need to be reclassified closer to the commons_coordination_reading (rope, minimal extraction). If it does, this reading''s tangled_rope classification and vindicated proposition are strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence_permissive_license_text, conceptual, 'Which of the three kernel readings best captures the license text''s structural effect; the readings are not merely differently-valenced opinions but structurally distinct empirical and normative claims.').

omega_variable(
    reciprocity_necessity_vs_sufficiency,
    'Is viral reciprocity (GPL-style copyleft) truly NECESSARY to prevent the extraction this reading identifies, or merely one sufficient remedy among others (e.g., dual-licensing, trademark-based commercial terms, foundation-mediated contribution agreements)?',
    'Case studies of non-copyleft mechanisms (e.g., Business Source License, contributor license agreements paired with commercial licensing) that have achieved comparable contribution-back rates without full viral reciprocity.',
    'If non-copyleft remedies achieve comparable outcomes, the vindicated_propositions claim (reciprocity_is_necessary_for_sustainable_commons) overstates its case and should be softened to ''reciprocity or an equivalent mechanism is necessary,'' which would not change the tangled_rope classification but would weaken this reading''s strongest normative claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_necessity_vs_sufficiency, empirical, 'Whether copyleft specifically, or reciprocity mechanisms generally, is the necessary structural remedy this reading claims.').

omega_variable(
    maintainer_consent_ambiguity,
    'Did maintainers who chose permissive licenses knowingly accept the risk of uncompensated commercial extraction as the price of maximizing adoption, making the outcome a consented-to tradeoff rather than exploitation?',
    'Survey historical licensing-choice rationale documented in project governance records and mailing lists at the time permissive licenses were selected, versus current maintainer sentiment about the outcome.',
    'If most maintainers made an informed, adoption-maximizing choice and do not retrospectively regret it, the extraction framing weakens toward the commons_coordination_reading; if most maintainers report the extraction outcome as unanticipated and unwanted, this reading''s exploitation framing is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maintainer_consent_ambiguity, empirical, 'Whether the extraction pattern reflects informed consent to a tradeoff or an unanticipated structural harm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(perm_tr_t5, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(perm_tr_t10, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(perm_tr_t15, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(perm_tr_t25, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 25, 0.3).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(perm_be_t15, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(perm_be_t25, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 25, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(perm_su_t5, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 5, 0.25).
narrative_ontology:measurement(perm_su_t10, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(perm_su_t15, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(perm_su_t20, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(perm_su_t25, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(permissive_license_text__copyleft_counterfactual_reading, 0.1).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, corporate_moat_reading).

% DUAL FORMULATION NOTE:
% Three readings of the same permissive_license_text kernel: commons_coordination_reading authors low epsilon and treats reciprocity absence as pure adoption-friction reduction; copyleft_counterfactual_reading (this story) authors high epsilon and treats reciprocity absence as the structural mechanism of uncompensated extraction, remediable specifically by viral copyleft; corporate_moat_reading authors high epsilon centered narrowly on proprietary moat construction and competitor harm rather than maintainer/contributor harm. Each is a distinct constraint with its own beneficiary/victim structure and classification, linked here rather than merged, per the ε-invariance decomposition principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
