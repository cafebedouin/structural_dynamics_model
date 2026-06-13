% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Permissive License Absence of Reciprocity Requirement (Copyleft Counterfactual Reading)
 *   domain: intellectual_property/software_governance/technology_economics
 *
 * SUMMARY:
 *   Permissive open-source licenses (MIT, BSD, Apache) are presented as
 *   maximizing freedom and reducing friction from legal complexity. This
 *   reading instantiates the copyleft-counterfactual frame: that permissive
 *   licenses, by declining to require reciprocal contribution, structurally
 *   enable proprietary builders to extract value from commons-produced code
 *   without investing back into commons infrastructure. The reading asserts
 *   that viral reciprocity (GPL-family copyleft) is the necessary alternative
 *   to prevent this exploitation. This is ONE OF THREE contested readings of
 *   the same kernel (permissive_license_text). The three sibling readings
 *   produce different constraint stories: commons-coordination-reading
 *   emphasizes frictionless reuse and distributed benefit;
 *   corporate-moat-reading emphasizes proprietary enclosure of improvement;
 *   copyleft-counterfactual-reading (THIS reading) emphasizes the absence of
 *   a mechanism to require reciprocal contribution, enabling asymmetric
 *   extraction. All three readings agree on the empirical facts (who uses the
 *   code, what licenses are deployed, what value flows); they disagree on the
 *   normative framing and the counterfactual (what would happen if
 *   reciprocity were required).
 *
 * KEY AGENTS:
 *   - proprietary_derivative_builders: institutional power, arbitrage exit — can incorporate permissively licensed code without returning improvements or source code.
 *   - commons_developers: moderate power, identity-locked exit — contribute to permissively licensed projects knowing their work may be enclosed.
 *   - downstream_free_software_ecosystem: powerless, trapped exit — depends on accumulated commons work but cannot require reciprocal contribution from users.
 *   - copyleft_license_advocates: organized power, mobile exit — argue that reciprocity requirements are necessary to prevent the extraction this reading names.
 *   - commons_coordination_advocates: organized power, mobile exit — excluded from this reading, argue that permissive licensing itself solves the coordination problem.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.68).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.52).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive License Absence of Reciprocity Requirement (Copyleft Counterfactual Reading)").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "intellectual_property/software_governance/technology_economics").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, '642aa56d-6806-45d4-af2c-923c00eb793d').
narrative_ontology:cs_kernel_codification('642aa56d-6806-45d4-af2c-923c00eb793d', fixed_text).
narrative_ontology:cs_authority_grounding('642aa56d-6806-45d4-af2c-923c00eb793d', expertise).
narrative_ontology:cs_interpretation_layer_present('642aa56d-6806-45d4-af2c-923c00eb793d').
narrative_ontology:cs_reading_relation('642aa56d-6806-45d4-af2c-923c00eb793d', permissive_license_text__commons_coordination_reading, influences).
narrative_ontology:cs_reading_relation('642aa56d-6806-45d4-af2c-923c00eb793d', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_axiom('642aa56d-6806-45d4-af2c-923c00eb793d', foundational, reciprocal_contribution_required_for_commons_sustainability).
narrative_ontology:cs_axiom_status(reciprocal_contribution_required_for_commons_sustainability, holdable).
narrative_ontology:cs_axiom_grounding('642aa56d-6806-45d4-af2c-923c00eb793d', reciprocal_contribution_required_for_commons_sustainability, empirically_contingent).
narrative_ontology:cs_axiom('642aa56d-6806-45d4-af2c-923c00eb793d', foundational, permissive_license_absence_of_reciprocity_enables_exploitation).
narrative_ontology:cs_axiom_status(permissive_license_absence_of_reciprocity_enables_exploitation, holdable).
narrative_ontology:cs_axiom_grounding('642aa56d-6806-45d4-af2c-923c00eb793d', permissive_license_absence_of_reciprocity_enables_exploitation, deontological).
narrative_ontology:cs_reference_frame('642aa56d-6806-45d4-af2c-923c00eb793d', permissive_license_maximal_freedom).
narrative_ontology:cs_drift_state('642aa56d-6806-45d4-af2c-923c00eb793d', platform_scale_proprietary_enclosure_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('642aa56d-6806-45d4-af2c-923c00eb793d', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, proprietary_derivative_builders).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, commons_developers).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, downstream_free_software_ecosystem).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).

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
 *   Extractiveness is authored at 0.68 because the constraint's operation transfers value (code, labor, innovation) from commons developers to proprietary builders without requiring return. The measurement series shows extractiveness accumulating from 0.35 (early era, when proprietary incorporation was limited) to 0.68 (contemporary, when cloud, mobile, and AI platforms routinely incorporate permissively licensed code as proprietary components). Plateauing at t=25+ reflects the steady state: proprietary builders have internalized the practice, commons developers have adapted expectations, and the extraction rate is now structural rather than growing. Suppression is 0.52 because the constraint's persistence requires suppressing two alternatives: (1) commons developers choosing copyleft instead of permissive licensing (suppressed by norm/tradition/'permissive is more free'), and (2) downstream users recognizing they could demand reciprocal contribution (suppressed by the license text itself, which makes no such requirement). Theater is 0.28 (moderate-low) because the arrangement is substantially functional — permissive licensing does genuinely reduce friction — but a growing share of its operation defends the extraction (enforcement against GPL adoption, rhetorical campaigns claiming copyleft is 'restrictive') rather than the original friction-reduction function. Accessibility collapse is 0.71 because once a commons developer publishes under MIT/BSD, their options to alter the license retroactively are almost completely gone (copyright law, accepted practice); the constraint locks in quickly. Resistance is 0.64 because copyleft advocates, some major FOSS projects (Linux kernel, GNU project), and some corporations (Red Hat, Canonical) actively resist and promote copyleft alternatives, generating friction against the permissive-default norm.
 *
 * PERSPECTIVAL GAP:
 *   From the proprietary builder's seat, the constraint is genuine coordination and freedom (they can use proven code without legal friction, can integrate rapid innovation from the commons). From the commons developer's seat, the same constraint is exploitation (their labor is captured, their ability to require reciprocal contribution is removed, they bear the maintenance cost). From the downstream ecosystem's seat, it is structural dependency and vulnerability (they cannot access improvements the ecosystem's own code enabled). The engine computes these divergences from the structural data: the power atoms differ (institutional vs. moderate vs. powerless), the exit options differ (arbitrage vs. identity-locked vs. trapped), the beneficiary/victim role differs. The claim/metric gap is deliberate: CLAIMED as tangled_rope (mixed coordination and extraction) and AUTHORED as extractive metrics (0.68 epsilon, 0.52 suppression) — the engine measures whether the claim holds or whether the metrics reveal a different type.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary builders benefit structurally (low d → low/negative extraction for them) because they can incorporate code without reciprocal obligation; they have arbitrage-level exit (they can choose which licenses and components to use). Commons developers are structural targets (high d → high extraction from them) because they bear the cost of developing and maintaining code that can be enclosed; their identity-locked exit means they cannot easily abandon commons ideology without experiencing it as loss of self. The downstream ecosystem is a collective victim (high d) with trapped exit (cannot access proprietary improvements, cannot require reciprocal contribution). Copyleft advocates sit near symmetric (moderate extraction) — they benefit from any adoption of copyleft licenses but bear the cost of advocating for it. The reading diverges from the commons-coordination reading precisely here: commons-coordination would place all parties near symmetric or beneficiary (no extraction, everyone benefits from reduced friction), while this reading places proprietary builders at beneficiary and commons seats at target.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (friction from licensing complexity in early open-source) is CONTESTED in its status. Permissive license advocates attest it is still live (legal complexity remains a barrier for new projects). Copyleft advocates and some corporations attest it is partly solved but that the solution enabled a secondary exploitation dynamic (proprietary enclosure) not foreseen in the 1990s. The disappearance verdict is WORLD_REARRANGES because if permissive licenses disappeared and all open-source used copyleft or commercial models, proprietary incorporation would face material friction and the investment landscape would shift. The copyleft-counterfactual reading resolves the mandatrophy tension by asserting that the original founding problem is partially solved but that solving it unleashed a secondary extraction mechanism (the ability to enclose commons-produced code), which is now the dominant function. The constraint persists not primarily because it solves the founding problem, but because it solves the derivative problem (for proprietary builders: how to access commons innovation without reciprocal cost). This shifts the constraint's mandate from 'reduce licensing friction' to 'enable proprietary enclosure of commons code' — the original mandate is obsolete as the primary function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_mechanism_empirical_necessity,
    'Is reciprocal licensing (GPL-style copyleft) a structurally necessary mechanism to prevent exploitation of permissively licensed commons, or can equivalent incentive structures be achieved through norms, funding, or reputation systems without legal reciprocity requirements?',
    'Long-term empirical measurement: do proprietary derivative builders contribute proportionally to commons maintenance when reciprocity is not legally required? Do funding models (sponsorship, embedded maintainers, foundation grants) sustain commons infrastructure without copyleft enforcement? Do reputation and market pressure generate reciprocal behavior?',
    'If empirical data shows proportional reciprocal contribution occurs without copyleft enforcement, the extraction reading weakens and the commons-coordination reading gains force. If contribution correlates with copyleft enforcement and permission, the tangled-rope extraction reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_mechanism_empirical_necessity, empirical, 'Whether copyleft is necessary or sufficient to prevent exploitation.').

omega_variable(
    identity_locked_commons_developer_exit,
    'Are commons developers identity-locked (exit appears as betrayal of core values), or merely exit-constrained (economic and professional barriers, but ideologically mobile)?',
    'Ethnographic study of developer decisions to leave commons work for proprietary roles. Track linguistic framing: do departing developers describe the shift as ''betrayal'' (identity-locked signal) or as ''economics forced my hand'' (constrained, ideologically sound). Monitor narrative reconstruction: do returnees to commons work describe proprietary experience as corrupting, or as parenthetical?',
    'If identity-locked, suppression and resistance metrics are underestimated because commons developers internalize the constraint; the effective extraction is higher than the structural measure suggests. If merely constrained, the standard directionality derivation holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_commons_developer_exit, empirical, 'Suppression mechanism: structural vs. internalized in commons developers.').

omega_variable(
    downstream_ecosystem_structural_dependence,
    'Is the ''downstream free software ecosystem'' a real structural agent with collective interests (coercible as a victim), or a disaggregated set of individual projects each making independent license choices?',
    'Do ecosystem-level coordination mechanisms exist (common funding pools, licensing coalitions, shared defense strategies)? Can ecosystem actors collectively enforce terms or negotiate with proprietary builders? Do individual projects perceive shared fate with the broader ecosystem, or only their own licensing terms?',
    'If structural, the ''downstream ecosystem'' is a real victim seat with trapped exit; if disaggregated, the extraction falls on individual commons developers, not a collective. The directionality derivation changes: a disaggregated set of moderate-power individual agents is different from a collective powerless victim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(downstream_ecosystem_structural_dependence, conceptual, 'Whether the downstream ecosystem is a coherent victim class or a disaggregated set of individual agents.').

omega_variable(
    kernel_reading_contention_location,
    'The three sibling readings (commons-coordination, copyleft-counterfactual, corporate-moat) disagree about whether permissive licensing enables coordination (reading 1), exploitative extraction (reading 2: this reading), or proprietary enclosure (reading 3). Where is the fundamental disagreement located: in empirical assessment of who benefits (same facts, different winners), in normative framing of ''benefit'' and ''harm'', or in counterfactual reasoning about what would happen under different licensing regimes?',
    'Construct three constraint stories (one per reading) with identical factual base (the same code, the same licensing choices, the same proprietary incorporations) and examine what differs: the identities of beneficiaries/victims, the epsilon values, the claimed types. The delta reveals the reading structure.',
    'If delta is in beneficiary identity (same effects, different winners), readings coexist (neither forecloses). If delta is in normative interpretation (''coordination'' vs. ''exploitation''), readings influence each other (corporate-moat challenges commons-coordination; copyleft-counterfactual challenges both). If delta is in counterfactual reasoning, each reading instantiates a different constraint (different epsilon).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contention_location, conceptual, 'Location of fundamental disagreement between kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(perm_tr_t5, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(perm_tr_t10, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(perm_tr_t15, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(perm_tr_t25, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(perm_tr_t30, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(perm_tr_t35, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 35, 0.28).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(perm_be_t15, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(perm_be_t25, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(perm_be_t30, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(perm_be_t35, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(perm_su_t5, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(perm_su_t10, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(perm_su_t15, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(perm_su_t20, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(perm_su_t25, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(perm_su_t30, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(perm_su_t35, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 35, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(permissive_license_text__copyleft_counterfactual_reading, 0.12).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__corporate_moat_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, open_source_maintainer_sustainability_crisis).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, proprietary_platform_commons_dependency).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel permissive_license_text. Sibling readings produce different constraint stories: commons_coordination_reading models permissive licensing as maximal-freedom coordination; corporate_moat_reading models it as proprietary enclosure. All three readings share the same empirical base (the code, the licensing choices, the incorporations) but disagree on normative framing and counterfactual reasoning. The three stories must be authored as separate constraints, each with its own epsilon, its own beneficiary/victim structure, and its own claimed type. Network edges link them to show they are readings of the same kernel and to enable cross-reading analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(permissive_license_text__copyleft_counterfactual_reading, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
