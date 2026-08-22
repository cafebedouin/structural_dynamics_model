% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Permissive License Text (No-Reciprocity Reading): Structural Enabler of Uncompensated Proprietary Capture
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the copyleft_counterfactual_reading of the
 *   permissive_license_text kernel: the claim that the absence of a
 *   reciprocity requirement in permissive license text (MIT/BSD/Apache-style)
 *   is not a neutral simplification but a structural enabler of uncompensated
 *   exploitation, and that viral reciprocity mechanisms (GPL-style copyleft)
 *   are the necessary corrective. Under this reading, the same license text
 *   that the commons_coordination_reading treats as friction-minimizing and
 *   the corporate_moat_reading treats as an extraction vector for closed
 *   derivatives is measured here as a tangled rope: it does coordinate a
 *   genuine commons (broad low-friction reuse), but it does so while allowing
 *   an asymmetric transfer of unpaid labor value to well-resourced commercial
 *   actors, and that transfer requires no coercion to occur — the license
 *   text itself, unmodified, is the mechanism. Rising extraction over the
 *   interval traces the growth of managed-cloud and SaaS business models
 *   built on permissively licensed foundations without funding flowing back.
 *
 * KEY AGENTS:
 *   - proprietary_derivative_vendors: primary beneficiary — captures commercial value without reciprocal contribution
 *   - cloud_hyperscalers: primary beneficiary — captures the largest share via managed services at global scale
 *   - original_permissive_maintainers: primary target — bears the uncompensated labor cost
 *   - unpaid_contributor_communities: primary target — sustains the commons that is captured
 *   - gpl_style_licensors: counterfactual comparator — excluded from this project's governance but structurally relevant as the road-not-taken
 *   - license_foundations: agenda-setter — administers the license text and could add reciprocity but has institutional reasons not to
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.72).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.38).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive License Text (No-Reciprocity Reading): Structural Enabler of Uncompensated Proprietary Capture").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "software_licensing/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, 'bdda1bd7-6714-4fa7-a0d2-e4d308eed089').
narrative_ontology:cs_kernel_codification('bdda1bd7-6714-4fa7-a0d2-e4d308eed089', fixed_text).
narrative_ontology:cs_authority_grounding('bdda1bd7-6714-4fa7-a0d2-e4d308eed089', practice).
narrative_ontology:cs_interpretation_layer_present('bdda1bd7-6714-4fa7-a0d2-e4d308eed089').
narrative_ontology:cs_reading_relation('bdda1bd7-6714-4fa7-a0d2-e4d308eed089', permissive_license_text__commons_coordination_reading, influences).
narrative_ontology:cs_reading_relation('bdda1bd7-6714-4fa7-a0d2-e4d308eed089', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_axiom('bdda1bd7-6714-4fa7-a0d2-e4d308eed089', foundational, reciprocity_is_necessary_for_commons_survival).
narrative_ontology:cs_axiom_status(reciprocity_is_necessary_for_commons_survival, holdable).
narrative_ontology:cs_axiom_grounding('bdda1bd7-6714-4fa7-a0d2-e4d308eed089', reciprocity_is_necessary_for_commons_survival, instrumental).
narrative_ontology:cs_axiom('bdda1bd7-6714-4fa7-a0d2-e4d308eed089', secondary, silent_terms_permit_asymmetric_capture_absent_intent).
narrative_ontology:cs_axiom_status(silent_terms_permit_asymmetric_capture_absent_intent, holdable).
narrative_ontology:cs_axiom_grounding('bdda1bd7-6714-4fa7-a0d2-e4d308eed089', silent_terms_permit_asymmetric_capture_absent_intent, empirically_contingent).
narrative_ontology:cs_reference_frame('bdda1bd7-6714-4fa7-a0d2-e4d308eed089', friction_minimization_founding_intent).
narrative_ontology:cs_drift_state('bdda1bd7-6714-4fa7-a0d2-e4d308eed089', cloud_native_saas_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bdda1bd7-6714-4fa7-a0d2-e4d308eed089', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, proprietary_derivative_vendors).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, cloud_hyperscalers).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, original_permissive_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, unpaid_contributor_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, downstream_open_source_users).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, downstream_open_source_users).
narrative_ontology:constraint_vindicates(permissive_license_text__copyleft_counterfactual_reading, reciprocity_is_necessary_for_commons_survival).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Take permissively licensed code, incorporate it into closed products or managed services, and sell it without any obligation to return improvements, pay license fees, or disclose derivative source. They select permissive licenses specifically because the license text does not require reciprocity, and they can walk away from any given upstream project without cost.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, proprietary_derivative_vendors, beneficiary,
    institutional, biographical, arbitrage, global).

% Operate managed-service offerings built on permissively licensed databases, frameworks, and tools, capturing the majority of commercial value from software whose maintainers receive no share of that revenue. Their scale and capital let them absorb any single upstream project's relicensing or fork without meaningful cost.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, cloud_hyperscalers, beneficiary,
    institutional, civilizational, arbitrage, global).

% Wrote and maintain the underlying project under a permissive license, often unpaid or under-resourced, while commercial derivatives built on their work generate revenue that does not flow back. Their options are burnout, relicensing prospectively (which does not recover past value extracted), or dual-licensing schemes that most cannot execute credibly once large vendors have already forked or productized the code.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, original_permissive_maintainers, payer,
    moderate, biographical, constrained, global).

% Volunteer labor and code review sustain the commons that permissive licensing exposes to uncompensated commercial capture. They are trapped in the sense that leaving does not recover sunk contribution history, and the absence of reciprocity means their labor subsidizes vendors they have no relationship with and no leverage over.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, unpaid_contributor_communities, payer,
    powerless, biographical, trapped, global).

% Represent the counterfactual this reading points to: projects that adopted copyleft (viral reciprocity) terms instead, forcing any distributed derivative to remain under the same terms. They are not parties to a permissively licensed project's dispute, but their existence is the argument's evidentiary anchor — this reading treats their outcomes as the road not taken by the permissive projects under analysis, and they are excluded from the permissive project's own governance conversation entirely.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, gpl_style_licensors, excluded,
    organized, generational, mobile, global).

% Benefit from free, low-friction access to the permissively licensed software in the near term, but bear the longer-run cost if uncompensated extraction causes maintainer burnout, project abandonment, or security neglect as commercial value is captured elsewhere without funding flowing back to sustain the project.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, downstream_open_source_users, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, downstream_open_source_users, payer).

% Draft and steward the permissive license texts (e.g., MIT, Apache, BSD variants) that set the reciprocity-free default. They administer the license language itself and could add reciprocity clauses in future revisions, but have structural reasons — adoption breadth, corporate contributor comfort, legal simplicity — to keep the text as-is.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, license_foundations, agenda_setter,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__copyleft_counterfactual_reading, cloud_hyperscalers).
narrative_ontology:fixing_cost_class(permissive_license_text__copyleft_counterfactual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permissive licensing genuinely lowers legal friction for anyone wanting to reuse, embed, or build on the code, enabling broad adoption without lawyers negotiating terms — this is a real coordination achievement the reading does not dispute.
% TRANSFER_FUNCTION: The license text's absence of a reciprocity requirement moves the value of unpaid maintainer and contributor labor into proprietary derivative products and managed cloud services, without any compensating flow of code, funding, or governance rights back to the originating project.
% ABSENT_VOICES: Original maintainers and volunteer contributors rarely have a seat when large vendors decide to build commercial offerings on their work; the license terms were often set early in a project's life, before its commercial value was foreseeable, by parties who did not anticipate the extraction pattern this reading identifies.
% DISAPPEARANCE_RATIONALE: If the no-reciprocity default in permissive license text vanished — replaced uniformly by a copyleft-style reciprocity requirement — commercial vendors and hyperscalers would either have to open-source their derivatives, negotiate paid alternative licenses, or abandon use of the affected code; the flow of unpaid value into closed products would be structurally interrupted and maintainer economics would shift substantially.
% FOUNDING_PROBLEM: Early open-source licensing sought to remove legal barriers to reuse so software could spread and be improved collaboratively without the friction of individualized negotiation.
% FOUNDING_PROBLEM_CORROBORATION: Maintainers of high-profile permissively licensed projects that were later productized by major vendors (documented in public relicensing disputes, e.g. projects that moved to source-available or copyleft terms specifically citing uncompensated cloud extraction) attest the founding problem of 'frictionless collaboration' has been overtaken by a different, unaddressed problem — uncompensated commercial capture. Vendors and license foundations dispute this characterization and maintain the original friction-reduction rationale remains sufficient justification for the license text as written.
narrative_ontology:disappearance_verdict(permissive_license_text__copyleft_counterfactual_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__copyleft_counterfactual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__copyleft_counterfactual_reading, 0.72, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.72 at interval end) because this reading holds that the absence of a copyleft-style reciprocity clause is not incidental — it is the specific structural feature that permits commercial derivatives to capture value without returning any of it, and that pattern has intensified as cloud-native business models matured. Suppression is authored moderate (0.38), lower than extraction, because no one is coerced into using permissively licensed code — the extraction operates through terms freely agreed to at time of license selection, not through blocked exits; the coercive element is diffuse and structural (path dependency, ecosystem lock-in) rather than direct. Theater ratio is low-to-moderate (0.28) — most of the coordination function (frictionless reuse) is genuinely functional, not performative, which is precisely what makes the tangled-rope diagnosis apt rather than a snare diagnosis: there is a real coordination achievement riding alongside the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary derivative vendors and cloud hyperscalers are coded as beneficiaries with arbitrage-grade exit: they can adopt, fork, or abandon any given permissively licensed project at will, and their capital lets them internalize the value of the commons without being bound to it. Original maintainers and unpaid contributors are coded as targets with constrained or trapped exit: their labor is sunk into a codebase governed by terms set (often early, often without foresight of commercial scale) that do not follow the code's commercial value back to them. This is the crux of the copyleft_counterfactual_reading — under a GPL-style regime, the same commercial reuse would trigger a reciprocal obligation, converting the vendor's exit from arbitrage-grade to constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading resists collapsing to pure extraction (snare) because the coordination function — frictionless reuse enabling broad ecosystem growth — is real and is not merely cover; permissively licensed software genuinely spread further and faster than it likely would have under stricter terms. It equally resists collapsing to pure coordination (rope) because the asymmetric, uncompensated transfer of value to well-capitalized actors is not incidental noise but a first-order structural consequence of the specific term (no reciprocity requirement) this reading isolates. Tangled rope is the correct classification precisely because both the coordination and the extraction are load-bearing and simultaneous, not sequential or separable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_necessity_vs_sufficiency,
    'Is a copyleft-style reciprocity requirement actually necessary to prevent the extraction this reading identifies, or merely one sufficient mechanism among several (e.g., trademark-based commercial licensing, foundation-funded maintenance, dual-licensing)?',
    'Comparative case study of projects that relicensed from permissive to source-available or copyleft terms specifically citing extraction concerns (e.g., Elasticsearch, MongoDB, Redis-adjacent disputes) versus projects that solved the funding problem through foundation stewardship or dual-licensing without changing the reciprocity terms.',
    'If reciprocity is merely sufficient rather than necessary, the reading''s central prescriptive claim weakens even if its descriptive diagnosis of extraction stands; other tangled-rope resolutions besides copyleft become live alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_necessity_vs_sufficiency, conceptual, 'Whether GPL-style reciprocity is uniquely necessary or one of several viable correctives.').

omega_variable(
    maintainer_consent_at_license_selection,
    'Did original maintainers genuinely consent to the extraction pattern when they selected a permissive license, given that commercial cloud-scale extraction was often not foreseeable at the time of selection?',
    'Historical review of project licensing decisions and contemporaneous maintainer statements at time of license adoption, compared against the commercial landscape that later emerged.',
    'If genuine informed consent existed, the extraction is closer to a bargained-for outcome the maintainers accepted; if foreseeability was absent, the tangled-rope''s victim classification is more clearly involuntary rather than a knowing trade-off.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintainer_consent_at_license_selection, empirical, 'Whether the no-reciprocity choice was informed consent or unforeseen exposure.').

omega_variable(
    kernel_framing_selection,
    'Is the permissive_license_text kernel better framed as the license TEXT alone (as this reading and its siblings assume), or as the license text PLUS the ecosystem-level distribution of bargaining power that determines who can exploit the text''s silence on reciprocity?',
    'Compare classification outcomes under a framing where the kernel is the bare legal text versus a framing where the kernel includes the prevailing market structure (concentration of cloud infrastructure ownership) that converts textual silence into extractive capacity.',
    'Under the text-only framing (adopted here), extraction is a property of the term itself, supporting a tangled_rope reading transferable across contexts. Under the text-plus-market-structure framing, extraction would be more contingent on hyperscaler market concentration specifically, potentially reclassifying this reading closer to a scaffold (transitional, tied to a particular market phase) rather than a persistent tangled_rope. This story adopts the text-only framing because the reciprocity clause itself is the portable, license-family-defining variable the sibling readings also key on; the alternative framing is flagged here for record.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_selection, conceptual, 'Alternative framing of the kernel boundary and its effect on classification stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(perm_tr_t5, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(perm_tr_t10, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(perm_tr_t15, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(perm_tr_t25, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(perm_be_t15, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(perm_be_t25, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 25, 0.72).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(permissive_license_text__copyleft_counterfactual_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(permissive_license_text__copyleft_counterfactual_reading, 0.12).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, corporate_moat_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the permissive_license_text kernel per the epsilon-invariance principle. commons_coordination_reading authors low epsilon (rope-leaning, friction-minimization framing). corporate_moat_reading authors the highest epsilon and treats the extraction as intentional corporate strategy (snare-leaning). This reading (copyleft_counterfactual_reading) sits between the two on intent but treats the extraction as structurally load-bearing rather than incidental, landing on tangled_rope: real coordination value coexists with asymmetric extraction that does not require any actor's bad intent, only the absence of a reciprocity clause plus normal profit-seeking behavior by well-capitalized adopters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
