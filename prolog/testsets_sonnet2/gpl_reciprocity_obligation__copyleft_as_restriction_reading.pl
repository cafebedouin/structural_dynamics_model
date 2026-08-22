% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_restriction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_restriction_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_restriction_reading
 *   human_readable: GPL Reciprocity Obligation, Read as a Restriction on Proprietary Integration
 *   domain: software_licensing/intellectual_property
 *
 * SUMMARY:
 *   This story reads the GPL's reciprocity clause through the lens of the
 *   business model it forecloses: any commercial actor who wants to integrate
 *   GPL-licensed code into a proprietary product must either open-source
 *   their addition or obtain a separate commercial license, typically from
 *   whoever holds copyright over the original codebase. Rather than framing
 *   this purely as freedom-preservation, this reading emphasizes the
 *   restriction's function as a chokepoint that some rights-holders
 *   (dual-licensing vendors) and market operators (proprietary fork operators
 *   exploiting license boundaries) convert into commercial leverage — value
 *   captured from the labor of commons contributors who did not intend to
 *   subsidize a licensing business.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.58).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.62).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Reciprocity Obligation, Read as a Restriction on Proprietary Integration").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "software_licensing/intellectual_property").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '34038b6a-0454-4efd-8182-70319a41f9a8').
narrative_ontology:cs_kernel_codification('34038b6a-0454-4efd-8182-70319a41f9a8', fixed_text).
narrative_ontology:cs_authority_grounding('34038b6a-0454-4efd-8182-70319a41f9a8', lineage).
narrative_ontology:cs_interpretation_layer_present('34038b6a-0454-4efd-8182-70319a41f9a8').
narrative_ontology:cs_reading_relation('34038b6a-0454-4efd-8182-70319a41f9a8', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('34038b6a-0454-4efd-8182-70319a41f9a8', gpl_reciprocity_obligation__copyleft_as_commons_reading, influences).
narrative_ontology:cs_axiom('34038b6a-0454-4efd-8182-70319a41f9a8', foundational, reciprocity_clause_is_commercializable_chokepoint).
narrative_ontology:cs_axiom_status(reciprocity_clause_is_commercializable_chokepoint, holdable).
narrative_ontology:cs_axiom_grounding('34038b6a-0454-4efd-8182-70319a41f9a8', reciprocity_clause_is_commercializable_chokepoint, empirically_contingent).
narrative_ontology:cs_axiom('34038b6a-0454-4efd-8182-70319a41f9a8', secondary, restriction_effect_dominates_protection_effect_in_practice).
narrative_ontology:cs_axiom_status(restriction_effect_dominates_protection_effect_in_practice, holdable).
narrative_ontology:cs_axiom_grounding('34038b6a-0454-4efd-8182-70319a41f9a8', restriction_effect_dominates_protection_effect_in_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('34038b6a-0454-4efd-8182-70319a41f9a8', reciprocity_as_anti_enclosure_safeguard).
narrative_ontology:cs_drift_state('34038b6a-0454-4efd-8182-70319a41f9a8', commercial_open_source_maturation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('34038b6a-0454-4efd-8182-70319a41f9a8', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, dual_licensing_vendors).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_fork_operators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, downstream_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, startup_founders_seeking_proprietary_extension).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Contribute code under the expectation that derivative works remain open, but under this reading their labor becomes leverage a dual-licensing vendor can monetize by selling proprietary exceptions to the same code they wrote for free. They cannot capture the commercial value their reciprocity obligation forces onto others, and forking away from the license does not recover the value already extracted upstream.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors, payer,
    moderate, generational, constrained, global).

% Want to combine GPL-licensed components with proprietary code to ship a commercial product. The license's copyleft clause forbids this without either open-sourcing their own additions or purchasing a commercial exception, forcing a binary choice between abandoning the component, giving away proprietary work, or paying licensing fees to whoever controls dual-licensing rights.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, downstream_integrators, payer,
    moderate, biographical, constrained, global).

% Built early products on GPL infrastructure before understanding the reciprocity terms. Now facing a choice between rewriting core infrastructure from scratch, open-sourcing their differentiator, or paying a rights-holder for a commercial license — with limited capital and no leverage to negotiate, they experience the obligation as a tax on the specific business model they chose to pursue.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, startup_founders_seeking_proprietary_extension, payer,
    powerless, biographical, trapped, national).

% Hold copyright over a GPL codebase and sell proprietary licenses to companies that want to avoid the reciprocity obligation. They benefit directly from the restriction's bite: the more painful the copyleft terms are for integrators, the more valuable the commercial exception becomes. They set the terms of both the open license and its paid alternative.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, dual_licensing_vendors, beneficiary,
    organized, generational, arbitrage, global).

% Operate businesses built on relicensing, wrapping, or exploiting ambiguity at the edges of the copyleft boundary (e.g., SaaS loopholes, permissive shims, license laundering through intermediary layers) to capture commercial value from commons code while minimizing what they give back.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_fork_operators, beneficiary,
    powerful, biographical, arbitrage, global).

% Drafted and maintain the license text and its enforcement norms, framing the reciprocity clause as protecting user freedom. Under this reading, their drafting choices structurally created the leverage point that dual-licensing vendors and fork operators now exploit commercially, whether or not that was the intent.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, original_license_authors, agenda_setter,
    institutional, civilizational, analytical, global).

% Offer functionally similar software under permissive (MIT/BSD-style) terms that integrators could choose instead. They are not part of the GPL governance conversation but structurally benefit whenever the copyleft restriction pushes commercial integrators toward their alternative — a voice absent from the license's own debates but present in the market.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, permissively_licensed_competitors, excluded,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_restriction_reading, dual_licensing_vendors).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_restriction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reciprocity clause solves a real collective-action problem in principle — preventing any single actor from privatizing improvements to shared code without contributing back — but this reading holds that in practice it also creates a chokepoint that specific rights-holders convert into a toll on commercial integration.
% TRANSFER_FUNCTION: Moves optionality and commercial flexibility away from downstream integrators and startups toward whoever holds the copyright and can sell a proprietary exception, and toward operators skilled at working the boundary of the license without triggering its obligations.
% ABSENT_VOICES: Permissively-licensed competitors and integrators who abandoned GPL-based stacks entirely are not represented in license-drafting or enforcement discussions; their exit is invisible to the community that maintains the license because they simply left rather than contest the terms.
% DISAPPEARANCE_RATIONALE: Beneficiary seats (dual-licensing vendors, fork operators) would lose a valuable leverage point and the world would rearrange toward more integration and more permissive forks; commons-preservation advocates dispute this and argue the commons itself would erode without the reciprocity mechanism, so the two readings of what 'the world' looks like after disappearance genuinely disagree.
% FOUNDING_PROBLEM: Proprietary software vendors in the 1980s were capturing improvements to shared code, redistributing modified binaries without source, and using copyright to lock users out of code they depended on.
% FOUNDING_PROBLEM_CORROBORATION: Free software foundations and original license authors attest the founding problem remains live (proprietary capture still occurs). Independent economic analysis from software licensing scholars outside both the free-software movement and the dual-licensing industry documents that the same clause now functions as a revenue mechanism for copyright-holding vendors — a function the founding text did not anticipate and that neither self-interested camp fully corroborates from outside its own position.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_restriction_reading, contested).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_restriction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the measured interval (0.30 to 0.58) as dual-licensing business models matured and commercial exception fees became a recognized revenue category; suppression climbs correspondingly (0.40 to 0.62) as license enforcement (through foundations, litigation, and automated scanning of shipped binaries) intensified. Theater ratio stays low and rises only modestly (0.10 to 0.20) because enforcement, even when serving a rent-collection function, still performs a real technical verification role (checking actual license compliance) rather than pure performance.
 *
 * PERSPECTIVAL GAP:
 *   From the dual-licensing vendor's seat, the restriction is a legitimate return on the investment of writing and maintaining the original code. From the startup founder's seat, the same clause is an unanticipated tax triggered by a licensing decision made before the commercial terrain was understood. The engine computes these as structurally different positions from the declared power/exit data, not from either party's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Dual-licensing vendors and proprietary fork operators sit near the beneficiary end: they hold leverage (copyright, boundary-exploitation expertise) that lets them convert the restriction into revenue or competitive advantage, and their exit options are effectively arbitrage — they can restructure their own licensing posture at will. Commons contributors, downstream integrators, and cash-constrained startup founders sit near the target end: their labor or business model is what the restriction constrains, and their exit options range from constrained (established integrators who can rewrite around the dependency) to trapped (founders with no capital to rewrite core infrastructure).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing proprietary capture without reciprocity) is contested as either live or displaced. This reading holds it has been partially displaced by a rent-extraction function riding on the same clause: the mechanism built to prevent one kind of capture (silent proprietary forking) now enables a different kind (commercial-exception fee extraction) for whichever party controls the copyright. Classifying this as tangled_rope rather than snare preserves the genuine coordination function the clause still performs (it does prevent naive proprietary capture) while registering the asymmetric extraction that has grown alongside it — collapsing to snare would erase the real coordination the freedom and commons readings correctly identify; collapsing to rope would erase the extraction this reading is built to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restriction_vs_protection_framing,
    'Is the reciprocity clause''s primary structural effect the prevention of enclosure (as the commons and freedom readings hold) or the creation of a commercializable chokepoint (as this reading holds), or are these two effects inseparable properties of the same mechanism?',
    'Comparative analysis of dual-licensing revenue as a share of total value created by GPL projects across a large sample, cross-referenced against counterfactual permissive-license outcomes for structurally similar codebases.',
    'If the two effects are inseparable, the three sibling readings are evaluating the same structure from different value positions rather than describing different mechanisms — which would argue for treating ε as reading-indexed rather than resolving to a single ''true'' value, consistent with OQ-26.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restriction_vs_protection_framing, conceptual, 'Whether restriction and protection are separable structural effects of the same clause or two evaluative framings of one inseparable mechanism.').

omega_variable(
    copyright_holder_concentration,
    'How concentrated is copyright ownership in dual-licensed GPL codebases, and does that concentration determine whether the reciprocity clause functions more as commons protection or vendor leverage?',
    'Survey of copyright assignment agreements (CLAs) across major dual-licensed open source projects, measuring what fraction of contributions are copyright-assigned to a single commercial entity versus retained by a distributed contributor base.',
    'High concentration would support this reading''s beneficiary claim (a single vendor captures the leverage); low concentration would weaken it and support the commons reading''s claim that reciprocity benefits are genuinely distributed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copyright_holder_concentration, empirical, 'Whether copyright concentration in dual-licensed projects determines which sibling reading better fits a given codebase.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly do the three sibling readings of this kernel diverge — is it in what the clause DOES (a factual/structural dispute) or in how its effects should be WEIGHED (a normative dispute)?',
    'None fully available; would require decomposing each reading''s claims into falsifiable structural predictions versus value judgments and testing agreement on the structural layer alone.',
    'If the disagreement is purely normative (all three agree on mechanism, disagree on evaluation), the kernel is better modeled as one constraint with three observer stances rather than three constraints — but per the ε-invariance principle, since each reading authors a materially different ε and different beneficiary/victim sets, this story treats them as structurally distinct claims, not merely differently valued ones.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Whether the three kernel readings disagree about facts or about values, and what that implies for whether they are really three constraints or one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(gpl__tr_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(gpl__tr_t15, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(gpl__tr_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(gpl__tr_t25, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 25, 0.2).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gpl__be_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(gpl__be_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(gpl__be_t15, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(gpl__be_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(gpl__be_t25, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gpl__su_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(gpl__su_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement(gpl__su_t15, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(gpl__su_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(gpl__su_t25, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the GPL reciprocity obligation' per the ε-invariance principle. copyleft_as_freedom_reading treats the same clause as low-extraction user-protection (Rope/Mountain-adjacent); copyleft_as_commons_reading treats it as anti-enclosure institutional technology; this story (copyleft_as_restriction_reading) treats it as a business-model constraint convertible into vendor leverage (Tangled Rope, higher ε). All three share the kernel text and the underlying legal mechanism but author different beneficiary/victim structures and different ε values, consistent with treating them as three linked but structurally distinct constraints rather than one constraint with an observer parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
