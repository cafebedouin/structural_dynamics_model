% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_commons_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Reciprocity Obligation as Commons-Preserving Institution
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The GNU General Public License requires that any distributed derivative
 *   of GPL-licensed code be released under the same license, with source
 *   available. Read as commons-preserving institutional technology, this is a
 *   coordination mechanism solving a genuine collective-action problem:
 *   absent a reciprocity requirement, a jointly built software commons is
 *   vulnerable to unilateral privatization by whichever actor is best
 *   positioned to extract and enclose it. This reading treats the commons
 *   itself as an institutional beneficiary and treats individual
 *   developers/firms who would prefer to extract value without returning it
 *   as the parties who bear the obligation's cost — a medium-extraction
 *   tangled rope, not a pure snare, because the coordination function
 *   (sustained collective codebase growth) is real and substantial, not
 *   merely cover for extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.42).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.55).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Reciprocity Obligation as Commons-Preserving Institution").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, '44b65ce2-f75c-4659-a203-3cc9d8039aa4').
narrative_ontology:cs_kernel_codification('44b65ce2-f75c-4659-a203-3cc9d8039aa4', fixed_text).
narrative_ontology:cs_authority_grounding('44b65ce2-f75c-4659-a203-3cc9d8039aa4', practice).
narrative_ontology:cs_interpretation_layer_present('44b65ce2-f75c-4659-a203-3cc9d8039aa4').
narrative_ontology:cs_reading_relation('44b65ce2-f75c-4659-a203-3cc9d8039aa4', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('44b65ce2-f75c-4659-a203-3cc9d8039aa4', gpl_reciprocity_obligation__copyleft_as_restriction_reading, influences).
narrative_ontology:cs_axiom('44b65ce2-f75c-4659-a203-3cc9d8039aa4', foundational, collective_codebase_is_a_protectable_good).
narrative_ontology:cs_axiom_status(collective_codebase_is_a_protectable_good, holdable).
narrative_ontology:cs_axiom_grounding('44b65ce2-f75c-4659-a203-3cc9d8039aa4', collective_codebase_is_a_protectable_good, conventional).
narrative_ontology:cs_axiom('44b65ce2-f75c-4659-a203-3cc9d8039aa4', secondary, reciprocity_obligation_is_necessary_anti_enclosure_mechanism).
narrative_ontology:cs_axiom_status(reciprocity_obligation_is_necessary_anti_enclosure_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('44b65ce2-f75c-4659-a203-3cc9d8039aa4', reciprocity_obligation_is_necessary_anti_enclosure_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('44b65ce2-f75c-4659-a203-3cc9d8039aa4', commons_protection_founding_intent).
narrative_ontology:cs_drift_state('44b65ce2-f75c-4659-a203-3cc9d8039aa4', contemporary_permissive_license_proliferation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('44b65ce2-f75c-4659-a203-3cc9d8039aa4', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, the_commons_as_institution).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_contributors).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, long_horizon_maintainers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, individual_exit_maximizers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_forkers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, end_users_of_derivative_software).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The accumulated body of GPL-licensed code and the norm of shared improvement it sustains. It is not an actor but the standing arrangement the reciprocity obligation exists to keep intact — every enforced copyleft clause adds to its stock and forecloses its unilateral privatization by any one contributor.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, the_commons_as_institution, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(gpl_reciprocity_obligation__copyleft_as_commons_reading, the_commons_as_institution).

% Developers and firms who build on GPL code and, because the license requires it, must release their derivative improvements under the same terms. They receive a growing shared codebase in exchange for the obligation, and most report the trade favorable over a multi-year horizon even though it forecloses the option to keep enhancements proprietary.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_contributors, beneficiary,
    organized, generational, constrained, global).

% Project stewards (e.g., FSF, kernel maintainers, foundation boards) who chose copyleft terms and defend them through license enforcement actions. They administer the reciprocity requirement, litigate or negotiate compliance with violators, and treat the mandatory-sharing clause as the mechanism that keeps the commons from being strip-mined by any single actor's short-term advantage.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, long_horizon_maintainers, beneficiary,
    organized, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_commons_reading, long_horizon_maintainers, agenda_setter).

% Individual developers or small teams who would prefer to take GPL code, modify it, and monetize the modification privately without releasing source. The reciprocity obligation forecloses this path entirely — their only options are to comply (release derivative source), avoid GPL code altogether, or violate the license and risk enforcement. From their seat, the obligation is a tax on their labor paid directly to a collective they did not choose to join by using the software.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, individual_exit_maximizers, payer,
    moderate, biographical, constrained, global).

% Firms with the resources to build substantial products atop GPL components who are blocked from a closed-source distribution model by the copyleft terms. They can afford legal review and sometimes negotiate dual-licensing, but absent that route they must either comply, engineer around the GPL dependency at real cost, or exit the ecosystem. Their scale means the reciprocity obligation costs them the most in absolute foregone-proprietary-value terms even though they have more workarounds than individual developers.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_forkers, payer,
    powerful, biographical, constrained, global).

% People who receive the source code and modification rights to software built on GPL components, because the license forced disclosure. They benefit incidentally from a fight they are not party to, gaining rights they did not bargain for and mostly do not exercise, but which remain available if they ever need them.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, end_users_of_derivative_software, beneficiary,
    powerless, biographical, mobile, global).

% The competing institutional logic — that source code disclosure should be a business choice, not a legal obligation attached to reuse — has no seat at the table when a maintainer chooses GPL terms for a project. Firms built around closed-source extension models discover the constraint only when they try to build on GPL code, at which point the terms are already fixed.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_software_industry_norms, excluded,
    institutional, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_commons_reading, the_commons_as_institution).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_commons_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: without a reciprocity requirement, any actor can take shared improvements, privatize a derivative, and never return value to the commons — the classic tragedy where individually rational extraction depletes a jointly produced resource. The GPL's copyleft clause makes continued participation conditional on continued contribution, which is what keeps the shared codebase growing rather than being drained.
% TRANSFER_FUNCTION: Moves the option value of proprietary exclusivity away from anyone who builds on GPL code and into the shared codebase as mandatory source disclosure — labor and modification value flow from individual users/modifiers of GPL software to the pool of all future users of that codebase, including competitors.
% ABSENT_VOICES: The proprietary software industry's preferred norm — that reuse and disclosure should be negotiated commercially rather than legally mandated — is not represented in the license terms themselves; a firm only encounters the constraint after choosing to build on GPL code, by which point the terms are fixed and non-negotiable except through dual-licensing deals the original maintainer may or may not offer.
% DISAPPEARANCE_RATIONALE: If the reciprocity obligation vanished overnight (imagine all GPL code silently relicensed permissively), the immediate practical effect on existing deployed software would be small, but the incentive structure for future contribution would shift sharply: firms could take improvements from the commons without returning them, contribution volume to the shared pool would likely fall as free-riding became costless, and the specific institutional promise that funds long-horizon collaborative projects (kernel development, compiler toolchains) would be broken for every future contributor who relied on it.
% FOUNDING_PROBLEM: Early free-software advocates observed that permissively licensed code was routinely taken into proprietary products with no improvements returned, and that some released code was later reappropriated into restrictive successor products that locked out the original contributors and users — the founding problem was capture and enclosure of jointly produced software commons by singular commercial actors.
% FOUNDING_PROBLEM_CORROBORATION: Long-horizon maintainers and FSF-aligned historians attest the enclosure problem remains live, citing ongoing proprietary-fork attempts and license-violation enforcement actions as evidence. Independent industry analysts and antitrust-adjacent commentary (outside both the FSF and the firms it targets) note that the software commons has diversified enough — permissive licenses, dual-licensing markets, cloud-service business models — that mandatory reciprocity is no longer the only viable anti-enclosure mechanism, making the founding problem's current necessity, as opposed to its historical validity, a live dispute rather than a settled fact.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_commons_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).
:- end_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42, rising mildly to 0.42 by interval end) reflects a genuine but moderate cost imposed on individual and corporate actors who must forgo proprietary capture of their own derivative work — this is neither negligible (a pure rope) nor severe (a snare), because the obligation trades a real constraint for a real, ongoing input of shared value. Suppression (0.55) reflects that the obligation is actively enforced through litigation and public compliance campaigns (e.g., SFC, SFLC enforcement actions) rather than resting on voluntary norm-following alone — it is a hard legal requirement backstopped by copyright law, not a soft convention. Theater ratio stays low (0.15, rising slightly) because enforcement actions substantially track genuine compliance concerns rather than performative signaling; the modest rise reflects growing bureaucratization of compliance-checking as the ecosystem scales.
 *
 * DIRECTIONALITY LOGIC:
 *   The commons-as-institution and long-horizon maintainers/contributors sit near the beneficiary end: they receive the accumulating shared codebase and administer the mechanism that sustains it, at low individual cost relative to what they gain collectively. Individual exit-maximizers and proprietary forkers sit near the target end: the reciprocity obligation directly forecloses their preferred extraction path (proprietary capture of derivative improvements), and their exit options are constrained — they can comply, avoid the GPL codebase, or exit the ecosystem, but cannot simply opt out of the obligation while still using the covered code. End users of derivative software are incidental beneficiaries: they gain source-availability rights they did not bargain for as a side effect of a dispute between maintainers and would-be enclosers.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mislabeling the GPL as pure extraction (a snare) precisely because it names a genuine, ongoing coordination function — the commons continues to grow and continues to depend on the reciprocity mechanism for its continued growth, which is why founding_problem_status is authored as contested rather than dead: the enclosure risk the GPL was built to prevent has neither vanished nor become fully hypothetical. Equally, it resists mislabeling the GPL as a pure rope (no extraction) because it names real victims — individual exit-maximizers and proprietary forkers genuinely bear a cost that the coordination story alone does not erase. The tangled_rope classification holds both facts without collapsing either into the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_vs_individual_freedom_framing,
    'Is the GPL''s reciprocity requirement better modeled as an institution protecting a collective good (the commons) from individual free-riding, or as a restriction on individual developers'' freedom to license their own labor as they choose?',
    'This is exactly the committer-structure question the kernel decomposition exists to resolve: it is not resolved within this story but by comparing this reading''s classification against the sibling readings (copyleft_as_freedom_reading, copyleft_as_restriction_reading) instantiated as separate constraints and linked via network.affects_constraints. Where the readings'' computed classifications diverge sharply, the divergence itself is the finding.',
    'Under the commons reading (this story), individual exit-maximizers are payers and the commons is a beneficiary, yielding tangled_rope. Under the restriction reading, there is no commons beneficiary and the classification shifts toward snare or a low-coordination tangled_rope. Under the freedom reading, the beneficiary shifts to end users and the victim set shifts to would-be proprietary redistributors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_vs_individual_freedom_framing, conceptual, 'Whether the GPL kernel is best modeled as commons-protection, freedom-protection, or business-restriction — the central committer ambiguity this kernel decomposition is designed to surface.').

omega_variable(
    commons_beneficiary_is_institution_not_actor,
    'Can an institution (the commons) meaningfully be a directionality beneficiary in the engine''s terms, or does treating it as a beneficiary smuggle in an unstated assumption that the commons has interests separable from its individual current contributors?',
    'Track whether the commons'' growth trajectory (lines of shared code, number of active maintained forks staying compliant) tracks contributor welfare over multi-decade horizons, or whether it primarily tracks the interests of the specific maintainer organizations (FSF, SFC) that administer enforcement.',
    'If commons growth diverges from aggregate contributor welfare, the ''commons as beneficiary'' framing may be a proxy for ''incumbent maintainer organizations as beneficiary,'' which would push this reading''s computed type closer to a captured tangled_rope or snare rather than genuine coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_beneficiary_is_institution_not_actor, conceptual, 'Whether treating an institution (not a stakeholder actor) as the beneficiary is a defensible modeling move or an unexamined proxy for maintainer-organization interests.').

omega_variable(
    enforcement_intensity_vs_ecosystem_maturity,
    'Is rising suppression (enforcement activity) over the measured interval evidence of a hardening, increasingly extractive institution, or evidence of a maturing ecosystem simply developing more formal compliance infrastructure (SFC, SPDX tooling) around a stable underlying obligation?',
    'Compare enforcement-action volume against violation-rate and voluntary-compliance-rate trends; if voluntary compliance is rising alongside enforcement infrastructure, the rise reflects maturation rather than intensifying coercion.',
    'If enforcement is intensifying disproportionately to actual violations, that supports a drift toward snare; if it tracks scale and voluntary compliance is stable or rising, it supports the coordination reading remaining stable over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_intensity_vs_ecosystem_maturity, empirical, 'Whether rising suppression_requirement reflects genuine enforcement hardening or normal scaling of compliance infrastructure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gpl__tr_t6, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 6, 0.09).
narrative_ontology:measurement(gpl__tr_t12, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(gpl__tr_t18, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 18, 0.12).
narrative_ontology:measurement(gpl__tr_t24, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement(gpl__tr_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gpl__be_t6, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(gpl__be_t12, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(gpl__be_t18, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 18, 0.38).
narrative_ontology:measurement(gpl__be_t24, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(gpl__be_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gpl__su_t6, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 6, 0.44).
narrative_ontology:measurement(gpl__su_t12, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(gpl__su_t18, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement(gpl__su_t24, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(gpl__su_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the gpl_reciprocity_obligation kernel, all instantiating the same license clause with different beneficiary/victim structures and different epsilon values: copyleft_as_commons_reading (this file, medium extraction, beneficiary=commons-institution, tangled_rope), copyleft_as_freedom_reading (beneficiary=end-user freedoms, victim=would-be proprietary redistributors), copyleft_as_restriction_reading (no coordination beneficiary claimed, victim=firms seeking proprietary integration, closer to snare/pure-restriction). Per the epsilon-invariance principle, these are three separate constraints, not one constraint measured three ways — each carries its own stable epsilon and is linked here for contamination-propagation and comparative analysis, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
