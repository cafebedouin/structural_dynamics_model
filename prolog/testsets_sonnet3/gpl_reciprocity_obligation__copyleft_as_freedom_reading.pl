% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_freedom_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_freedom_reading
 *   human_readable: GPL Reciprocity Obligation — Freedom-Preservation Reading
 *   domain: software_licensing_intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the freedom-preservation reading of the GPL
 *   reciprocity obligation: the clause requiring derivative works to be
 *   distributed under the same license is framed, from this seat, as the
 *   mechanism that keeps the four software freedoms (run, study, modify,
 *   redistribute) attached to code as it moves downstream, preventing any
 *   actor in the chain from enclosing shared work into a proprietary,
 *   unmodifiable form. This is one of three readings of the same kernel
 *   (gpl_reciprocity_obligation). The restriction reading treats the
 *   identical clause as a prohibition on proprietary integration and
 *   business-model constraint; the commons reading treats it as institutional
 *   infrastructure against enclosure at the ecosystem level. All three share
 *   the same license text and the same enforcement mechanism (copyright-based
 *   conditional distribution); they diverge in what they name as the referent
 *   good the mechanism protects — individual downstream freedom (this
 *   reading), collective commons integrity (commons reading), or unburdened
 *   commercial flexibility (restriction reading, where the obligation itself
 *   is the harm).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.28).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.62).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity Obligation — Freedom-Preservation Reading").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "software_licensing_intellectual_property").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, '4fc99627-3902-4b03-a14c-f969a1ba3216').
narrative_ontology:cs_kernel_codification('4fc99627-3902-4b03-a14c-f969a1ba3216', formalized).
narrative_ontology:cs_authority_grounding('4fc99627-3902-4b03-a14c-f969a1ba3216', extraction).
narrative_ontology:cs_interpretation_layer_present('4fc99627-3902-4b03-a14c-f969a1ba3216').
narrative_ontology:cs_reading_relation('4fc99627-3902-4b03-a14c-f969a1ba3216', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_reading_relation('4fc99627-3902-4b03-a14c-f969a1ba3216', gpl_reciprocity_obligation__copyleft_as_commons_reading, influences).
narrative_ontology:cs_axiom('4fc99627-3902-4b03-a14c-f969a1ba3216', foundational, software_freedom_is_transitive).
narrative_ontology:cs_axiom_status(software_freedom_is_transitive, holdable).
narrative_ontology:cs_axiom_grounding('4fc99627-3902-4b03-a14c-f969a1ba3216', software_freedom_is_transitive, deontological).
narrative_ontology:cs_axiom('4fc99627-3902-4b03-a14c-f969a1ba3216', foundational, restriction_on_capture_is_not_restriction_on_freedom).
narrative_ontology:cs_axiom_status(restriction_on_capture_is_not_restriction_on_freedom, holdable).
narrative_ontology:cs_axiom_grounding('4fc99627-3902-4b03-a14c-f969a1ba3216', restriction_on_capture_is_not_restriction_on_freedom, conventional).
narrative_ontology:cs_reference_frame('4fc99627-3902-4b03-a14c-f969a1ba3216', four_freedoms_founding_charter).
narrative_ontology:cs_drift_state('4fc99627-3902-4b03-a14c-f969a1ba3216', post_saas_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4fc99627-3902-4b03-a14c-f969a1ba3216', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, derivative_work_recipients).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, fsf_aligned_developer_community).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, closed_source_product_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives software built on GPL-licensed code and, because of the reciprocity clause, is guaranteed access to the corresponding source and the right to run, study, modify, and redistribute it. Without the clause, a downstream vendor could ship a modified binary and withhold the modifications, leaving this user with no path to inspect or alter software running on their own hardware. The clause is the mechanism that keeps their freedom attached to the code as it moves through the supply chain.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users, beneficiary,
    powerless, generational, constrained, global).

% Anyone who receives a derivative of GPL code automatically inherits the same four freedoms the original licensor extended. They benefit from a chain of custody they did nothing to negotiate — the license negotiates it for them, forward, indefinitely, as long as the work is distributed.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, derivative_work_recipients, beneficiary,
    powerless, generational, constrained, global).

% Drafts, maintains, and litigates the license text; frames the reciprocity clause as the load-bearing mechanism of software freedom rather than a restriction. Can choose to license new work under GPL or a permissive alternative, so their own exposure to the obligation is voluntary and revisable for future projects, even though it binds anyone who builds on existing GPL code.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, fsf_aligned_developer_community, agenda_setter,
    organized, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_freedom_reading, fsf_aligned_developer_community, beneficiary).

% Wants to combine GPL-licensed components into a closed-source product but cannot do so without triggering the obligation to release the combined work's source under the same terms. From this seat, the freedom-preservation story reads as a poison pill: the technically superior or cheaper option is available only at the cost of surrendering the proprietary model entirely. Exit means re-implementing the functionality from scratch or paying for a dual-licensed alternative.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Smaller vendors without the engineering headcount to avoid or re-implement GPL-encumbered components are effectively locked out of using large swaths of high-quality tooling and libraries unless they open their own source, which many cannot afford to do commercially. They bear the obligation's cost more acutely than large integrators who can negotiate dual licenses or fund clean-room reimplementation.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, closed_source_product_vendors, payer,
    moderate, biographical, constrained, national).

% Advocates for MIT/BSD-style licensing on the premise that developer freedom to relicense (including into proprietary products) is itself a freedom the GPL suppresses. From this reading's vantage they are not consulted on the framing — the freedom-preservation account treats their model as a defection risk (a vector for proprietary capture) rather than a legitimate alternative conception of freedom.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, permissive_license_ecosystem, excluded,
    organized, civilizational, mobile, global).

% Litigates and audits GPL compliance, documents violations, and produces the empirical record of enforcement actions that this reading cites as evidence the mechanism works as designed.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_freedom_conservancy, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_freedom_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that improvements made to a shared codebase remain available to everyone downstream, preventing any single actor in the distribution chain from privatizing collective work and cutting off the freedoms extended to them.
% TRANSFER_FUNCTION: Moves the right to inspect, modify, and redistribute source code forward through every derivative and distribution event; correspondingly moves the option to keep modifications proprietary away from any party who chooses to build on GPL code and distribute the result.
% ABSENT_VOICES: Proprietary integrators and the permissive-license ecosystem would object that 'freedom preservation' language obscures a restriction on their freedom to relicense; they are structurally present as payers/excluded but this reading's own framing does not treat their objection as a competing freedom claim, only as a capture risk to be prevented.
% DISAPPEARANCE_RATIONALE: If the reciprocity obligation vanished, distributors could fork GPL codebases, add proprietary extensions, and ship closed derivatives without releasing source — downstream users would lose the guarantee that improvements to software they depend on remain inspectable and modifiable. Large ecosystems (Linux-adjacent tooling, GCC) would likely fragment into proprietary forks within a few product cycles.
% FOUNDING_PROBLEM: In the 1980s, hardware vendors and software companies began withholding source code and prohibiting modification of software that users had previously been able to freely share and adapt (the printer driver dispute is the canonical origin case) — the founding problem was the enclosure of previously-shared code into proprietary, unmodifiable form.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and Software Freedom Conservancy (drafting/enforcing parties) attest the enclosure risk remains live, citing ongoing embedded-device and IoT lockdown practices. Independent industry economists and permissive-license advocates (outside the benefiting community) argue the original enclosure problem has been substantially addressed by market and regulatory changes (right-to-repair movements, disclosure norms) and that the obligation now functions primarily to determine which business models can use certain code, not to prevent enclosure per se — a genealogy dispute this reading resolves in favor of the founding account still being live.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_freedom_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).
:- end_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28) under this reading because the obligation's own beneficiaries (downstream users, derivative recipients) pay nothing and gain guaranteed rights; what is extracted is a foreclosed option (proprietary relicensing) from integrators who chose to build on GPL code, not a resource transfer from a captive population. Suppression is authored higher (0.62) because the clause is genuinely coercive toward one specific choice — an integrator cannot use the code and keep derivatives closed, and this is enforced through copyright litigation, not persuasion. Accessibility collapse is moderate (0.4): substitute permissively-licensed or proprietary alternatives to any given GPL component usually exist, so the collapse is partial, not total. Resistance is substantial (0.55) reflecting the decades of business-model friction and litigation this obligation generates precisely because it binds a powerful, well-resourced payer class.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream users and derivative-work recipients are declared beneficiaries: the clause transfers rights to them at zero cost, and its removal would strip protections they never had to negotiate for themselves — this pushes their directionality toward the subsidized end. Proprietary integrators and closed-source vendors are declared victims: the same clause forecloses their preferred use of the code, and their exit options are constrained (reimplement, negotiate dual license, or avoid the codebase) rather than free — this pushes their directionality toward the target end. The FSF-aligned developer community sits as agenda_setter with mobile exit on new work (they choose GPL voluntarily going forward) but their historical choice binds everyone downstream of code already released, which is the asymmetry this reading calls freedom-preserving and the sibling restriction reading calls coercive.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proprietary enclosure of previously-shared code) is contested as still-live: this reading holds it is live (citing IoT/embedded lockdown practices), while outside observers note the software ecosystem and legal environment have changed substantially since 1989. Because tangled_rope requires both a real coordination function AND active enforcement with victims, this reading does not resolve the mandatrophy question by re-classifying the constraint as a piton — it takes the coordination function to remain genuinely load-bearing (preventing enclosure) even as the specific 1980s printer-driver scenario has receded, which is precisely why this is authored as a live, contested founding-problem status rather than dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_referent_ambiguity,
    'When this reading says the obligation ''preserves freedom,'' whose freedom is the load-bearing referent — the downstream user''s freedom to inspect/modify, or the code''s freedom from proprietary capture as an abstract collective good? The commons reading names the latter; this reading names the former; conflating them changes which victim set counts as legitimate.',
    'Textual and historical analysis of FSF founding documents (the four freedoms definitions) against Conservancy enforcement case selection: do enforcement actions target situations where an identifiable downstream user was denied access, or situations where an ecosystem was at risk of fragmentation regardless of individual user harm?',
    'If enforcement consistently protects identifiable individual downstream users, this reading''s framing is well-grounded; if enforcement primarily protects ecosystem-level commons integrity even absent an identifiable harmed user, this reading and the commons reading may be less distinct than the kernel decomposition assumes, warranting a merge or a corroborating omega in the commons-reading file.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(freedom_referent_ambiguity, conceptual, 'Whether the freedom this reading protects is individual-downstream or ecosystem-collective.').

omega_variable(
    restriction_reading_incommensurability,
    'Is the proprietary integrator''s foreclosed freedom to relicense a genuine competing freedom claim (as the restriction reading holds) or a preference this reading is entitled to treat as illegitimate (mere business-model convenience, not freedom)?',
    'Compare against how this reading and the restriction reading each treat symmetric cases outside software licensing (e.g., is a landlord''s foreclosed freedom to evict without cause treated as a genuine freedom claim in tenant-rights frameworks this reading''s proponents also hold?) to test for principled versus ad hoc asymmetry in what counts as ''freedom.''',
    'If the asymmetry is principled (freedoms that only constrain others'' unilateral power over shared resources count; freedoms to unilaterally enclose do not), the freedom-preservation reading is internally consistent. If ad hoc, the reading is doing rhetorical work the restriction reading''s framing exposes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restriction_reading_incommensurability, conceptual, 'Whether excluding the integrator''s relicensing freedom from the freedom ledger is principled or question-begging.').

omega_variable(
    enclosure_risk_currency,
    'Is the 1989 enclosure scenario (proprietary printer drivers, unmodifiable vendor firmware) still the operative risk in 2024, or has the software distribution landscape (SaaS, cloud APIs, right-to-repair law) changed the mechanism of potential capture such that the GPL''s specific reciprocity clause addresses a largely historical problem?',
    'Empirical survey of GPL enforcement actions 2015-2024: what fraction target genuine source-withholding/enclosure attempts versus routine compliance paperwork violations by otherwise-compliant redistributors?',
    'A low fraction of genuine enclosure-prevention actions relative to paperwork enforcement would support reclassifying founding_problem_status toward dead/contested-toward-dead, strengthening a piton reading; sustained enclosure-prevention enforcement would support the live status this reading currently asserts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enclosure_risk_currency, empirical, 'Whether the founding enclosure problem remains the dominant driver of current enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1989, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 1989, 0.05).
narrative_ontology:measurement(gpl__tr_t1996, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 1996, 0.06).
narrative_ontology:measurement(gpl__tr_t2003, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2003, 0.08).
narrative_ontology:measurement(gpl__tr_t2010, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(gpl__tr_t2017, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2017, 0.1).
narrative_ontology:measurement(gpl__tr_t2024, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1989, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 1989, 0.15).
narrative_ontology:measurement(gpl__be_t1996, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 1996, 0.18).
narrative_ontology:measurement(gpl__be_t2003, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2003, 0.22).
narrative_ontology:measurement(gpl__be_t2010, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement(gpl__be_t2017, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2017, 0.27).
narrative_ontology:measurement(gpl__be_t2024, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1989, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 1989, 0.45).
narrative_ontology:measurement(gpl__su_t1996, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 1996, 0.5).
narrative_ontology:measurement(gpl__su_t2003, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2003, 0.55).
narrative_ontology:measurement(gpl__su_t2010, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(gpl__su_t2017, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2017, 0.6).
narrative_ontology:measurement(gpl__su_t2024, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, information_standard).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.05).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories share the gpl_reciprocity_obligation kernel and the identical license mechanism (conditional distribution under copyright law): copyleft_as_freedom_reading (this file, ε=0.28, tangled_rope, beneficiary=downstream users), copyleft_as_restriction_reading (expected higher ε, beneficiary/victim structure inverted in emphasis — proprietary integrators as primary aggrieved party), and copyleft_as_commons_reading (ecosystem-level coordination framing, beneficiary=the commons as an institution rather than individual downstream users). Per the ε-invariance principle these are NOT the same constraint measured three ways; each reading authors its own stable ε from its own premises about what the mechanism is for.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
