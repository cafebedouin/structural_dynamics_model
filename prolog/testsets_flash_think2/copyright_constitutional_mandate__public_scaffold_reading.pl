% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__public_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__public_scaffold_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: copyright_constitutional_mandate__public_scaffold_reading
 *   human_readable: Copyright as Public Domain Scaffold (Constitutional Mandate Reading)
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'public scaffold' reading of the
 *   US Constitutional Copyright Mandate. In this reading, copyright is
 *   understood as a temporary, limited monopoly granted to creators primarily
 *   to 'promote the Progress of Science and useful Arts' by ensuring works
 *   eventually enter a rich public domain. The monopoly is a means to a
 *   public-good end, not an end in itself. This reading emphasizes fair use,
 *   shorter terms, and anti-enclosure norms.
 *
 * KEY AGENTS:
 *   - the_public_domain: Primary beneficiary (conceptual agent)
 *   - creators_of_derivative_works: Direct beneficiaries (mobile/moderate)
 *   - educators_researchers: Direct beneficiaries (mobile/moderate)
 *   - copyright_holders: Payer (powerful/constrained) — bear the cost of limited terms
 *   - us_congress: Agenda setter (institutional/generational)
 *   - us_supreme_court: Agenda setter (institutional/civilizational)
 *   - corporate_lobbyists: Excluded (powerful/mobile) — advocate for opposing readings
 *   - public_interest_advocates: Observer (organized/generational)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__public_scaffold_reading, 0.25).
domain_priors:suppression_score(copyright_constitutional_mandate__public_scaffold_reading, 0.15).
domain_priors:theater_ratio(copyright_constitutional_mandate__public_scaffold_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__public_scaffold_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__public_scaffold_reading, "Copyright as Public Domain Scaffold (Constitutional Mandate Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__public_scaffold_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:has_sunset_clause(copyright_constitutional_mandate__public_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__public_scaffold_reading, 'efd3545e-f615-47a4-8aad-227a4fee891b').
narrative_ontology:cs_kernel_codification('efd3545e-f615-47a4-8aad-227a4fee891b', fixed_text).
narrative_ontology:cs_authority_grounding('efd3545e-f615-47a4-8aad-227a4fee891b', lineage).
narrative_ontology:cs_interpretation_layer_present('efd3545e-f615-47a4-8aad-227a4fee891b').
narrative_ontology:cs_reading_relation('efd3545e-f615-47a4-8aad-227a4fee891b', copyright_constitutional_mandate__corporate_enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('efd3545e-f615-47a4-8aad-227a4fee891b', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('efd3545e-f615-47a4-8aad-227a4fee891b', foundational, copyright_is_public_good_instrument).
narrative_ontology:cs_axiom_status(copyright_is_public_good_instrument, holdable).
narrative_ontology:cs_axiom_grounding('efd3545e-f615-47a4-8aad-227a4fee891b', copyright_is_public_good_instrument, instrumental).
narrative_ontology:cs_axiom('efd3545e-f615-47a4-8aad-227a4fee891b', foundational, monopoly_is_temporary_incentive).
narrative_ontology:cs_axiom_status(monopoly_is_temporary_incentive, holdable).
narrative_ontology:cs_axiom_grounding('efd3545e-f615-47a4-8aad-227a4fee891b', monopoly_is_temporary_incentive, instrumental).
narrative_ontology:cs_reference_frame('efd3545e-f615-47a4-8aad-227a4fee891b', public_good_balancing).
narrative_ontology:cs_drift_state('efd3545e-f615-47a4-8aad-227a4fee891b', contemporary_copyright_extensions, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('efd3545e-f615-47a4-8aad-227a4fee891b', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, the_public_domain).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, creators_of_derivative_works).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, educators_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__public_scaffold_reading, copyright_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate beneficiary of copyright, receiving works after their limited term for free use and adaptation. Its 'power' is conceptual, relying on legal frameworks to expand.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, the_public_domain, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__public_scaffold_reading, the_public_domain).

% Benefit from a rich public domain as a source of inspiration and material for new creations, fostering innovation and cultural production.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, creators_of_derivative_works, beneficiary,
    moderate, biographical, mobile, global).

% Benefit from free access to a vast body of knowledge and creative works for teaching, scholarship, and dissemination, without licensing burdens.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, educators_researchers, beneficiary,
    moderate, biographical, mobile, global).

% While benefiting from the initial monopoly, they 'pay' by accepting the temporary nature of their rights and the eventual entry of their works into the public domain, which they would prefer to avoid.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, copyright_holders, payer,
    powerful, biographical, constrained, global).

% Has the constitutional power to set copyright terms and conditions, but under this reading, is constrained by the public good mandate and the 'limited times' clause.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, us_congress, agenda_setter,
    institutional, generational, constrained, national).

% Interprets the constitutional limits of copyright, and under this reading, would actively scrutinize legislative extensions to ensure they serve the public domain.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, us_supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Represent the interests of large copyright holders, advocating for maximal and extended terms. Their arguments for perpetual property rights are structurally excluded from this reading's core premise.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, corporate_lobbyists, excluded,
    powerful, biographical, mobile, national).

% Actively promote and defend this reading of copyright, pushing for policies that prioritize the public domain and limit corporate enclosure.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, public_interest_advocates, observer,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__public_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__public_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To incentivize the creation and dissemination of new works by granting creators a temporary, limited monopoly, ensuring these works eventually enrich the public domain for future generations.
% TRANSFER_FUNCTION: Transfers temporary exclusive control over creative works from the public (who would otherwise have immediate, free access) to creators, in exchange for the eventual, guaranteed entry of these works into the public domain.
% ABSENT_VOICES: Corporate copyright holders and their lobbyists, who advocate for maximal, perpetual property rights, would object to the emphasis on public domain and temporary nature. Their arguments for property-first are structurally incompatible with this reading's public-good premise.
% DISAPPEARANCE_RATIONALE: If the constitutional mandate for copyright as a public scaffold vanished, the legal framework for intellectual property would collapse. This would likely lead to either a free-for-all (no incentive for creation) or a purely private, perpetual enclosure system (no public domain), fundamentally altering cultural production and access.
% FOUNDING_PROBLEM: To promote the Progress of Science and useful Arts by securing for limited Times to Authors and Inventors the exclusive Right to their respective Writings and Discoveries, balancing creator incentive with public access.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars specializing in intellectual property history, public interest legal organizations, and historical legislative records (e.g., Madison's writings, early copyright acts) corroborate the original intent of balancing public good with private incentive. This corroboration comes from sources outside the direct beneficiaries of extended copyright terms.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__public_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__public_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__public_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(copyright_constitutional_mandate__public_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__public_scaffold_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).
:- end_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Scaffold because its core function is transitional: to provide temporary support (monopoly) for a specific public purpose (progress and public domain enrichment), with an inherent sunset ('limited times'). Extractiveness is low (0.25) because the temporary monopoly is seen as a necessary, minimal cost for a greater public benefit. Suppression is low (0.15) as enforcement is primarily to uphold the temporary nature and ensure public access, not to suppress alternatives to the system itself. Theater ratio is low (0.10) as the public good function is genuinely pursued in this reading. The slight increase in extractiveness over the interval reflects the historical stretching of 'limited times' even within this reading's ideal, acknowledging a subtle drift from the purest form of the scaffold.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the public domain and derivative creators, this reading represents a beneficial coordination mechanism. From the perspective of copyright holders, it imposes a 'cost' by limiting their control, even though they benefit from the overall incentive structure. The agenda setters (Congress, Supreme Court) are seen as stewards of this balance, rather than enforcers of pure property rights.
 *
 * DIRECTIONALITY LOGIC:
 *   The public domain, derivative creators, and educators are clear beneficiaries (low d) as the constraint's purpose is to enrich them. Copyright holders are positioned as payers (higher d) in this reading because their desire for maximal, perpetual control is curtailed by the 'limited times' and public good mandate. Congress and the Supreme Court are agenda setters, responsible for upholding the constitutional balance. Corporate lobbyists are excluded, as their 'corporate enclosure' reading is fundamentally opposed to this scaffold interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading inherently guards against mandatrophy by emphasizing the 'limited times' and public good purpose. It prevents mislabeling the temporary monopoly as a permanent property right, which would transform the scaffold into a snare. The contest over the 'founding_problem_status' (contested) highlights the ongoing struggle to prevent the mandate from atrophying into a mere justification for private enclosure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_copyright_mandate,
    'Is this constraint a genuine ''public scaffold'' reading of the US Constitutional Copyright Mandate, or is it better understood through a ''corporate enclosure'' or ''judicial ambiguity'' lens?',
    'Analysis of legislative intent, judicial precedent, and economic outcomes through the lens of public domain growth vs. private asset accumulation. A shift in judicial interpretation or legislative action towards shorter terms and expanded fair use would corroborate this reading.',
    'If the ''corporate enclosure'' reading were adopted, the constraint would reclassify as a Snare or Tangled Rope with significantly higher extraction and suppression. If ''judicial ambiguity'' were adopted, it might reclassify as a Piton or Tangled Rope, reflecting a less active, more deferential stance to legislative drift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_copyright_mandate, conceptual, 'Ambiguity in the core constitutional mandate for copyright.').

omega_variable(
    limited_times_interpretation,
    'What constitutes ''limited Times'' in the context of copyright, and how does current practice align with the original intent of this constitutional phrase?',
    'Historical analysis of early copyright terms, economic studies on incentive vs. term length, and comparative legal analysis of international copyright regimes. A judicial ruling or legislative reform that significantly shortens copyright terms would resolve this.',
    'If ''limited Times'' is interpreted strictly (e.g., 14-28 years), the extractiveness of the constraint would decrease further, and its scaffold nature would be reinforced. If interpreted maximally (e.g., life of author + 70 years), the constraint''s extractiveness would increase, pushing it towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(limited_times_interpretation, empirical, 'Ambiguity in the interpretation of ''limited Times'' for copyright duration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__public_scaffold_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(copy_tr_t20, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(copy_tr_t40, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(copy_tr_t60, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 60, 0.09).
narrative_ontology:measurement(copy_tr_t80, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(copy_tr_t100, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(copy_be_t20, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement(copy_be_t40, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 40, 0.19).
narrative_ontology:measurement(copy_be_t60, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 60, 0.21).
narrative_ontology:measurement(copy_be_t80, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 80, 0.23).
narrative_ontology:measurement(copy_be_t100, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 100, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t0, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(copy_su_t20, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 20, 0.11).
narrative_ontology:measurement(copy_su_t40, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement(copy_su_t60, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 60, 0.13).
narrative_ontology:measurement(copy_su_t80, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 80, 0.14).
narrative_ontology:measurement(copy_su_t100, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 100, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
