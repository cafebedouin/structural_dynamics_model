% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: GPL Reciprocity Obligation — Copyleft as User-Freedom Guarantee (Freedom Reading)
 *   domain: legal/technological
 *
 * SUMMARY:
 *   A family of software licenses conditions redistribution of covered code
 *   on reciprocity: anyone who distributes a derivative must offer its source
 *   under the same terms, forfeiting the option to close it. This file
 *   authors that arrangement as the freedom reading sees it — a guarantee of
 *   user sovereignty over code, enforced against proprietary capture. The
 *   guarantee is administered by steward institutions holding strategic
 *   copyrights, funds itself on volunteer contributions, and is maintained by
 *   active enforcement rather than universal enthusiasm. KEY AGENTS (by
 *   structural relationship): - downstream_users: primary beneficiary
 *   (moderate/mobile) — hold run-study-modify-share rights the terms
 *   guarantee - volunteer_contributors: beneficiary contributors
 *   (moderate/mobile) — fund the commons with improvements -
 *   free_software_stewards: agenda setter (institutional/identity_locked) —
 *   hold copyrights, enforce terms, define the doctrine -
 *   proprietary_integrators: primary target (powerful/constrained) — bear the
 *   forfeiture of proprietary derivative rights - cloud_service_operators:
 *   beneficiary free-riders (institutional/mobile) — monetize the commons
 *   through the distribution loophole - tivoized_device_owners: excluded
 *   voice (powerless/trapped) — hold granted-but-unexercisable freedoms -
 *   license_compliance_auditors: analytical observer (organized/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.38).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.68).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity Obligation — Copyleft as User-Freedom Guarantee (Freedom Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "legal/technological").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, '780d6982-fc94-40d0-b74a-a93e849c0ab4').
narrative_ontology:cs_kernel_codification('780d6982-fc94-40d0-b74a-a93e849c0ab4', fixed_text).
narrative_ontology:cs_authority_grounding('780d6982-fc94-40d0-b74a-a93e849c0ab4', lineage).
narrative_ontology:cs_interpretation_layer_present('780d6982-fc94-40d0-b74a-a93e849c0ab4').
narrative_ontology:cs_reading_relation('780d6982-fc94-40d0-b74a-a93e849c0ab4', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_reading_relation('780d6982-fc94-40d0-b74a-a93e849c0ab4', gpl_reciprocity_obligation__copyleft_as_commons_reading, influences).
narrative_ontology:cs_axiom('780d6982-fc94-40d0-b74a-a93e849c0ab4', foundational, user_sovereignty_over_code).
narrative_ontology:cs_axiom_status(user_sovereignty_over_code, holdable).
narrative_ontology:cs_axiom_grounding('780d6982-fc94-40d0-b74a-a93e849c0ab4', user_sovereignty_over_code, deontological).
narrative_ontology:cs_axiom('780d6982-fc94-40d0-b74a-a93e849c0ab4', foundational, proprietary_closure_of_shared_code_is_unfreedom).
narrative_ontology:cs_axiom_status(proprietary_closure_of_shared_code_is_unfreedom, holdable).
narrative_ontology:cs_axiom_grounding('780d6982-fc94-40d0-b74a-a93e849c0ab4', proprietary_closure_of_shared_code_is_unfreedom, deontological).
narrative_ontology:cs_reference_frame('780d6982-fc94-40d0-b74a-a93e849c0ab4', four_freedoms_user_sovereignty).
narrative_ontology:cs_drift_state('780d6982-fc94-40d0-b74a-a93e849c0ab4', contemporary_cloud_and_embedded_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('780d6982-fc94-40d0-b74a-a93e849c0ab4', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, volunteer_contributors).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, cloud_service_operators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, cloud_service_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run, study, modify, and redistribute software under terms that guarantee those rights to every recipient. They pay nothing for the grant and bear no compliance duties unless they redistribute; their practical stake is the continued health of the shared codebase the terms maintain. Exit is ordinary software substitution.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users, beneficiary,
    moderate, biographical, mobile, global).

% Contribute patches and modules to reciprocally licensed projects, receiving in exchange a guarantee that their work cannot be closed off by a downstream distributor. They carry modest source-disclosure duties when they distribute binaries themselves. Exit is contributing to permissively licensed projects instead.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, volunteer_contributors, beneficiary,
    moderate, biographical, mobile, global).

% Hold copyright on key components, publish and revise the license text, operate compliance enforcement through demand letters, audits, and litigation, and articulate the doctrine the terms serve. Their organizational purpose is inseparable from administering these terms; abandoning administration would dissolve the institution's reason to exist. They collect no fee from the license itself; funding comes from donations.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_stewards, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Build products incorporating reciprocally licensed code and must either publish derivative sources under the same terms or refrain from distribution. Product plans that assumed closed derivatives must be redesigned around disclosure; some firms maintain parallel permissive stacks to avoid the terms altogether. Leaving means forgoing the codebase, which for kernel-dependent products is rarely feasible.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Offer reciprocally licensed software as hosted services, modifying it internally without triggering the source-disclosure duties that attach only to distribution. They capture substantial revenue from the shared codebase while returning comparatively few patches; their exposure is reputational and, where network-use license variants apply, contractual.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, cloud_service_operators, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_freedom_reading, cloud_service_operators, payer).

% Own consumer hardware shipping reciprocally licensed code behind signature-checked bootloaders that refuse modified kernels. The license grants them modification rights their hardware refuses to honor; they had no seat in the license-drafting process, and their remedies are replacement hardware or litigation they cannot fund.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, tivoized_device_owners, excluded,
    powerless, biographical, trapped, global).

% Law firms and consultancies that audit supply chains for license obligations, advise integrators on source-disclosure scope, and mediate between stewards and alleged violators. They take no side in the underlying doctrine; their income depends on the terms remaining operative and legible.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, license_compliance_auditors, observer,
    organized, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared body of modifiable software by conditioning redistribution on reciprocity: everyone who receives and improves the code must offer those improvements onward on equal terms. This solves the free-rider problem that would otherwise let proprietary forks privatize collectively produced improvements.
% TRANSFER_FUNCTION: Moves exclusivity from downstream integrators to the commons: each distributor forfeits the right to close derivative works and passes that same forfeiture along, while transferring source access and modification rights to every recipient. The price of distribution is the surrender of proprietary control over the derivative.
% ABSENT_VOICES: Owners of consumer devices embedding licensed code behind signature-checked bootloaders hold license-granted freedoms they cannot exercise; they were not represented when the second license revision was drafted. Businesses committed to permissive-only supply chains object to the reciprocity terms, but their objection registers only as avoidance and procurement policy, never as a negotiated seat.
% DISAPPEARANCE_RATIONALE: If the reciprocity obligation vanished overnight, the largest shared codebases (the kernel and its surrounding toolchain) would be immediately exposed to proprietary forking; incumbents with engineering capacity would close their trees, and the ecosystem would fragment into private derivatives arranged roughly as enclosure dynamics arranged software before 1989.
% FOUNDING_PROBLEM: In the early 1980s, collaboratively developed software was repeatedly enclosed by vendors who accepted improvements and released none; the GNU project was founded to build an operating system that could never be made proprietary, and the license was drafted as the legal mechanism guaranteeing that outcome.
% FOUNDING_PROBLEM_CORROBORATION: Corporate compliance programs at dozens of large firms attest the terms bind real transactions; the historical enclosure record — Unix vendor fragmentation and the USL–BSDi litigation — corroborates the founding problem from outside the beneficiary set, as does academic IP scholarship on commons enclosure. Proprietary integrators attest only that the terms bind them, not that the founding problem remains live; no attestation of liveness comes from the paying seat.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.38: the freedom reading assesses the standing arrangement as protective, conceding a real but legitimate incidence on integrators — the forfeiture is the admitted price of the commons, not a taking. Suppression is high (0.68) because the reading openly endorses foreclosing proprietary derivatives; from this seat the foreclosure is a feature, and the enforcement machinery (audits, demand letters, litigation) that sustains it has grown steadily since the late 1990s. Theater is low (0.15): enforcement activity is overwhelmingly functional, with only a growing fringe of ceremonial compliance signaling. Accessibility collapse is moderate (0.55): once a firm builds on covered code, closing the derivative collapses entirely, but input-stage alternatives — permissive stacks, original development — persist and are actively used. Resistance is moderate (0.55): documented industry avoidance, contamination policies, and permissive-only procurement rules meet the terms continuously. Claim and metrics are independent authored facts: the tangled_rope claim follows from the structural data (genuine coordination function, declared victim, active enforcement), while the metric values describe observed operation; the engine computes per-seat types from the structural data and may diverge from the claim. All three temporal series share one grid (1989, 1995, 2001, 2007, 2015, 2021, 2026) so no metric is sampled against another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural facts. From the integrator seat the arrangement operates as enforced forfeiture with constrained exit; from the user and contributor seats it operates as a guarantee that costs them nothing; from the steward seat it is an identity — the organization has become its enforcement function, and exit is unthinkable without dissolving the institution itself (identity-lock of the institutional kind). The auditor seat sees only a legible obligation surface. The engine derives these divergent classifications from power, exit, and directional position; this file does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream users and volunteer contributors sit near the beneficiary end: the terms subsidize their rights at no compliance cost absent redistribution. Proprietary integrators sit near the full-target end: they bear the entire forfeiture, and their exit is constrained because kernel-dependent product lines cannot feasibly forgo the codebase. Cloud service operators derive near-beneficiary directionality despite thin reciprocity — the distribution trigger misses network use, so they capture commons value while escaping the transfer. Stewards sit near symmetric: they bear enforcement costs and collect no license rents; the doctrine their operation vindicates is recorded as a vindicated proposition, not a beneficiary. Device owners are deliberately kept OUT of the victim declaration: the freedom reading locates their injury in hardware lockdown the license (pre-revision) failed to prevent, not in extraction by the terms — that judgment is carried as an omega rather than asserted.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — vendor enclosure of collaborative software — remains live, so no mandate-atrophy question arises yet and no sunset applies. The classification discipline cuts both ways: declaring the coordination function keeps the payer seat's experience of the terms from collapsing the whole arrangement into pure extraction, while the declared victim and active-enforcement requirements keep the beneficiary seats' experience from laundering the incidence as costless coordination. The known residual risk is the device-owner gap: if granted-but-unexercisable freedoms prove widespread, the freedom reading's own account is incomplete, and that possibility is routed through an omega rather than resolved by assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (copyleft_as_freedom_reading) of the kernel gpl_reciprocity_obligation; which structural facts would change under the sibling readings?',
    'Compare the three reading files'' authored epsilon, beneficiary/victim sets, and computed types. The disagreement is located in the moral status of the integrator-side incidence, not in any observable fact about the license text.',
    'The restriction reading authors high epsilon with integrators as injured parties and would classify the same arrangement as pure extraction; the commons reading authors moderate epsilon centered on enclosure-prevention and could classify as coordination. Classification of the kernel is indexical to the reading; only the trio jointly characterizes it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one kernel, three readings; this file holds the freedom reading.').

omega_variable(
    tivoization_protection_gap,
    'Does the freedom reading understate the arrangement''s costs by omitting device owners whose hardware blocks exercise of license-granted modification rights?',
    'Census of shipped embedded devices distributing covered code with verified-boot lockdowns, and adoption rates of the lockdown-addressing license revision among embedded vendors after 2007.',
    'If widespread, the victim set is incomplete: adding device owners raises effective extraction at that seat and pushes the computed type toward pure extraction for hardware-bound users, exposing a blind spot in the freedom reading''s own account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tivoization_protection_gap, empirical, 'Granted-but-unexercisable freedoms as an uncounted cost.').

omega_variable(
    consent_at_entry_vs_ecosystem_lockin,
    'Is the measured suppression of proprietary integration consensual at the point of license acceptance, or coercive once a product line is embedded in dependent infrastructure?',
    'Trace firm-level decision histories: did integrators accept the terms knowingly at dependency-selection time, or did ecosystem entrenchment foreclose later exit?',
    'If suppression is mostly entry-consented, effective suppression is lower than the scalar suggests and the arrangement sits nearer voluntary coordination; if entrenchment forecloses exit, the integrator seat computes closer to full target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_at_entry_vs_ecosystem_lockin, conceptual, 'Timing of consent determines whether foreclosure is chosen or imposed.').

omega_variable(
    enforcement_steward_concentration,
    'Does the freedom guarantee depend on a small set of enforcement stewards whose decline would leave the terms theatrically maintained?',
    'Track enforcement action volume, steward funding, and successor-institution formation over the coming decade.',
    'Steward decay would shift the arrangement toward inertial maintenance — terms cited but unenforced — without any change to the license text itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_steward_concentration, empirical, 'Persistence depends on concentrated enforcement capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 1989, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1989, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 1989, 0.05).
narrative_ontology:measurement(gpl__tr_t1995, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 1995, 0.06).
narrative_ontology:measurement(gpl__tr_t2001, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2001, 0.08).
narrative_ontology:measurement(gpl__tr_t2007, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2007, 0.1).
narrative_ontology:measurement(gpl__tr_t2015, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(gpl__tr_t2021, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2021, 0.14).
narrative_ontology:measurement(gpl__tr_t2026, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2026, 0.15).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1989, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 1989, 0.15).
narrative_ontology:measurement(gpl__be_t1995, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 1995, 0.2).
narrative_ontology:measurement(gpl__be_t2001, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2001, 0.27).
narrative_ontology:measurement(gpl__be_t2007, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2007, 0.34).
narrative_ontology:measurement(gpl__be_t2015, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2015, 0.37).
narrative_ontology:measurement(gpl__be_t2021, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2021, 0.38).
narrative_ontology:measurement(gpl__be_t2026, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2026, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1989, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 1989, 0.3).
narrative_ontology:measurement(gpl__su_t1995, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(gpl__su_t2001, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2001, 0.48).
narrative_ontology:measurement(gpl__su_t2007, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2007, 0.58).
narrative_ontology:measurement(gpl__su_t2015, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2015, 0.62).
narrative_ontology:measurement(gpl__su_t2021, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2021, 0.66).
narrative_ontology:measurement(gpl__su_t2026, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2026, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'viral licensing' decomposes into three structurally distinct claims sharing one kernel (gpl_reciprocity_obligation): a freedom guarantee (this file), a business-model restriction (restriction reading), and a commons-preservation technology (commons reading). Their epsilon values diverge because each reading assesses the same standing arrangement by its own lights; the freedom reading's rights vocabulary supplies the normative grounding the commons reading's institutional design presupposes, hence the influences edge toward the commons sibling. Family members link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
