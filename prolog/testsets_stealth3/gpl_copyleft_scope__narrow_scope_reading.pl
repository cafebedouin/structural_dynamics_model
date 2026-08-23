% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__narrow_scope_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__narrow_scope_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__narrow_scope_reading
 *   human_readable: GPL Section 2(b) Covered-Set Boundary (Narrow Doctrine-Following Construal)
 *   domain: legal/economic - software licensing, intellectual property, open-source governance
 *
 * SUMMARY:
 *   This story instantiates one reading of the GPL copyleft-scope kernel; the
 *   contest among readings is carried in commentary.kernel_context and the
 *   omega variables and is not part of this constraint. Under the
 *   doctrine-following construal modeled here, GPL Section 2(b)'s sharing
 *   obligation attaches only to works that traditional copyright law itself
 *   classifies as derivative works. Mere aggregation of separate programs,
 *   plugin architectures communicating through documented interfaces, and
 *   certain forms of dynamic linking fall outside the obligation; the covered
 *   set is whatever copyright doctrine's derivation criteria yield, nothing
 *   broader. The arrangement's referent for epsilon is the standing
 *   arrangement under contest - Section 2(b) as operated under this construal
 *   - assessed by this reading's own lights: the obligation is confined,
 *   consented at adoption, priced transparently, and reciprocated within its
 *   declared boundary, so extraction is moderate-low and stable. This reading
 *   endorses the construal itself as the correct operation of the standing
 *   license, so there is no reform alternative whose merits could contaminate
 *   the referent. claimed_type (rope) is this reading's structural
 *   assessment; the metrics are authored independently as descriptive
 *   measurements of actual operation; any divergence the engine computes
 *   between claim and computed type is signal, not error to be reconciled.
 *   KEY AGENTS (by structural relationship): - fsf_license_steward:
 *   agenda-setting steward (organized/identity_locked) - maintains license
 *   text, FAQ, and enforcement posture on a generational horizon -
 *   gpl_enforcement_organizations: ex-post compliance actors
 *   (organized/identity_locked) - pursue claims that stall at the boundary
 *   cases - commercial_software_vendors: principal beneficiaries
 *   (powerful/mobile) - embed GPL components behind proprietary layers,
 *   disclosing only covered derivatives - proprietary_device_driver_vendors:
 *   beneficiaries at the contested edge (powerful/mobile) - ship loadable
 *   modules treated as separable - gpl_project_contributors: supply-side
 *   beneficiaries (moderate/constrained) - receive disclosed source of
 *   covered derivatives back - downstream_enterprise_deployers: beneficiaries
 *   of boundary stability (institutional/mobile) - dual_license_vendors:
 *   boundary arbitrageurs (powerful/arbitrage) - sell the alternative to the
 *   obligation - inactive_original_copyright_holders: excluded
 *   (powerless/trapped) - consent-gating, unorganized, absent -
 *   ip_litigation_courts: analytical observers (institutional/analytical) -
 *   dormant case-by-case adjudicators of the boundary - academic_ip_scholars:
 *   analytical observers (analytical/analytical) - supply the doctrinal
 *   vocabulary both camps cite
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.38).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.33).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.33).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Section 2(b) Covered-Set Boundary (Narrow Doctrine-Following Construal)").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "legal/economic - software licensing, intellectual property, open-source governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__narrow_scope_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, '663d7ef9-6559-4135-be8c-0235fe0b6827').
narrative_ontology:cs_kernel_codification('663d7ef9-6559-4135-be8c-0235fe0b6827', fixed_text).
narrative_ontology:cs_authority_grounding('663d7ef9-6559-4135-be8c-0235fe0b6827', lineage).
narrative_ontology:cs_interpretation_layer_present('663d7ef9-6559-4135-be8c-0235fe0b6827').
narrative_ontology:cs_reading_relation('663d7ef9-6559-4135-be8c-0235fe0b6827', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('663d7ef9-6559-4135-be8c-0235fe0b6827', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('663d7ef9-6559-4135-be8c-0235fe0b6827', foundational, covered_set_follows_traditional_copyright_derivatives).
narrative_ontology:cs_axiom_status(covered_set_follows_traditional_copyright_derivatives, holdable).
narrative_ontology:cs_axiom_grounding('663d7ef9-6559-4135-be8c-0235fe0b6827', covered_set_follows_traditional_copyright_derivatives, conventional).
narrative_ontology:cs_axiom('663d7ef9-6559-4135-be8c-0235fe0b6827', secondary, non_derivative_integration_carries_no_sharing_obligation).
narrative_ontology:cs_axiom_status(non_derivative_integration_carries_no_sharing_obligation, holdable).
narrative_ontology:cs_axiom_grounding('663d7ef9-6559-4135-be8c-0235fe0b6827', non_derivative_integration_carries_no_sharing_obligation, conventional).
narrative_ontology:cs_reference_frame('663d7ef9-6559-4135-be8c-0235fe0b6827', traditional_copyright_derivation_doctrine).
narrative_ontology:cs_drift_state('663d7ef9-6559-4135-be8c-0235fe0b6827', contemporary_linking_litigation_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('663d7ef9-6559-4135-be8c-0235fe0b6827', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_software_vendors).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, proprietary_device_driver_vendors).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, downstream_enterprise_deployers).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, gpl_project_contributors).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, dual_license_vendors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, commercial_software_vendors).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__narrow_scope_reading, traditional_copyright_derivation_criteria).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__narrow_scope_reading, documented_api_decoupling_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publishes and maintains the GPL license text and its official interpretive FAQ; answers integrators' boundary questions; initiates or funds compliance litigation when derivative works of GNU code are shipped without source. Its staffing, funding, and reputation are constituted by stewardship of this license family, so departing the role would dissolve the organization's purpose. It holds normative authority disproportionate to its budget.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, fsf_license_steward, agenda_setter,
    organized, generational, identity_locked, global).

% Bring infringement claims and compliance audits against distributors of GPL-covered works, concentrating in recent years on embedded devices and on couplings whose covered status is disputed. Their claims succeed against outright violations and stall at the disputed boundary cases; they run on donations and volunteer counsel. Their continuing relevance depends on visible violations remaining frequent enough to fund the work.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, gpl_enforcement_organizations, agenda_setter,
    organized, generational, identity_locked, global).

% Build operating systems, compiler toolchains, databases, and cloud services that embed GPL-licensed components beneath proprietary management layers, and distribute appliances containing GPL firmware. They disclose source for the combinations their counsel classifies as covered derivative works and keep everything else closed. Switching to permissively licensed or in-house substitutes is possible at significant engineering cost, and some firms do so to simplify compliance.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, commercial_software_vendors, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__narrow_scope_reading, commercial_software_vendors, payer).

% Ship graphics, networking, and storage drivers as loadable modules for GPL kernels without publishing driver source, relying on the separateness of loadable modules under the doctrine-based boundary. They invest in kernel interface stability arrangements and occasionally face infringement allegations over module status. Hardware revenue does not turn on the outcome, but software margins do.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, proprietary_device_driver_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Write and contribute code to GPL projects under the license's terms, expecting improvements to covered derivative works to come back under the same license. They receive the disclosed sources of covered derivatives and build on them; they have no individual legal capacity to police the boundary and depend on stewards, enforcement organizations, and courts. Moving past contributions to different terms requires locating and persuading every copyright holder, which is rarely feasible.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, gpl_project_contributors, beneficiary,
    moderate, biographical, constrained, global).

% Procure servers, phones, and cloud stacks that mix GPL infrastructure with licensed proprietary tooling; their annual compliance sign-off depends on the boundary staying stable enough to certify. They hold purchasing leverage over vendors and foundations alike but do not participate in boundary interpretation. Exit means re-platforming, at multi-year cost.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, downstream_enterprise_deployers, beneficiary,
    institutional, biographical, mobile, global).

% Offer identical code under the GPL and under paid commercial terms, selling exemption from the sharing obligation to customers whose use falls on the covered side of the boundary. Their pricing assumes the boundary is predictable: an expanded boundary pushes customers toward permissive competitors, and a collapsed one removes the reason to buy at all. They earn from both sides of the line and press quietly for its stability.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, dual_license_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Assigned or contributed code to GPL projects years ago and scattered; their consent is a legal prerequisite for changing the terms of any work they touched, yet they are frequently unreachable, retired, or indifferent. They hold veto power they rarely learn they possess. Their presumed views on scope are invoked rhetorically by every faction, but they deliberate nowhere and enforce nothing.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, inactive_original_copyright_holders, excluded,
    powerless, generational, trapped, global).

% Hear the occasional infringement claim touching the boundary and resolve it under general copyright doctrine without issuing the kind of broad precedent that would fix the covered set; GPL questions arrive embedded in larger commercial disputes. Their few, case-specific pronouncements become the citations every faction quarrels over.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, ip_litigation_courts, observer,
    institutional, generational, analytical, national).

% Publish the doctrinal analyses - joint works, derivations, abstraction-filtration-comparison - that corporate counsel and movement lawyers both cite, and host the forums where boundary questions are argued. They hold no enforcement capacity and take funding from neither side. Their framing choices quietly shape which construals look respectable.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, academic_ip_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__narrow_scope_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__narrow_scope_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single widely recognized criterion for which combinations of GPL-licensed and independently developed code trigger the source-sharing obligation, so that thousands of routine integration decisions do not each require bespoke legal negotiation and developers are not forced to avoid GPL components altogether.
% TRANSFER_FUNCTION: Moves source-code disclosure from makers of covered derivative works to the public commons, and moves nothing from developers whose couplings fall outside the derivative boundary - mere aggregation, plugin architectures, and certain dynamic linking forms retain their proprietary status. Implicitly, GPL infrastructure becomes usable at zero license fee by commercial integrators who comply on the covered portion.
% ABSENT_VOICES: Inactive copyright holders of long-contributed code would object to boundary decisions made without them but are dispersed, unreachable, and hold unorganized veto power; individual contributors without litigation budgets have no seat in FAQ drafting or courtroom argument; end users of combined proprietary/GPL products have no vehicle for their preferences about how the combination is treated. All sit in the excluded position: affected, unorganized, outside the interpretive conversation.
% DISAPPEARANCE_RATIONALE: If the boundary criterion vanished overnight, every integration involving GPL code would revert to first-principles legal judgment; risk-averse commercial adopters would substitute permissive or proprietary components, hybrid products would fragment, and the mixed-economy division of labor the boundary enables would dissolve into either blanket avoidance of GPL code or blanket enclosure of derived improvements.
% FOUNDING_PROBLEM: Early-1990s free-software advocacy faced a dilemma: a license strict enough to keep all derived works free made GPL code unusable alongside independent proprietary programs and chilled adoption, while a lax license let companies privatize improved code wholesale. Section 2(b), together with the aggregation clarification maintained in the license's FAQ tradition, was built to thread that needle - attaching the sharing obligation to genuine derivations only, so legitimate combination remained possible while privatization of derived improvements stayed blocked.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: court adjudications of GPL-related infringement claims have repeatedly applied general copyright derivative-work criteria to the boundary question; academic copyright scholarship on joint works and derivations supplies the doctrinal foundation this reading invokes; practitioner licensing literature written for commercial clients documents the adoption problem the aggregation and plugin carve-outs resolve. No single attester speaks for the arrangement; the corroboration is the convergence of courts, scholarship, and practitioner records, each outside the license-steward seat.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__narrow_scope_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__narrow_scope_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_copyleft_scope__narrow_scope_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__narrow_scope_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__narrow_scope_reading_tests).
:- end_tests(gpl_copyleft_scope__narrow_scope_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.38: under this construal the sharing obligation reaches only doctrine-true derivatives, a subset of integrations that shrinks as interface-based and dynamically linked patterns proliferate. The obligation is consented (adopters accept Section 2(b) knowingly), priced transparently, and reciprocated (disclosed derivative source returns to the same commons the adopter drew from), so extraction is moderate-low and nearly flat across the interval - the early dip reflects the aggregation FAQ clarification reducing accidental chilling, the late stabilization reflects contested-zone litigation raising scrutiny marginally. Suppression 0.33: the arrangement rests on ordinary copyright enforcement plus episodic litigation rather than exclusionary machinery; alternatives (permissive licenses, LGPL, purchased dual-licensing, in-house substitution) remain fully accessible, so no suppression-of-exits structure operates. Theater 0.28: compliance checking and FAQ adjudication are functionally load-bearing, but a growing minority of enforcement-adjacent activity consists of boundary assertions aimed at couplings this construal places outside the covered set - performative relative to this reading, however sincere their authors. Accessibility collapse 0.30 and resistance 0.50: alternatives do not collapse (the license competes in an open market of licensing terms) and the construal meets sustained interpretive resistance from actors who hold the covered set to be larger - doctrinal contestation and episodic litigation rather than refusal by covered parties, who comply at high rates. All three temporal series run on one shared seven-point grid (1991-2025), every tracked metric authored at every point. The suppression_requirement series traces a single enforcement wave: capacity built through the 2000s around embedded-device violation clusters, peaked circa 2009, then decayed as boundary-targeting suits failed to produce broad-coverage precedent. This is a one-cycle arc driven by external violation clusters and litigation outcomes, not intermittent reinforcement, so a seven-point grid suffices rather than the eight-to-ten-point oscillation battery; base_properties values are authored at the interval's end state, post-decay. Suppression is authored as a raw structural property and is not scaled by power or scope - only extractiveness enters the engine's directionality and scope scaling. Boltzmann coordination_type is information_standard: the constraint's primary service is definitional, a classification protocol telling integrators which combinations oblige sharing; enforcement machinery backs the protocol but is not the service itself, and the low type floor (0.02) poses no benignity pass here since measured extraction sits well above it. Receipt surface: gain_flow is authored 'diffuse' after checking every named seat - the arrangement's receipts (disclosed source of covered derivatives) accrue to the developer commons at large, and no named seat captures them; commercial vendors receive avoided obligation (retention of proprietary layers), which is relief from burden, not receipt of the extracted flow. fixing_cost is authored 'prohibitive': attempts to redefine the determinative boundary have repeatedly proven ecosystem-splitting (license-version fragmentation, relicensing blocked by dispersed copyright holders, boundary litigation without resulting precedent), so imposing any different rule costs more than ending the ambiguity benefits any seat positioned to try. Note for consumers: 'diffuse' plus 'prohibitive' is nominally the piton cell, but the structure refutes a piton - maintenance is active, distributed, and eager across beneficiary seats, theatricality is low, and the cost asymmetry is a transition cost imposed by third-party consent requirements, not administrator inertia. The divergence is left as authored data, not reconciled to the rope claim.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. The steward seat (identity_locked, generational) experiences the license as its life's work and evaluates every boundary question against the mission of keeping derived works free; its published guidance and enforcement posture form inside that frame, and exit from the role is implausible because its identity is constituted by the role. The vendor seats (powerful, mobile, biographical) experience the same license text as a set of engineering-relevant parameters with known treatments - aggregate separately, communicate through interfaces, load modules - and manage legal exposure as routine compliance. Contributors (moderate, constrained) experience the boundary as the perimeter of the reciprocity they were promised: protective inside it, and dependent on stewards and courts for anything at its edge. The courts experience dormancy: adjudication arrives case-by-case under general doctrine without a settled precedent, so the same integration can be lawful in one counsel's reading and actionable in another's. Same-power divergence: the steward and a major vendor each hold substantial power in their own currency (normative authority versus deployment gravity) yet experience opposite arrangements because their exit options and time horizons differ - identity_locked/generational versus mobile/biographical - not because nominal power differs. Identity-lock dynamics: the steward's lock is institutional-ideological fusion (organization and mission have merged); if the frame broke - say, a definitive appellate ruling fixing the covered set - the steward's evaluation of every boundary case would snap to the ruling rather than to mission, and the enforcement posture would reorganize around compliance monitoring. The enforcement organizations' lock is similar but thinner, funded as it is by the continued visibility of violations.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared beneficiary derives directionality toward the subsidized end. commercial_software_vendors and proprietary_device_driver_vendors surrender source only on the combinations their counsel classifies as covered - a cost priced at license adoption - and keep everything the construal places outside the covered set; their net position is strongly favorable, which the secondary payer role on commercial_software_vendors records without overturning the beneficiary-side derivation. gpl_project_contributors surrender code voluntarily and receive disclosed derivative source back under the same terms; relative to a no-license counterfactual in which their code could be enclosed wholesale, the arrangement subsidizes them. downstream_enterprise_deployers and dual_license_vendors draw pure coordination value (certifiable compliance; monetizable boundary). No victims are declared, deliberately and defensibly: under this construal's own lights, no party bears a cost it did not consent to, at a posted price, with reciprocation inside the declared boundary. Actors who hold that the covered set should be larger (the steward, the enforcement organizations) are not extracted-from by this construal - their grievance concerns which construal should govern, a cross-reading question routed to the omega variables rather than manufactured into victimhood inside this story. Agenda-setter seats derive near-symmetric stewardship directionality; the analytical seats stand outside the chi arithmetic. The absence of any high-directionality seat is the structural signature the rope claim rests on: extraction exists (covered derivative-makers disclose source) but is consented and reciprocated rather than coerced from a trapped population.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope classification guards against two mislabels. Read naively as 'a license that restricts,' the arrangement invites a pure-extraction scoring; the snare reading fails structurally because it cannot name victims coerced past real exits - exits are genuine (permissive substitutes, purchased exemptions, clean-room reimplementation), the burden is consented and posted, and receipts flow back to the paying side's commons. Read cynically as 'a fading formality,' the arrangement invites a degraded-inertia scoring; that fails too because theater is low, maintenance is active and distributed, and the founding problem is live - every new coupling technology reopens the boundary question, so the mandate has not outlived its function and no mandatrophy_resolved flag is set. The scaffold reading also fails honestly: the boundary rule is steady-state infrastructure rather than transitional support and carries no sunset clause. The R5 genealogy interview corroborates liveness from outside the benefiting parties (courts, scholarship, practitioner records), so the mismatch consumer finds founding_problem_status=live paired with disappearance_verdict=world_rearranges - aligned, no zombie flag. The analysis chiefly protects the opposite confusion here: preventing genuine coordination (the boundary convention that makes mixed development legible to thousands of daily decisions) from being mislabeled as extraction because the surrounding license is restrictive in character.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which construal of GPL Section 2(b)''s covered set is authoritative: doctrine-following derivation criteria (this reading), technical-coupling criteria reaching all linked works (strong sibling), or enforcement-capacity-dependent realization (vacuum sibling)?',
    'A definitive appellate ruling on whether dynamic linking produces a derivative work, or a successor license version that states the covered set explicitly.',
    'If the strong sibling''s rule prevails, this story''s beneficiaries lose their retention rights, epsilon rises substantially, and the computed profile shifts toward tangled_rope or snare; if the vacuum sibling''s account prevails, realized scope becomes context-dependent and this story''s clean epsilon holds only in doctrine-respecting jurisdictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel-level contest over the determinative boundary rule; this file authors only the narrow reading.').

omega_variable(
    dynamic_linking_derivative_status,
    'Does dynamic linking produce a derivative work under applicable copyright doctrine, and if so, which linking forms (address-space integration, symbol interdependence, header exposure) tip the balance?',
    'Appellate precedent or systematic analysis of Copyright Office registration practice applied to linking cases.',
    'Resolves the widest epsilon band inside this reading: if common linking forms count as derivations, the covered set expands and effective extraction on vendors rises sharply; if none do, the covered set contracts further and the rope profile strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dynamic_linking_derivative_status, empirical, 'The empirical core of the boundary question: linking versus derivation.').

omega_variable(
    contributor_expectation_shortfall,
    'Did contributors who understood the sharing obligation to extend beyond doctrine-true derivations suffer uncompensated losses when the narrower construal became operative in practice?',
    'Contributor surveys on boundary understanding at contribution time, plus relicensing-project records quantifying how often consent-gathering failed because scope expectations differed.',
    'If the shortfall is substantial and uncompensated, a victim class exists that this story''s structural declarations omit, shifting the computed profile toward tangled_rope and revising the directionality map.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contributor_expectation_shortfall, empirical, 'Cross-reading delta routed here per Rule 1: expectation losses attributable to reading selection rather than to this reading''s operation.').

omega_variable(
    enforcement_capacity_dependence,
    'Does realized scope vary systematically with which interpretive community holds enforcement capacity in a given ecosystem (steward-aligned projects versus industry-dominated foundations)?',
    'Comparative audit of compliance outcomes across ecosystems matched for integration pattern but differing in enforcement posture.',
    'High variance means this story''s epsilon describes doctrine-respecting contexts specifically and the vacuum sibling''s account governs elsewhere; low variance confirms a single operable boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_dependence, empirical, 'Vacuum-sibling delta: whether the boundary''s realized strength is context-dependent.').

omega_variable(
    novel_coupling_forms_coherence,
    'Do network-delivered services, container image distribution, and machine-learning ingestion of GPL corpora fit the binary derivation/aggregation taxonomy at all?',
    'Doctrinal analysis of the new integration forms against derivation criteria, plus observed licensing responses (network-copyleft adoption, source-available pivots) in affected projects.',
    'If the new forms sit outside the taxonomy, effective freedom exceeds what this reading models and the boundary''s grip erodes silently; if they map onto it, the construal extends without revision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(novel_coupling_forms_coherence, empirical, 'Whether the reading''s taxonomy remains coherent for post-binary coupling technologies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 1991, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_narrow_scope_reading_tr_t1991, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 1991, 0.12).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_tr_t1991, observed).
narrative_ontology:measurement(gpl_narrow_scope_reading_tr_t1997, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 1997, 0.14).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_tr_t1997, observed).
narrative_ontology:measurement(gpl_narrow_scope_reading_tr_t2003, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2003, 0.17).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_tr_t2003, observed).
narrative_ontology:measurement(gpl_narrow_scope_reading_tr_t2009, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2009, 0.21).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_tr_t2009, observed).
narrative_ontology:measurement(gpl_narrow_scope_reading_tr_t2015, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_tr_t2015, observed).
narrative_ontology:measurement(gpl_narrow_scope_reading_tr_t2020, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_tr_t2020, observed).
narrative_ontology:measurement(gpl_narrow_scope_reading_tr_t2025, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(gpl_narrow_scope_reading_be_t1991, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 1991, 0.42).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_be_t1991, observed).
narrative_ontology:measurement(gpl_narrow_scope_reading_be_t1997, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 1997, 0.4).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_be_t1997, observed).
narrative_ontology:measurement(gpl_narrow_scope_reading_be_t2003, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2003, 0.38).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_be_t2003, observed).
narrative_ontology:measurement(gpl_narrow_scope_reading_be_t2009, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2009, 0.37).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_be_t2009, observed).
narrative_ontology:measurement(gpl_narrow_scope_reading_be_t2015, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2015, 0.36).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_be_t2015, observed).
narrative_ontology:measurement(gpl_narrow_scope_reading_be_t2020, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2020, 0.37).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_be_t2020, observed).
narrative_ontology:measurement(gpl_narrow_scope_reading_be_t2025, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2025, 0.38).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl_narrow_scope_reading_su_t1991, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 1991, 0.3).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_su_t1991, observed).
narrative_ontology:measurement(gpl_narrow_scope_reading_su_t1997, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 1997, 0.32).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_su_t1997, observed).
narrative_ontology:measurement(gpl_narrow_scope_reading_su_t2003, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2003, 0.38).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_su_t2003, observed).
narrative_ontology:measurement(gpl_narrow_scope_reading_su_t2009, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2009, 0.47).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_su_t2009, observed).
narrative_ontology:measurement(gpl_narrow_scope_reading_su_t2015, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2015, 0.43).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_su_t2015, observed).
narrative_ontology:measurement(gpl_narrow_scope_reading_su_t2020, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2020, 0.37).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_su_t2020, observed).
narrative_ontology:measurement(gpl_narrow_scope_reading_su_t2025, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2025, 0.33).
narrative_ontology:measurement_basis(gpl_narrow_scope_reading_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, information_standard).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__enforcement_vacuum_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, copyright_derivative_work_definition).

% DUAL FORMULATION NOTE:
% 'GPL copyleft scope' decomposes into three structurally distinct constraints per the epsilon-invariance principle: this narrow reading (covered set equals doctrine-true derivatives; epsilon moderate-low), the strong reading (covered set extends to all coupled works; epsilon substantially higher; separate file), and the enforcement-vacuum reading (realized scope equals enforcement-capacity-dependent; epsilon context-dependent; separate file). The colloquial label conflated them; each file carries its own beneficiaries, metrics, and classification, and the family is linked through affects_constraints. Edges: this reading cites traditional copyright doctrine as its determinative authority (upstream edge to copyright_derivative_work_definition, the settled legal category with high empirical confidence); its persistence uncontradicted is the very condition the enforcement-vacuum reading describes (downstream edge); and its survival against broad-coverage litigation shapes the strong reading's operating environment without resolving it. Upstream-downstream structure follows the family pattern: the settled doctrine claim feeds this reading's rule, which in turn structures both siblings' environments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
