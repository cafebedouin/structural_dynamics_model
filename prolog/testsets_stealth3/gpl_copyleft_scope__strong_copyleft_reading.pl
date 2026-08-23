% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__strong_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__strong_copyleft_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: gpl_copyleft_scope__strong_copyleft_reading
 *   human_readable: GPL Section 2(b) Copyleft Scope — Strong Coupling Reading
 *   domain: legal/economic/technological
 *
 * SUMMARY:
 *   GPL Section 2(b), the copyleft clause, is a contested kernel: the same
 *   license text supports three structurally different constraints depending
 *   on where the derivative-work boundary is drawn. This story instantiates
 *   the strong copyleft reading — the boundary extends to all forms of code
 *   coupling, so any combined or dynamically linked work must be licensed in
 *   full under the GPL. At this scope the arrangement binds an identifiable
 *   class: proprietary vendors and device manufacturers who couple GPL
 *   components, who must open their entire combined work, forgo the
 *   component, engineer around the boundary, or buy a paid exception. Family
 *   epsilon differences: the narrow_scope_reading
 *   (gpl_copyleft_scope__narrow_scope_reading) binds only traditional
 *   derivative works and authors low epsilon — near pure consent-based
 *   commons membership; the enforcement_vacuum_reading
 *   (gpl_copyleft_scope__enforcement_vacuum_reading) authors
 *   context-dependent epsilon tracking whichever interpretive community holds
 *   enforcement capacity in a given context; this reading authors the
 *   family's highest epsilon (0.72) because its scope maximally captures
 *   coupled proprietary code while the reciprocity guarantee it funds is
 *   real. The generation manifest seeded a high-epsilon snare hypothesis;
 *   this authoring claims tangled_rope on the structural grounds given in
 *   logic_rationale, and the seed/claim divergence is retained as data rather
 *   than reconciled.
 *
 * KEY AGENTS:
 *   - fsf_license_steward: agenda-setter and enforcer (institutional / identity_locked) — publishes the interpretive positions defining the coupling boundary, runs enforcement; its mission is constituted by the mechanism it administers
 *   - free_software_community: primary beneficiary (organized / constrained) — receives the reciprocity guarantee that coupled improvements flow back under the same license
 *   - downstream_source_users: secondary beneficiary (moderate / mobile) — hold guaranteed study/modify/redistribute rights at no obligation
 *   - proprietary_software_vendors: primary target (powerful / constrained) — bear the coupling obligation across their entire combined works
 *   - embedded_device_manufacturers: primary enforcement target (moderate / constrained) — firmware source obligations enforced by injunction against product lines
 *   - dual_licensing_businesses: secondary beneficiary (organized / mobile) — monetize paid exceptions whose value the strong scope creates
 *   - permissive_license_ecosystem: indirect beneficiary (organized / mobile) — captures vendor flight from copyleft components
 *   - courts_and_regulators: excluded adjudicator (institutional / analytical) — holds definitive interpretive authority that has never been exercised on the merits
 *   - ip_law_scholars: analytical observer (moderate / analytical) — shape the interpretive climate without enforcement capacity or stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.72).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.58).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Section 2(b) Copyleft Scope — Strong Coupling Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "legal/economic/technological").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, 'a464b9c0-916f-4392-ad04-25af25a66517').
narrative_ontology:cs_kernel_codification('a464b9c0-916f-4392-ad04-25af25a66517', fixed_text).
narrative_ontology:cs_authority_grounding('a464b9c0-916f-4392-ad04-25af25a66517', lineage).
narrative_ontology:cs_interpretation_layer_present('a464b9c0-916f-4392-ad04-25af25a66517').
narrative_ontology:cs_reading_relation('a464b9c0-916f-4392-ad04-25af25a66517', gpl_copyleft_scope__narrow_scope_reading, forecloses).
narrative_ontology:cs_reading_relation('a464b9c0-916f-4392-ad04-25af25a66517', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('a464b9c0-916f-4392-ad04-25af25a66517', foundational, dynamic_linking_creates_derivative_work).
narrative_ontology:cs_axiom_status(dynamic_linking_creates_derivative_work, holdable).
narrative_ontology:cs_axiom_grounding('a464b9c0-916f-4392-ad04-25af25a66517', dynamic_linking_creates_derivative_work, conventional).
narrative_ontology:cs_axiom('a464b9c0-916f-4392-ad04-25af25a66517', foundational, coupling_must_not_privatize_commons).
narrative_ontology:cs_axiom_status(coupling_must_not_privatize_commons, holdable).
narrative_ontology:cs_axiom_grounding('a464b9c0-916f-4392-ad04-25af25a66517', coupling_must_not_privatize_commons, instrumental).
narrative_ontology:cs_reference_frame('a464b9c0-916f-4392-ad04-25af25a66517', plain_text_full_reciprocity_scope).
narrative_ontology:cs_drift_state('a464b9c0-916f-4392-ad04-25af25a66517', contemporary_selective_enforcement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a464b9c0-916f-4392-ad04-25af25a66517', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_community).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, downstream_source_users).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, fsf_license_steward).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, dual_licensing_businesses).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, permissive_license_ecosystem).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, embedded_device_manufacturers).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__strong_copyleft_reading, copyleft_reciprocity_doctrine).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__strong_copyleft_reading, software_freedom_as_structural_guarantee).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds copyright in the license text, publishes the interpretive positions that define what the copyleft clause covers, and runs enforcement through litigation and negotiated settlements. Its funding, staffing, and institutional mission are built around administering and defending this mechanism; walking away from the strong reading would leave the organization without its core function. It collects enforcement settlements and institutional standing, and bears almost no cost from the rules it administers.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, fsf_license_steward, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__strong_copyleft_reading, fsf_license_steward, beneficiary).

% The distributed body of developers and projects whose code carries the GPL. They receive the reciprocity guarantee: improvements made by anyone who couples their components must flow back under the same license. They bear no coupling obligation themselves, though individual contributors sometimes dissent from enforcement actions carried out in the community's name. Exit — relicensing projects as permissive — is possible and occasionally taken, but means abandoning decades of accumulated commons.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_community, beneficiary,
    organized, generational, constrained, global).

% Anyone who receives GPL-covered software: they get guaranteed rights to study, modify, and redistribute, with source code, at no cost and with no obligation unless they redistribute a coupled work. The guarantee travels with the code and does not depend on any vendor's continued goodwill. Their main cost is indirect: products they might want run on components their makers refuse to build on GPL terms, so some capabilities stay proprietary-locked.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, downstream_source_users, beneficiary,
    moderate, biographical, mobile, global).

% Firms building proprietary products who want the functionality of GPL components — compilers, libraries, kernels, utilities. At this reading's scope, any combined or dynamically linked work must be licensed in full under the GPL, so their options are: release their source, forgo the component and accept a weaker or costlier alternative, buy a commercial exception where dual licensing exists, or engineer around the boundary with process separation at real cost. Litigation risk hangs over borderline architectures, and sophisticated vendors increasingly avoid GPL components altogether.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Firms shipping GPL components — kernels, BusyBox, libraries — inside consumer and industrial devices. The coupling obligation requires them to offer complete corresponding source for firmware modifications, an obligation their supply-chain and engineering processes were not designed to meet. They are the historically most-litigated class: injunctions in this space have halted product sales. Compliance requires process overhaul across the product line; non-compliance risks the shipments themselves, and already-shipped products cannot be retrofitted out of the obligation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, embedded_device_manufacturers, payer,
    moderate, biographical, constrained, global).

% Firms that hold copyright in popular GPL components and sell commercial exceptions alongside the free license. The broader the coupling obligation, the more valuable their paid exception: the strong reading is, functionally, their sales mechanism. They fund component development from exception revenue and depend on enforcement credibility to keep the exception worth buying.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, dual_licensing_businesses, beneficiary,
    organized, biographical, mobile, global).

% BSD/MIT/Apache-licensed projects and their foundations. They capture vendor flight: when the strong reading makes GPL components unusable in proprietary products, vendors seeking open-source building blocks route toward permissive licenses, bringing corporate contributions with them. They also carry a cost: the aggressiveness of the copyleft scope claim feeds vendor narratives that all copyleft is legally hazardous, and the resulting legitimacy contest spills onto them.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, permissive_license_ecosystem, beneficiary,
    organized, generational, mobile, global).

% Hold the only authority that could definitively fix the derivative-work boundary for linking, but the question has never reached final adjudication on the merits — enforcement actions settle or end on procedural grounds. Their absence is structural: it is what allows interpretive authority to rest with the license steward, and a definitive ruling would either ratify or destroy the strong reading in a single judgment.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, courts_and_regulators, excluded,
    institutional, generational, analytical, continental).

% Academic and practitioner commentators who analyze the linking question, publish competing scope readings, and shape the interpretive climate through journals, amicus briefs, and conference debate. They hold no enforcement capacity and no financial stake; their influence runs entirely through the plausibility of their arguments.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, ip_law_scholars, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__strong_copyleft_reading, free_software_community).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__strong_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in commons-based software production. Without a reciprocity guarantee, a firm can incorporate commons code into a proprietary product, capture the improvement value, and return nothing — and rational firms then under-contribute to the commons. The copyleft clause makes source availability for coupled works the non-negotiable price of using GPL components, converting one-sided appropriation into a reciprocal flow.
% TRANSFER_FUNCTION: Moves complete source code and full licensing rights for any combined or dynamically linked work from proprietary vendors and device manufacturers to the commons; moves paid exception revenue from vendors to dual-licensing businesses where such businesses exist; and converts the freedom to study, modify, and redistribute from a vendor-granted privilege into a guaranteed right for downstream users.
% ABSENT_VOICES: Courts and regulators: the only seats with authority to fix the derivative-work boundary for linking have never ruled on the merits — every enforcement action has settled or ended on procedural grounds, so the de facto scope is set by the license steward's interpretive publications and by vendor avoidance behavior. Vendors bound by the reading also had no seat in its formulation: the interpretive community that produced the strong reading is steward-aligned. Both absent seats would contest the reading's scope; their absence is what lets the strong reading operate as if settled.
% DISAPPEARANCE_RATIONALE: If the strong reading vanished overnight — say, a definitive ruling adopted the narrow scope — vendor integration patterns would shift within product cycles as proprietary products absorbed GPL components without source release, dual-licensing revenue models would collapse as exceptions lost their value, the steward's enforcement posture would end, and the commons' reciprocity engine would weaken: the guarantee that improvements to GPL components flow back is what the whole arrangement holds in place. The free software ecosystem would not vanish, but its funding and contribution structure would reorganize around consent-based permissive licensing.
% FOUNDING_PROBLEM: In the early 1980s, software that had previously circulated with source among research institutions was being closed off by proprietary vendors. The GNU project was founded to rebuild a free system, and its license needed a mechanism that would stop firms from privatizing improvements to freely licensed code — making freedom self-propagating rather than dependent on each contributor's goodwill.
% FOUNDING_PROBLEM_CORROBORATION: The arrangement's opponents corroborate that the founding problem persists: the continuing docket of commercial appropriation attempts (the BusyBox litigation, FSF v. Cisco, the gpl-violations.org injunctions across European jurisdictions) exists only because firms keep coupling commons code into proprietary products without compliance; permissive-license advocates, who oppose this arrangement, build their own case on the premise that commons code attracts commercial appropriation. No serious party disputes the historical closure of the early-1980s software commons; the live dispute is over whether maximal coupling scope remains a proportionate response — which is the kernel contest, not the founding problem's existence. Corroboration therefore comes from enforcement dockets and from opponents outside the beneficiary set, not from the arrangement's own beneficiaries.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__strong_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__strong_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__strong_copyleft_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_copyleft_scope__strong_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__strong_copyleft_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the coupling obligation, at this reading's scope, reaches the vendor's entire combined work: compliance cost scales with the vendor's proprietary codebase rather than with the GPL component used, and the boundary is drawn at maximal reach — every form of coupling triggers it. Suppression (0.58) reflects real coercive instruments: license termination, injunctions that have halted product sales in embedded-device litigation, and settlement leverage — but it does not reflect alternative-suppression, which is the line between this profile and a snare: permissive-licensed equivalents, clean-room reimplementation, dual-license purchase, and architectural separation all remain lawful and available. Theater (0.27) is low-moderate: the stated function — guaranteed source availability for coupled works — is demonstrably the operating function, with a growing theatrical share in compliance tokenism and selective-enforcement rhetoric. Accessibility collapse (0.40) and resistance (0.60) describe an actively defended human construct, not a natural law: alternatives remain partly workable and the arrangement meets organized pushback. Claimed type: tangled_rope, diverging deliberately from the seeded snare hypothesis. The snare test requires the coordination story to be cover; here the coordination function — solving the free-rider problem that otherwise under-produces commons software — is genuine, primary, and verifiable in the world: the Linux/GCC/GNU commons remains free and available after three decades of operation. Extraction and coordination run through the same structure — the same clause that guarantees availability imposes the coupling obligation — which is the tangled-rope signature: genuine coordination plus asymmetric extraction plus active enforcement. All three metric series run on one shared time grid (seven points, every metric authored at every point). The suppression_requirement series is authored because enforcement capacity is a tracked dynamic here: it builds through the enforcement-maturity era (European injunctions, the BusyBox suits, the Cisco settlement), peaks, then relaxes into strategic selectivity after the VMware dismissal before compliance automation re-ratchets it.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently by construction. From the steward's seat the arrangement is a freedom-guaranteeing reciprocity mechanism the organization is constitutionally bound to defend — an identity-locked seat in the institutional sense: the organization has become its function, and exit would mean dissolution. From the proprietary-vendor seat the same structure operates as a scope claim that captures its codebase on terms no court has ever ratified; from the embedded-manufacturer seat it arrives as an injunction halting shipments. The excluded court seat perceives a third, different constraint — an unsettled legal question — where the steward's seat perceives a settled one; the strong reading's practical force lives entirely inside that gap between interpretive assertion and adjudicated fact. Same-level divergence also appears among payers: vendors and embedded manufacturers face the same nominal obligation, but shipped-product lock-in makes the embedded seat's exit costlier and its compliance posture more coerced despite lower formal power.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: the free software community and downstream source users receive the guarantee and bear no coupling obligation; the steward collects enforcement standing and institutional relevance; dual-licensing businesses collect exception revenue the strong scope makes valuable. Victim declarations drive high d: proprietary vendors and embedded manufacturers bear the coupling obligation with constrained exits — rewriting or forgoing components is costly, and shipped firmware is effectively locked in. One directionality override: permissive_license_ecosystem derives a near-beneficiary d from its declared role, but its benefit is indirect (capturing vendor flight) and partly offset by the legitimacy contest the strong reading fuels against all copyleft, so d is overridden to 0.40, near-symmetric. Suppression is authored as a raw structural property and enters the engine's computation unscaled; only extractiveness is scaled, by directionality and spatial scope — and this arrangement's global scope amplifies effective extraction on its targets because verifying coupling compliance across jurisdictions is hard.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — proprietary appropriation of freely circulating code — is live, attested by the arrangement's own opponents, so no mandatrophy is declared: the mechanism has not outlived its function. The tangled-rope classification does protective work in both directions. Against the vendor framing (the GPL as a viral license), it preserves the genuine coordination function that a pure-extraction reading would erase; against the movement framing (pure freedom at no cost), it keeps the concentrated, real costs borne by the payer seats visible in the classification. If the founding problem ever died — if commons code lost its appropriation value — the same structure would decay toward a piton profile: enforcement theater around a guarantee no one needs, with the theater_ratio series overtaking function. The current series shows the opposite: theater grows slowly while extraction holds, consistent with a live mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (strong_copyleft_reading) of the contested kernel gpl_copyleft_scope; what would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Definitive appellate adjudication of whether dynamic linking creates a derivative work for GPL purposes would collapse the kernel contest into a single reading; until then the three readings persist as separate constraints with different victim sets and epsilon values. The disagreement is located entirely in the derivative-work boundary determination, not in the license text itself.',
    'If the narrow reading were ratified, this constraint''s victim set (proprietary vendors, embedded manufacturers) empties and epsilon collapses toward a consent-based membership rope; if the vacuum reading were ratified, epsilon becomes context-dependent on local enforcement capacity rather than a stable structural property of the arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is the strong reading of the gpl_copyleft_scope kernel; siblings instantiate different constraints, not alternative opinions about this one.').

omega_variable(
    linking_derivative_work_doctrine,
    'Does copyright doctrine actually support treating dynamic linking and other loose coupling as creating a derivative work, as this reading''s scope claim asserts?',
    'Merits adjudication in a major jurisdiction. No enforcement action to date has reached final judgment on the question — settlements and procedural dismissals only — so the premise beneath the scope claim remains untested.',
    'A narrow ruling empties the victim set and pushes the arrangement toward a rope profile; a strong ruling ratifies this reading''s scope, hardens enforcement, and raises effective extraction further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linking_derivative_work_doctrine, empirical, 'The unadjudicated legal doctrine beneath the reading''s maximal coupling scope.').

omega_variable(
    chilling_mechanism_composition,
    'How much of vendor over-compliance — avoiding GPL components entirely where borderline coupling might be defensible — is driven by structural legal risk versus internalized community-norm and reputational pressure?',
    'Compare vendor architecture decisions across jurisdictions and eras with different enforcement intensity; survey engineering-lead rationales for GPL avoidance decisions.',
    'If norm-driven, the arrangement''s suppressive force persists even under enforcement decay — an ecosystem-level identity-lock dynamic in which vendors carry the avoidance posture with them regardless of legal exposure; if purely structural, enforcement capacity is the whole story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_mechanism_composition, empirical, 'Structural versus internalized suppression in vendor avoidance behavior.').

omega_variable(
    enforcement_capacity_durability,
    'Is the enforcement capacity that makes this reading''s threats credible durable, or does it depend on a small set of stewards whose capacity could decay?',
    'Track enforcement filings, settlement structures, and compliance-industry activity over the next decade; the current capacity concentrates in a handful of organizations and a maturing compliance-services market.',
    'Enforcement decay pushes the effective arrangement toward the enforcement_vacuum_reading''s profile — epsilon becomes context-dependent — with no change to the license text itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_durability, empirical, 'Durability of the enforcement capacity beneath the credible-threat structure.').

omega_variable(
    dual_licensing_capture_question,
    'Does the enforcement machinery primarily serve commons integrity, or does a concentrated beneficiary class — dual-licensing businesses selling paid exceptions — capture a substantial share of its product?',
    'Compare enforcement targets and settlement structures across dual-licensed versus non-dual-licensed projects; trace exception-revenue flows against enforcement expenditure.',
    'If capture is substantial, the arrangement gains snare flavor — enforcement functioning as a sales mechanism — and the receipt surface splits between the commons and exception sellers; if not, the tangled-rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_licensing_capture_question, empirical, 'Beneficiary-structure contest: commons guarantee versus exception monetization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_strong_reading_tr_t0, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(gpl_strong_reading_tr_t0, observed).
narrative_ontology:measurement(gpl_strong_reading_tr_t5, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(gpl_strong_reading_tr_t5, observed).
narrative_ontology:measurement(gpl_strong_reading_tr_t10, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(gpl_strong_reading_tr_t10, observed).
narrative_ontology:measurement(gpl_strong_reading_tr_t15, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(gpl_strong_reading_tr_t15, observed).
narrative_ontology:measurement(gpl_strong_reading_tr_t20, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(gpl_strong_reading_tr_t20, observed).
narrative_ontology:measurement(gpl_strong_reading_tr_t25, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(gpl_strong_reading_tr_t25, observed).
narrative_ontology:measurement(gpl_strong_reading_tr_t30, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement_basis(gpl_strong_reading_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(gpl_strong_reading_be_t0, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(gpl_strong_reading_be_t0, observed).
narrative_ontology:measurement(gpl_strong_reading_be_t5, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 5, 0.57).
narrative_ontology:measurement_basis(gpl_strong_reading_be_t5, observed).
narrative_ontology:measurement(gpl_strong_reading_be_t10, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(gpl_strong_reading_be_t10, observed).
narrative_ontology:measurement(gpl_strong_reading_be_t15, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(gpl_strong_reading_be_t15, observed).
narrative_ontology:measurement(gpl_strong_reading_be_t20, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement_basis(gpl_strong_reading_be_t20, observed).
narrative_ontology:measurement(gpl_strong_reading_be_t25, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement_basis(gpl_strong_reading_be_t25, observed).
narrative_ontology:measurement(gpl_strong_reading_be_t30, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(gpl_strong_reading_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl_strong_reading_su_t0, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(gpl_strong_reading_su_t0, observed).
narrative_ontology:measurement(gpl_strong_reading_su_t5, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement_basis(gpl_strong_reading_su_t5, observed).
narrative_ontology:measurement(gpl_strong_reading_su_t10, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(gpl_strong_reading_su_t10, observed).
narrative_ontology:measurement(gpl_strong_reading_su_t15, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement_basis(gpl_strong_reading_su_t15, observed).
narrative_ontology:measurement(gpl_strong_reading_su_t20, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement_basis(gpl_strong_reading_su_t20, observed).
narrative_ontology:measurement(gpl_strong_reading_su_t25, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement_basis(gpl_strong_reading_su_t25, observed).
narrative_ontology:measurement(gpl_strong_reading_su_t30, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(gpl_strong_reading_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__narrow_scope_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__enforcement_vacuum_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, agpl_network_copyleft).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, linux_kernel_module_boundary).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'the GPL's copyleft scope' decomposes into three stories per the epsilon-invariance principle — measuring the arrangement at narrow scope yields low epsilon (consent-based membership), at vacuum scope yields context-dependent epsilon, and at strong scope yields high epsilon with a fixed victim set. These are not one constraint viewed from different angles; they are three constraints with different victim sets, enforcement surfaces, and classifications, linked here and in each sibling. The upstream member by evidentiary confidence is the fixed license text itself; this strong reading is the downstream member whose classification the other two contest, and whose enforcement successes in turn erode the vacuum reading's empirical domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__strong_copyleft_reading, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
