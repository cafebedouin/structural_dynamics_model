% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__strong_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
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
 *   constraint_id: gpl_copyleft_scope__strong_copyleft_reading
 *   human_readable: GPL Strong Copyleft Linking Scope (Section 2(b))
 *   domain: legal/technological
 *
 * SUMMARY:
 *   Under the strong copyleft reading, GPL Section 2(b) reaches any combined
 *   or dynamically linked work: a proprietary product that incorporates
 *   GPL-covered code — however loosely coupled — must be released whole under
 *   the GPL. The arrangement solves a real collective-action problem
 *   (guaranteeing that improvements to shared code stay shared) while
 *   imposing a sharp asymmetric burden: any vendor wishing to combine covered
 *   components with proprietary code pays in full source disclosure or
 *   abstains. Enforcement is active and maturing — license-steward compliance
 *   campaigns, settlement precedents, and corporate risk regimes that treat
 *   ingestion of covered code as a managed hazard. This file instantiates ONE
 *   reading of the gpl_copyleft_scope kernel; the narrow_scope_reading and
 *   enforcement_vacuum_reading are separate constraints with their own
 *   epsilon, beneficiary/victim sets, and classifications, and the contest
 *   between readings is carried entirely in the omega variables. KEY AGENTS
 *   (by structural relationship): - fsf_license_steward: Agenda setter
 *   (institutional/arbitrage) — publishes the license text, defines the
 *   reading's operative content, runs compliance enforcement -
 *   gpl_contributing_developers: Primary beneficiary (moderate/mobile) —
 *   contribute on the guarantee that derivatives stay free -
 *   downstream_source_recipients: Primary beneficiary (powerless/mobile) —
 *   receive source and modification rights - saas_cloud_operators: Structural
 *   beneficiary (institutional/mobile) — internal networked use escapes the
 *   conveyance trigger entirely - dual_licensing_commercial_distributors:
 *   Secondary beneficiary (organized/mobile) — monetize the terms'
 *   exclusivity via commercial exceptions - proprietary_software_vendors:
 *   Primary target (powerful/constrained) — bear the disclose-or-abstain
 *   choice - embedded_device_manufacturers: Secondary target
 *   (organized/constrained) — ship covered code in firmware, accumulate
 *   violation exposure - embedded_device_end_users: Excluded seat
 *   (powerless/trapped) — the constituency whose promised rights are most
 *   often silently denied - copyright_courts: Analytical observer
 *   (institutional/analytical) — the tribunal whose silence sustains the
 *   reading plurality
 *
 * KEY AGENTS:
 *   - fsf_license_steward: agenda setter, institutional power, arbitrage exit — controls the license text and enforcement agenda
 *   - gpl_contributing_developers: primary beneficiary, moderate power, mobile exit — individual relicensing always available
 *   - downstream_source_recipients: primary beneficiary, powerless individually, mobile exit — the protected constituency
 *   - saas_cloud_operators: structural beneficiary, institutional power, mobile exit — trigger-exempt internal use
 *   - dual_licensing_commercial_distributors: secondary beneficiary, organized power, mobile exit — sells relief from the terms' friction
 *   - proprietary_software_vendors: primary target, powerful, constrained exit — disclose-or-abstain gate
 *   - embedded_device_manufacturers: secondary target, organized, constrained exit — accumulated firmware violation exposure
 *   - embedded_device_end_users: excluded, powerless, trapped exit — denied the rights the terms promise them
 *   - copyright_courts: analytical observer, institutional, national scope — has never ruled on the linking boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.72).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.62).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Strong Copyleft Linking Scope (Section 2(b))").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "legal/technological").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, 'e14dd328-b088-43a3-8e15-e12b1038a501').
narrative_ontology:cs_kernel_codification('e14dd328-b088-43a3-8e15-e12b1038a501', fixed_text).
narrative_ontology:cs_authority_grounding('e14dd328-b088-43a3-8e15-e12b1038a501', lineage).
narrative_ontology:cs_interpretation_layer_present('e14dd328-b088-43a3-8e15-e12b1038a501').
narrative_ontology:cs_reading_relation('e14dd328-b088-43a3-8e15-e12b1038a501', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_reading_relation('e14dd328-b088-43a3-8e15-e12b1038a501', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('e14dd328-b088-43a3-8e15-e12b1038a501', foundational, all_code_coupling_is_derivation).
narrative_ontology:cs_axiom_status(all_code_coupling_is_derivation, holdable).
narrative_ontology:cs_axiom_grounding('e14dd328-b088-43a3-8e15-e12b1038a501', all_code_coupling_is_derivation, conventional).
narrative_ontology:cs_axiom('e14dd328-b088-43a3-8e15-e12b1038a501', foundational, software_freedom_requires_reciprocal_disclosure).
narrative_ontology:cs_axiom_status(software_freedom_requires_reciprocal_disclosure, holdable).
narrative_ontology:cs_axiom_grounding('e14dd328-b088-43a3-8e15-e12b1038a501', software_freedom_requires_reciprocal_disclosure, deontological).
narrative_ontology:cs_reference_frame('e14dd328-b088-43a3-8e15-e12b1038a501', all_code_coupling_is_derivation).
narrative_ontology:cs_drift_state('e14dd328-b088-43a3-8e15-e12b1038a501', contemporary_no_linking_precedent, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e14dd328-b088-43a3-8e15-e12b1038a501', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, gpl_contributing_developers).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, downstream_source_recipients).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, saas_cloud_operators).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, dual_licensing_commercial_distributors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, embedded_device_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, fsf_license_steward).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publishes and periodically revises the license text, answers interpretive questions through published FAQs, and pursues compliance against violators through litigation support and settlements. Collects settlement resources and institutional standing from successful enforcement. Holds unilateral power to redefine the text — and has exercised it, issuing version 3 — making it the seat that could redraw the linking boundary at will.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, fsf_license_steward, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__strong_copyleft_reading, fsf_license_steward, beneficiary).

% Write and maintain code under the terms on the understanding that anyone building on it must pass the same freedoms along. Compensation is reputational and communal rather than monetary. They retain copyright in their own contributions and can relicense them or start new projects under other terms, so departure is individually available at any time.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, gpl_contributing_developers, beneficiary,
    moderate, biographical, mobile, global).

% Receive the source, the right to modify, and the right to redistribute for every covered program, including improvements made by others. Individually they hold little leverage over the terms, but collectively they are the constituency the terms exist to protect, and they can adopt or abandon any given program freely.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, downstream_source_recipients, beneficiary,
    powerless, biographical, mobile, global).

% Run covered code on their own servers to deliver services over networks. Because offering a service is not conveying a copy, their internal use triggers no source-disclosure obligation to anyone, allowing them to take the code's full value without passing rights to their customers. Several major platforms are built substantially on this pattern.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, saas_cloud_operators, beneficiary,
    institutional, biographical, mobile, global).

% Hold copyright to a covered project and sell commercial exceptions to its terms alongside the free license. Their fee revenue exists only because the terms create friction for proprietary integrators; the model applies to a small minority of projects, but it demonstrates how the terms' exclusivity converts into payment for whoever is positioned to sell relief.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, dual_licensing_commercial_distributors, beneficiary,
    organized, biographical, mobile, global).

% Build products that would benefit from incorporating mature covered components — cryptography libraries, compilers, databases, media frameworks. Integration obligates release of their own combined source, surrendering the secrecy their business model rests on. Realistic options are abstaining from covered components, purchasing commercial exceptions where offered, restructuring delivery around network services, or funding clean-room substitutes; each carries substantial cost, and the bind is sharpest for products already mid-build on a covered dependency.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Ship consumer hardware containing covered code in firmware and userspace. Compliance would require assembling and publishing source trees most never maintained, so the sector accumulates widespread technical violation, exposed to enforcement letters and product-line injunction risk. Future product lines can select components differently, but shipped inventory and sunk development investment lock in exposure.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, embedded_device_manufacturers, payer,
    organized, biographical, constrained, global).

% Own devices whose firmware incorporates covered code but receive no source and no practical ability to modify or repair the software running on hardware they possess. They are the constituency whose promised rights are most often silently denied, and they hold no seat in enforcement prioritization decisions.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, embedded_device_end_users, excluded,
    powerless, biographical, trapped, global).

% Adjudicate infringement disputes that would force a ruling on whether linking creates a derivative work. No appellate court has taken the question, so the boundary persists by adjudicative avoidance; their eventual answer would redefine the arrangement for every other seat at once.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, copyright_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__strong_copyleft_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__strong_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Secures, once and centrally by license terms, that every copy and derivative of covered code carries source availability and modification rights — solving the commons-appropriation problem that bilateral trust between thousands of anonymous contributors and integrators cannot solve.
% TRANSFER_FUNCTION: Moves source-code disclosure from proprietary integrators (who must release the full combined work's source or abstain from integration) into the public commons, and moves guaranteed use-modify-redistribute rights from the commons to all comers, including the integrators themselves should they comply.
% ABSENT_VOICES: Proprietary vendors who never assented to the terms encounter the obligation only through integration choices their counsel contest; embedded-device end users whose firmware rights are mass-violated rarely shape enforcement priorities; and the judiciary — the seat that could definitively fix the linking boundary — has never ruled, leaving the interpretive settlement to enforcement capacity rather than adjudication.
% DISAPPEARANCE_RATIONALE: If the strong reading vanished overnight, covered code would flow into proprietary products without reciprocal disclosure, contributor guarantees would collapse, dual-licensing leverage would evaporate, and the free-software ecosystem would reorganize around permissive licenses or new enforcement vehicles — the current allocation of code rights across the industry is arranged around this boundary existing.
% FOUNDING_PROBLEM: Proprietary appropriation of freely shared code: the early-1980s pattern, symbolized by the Symbolics–LMI split, in which free code was absorbed into proprietary products with improvements withheld, threatening one-way depletion of the commons.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: contemporaneous documentation of the Symbolics dispute; corporate legal-risk regimes that treat ingestion of covered code as a managed hazard, attesting perceived enforceability; the parallel emergence of permissive-license ecosystems that acknowledged the appropriation problem but rejected this remedy; and academic commons-governance literature. No court has corroborated this reading's specific linking boundary — the corroboration covers the founding problem, not the strong reading's scope claim.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__strong_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__strong_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__strong_copyleft_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.72) because the obligation is decoupled from harm: a vendor integrating a covered utility library owes the full source of its combined product regardless of how much or little covered code it used, and the obligation attaches at integration rather than at copying. Suppression (0.62) reflects copyright law backing plus a maturing enforcement apparatus — the arrangement's persistence against well-resourced resistance depends on credible litigation threat, not participant preference. Theater is low (0.20): compliance activity is overwhelmingly functional (actual source publications, actual settlements), with a modest symbolic overlay of ideological enforcement. Accessibility collapse is moderate (0.50): alternatives do not vanish — permissively licensed substitutes, clean-room reimplementation, network-delivery restructuring, and commercial exceptions all exist — but they narrow sharply once a product is mid-build on a covered dependency. Resistance is substantial (0.65): decades of corporate policy bans, hostile characterizations of the license, and strategic avoidance attest that the terms must be actively defended.
 *   
 *   Claim/metric independence: the originating manifest hypothesized a high-epsilon snare. I author tangled_rope instead, because the coordination function is genuine, openly declared, and constitutive rather than cover — the terms exist precisely to guarantee code availability, and the contributing community adopts them voluntarily and net-beneficially. The extraction component (asymmetric burden on proprietary integrators through the same structure) is equally real, which is what makes this a hybrid rather than pure coordination. The engine computes per-seat classifications from the structural data; vendor seats — especially post-integration discoverers — may legitimately compute closer to pure extraction, and that divergence is the measurement, not an error to reconcile.
 *   
 *   Receipt surface: gain_flow is authored as 'diffuse' as an affirmative checked claim. Re-reading every seat: the dominant extracted good is source code, which by design flows to the unowned commons and cannot be captured — anti-appropriation is the arrangement's defining engineering. Two secondary monetizable slices exist and were checked: enforcement settlements accrue to the steward seat, and commercial-exception fees accrue to dual-licensing distributors, but both are marginal valves on a minority of projects, not the main receipt. fixing_cost is 'prohibitive': the seats that could change the arrangement (the steward, by revising the text; courts, by ruling) face a removal cost — unraveling the reciprocity guarantee across thousands of interdependent projects — that vastly exceeds the benefit of relieving vendor burden.
 *   
 *   Temporal grid: all three tracked metrics are authored on one shared grid (t = 0, 6, 12, 18, 24, 30, 36; the interval indexes years since GPLv2's publication, t0 ≈ 1991, tn ≈ 2027). The suppression_requirement series is authored deliberately: the story's narrative specifically traces enforcement-capacity maturation — from an untested litigation threat to established compliance machinery — which is exactly the enforcement-infrastructure change the scalar base_properties.suppression cannot carry alone. Extractiveness rises as scope claims hardened and compliance spread; theater stays low throughout, indicating the arrangement has not decayed into performance.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/steward seats should compute differently. From the steward's position the arrangement is a guarantee it authored and defends: every disclosure is the system working. From a contributing developer's position it is the precondition for trusting shared work at all. From a proprietary vendor's position — particularly one that learned the scope claim only after integrating — the same structure operates as an unconditional demand backed by litigation risk, with exits (rewrite, substitute, restructure around network delivery) that are real but expensive. The engine computes this divergence from power, exit options, and directional position; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Contributing developers and downstream recipients are declared beneficiaries with mobile exits — they sit near the full-beneficiary end (d near 0), and effective extraction inverts toward subsidy for them. SaaS operators are the deepest beneficiaries: the conveyance trigger never fires on internal networked use, so they take the code's full value with no reciprocal obligation — structurally nearer the beneficiary pole than contributors themselves. Dual-licensing distributors benefit incidentally: the terms' friction is their revenue source. Proprietary vendors and embedded manufacturers are declared victims (role: payer) with constrained exits — trapped by mid-build dependencies and shipped inventory respectively — placing them near the full-target end (d near 1), where effective extraction is amplified. The steward sits at the agenda-setting seat with an arbitrage-grade exit (it can revise the text); its beneficiary secondary role keeps its d low. Courts are analytical and collect nothing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — proprietary appropriation of freely shared code — remains live: cloud delivery, embedded noncompliance, and absorb-and-extend strategies are its current forms, so the mandate has not outlived its function and mandatrophy is not resolved. The R5 mismatch consumer reads founding_problem_status (live) against disappearance_verdict (world_rearranges): aligned, no zombie flag. The hybrid classification earns its keep here: labeling the arrangement pure coordination would erase the sharp asymmetric burden vendors bear through the identical structure that guarantees the commons; labeling it pure extraction would erase the genuine, voluntarily adopted, openly declared guarantee that constitutes the arrangement's entire point. The tangled form holds both facts without averaging them away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_plurality,
    'This constraint instantiates the strong_copyleft_reading of kernel gpl_copyleft_scope; the sibling readings narrow_scope_reading and enforcement_vacuum_reading instantiate different constraints with different victim sets and different epsilon. Which reading governs a given integration in fact?',
    'Definitive appellate precedent on whether linking creates a derivative work would collapse the plurality into a single legally authoritative reading.',
    'If the narrow reading were authoritatively adopted, this story''s payer seats shrink to verbatim and direct-copy infringers, epsilon drops sharply, and the disclose-or-abstain gate dissolves into ordinary license compliance. If the strong reading were authoritatively adopted, the payer set expands and suppression rises further.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_plurality, conceptual, 'Committer structure: one reading of a contested kernel; the contest is routed here, not into the constraint body.').

omega_variable(
    linking_derivation_legal_status,
    'Does dynamic linking in fact create a derivative work under applicable copyright law, as this reading''s foundational doctrinal axiom asserts?',
    'Appellate adjudication of a linking case with a full technical and factual record; no such ruling currently exists in any major jurisdiction.',
    'An affirmative ruling hardens this reading into settled law (suppression rises, vendor exits narrow to abstention and clean-room substitution); a negative ruling collapses this constraint toward the narrow reading''s boundary and strands the enforcement machinery built on the broader claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linking_derivation_legal_status, empirical, 'Whether the coupling-implies-derivation premise is legally sound or merely untested.').

omega_variable(
    enforcement_capacity_locus,
    'Does the arrangement''s operative content in a given ecosystem track this reading''s declared scope, or does it collapse to whatever the local enforcement balance permits, depending on which interpretive community holds enforcement capacity?',
    'Comparative audit of enforcement outcomes and integration practices across steward-aligned projects versus industry-dominated ecosystems.',
    'If enforcement-vacuum dynamics dominate outside steward-aligned projects, this story''s epsilon overstates realized extraction in industry contexts, and the effective scope is locally negotiated rather than fixed by the license text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_locus, empirical, 'Where enforcement capacity sits determines which reading is operative in practice.').

omega_variable(
    vendor_seat_snare_experience,
    'For vendors that integrated covered components before understanding the scope claim, does the arrangement operate as unconditional extraction at their seat despite the story-level coordination function?',
    'Longitudinal comparison of compliance decisions among post-integration discoverers versus ex-ante informed adopters; rewrite-cost accounting for mid-build dependencies.',
    'If post-integration discoverers dominate the payer population, per-seat classifications for those seats approach pure extraction and the aggregate warrant for a hybrid coordination/extraction reading weakens; if informed abstention dominates, the conditional-gate framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_seat_snare_experience, conceptual, 'Seat-relative divergence: the same structure may be experienced as a trap by captive integrators and as a guarantee by contributors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_strong_copyleft_tr_t0, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl_strong_copyleft_tr_t6, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(gpl_strong_copyleft_tr_t12, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 12, 0.14).
narrative_ontology:measurement(gpl_strong_copyleft_tr_t18, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 18, 0.16).
narrative_ontology:measurement(gpl_strong_copyleft_tr_t24, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(gpl_strong_copyleft_tr_t30, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(gpl_strong_copyleft_tr_t36, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 36, 0.2).

% Extraction over time
narrative_ontology:measurement(gpl_strong_copyleft_be_t0, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gpl_strong_copyleft_be_t6, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 6, 0.59).
narrative_ontology:measurement(gpl_strong_copyleft_be_t12, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(gpl_strong_copyleft_be_t18, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 18, 0.66).
narrative_ontology:measurement(gpl_strong_copyleft_be_t24, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 24, 0.69).
narrative_ontology:measurement(gpl_strong_copyleft_be_t30, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement(gpl_strong_copyleft_be_t36, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 36, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(gpl_strong_copyleft_su_t0, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(gpl_strong_copyleft_su_t6, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement(gpl_strong_copyleft_su_t12, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(gpl_strong_copyleft_su_t18, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 18, 0.52).
narrative_ontology:measurement(gpl_strong_copyleft_su_t24, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(gpl_strong_copyleft_su_t30, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(gpl_strong_copyleft_su_t36, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 36, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, narrow_scope_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, enforcement_vacuum_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, agpl_network_use_extension).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'GPL copyleft scope' decomposes into three structurally distinct claims per the epsilon-invariance principle. This file is the strong reading (high epsilon, broad victim set: all coupling triggers disclosure). narrow_scope_reading carries a low epsilon and a victim set limited to verbatim/direct derivations. enforcement_vacuum_reading carries no fixed epsilon — its structure is enforcement-capacity-indexed. The strong reading influences both siblings: its enforcement successes and failures supply the evidentiary record the vacuum reading metabolizes, and its refusal to reach networked internal use is the precise gap that motivated the AGPL extension (agpl_network_use_extension), which is downstream of this story. Each member links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
