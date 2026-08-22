% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__freedom_imperative_reading, []).

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
 *   constraint_id: software_control_legitimacy__freedom_imperative_reading
 *   human_readable: Proprietary Software Control Regime (Freedom-Imperative Reading)
 *   domain: economic/technological/legal
 *
 * SUMMARY:
 *   This story instantiates the freedom-imperative reading of the
 *   software_control_legitimacy kernel: software control is a matter of
 *   fundamental user freedom, and the standing proprietary arrangement
 *   (closed source, restrictive licenses, DRM, anti-circumvention law) is
 *   authored from this seat as a categorical denial of rights. Per the
 *   epsilon-referent rule, epsilon's referent is the proprietary arrangement
 *   as this reading assesses it, not the free-software arrangement this
 *   reading would put in its place; the reading-indexed value is high because
 *   the reading holds the surrendered freedoms to be fundamental, making the
 *   burden rights-indexed rather than price-indexed. The claim/metric
 *   relationship is deliberate: the snare claim is authored from this
 *   reading's categorical lights while the metrics describe the arrangement's
 *   actual operation on the shared grid, and the engine computes per-seat
 *   classifications from the structural data without reconciling either to
 *   the other.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: Primary beneficiary and agenda-setter (institutional/arbitrage) - sets license terms, collects revenue and decision authority, can exit at will
 *   - proprietary_software_users: Primary target (moderate/constrained) - bears the freedom denial, exit costly via formats and ecosystems
 *   - enterprise_licensees: Organized dual-position seat (organized/constrained) - pays in fees and lock-in while receiving support and defensible procurement
 *   - independent_developers: Secondary target (powerless/constrained) - barred from building on and inspecting proprietary code, coalition capacity unproven
 *   - drm_licensing_consortiums: Secondary beneficiary (institutional/arbitrage) - collects technical-protection licensing fees, exists only within the regime
 *   - free_software_advocates: Excluded contesting party (organized/constrained) - holds the doctrine, holds no seat where terms are set
 *   - security_researchers: Excluded party (moderate/constrained) - barred from inspection by statute and contract regardless of purpose
 *   - competition_regulators: Analytical observer (institutional/analytical) - sees a market-power dispute, not the freedom dispute this reading authors
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, 0.85).
domain_priors:suppression_score(software_control_legitimacy__freedom_imperative_reading, 0.8).
domain_priors:theater_ratio(software_control_legitimacy__freedom_imperative_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_control_legitimacy__freedom_imperative_reading, "Proprietary Software Control Regime (Freedom-Imperative Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__freedom_imperative_reading, "economic/technological/legal").

domain_priors:requires_active_enforcement(software_control_legitimacy__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__freedom_imperative_reading, 'a93f497a-f66c-4bf1-9a32-f3a8f88d90a8').
narrative_ontology:cs_kernel_codification('a93f497a-f66c-4bf1-9a32-f3a8f88d90a8', distributed).
narrative_ontology:cs_authority_grounding('a93f497a-f66c-4bf1-9a32-f3a8f88d90a8', lineage).
narrative_ontology:cs_interpretation_layer_present('a93f497a-f66c-4bf1-9a32-f3a8f88d90a8').
narrative_ontology:cs_reading_relation('a93f497a-f66c-4bf1-9a32-f3a8f88d90a8', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('a93f497a-f66c-4bf1-9a32-f3a8f88d90a8', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('a93f497a-f66c-4bf1-9a32-f3a8f88d90a8', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('a93f497a-f66c-4bf1-9a32-f3a8f88d90a8', foundational, user_computing_control_is_fundamental_freedom).
narrative_ontology:cs_axiom_status(user_computing_control_is_fundamental_freedom, holdable).
narrative_ontology:cs_axiom_grounding('a93f497a-f66c-4bf1-9a32-f3a8f88d90a8', user_computing_control_is_fundamental_freedom, deontological).
narrative_ontology:cs_axiom('a93f497a-f66c-4bf1-9a32-f3a8f88d90a8', foundational, proprietary_restriction_categorically_illegitimate).
narrative_ontology:cs_axiom_status(proprietary_restriction_categorically_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('a93f497a-f66c-4bf1-9a32-f3a8f88d90a8', proprietary_restriction_categorically_illegitimate, deontological).
narrative_ontology:cs_axiom('a93f497a-f66c-4bf1-9a32-f3a8f88d90a8', secondary, functional_benefit_cannot_redeem_rights_denial).
narrative_ontology:cs_axiom_status(functional_benefit_cannot_redeem_rights_denial, holdable).
narrative_ontology:cs_axiom_grounding('a93f497a-f66c-4bf1-9a32-f3a8f88d90a8', functional_benefit_cannot_redeem_rights_denial, deontological).
narrative_ontology:cs_reference_frame('a93f497a-f66c-4bf1-9a32-f3a8f88d90a8', user_sovereignty_four_freedoms).
narrative_ontology:cs_drift_state('a93f497a-f66c-4bf1-9a32-f3a8f88d90a8', contemporary_saas_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('a93f497a-f66c-4bf1-9a32-f3a8f88d90a8', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, drm_licensing_consortiums).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, proprietary_software_users).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, enterprise_licensees).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, independent_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, proprietary_software_users).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, enterprise_licensees).
narrative_ontology:constraint_vindicates(software_control_legitimacy__freedom_imperative_reading, copyright_incentive_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish software under licenses that withhold source code and impose use, modification, and redistribution restrictions; collect license and subscription revenue; enforce through copyright, clickwrap contract, technical protection measures, and anti-circumvention statutes. They hold the source code outright and can relicense, open, or abandon any product at will, so their position in the arrangement is fully voluntary.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors, beneficiary).

% Administer technical-protection standards (key management, content-protection licensing) that make user modification technically infeasible on compliant devices; collect per-device and per-content licensing fees from implementers. Their revenue exists only insofar as the restriction regime persists, and their patents and standards positions let them exit any single market.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, drm_licensing_consortiums, beneficiary,
    institutional, generational, arbitrage, global).

% Run software they cannot inspect, modify, or lawfully share; accept terms by clickwrap with no negotiation; receive functioning tools, often at zero price, in exchange for surrender of control over their computing and, increasingly, telemetry access to their data. Switching to free alternatives carries format, workflow, and ecosystem costs, and many no longer conceive of modification as a possibility at all.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_users, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__freedom_imperative_reading, proprietary_software_users, beneficiary).

% Buy licenses and support contracts at negotiated scale; receive service-level agreements, security patching, and audit-defensible procurement; pay in license fees, lock-in (proprietary formats, vendor-specific skills), and surrender of internal control over software the organization depends on. Migration projects are measured in years and budget risk, so exit is contemplated but rarely executed.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, enterprise_licensees, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__freedom_imperative_reading, enterprise_licensees, beneficiary).

% Build products and integrations on proprietary platforms under API and marketplace terms set unilaterally by vendors; barred from reverse engineering by contract and statute; face takedowns, terms changes, and platform shifts they cannot contest individually. Coalition action through developer associations and litigation is possible but historically rare and unevenly resourced.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, independent_developers, payer,
    powerless, biographical, constrained, global).

% Maintain the four-freedoms doctrine and a parallel free-software ecosystem; campaign against DRM and for right-to-repair; hold no seat in the licensing conversations where terms are set, because clickwrap admits no counterparty. Their access to the arrangement is limited to refusal, which ecosystem network effects make costly, and to building the alternative they endorse.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, free_software_advocates, excluded,
    organized, generational, constrained, global).

% Need to inspect software to find vulnerabilities; anti-circumvention statutes and license terms bar the inspection itself regardless of purpose, and vendor bounty programs set the terms of what may be examined and disclosed. Their objection, that inspection is a public good, has no seat in license drafting or in the drafting of the statutes that enforce it.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, security_researchers, excluded,
    moderate, biographical, constrained, global).

% Investigate lock-in, self-preferencing, and tying in software markets; can compel interoperability disclosures and data portability in specific jurisdictions; assess the arrangement through market-power lenses that do not adjudicate the freedom claim itself, and so see a different dispute than the one this reading authors.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, competition_regulators, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_control_legitimacy__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools user payments into a single funded development effort: one vendor directs roadmap, quality assurance, security response, and support for widely used software, solving the problem of financing large-scale software development without free-riding on unrecompensed contributors.
% TRANSFER_FUNCTION: Moves license and subscription revenue from users to vendors, and moves decision authority over computing (what runs, what may be inspected, what may interoperate, what data is shared) from users to vendors; what moves back is the compiled artifact and metered services.
% ABSENT_VOICES: Free-software advocates, security researchers, and independent developers would object but hold no seat: clickwrap licensing admits no negotiation, and anti-circumvention law bars the researchers' method before any conversation starts. Users as a class are present only as aggregate click-through consent.
% DISAPPEARANCE_RATIONALE: If restrictive licensing and its enforcement vanished overnight, source availability would become the default, service and support markets would reorganize around the freed code, business models built on restriction would collapse, and the software economy would rearrange around maintenance, hosting, and contribution rather than license rents. The named seats all have arrangements that depend on the regime: vendors on revenue, enterprises on supported procurement, developers on platform terms.
% FOUNDING_PROBLEM: Financing standalone software development: once software separated from hardware sales in the late 1960s and 1970s, developers needed a mechanism to charge for copies and to stop unrestricted redistribution from undercutting the revenue that funds development.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated from outside the beneficiary set: business historians document the unbundling crisis of 1969-1976, and the 1976 Open Letter states the vendors' case in their own words; free-software economists, and the freedom movement itself, attest that development funding is a real problem while disputing that proprietary control is its only solution. The dispute is over the solution, not over the problem's existence, and no party denies the problem was real.
narrative_ontology:disappearance_verdict(software_control_legitimacy__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__freedom_imperative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__freedom_imperative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__freedom_imperative_reading, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.85 from this reading's lights: every use of proprietary software surrenders freedoms the reading holds fundamental, so the burden does not discount for zero price or high quality. Suppression is 0.80 and structural, not internalized: the arrangement persists through copyright, clickwrap contract, anti-circumvention statute, and DRM architecture, with only a minor internalized component (users who no longer conceive of modification as possible). Theater is 0.40: real functions exist (funded development, security response, support), but a growing share of justification language is consent theater and security framing for restriction. Accessibility collapse is 0.48, deliberately moderate: free alternatives exist and work, which is this reading's own central claim, and only network effects and format lock-in partially collapse them. Resistance is 0.70: four decades of organized copyleft, anti-DRM campaigning, and right-to-repair organizing. The measurement series run on one shared grid (1983-2025, eight points) for all three tracked metrics; the suppression_requirement series is authored because the story genuinely tracks enforcement-capacity change: the 1998 step is the anti-circumvention statute, and the post-2013 rise reflects enforcement migrating from law into service architecture. The extractiveness series rises as control extends to the primary personal device (2007) and then to software the user never possesses (2013 onward), plateauing at the authored end-state.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor seat the arrangement computes as coordination it built and voluntarily maintains; its exit is arbitrage-grade, so the derivation places it near the beneficiary end despite its enforcement role. From the user and developer seats the same structure computes as enforced denial of inspect, modify, and share with constrained exit, pushing toward the full-target end. From the enterprise seat it computes as both at once, which is why the dual role is authored: paid-for capability and lock-in arrive in the same contract. The freedom reading's categorical frame predicts the user and developer seats dominate; the enterprise seat's mixed position is exactly where the sibling readings recruit their support. The engine computes these divergences from the structural data; this story authors the structure and does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors and DRM consortiums sit at the beneficiary end: revenue and decision authority flow to them, and their exit is arbitrage-grade. Users and independent developers sit at the target end: they bear the freedom denial, and constrained exit (formats, ecosystems, statute) holds them near the full-target position. Enterprise licensees sit between: genuinely received capability damps the target reading, lock-in and surrendered internal control push toward it. Free-software advocates and security researchers are excluded seats that contest without collecting or paying; competition regulators are analytical. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled by directionality and the global scope of the arrangement, which the engine applies.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, financing standalone software development, is live; what is contested is whether freedom-denial is its necessary price. The mandatrophy resolution cuts both ways. The arrangement is not a dead mandate maintained by inertia: the funding function is real, which blocks a piton reading and blocks any claim that the arrangement is pure theater. But the funding function also cannot launder the categorical denial, which blocks rope and tangled_rope readings from this seat: the snare claim turns on enforcement dependence, since the arrangement persists through statute, contract, and technical protection rather than through participant preference, and the funding story is what makes the coercion durable rather than what makes it legitimate. The reading's own obsolescence risk runs in the opposite direction: if service delivery completes the migration beyond possession, the copyright-centered freedom toolkit ages out, and the imperative must either expand its target set or atrophy into a doctrine about a shrinking class of artifacts. That risk is tracked in the saas_enforcement_migration omega rather than resolved here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexical,
    'This constraint is one reading (freedom_imperative_reading) of the software_control_legitimacy kernel; what would each sibling reading change structurally if instantiated instead?',
    'Author the sibling files and compare victim sets, beneficiary structure, and epsilon over the shared referent (the standing proprietary arrangement); the disagreement is located in the normative status of user control over computing, not in any empirical fact.',
    'property_rights_reading would empty the user victim set (license acceptance as voluntary exchange, epsilon near coordination cost); pragmatic_openness_reading would push classification toward rope or tangled_rope (methodology trade-off); commons_reading would recast victims as governance participants in a negotiable arrangement. This file''s snare verdict is reading-indexed, not topic-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexical, conceptual, 'Reading-indexed classification over a shared kernel; sibling readings are separate constraints.').

omega_variable(
    side_constraint_vs_tradeoff,
    'Is user control over computing a side-constraint that no quantity of other goods (price, quality, innovation, funded development) can purchase, or one value traded against those goods?',
    'Conceptual analysis within the readings'' own texts: the deontological framing of the freedom movement versus welfarist and property-based counterarguments. No empirical data resolves it; it is the location of the kernel dispute itself.',
    'If side-constraint, the snare verdict stands regardless of how much genuine coordination benefit the arrangement delivers; if tradable, the arrangement reclassifies toward tangled_rope and epsilon drops materially, since part of the measured burden becomes the price of funded development rather than rights denial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(side_constraint_vs_tradeoff, conceptual, 'Whether the freedom premise is categorical or a trade-off term.').

omega_variable(
    experiential_vs_rights_harm,
    'Does the denial of inspect/modify/share impose harms users actually experience, or is the burden a rights-indexed quantity that is insensitive to felt experience?',
    'Measure how many users would exercise source access, modification, and redistribution rights if granted: patch-culture participation studies, right-to-repair demand data, enterprise source-escrow usage.',
    'Low exercise rates would not lower this reading''s epsilon (the burden is rights-indexed, not welfare-indexed) but would weaken its political traction and narrow the gap to the pragmatic sibling''s assessment; high exercise rates would confirm the reading''s harm claims and harden the snare verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(experiential_vs_rights_harm, empirical, 'Whether the measured burden is experiential or rights-indexed.').

omega_variable(
    saas_enforcement_migration,
    'Is the arrangement''s coercive core migrating beyond copyright into service delivery (software accessed rather than possessed), where copyright-based freedom remedies are structurally inapplicable?',
    'Track the revenue and user share of service-delivered versus possessed software; legal analysis of whether anti-circumvention and contract law reach service substitution; observe whether the movement''s own doctrine expands (its Service-as-a-Software-Substitute critique signals acknowledged drift).',
    'If migration succeeds, the freedom imperative''s constraint set must expand beyond the four-freedoms toolkit or the reading''s reference frame becomes permanently incomplete; the suppression trajectory would keep rising without any change in legal enforcement, because enforcement would have moved into architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saas_enforcement_migration, empirical, 'Whether enforcement migrates from law to service architecture, outflanking the freedom toolkit.').

omega_variable(
    user_coalition_viability,
    'Can dispersed users and the powerless developer seat convert numbers into coalition power (right-to-repair legislation, collective litigation, procurement standards, developer associations)?',
    'Track right-to-repair legislative outcomes, class-action history on license terms, and public-procurement free-software mandates across jurisdictions.',
    'Viable coalitions would raise the arrangement''s enforcement costs and resistance levels, potentially degrading the snare toward a contested tangled_rope in which users extract concessions; persistent coalition failure would confirm the exit-constrained, unorganized structure this story authors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_coalition_viability, empirical, 'Whether the victim class can organize against the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__freedom_imperative_reading, 1983, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1983, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1983, 0.18).
narrative_ontology:measurement_basis(soft_tr_t1983, observed).
narrative_ontology:measurement(soft_tr_t1991, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1991, 0.22).
narrative_ontology:measurement_basis(soft_tr_t1991, observed).
narrative_ontology:measurement(soft_tr_t1998, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1998, 0.28).
narrative_ontology:measurement_basis(soft_tr_t1998, observed).
narrative_ontology:measurement(soft_tr_t2003, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2003, 0.32).
narrative_ontology:measurement_basis(soft_tr_t2003, observed).
narrative_ontology:measurement(soft_tr_t2007, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2007, 0.38).
narrative_ontology:measurement_basis(soft_tr_t2007, observed).
narrative_ontology:measurement(soft_tr_t2013, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2013, 0.4).
narrative_ontology:measurement_basis(soft_tr_t2013, observed).
narrative_ontology:measurement(soft_tr_t2019, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2019, 0.39).
narrative_ontology:measurement_basis(soft_tr_t2019, observed).
narrative_ontology:measurement(soft_tr_t2025, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2025, 0.4).
narrative_ontology:measurement_basis(soft_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t1983, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1983, 0.7).
narrative_ontology:measurement_basis(soft_be_t1983, observed).
narrative_ontology:measurement(soft_be_t1991, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1991, 0.72).
narrative_ontology:measurement_basis(soft_be_t1991, observed).
narrative_ontology:measurement(soft_be_t1998, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1998, 0.76).
narrative_ontology:measurement_basis(soft_be_t1998, observed).
narrative_ontology:measurement(soft_be_t2003, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2003, 0.78).
narrative_ontology:measurement_basis(soft_be_t2003, observed).
narrative_ontology:measurement(soft_be_t2007, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2007, 0.82).
narrative_ontology:measurement_basis(soft_be_t2007, observed).
narrative_ontology:measurement(soft_be_t2013, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2013, 0.84).
narrative_ontology:measurement_basis(soft_be_t2013, observed).
narrative_ontology:measurement(soft_be_t2019, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2019, 0.85).
narrative_ontology:measurement_basis(soft_be_t2019, observed).
narrative_ontology:measurement(soft_be_t2025, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2025, 0.85).
narrative_ontology:measurement_basis(soft_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1983, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1983, 0.45).
narrative_ontology:measurement_basis(soft_su_t1983, observed).
narrative_ontology:measurement(soft_su_t1991, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1991, 0.5).
narrative_ontology:measurement_basis(soft_su_t1991, observed).
narrative_ontology:measurement(soft_su_t1998, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1998, 0.62).
narrative_ontology:measurement_basis(soft_su_t1998, observed).
narrative_ontology:measurement(soft_su_t2003, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2003, 0.68).
narrative_ontology:measurement_basis(soft_su_t2003, observed).
narrative_ontology:measurement(soft_su_t2007, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2007, 0.72).
narrative_ontology:measurement_basis(soft_su_t2007, observed).
narrative_ontology:measurement(soft_su_t2013, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2013, 0.76).
narrative_ontology:measurement_basis(soft_su_t2013, observed).
narrative_ontology:measurement(soft_su_t2019, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2019, 0.78).
narrative_ontology:measurement_basis(soft_su_t2019, observed).
narrative_ontology:measurement(soft_su_t2025, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2025, 0.8).
narrative_ontology:measurement_basis(soft_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__freedom_imperative_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, commons_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'software control legitimacy' decomposes into four structurally distinct readings of one kernel. This file instantiates the freedom-imperative reading only: the standing proprietary arrangement authored as a categorical rights violation, with epsilon reading-indexed over the shared referent (the proprietary arrangement as this reading assesses it, never the free-software arrangement this reading endorses). The property-rights reading authors the same referent as legitimate creator control; the pragmatic reading authors it as a methodology trade-off; the commons reading authors it as negotiated governance. Per the epsilon-invariance principle no single story averages across readings; the sibling files carry their own epsilon, beneficiary structures, and claimed types, and this family is linked through network edges for contamination and cross-reading comparison.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
