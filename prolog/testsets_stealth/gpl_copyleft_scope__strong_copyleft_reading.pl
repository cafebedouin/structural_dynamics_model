% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__strong_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: GPL Strong Copyleft Scope — Coupling-Creates-Derivative Reading
 *   domain: economic/technological/legal
 *
 * SUMMARY:
 *   This story instantiates the strong_copyleft_reading of the
 *   gpl_copyleft_scope kernel: the claim that GPL Section 2(b) reaches any
 *   combined or dynamically linked work, so the derivative-work boundary
 *   extends to all forms of code coupling. The ε referent is the standing
 *   arrangement under contest — the regime in which linking proprietary code
 *   to GPL components obligates GPL licensing of the whole — assessed
 *   descriptively by this reading's own lights, never the narrow-scope
 *   arrangement it argues against. The manifest hypothesized a high-epsilon
 *   snare; this story authors tangled_rope instead, because the reading's
 *   coordination function (guaranteeing that code built on GPL components
 *   stays available) is genuine and sincerely pursued rather than cover,
 *   while asymmetric extraction on proprietary seats is real and actively
 *   enforced. The snare-shaped experience is a property of the trapped vendor
 *   seats, which the engine computes per-seat from power and exit data; the
 *   manifest hypothesis is preserved in uke_scope and the divergence is
 *   deliberate, not reconciled. Constraint family: narrow_scope_reading and
 *   enforcement_vacuum_reading are sibling constraints over the same license
 *   text, linked through network.affects_constraints; their ε values differ
 *   because they draw different obligation boundaries over the same kernel,
 *   not because the same constraint is measured two ways.
 *
 * KEY AGENTS:
 *   - fsf_aligned_copyright_holders: Agenda-setter and partial beneficiary (institutional/mobile) — hold copyright in core GPL works, publish the strong-scope interpretation, run enforcement, collect settlements and compliance.
 *   - copyleft_development_communities: Primary beneficiary (organized/constrained) — contribute to and maintain GPL projects; receive the enclosure guarantee the strong reading provides.
 *   - downstream_source_users: Beneficiary (moderate/mobile) — receive source-availability guarantees for GPL-derived systems without paying into enforcement.
 *   - dual_license_commercial_vendors: Contingent beneficiary (organized/mobile) — monetize proprietary exceptions whose market exists only while the strong reading is credible.
 *   - proprietary_software_vendors: Primary target (powerful/constrained) — bear source-release or rewrite costs when integrating GPL components; can contest but not cheaply exit.
 *   - embedded_device_manufacturers: Primary target (moderate/trapped) — ship firmware fused with GPL components; post-shipment compliance demands meet no viable exit.
 *   - courts_and_legislatures: Excluded institutional seat — could authoritatively fix the derivative-work boundary but have never definitively ruled.
 *   - licensing_scholars: Analytical observer — map the contest and its enforcement dynamics without collecting or paying.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.67).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.5).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Strong Copyleft Scope — Coupling-Creates-Derivative Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "economic/technological/legal").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, 'a6adb193-1671-463c-be1d-e35a835f8864').
narrative_ontology:cs_kernel_codification('a6adb193-1671-463c-be1d-e35a835f8864', fixed_text).
narrative_ontology:cs_authority_grounding('a6adb193-1671-463c-be1d-e35a835f8864', lineage).
narrative_ontology:cs_interpretation_layer_present('a6adb193-1671-463c-be1d-e35a835f8864').
narrative_ontology:cs_reading_relation('a6adb193-1671-463c-be1d-e35a835f8864', gpl_copyleft_scope__narrow_scope_reading, forecloses).
narrative_ontology:cs_reading_relation('a6adb193-1671-463c-be1d-e35a835f8864', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('a6adb193-1671-463c-be1d-e35a835f8864', foundational, code_coupling_constitutes_derivation).
narrative_ontology:cs_axiom_status(code_coupling_constitutes_derivation, holdable).
narrative_ontology:cs_axiom_grounding('a6adb193-1671-463c-be1d-e35a835f8864', code_coupling_constitutes_derivation, conventional).
narrative_ontology:cs_axiom('a6adb193-1671-463c-be1d-e35a835f8864', secondary, software_freedom_requires_copyleft_closure).
narrative_ontology:cs_axiom_status(software_freedom_requires_copyleft_closure, holdable).
narrative_ontology:cs_axiom_grounding('a6adb193-1671-463c-be1d-e35a835f8864', software_freedom_requires_copyleft_closure, deontological).
narrative_ontology:cs_reference_frame('a6adb193-1671-463c-be1d-e35a835f8864', copyleft_closure_over_coupled_works).
narrative_ontology:cs_drift_state('a6adb193-1671-463c-be1d-e35a835f8864', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a6adb193-1671-463c-be1d-e35a835f8864', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, copyleft_development_communities).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, downstream_source_users).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, dual_license_commercial_vendors).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, fsf_aligned_copyright_holders).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, embedded_device_manufacturers).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__strong_copyleft_reading, software_freedom_doctrine).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__strong_copyleft_reading, copyleft_reciprocity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold copyright in core GPL-licensed works and steward the license text. Publish interpretations holding that linking a proprietary program to GPL code creates a combined work that must be GPL-licensed. Fund enforcement through licensing-violation complaints and receive negotiated settlements and compliance agreements. Could de-emphasize enforcement at will, though the GPLv2-only status of flagship projects makes unilateral relicensing impractical.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, fsf_aligned_copyright_holders, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__strong_copyleft_reading, fsf_aligned_copyright_holders, beneficiary).

% Contribute to and maintain GPL-licensed projects under the assurance that improvements built on their work cannot be closed off. When the broad scope reading holds, any vendor linking against their code must publish sources, which flows improvements back upstream. Their alternative — migrating projects to permissive licenses — requires tracking down and renegotiating with every contributor, so they stay with the license they have.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, copyleft_development_communities, beneficiary,
    organized, generational, constrained, global).

% Run and build on GPL-licensed systems and receive the guarantee that the combined systems they depend on ship with source. They pay nothing into the enforcement machinery and can switch to permissively licensed stacks at any time, though comparable permissive alternatives do not exist in every domain.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, downstream_source_users, beneficiary,
    moderate, biographical, mobile, global).

% Sell proprietary-use licenses for code they also release under the GPL. Their commercial revenue exists because customers believe the broad scope reading makes the GPL obligations risky enough to buy exceptions. If the narrow reading prevailed across the industry, the market for their exceptions would shrink sharply. They do not run enforcement themselves but benefit from its credibility.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, dual_license_commercial_vendors, beneficiary,
    organized, biographical, mobile, global).

% Build products that combine or dynamically link against GPL components. Under the broad scope reading they must either release the source of the combined work, which can expose their entire codebase, or engineer the GPL component out at substantial cost. They have legal resources to contest the reading and some maintain internal policies assuming the narrow reading; leaving the GPL component behind is a multi-year engineering project.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Ship consumer and industrial hardware running firmware built on GPL components, typically the Linux kernel. The firmware is fused with their product line: devices are already in the field, the codebase cannot be unwound without redesigning the product, and their engineering organizations are built around the existing stack. Compliance demands arrive after shipment, when the remaining options are publishing source or fighting.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, embedded_device_manufacturers, payer,
    moderate, biographical, trapped, global).

% Hold the authority to fix the derivative-work boundary for code coupling but have never definitively ruled on it; cases settle, turn on procedural grounds, or avoid the question. Their absence is what keeps the interpretive contest open and lets enforcement capacity stand in for doctrine.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, courts_and_legislatures, excluded,
    institutional, generational, constrained, national).

% Study and publish on the copyleft scope question from law faculties and policy institutes. They map the interpretive positions, track enforcement outcomes, and advise both vendor counsel and community projects. They collect no compliance value and bear none of its costs.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, licensing_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__strong_copyleft_reading, copyleft_development_communities).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__strong_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The strong reading solves a real collective-action problem: it prevents proprietary enclosure of collectively-built code by guaranteeing that anyone who builds on GPL components passes the freedom obligations downstream. It converts dispersed individual contributions into a protected commons that no single integrator can close.
% TRANSFER_FUNCTION: Moves source-disclosure obligations and compliance costs from proprietary integrators (software vendors, embedded manufacturers) to the benefit of the free software commons; moves licensing revenue from fear of the broad scope to dual-licensing commercial vendors; moves enforcement settlements to copyright-holding nonprofits.
% ABSENT_VOICES: Courts and legislatures — the only seats that could authoritatively fix the derivative-work boundary — have never definitively entered the conversation, so the contest is adjudicated by enforcement capacity and community norms. End users of embedded devices, who bear the freedom loss when firmware ships closed, also have no seat.
% DISAPPEARANCE_RATIONALE: If the strong reading vanished overnight, proprietary integration of GPL components would surge under narrow-reading assumptions, dual-licensing revenue models would collapse, the enclosure guarantee underlying contributor participation would dissolve, and large parts of the free software ecosystem would reorganize around permissive licensing or defensive relicensing efforts.
% FOUNDING_PROBLEM: Proprietary vendors were absorbing freely available code, improving it, and shipping closed derivatives without returning anything — converting a commons into private product. Copyleft was built to make freedom self-enforcing: any work derived from the code must remain free, so improvement flows back instead of being enclosed.
% FOUNDING_PROBLEM_CORROBORATION: The enclosure problem is attested outside the beneficiary set: academic histories of the free software movement document the 1980s appropriation conflicts with participation from the appropriating side; industry analysts and the vendors' own product histories show ongoing non-upstreamed forks of permissively-licensed code (demonstrating enclosure pressure is real even without copyleft); and corporate open-source policies across the industry acknowledge code-appropriation risk as a live concern.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__strong_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__strong_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__strong_copyleft_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_copyleft_scope__strong_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__strong_copyleft_reading, 0.67, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.67) because the reading sweeps entire proprietary codebases into source-release obligations whose cost is decoupled from any service rendered in exchange; the extraction is conditional on voluntary adoption of GPL components, which tempers it below pure-snare levels. Suppression (0.50) is moderate and authored as a raw structural property — the engine scales only extractiveness, by directionality and scope: permissive alternatives exist in many domains and adoption of GPL code is voluntary, but kernel- and libc-class components have no comparable permissive substitute, and post-integration exit means multi-year rewrites. Theater is low (0.22): enforcement demands real source releases, though compliance-audit ritual has grown as enforcement institutionalized. Accessibility collapse (0.40) is low because counter-readings and workarounds persist; resistance (0.60) is high because industry actively maintains narrow-reading positions. The three tracked metrics share one eight-point grid (interval 0–35 mapping to 1991–2026): extractiveness rises with the embedded-Linux integration wave and enforcement wave, then plateaus as cooperation commitments institutionalize; theater drifts up slowly with compliance formalization; suppression_requirement is authored because the story specifically traces enforcement-capacity change — build-up through the cease-and-desist and litigation eras, peak around the enforcement-suit years, then partial institutionalization into negotiation-first commitments.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute opposite constraints from the same clause. From embedded_device_manufacturers — trapped, firmware fused with the product line, moderate power — the strong reading operates as near-total extraction with no viable exit, a snare-shaped seat experience. From copyleft_development_communities — net beneficiaries with a real enclosure guarantee — the same clause operates as coordination they defend voluntarily, a rope-shaped seat experience. proprietary_software_vendors sit between: powerful enough to contest and to maintain narrow-reading internal policies, constrained enough that integration decisions made years ago now bind them. dual_license_commercial_vendors hold a contingent beneficiary seat: their revenue exists only while the strong reading stays credible, giving them a structural interest in enforcement they do not themselves administer. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (development communities, downstream users, dual-licensers, FSF-aligned holders) drive d toward the beneficiary end for those seats; victim declarations (proprietary vendors, embedded manufacturers) drive d toward the full-target end, amplified for embedded manufacturers by trapped exit and product-line fusion. FSF-aligned holders are dual-positioned: agenda-setters who also collect settlements, so their derived d sits near but not at the beneficiary end. Downstream users sit near the beneficiary end with negligible payment. No directionality overrides are used — beneficiary/victim declarations plus exit options produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — proprietary appropriation of commons code — is live, so no mandatrophy is declared. The classification work this story does is boundary-keeping between two mislabels: a snare verdict (the manifest's hypothesis) would erase the genuine coordination function that millions of contributors voluntarily sustain; a rope verdict would erase the asymmetric extraction that trapped embedded manufacturers demonstrably bear. Tangled rope holds both halves: real coordination, asymmetric payment, active enforcement. On the R5 mismatch check, founding_problem_status=live combined with disappearance_verdict=world_rearranges raises no capture/zombie flag — the arrangement persists because the problem persists, not because anyone administers a dead mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story is one reading (strong_copyleft_reading) of the gpl_copyleft_scope kernel; what would the sibling readings change structurally, and where exactly does the disagreement bind?',
    'Definitive appellate precedent on whether linking creates a derivative work for license purposes, or an authoritative license-steward restatement adopted across the enforcement community.',
    'Under narrow_scope_reading the victim set shrinks to direct derivatives, ε drops well below 0.4, and proprietary vendors exit the target seats; under enforcement_vacuum_reading the classification becomes context-dependent on enforcement capacity rather than a property of the clause.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; disagreement located at the linking-equals-derivation premise.').

omega_variable(
    derivative_work_doctrine_resolution,
    'Does copyright doctrine actually support the premise that dynamic linking produces a derivative work, or does the strong reading over-claim beyond what copyright law grants?',
    'Definitive case law on software linking and derivative-work status; secondary: convergent doctrinal scholarship.',
    'If courts reject the premise, the reading''s enforcement threats lose legal foundation and its extraction collapses toward the vacuum reading''s capacity-dependent baseline; if affirmed, the reading hardens into enforceable doctrine and vendor seats compute higher effective extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(derivative_work_doctrine_resolution, empirical, 'Legal status of the linking-creates-derivation premise.').

omega_variable(
    enforcement_capacity_dependence,
    'Does the strong reading''s operative force track enforcement capacity rather than license text — does the constraint bind only where FSF-aligned holders can credibly pursue violations?',
    'Compare compliance rates for GPL components held by enforcement-active copyright holders versus equivalent components held by dispersed or inactive holders, controlling for project prominence.',
    'If capacity-dependent, the constraint operates with full force only inside enforcement-reachable contexts and as theater elsewhere; the vacuum reading would then describe the real mechanism rather than a rival interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_dependence, empirical, 'Whether operative scope is a function of enforcement capacity.').

omega_variable(
    voluntary_adoption_extraction_status,
    'Is the extraction borne by vendors a compelled transfer, or the agreed price of a voluntary bargain entered with adequate knowledge of the reading''s blast radius?',
    'Vendor integration-decision records and surveys: did adopters understand the strong scope at integration time, and were permissive equivalents available at comparable quality in their domain?',
    'If adoption was informed with real alternatives, ε is overstated and the vendor seats approach symmetric exchange; if adopters were unaware or alternatives absent in kernel-class domains, extraction is real and the authored ε may even be understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_adoption_extraction_status, conceptual, 'Whether vendor-side extraction is bargain price or compelled transfer.').

omega_variable(
    scope_necessity_for_commons,
    'Is the strong scope necessary for the enclosure-prevention coordination function, or would the narrow reading''s coverage of direct derivatives suffice?',
    'Compare enclosure and non-upstreaming rates between copyleft projects and functionally comparable permissively-licensed projects, and between GPL libraries (strong scope) and LGPL libraries (weakened scope).',
    'If narrow scope suffices, the reading''s excess extraction over the narrow baseline is leverage rather than coordination cost, pushing the classification toward the manifest''s snare hypothesis; if not, the excess extraction is the price of the commons and tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_necessity_for_commons, empirical, 'Separability of the strong scope from the coordination function it funds.').

omega_variable(
    cs_framing_text_vs_tradition,
    'Is the kernel best framed as the fixed license text (authority grounded in the steward''s lineage) or as the interpretive tradition of the free-software movement (authority grounded in community practice)?',
    'Test which framing predicts enforcement behavior: if enforcement follows the text''s plain scope across stewards, the text framing holds; if it follows movement consensus where the text is ambiguous, the tradition framing holds.',
    'Under the tradition framing, authority_grounding shifts from lineage to practice and the reading''s reference frame becomes movement consensus rather than the license text, changing the drift vector''s anchor and potentially the cs_pattern verdict.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_text_vs_tradition, conceptual, 'CS framing under-determination: text-kernel versus tradition-kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t5, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement_basis(gpl__tr_t5, observed).
narrative_ontology:measurement(gpl__tr_t10, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement_basis(gpl__tr_t10, observed).
narrative_ontology:measurement(gpl__tr_t15, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement_basis(gpl__tr_t15, observed).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(gpl__tr_t20, observed).
narrative_ontology:measurement(gpl__tr_t25, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 25, 0.19).
narrative_ontology:measurement_basis(gpl__tr_t25, observed).
narrative_ontology:measurement(gpl__tr_t30, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement_basis(gpl__tr_t30, observed).
narrative_ontology:measurement(gpl__tr_t35, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 35, 0.22).
narrative_ontology:measurement_basis(gpl__tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t5, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement_basis(gpl__be_t5, observed).
narrative_ontology:measurement(gpl__be_t10, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(gpl__be_t10, observed).
narrative_ontology:measurement(gpl__be_t15, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(gpl__be_t15, observed).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(gpl__be_t20, observed).
narrative_ontology:measurement(gpl__be_t25, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement_basis(gpl__be_t25, observed).
narrative_ontology:measurement(gpl__be_t30, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(gpl__be_t30, observed).
narrative_ontology:measurement(gpl__be_t35, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 35, 0.67).
narrative_ontology:measurement_basis(gpl__be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t5, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 5, 0.25).
narrative_ontology:measurement_basis(gpl__su_t5, observed).
narrative_ontology:measurement(gpl__su_t10, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement_basis(gpl__su_t10, observed).
narrative_ontology:measurement(gpl__su_t15, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(gpl__su_t15, observed).
narrative_ontology:measurement(gpl__su_t20, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(gpl__su_t20, observed).
narrative_ontology:measurement(gpl__su_t25, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(gpl__su_t25, observed).
narrative_ontology:measurement(gpl__su_t30, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(gpl__su_t30, observed).
narrative_ontology:measurement(gpl__su_t35, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 35, 0.5).
narrative_ontology:measurement_basis(gpl__su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__narrow_scope_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__enforcement_vacuum_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, lgpl_library_linking_exception).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, agpl_network_use_scope).

% DUAL FORMULATION NOTE:
% The natural-language label 'GPL copyleft scope' decomposes into a three-reading constraint family over one kernel (gpl_copyleft_scope): this strong reading (coupling of any form obligates the whole), narrow_scope_reading (traditional copyright boundary), and enforcement_vacuum_reading (capacity-determined operative scope). The siblings are separate constraints with separate ε values, not observables over one constraint: the strong reading draws the obligation boundary at the linker, the narrow reading at the direct derivative, and the vacuum reading at the enforcement perimeter. The upstream/downstream structure runs through enforcement: each enforcement action and license revision under this reading changes the terrain on which the vacuum reading operates, while the LGPL and AGPL stories are structural downstream responses (a scoped relaxation and a network-use extension respectively).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
