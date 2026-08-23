% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__sovereignty_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: Security Council Permanent-Member Consent Guarantee (Westphalian Sovereignty Reading)
 *   domain: international_relations/institutional_design
 *
 * SUMMARY:
 *   Article 27(3) of the UN Charter gives each of the five permanent Security
 *   Council members the right to block any non-procedural resolution. This
 *   story instantiates the sovereignty reading of that arrangement: the veto
 *   is the Charter's operational expression of the Westphalian principle that
 *   no state can be bound by international law without its consent, extended
 *   to states whose global-reach enforcement capacity makes that principle
 *   physically binding rather than declaratory. On this reading the veto adds
 *   nothing to the underlying distribution of coercive capacity — a great
 *   power that refuses consent cannot be compelled with or without Article 27
 *   — so it collects no rents, extracts nothing, and functions as
 *   constitutional recognition of an impossibility. It is claimed here as
 *   mountain: a structural feature that would reappear in any successor
 *   institution empowered to command great powers. KEY AGENTS (by structural
 *   relationship): p5_permanent_members (institutional/arbitrage) — holders
 *   and maintainers of the consent guarantee; non_p5_member_states
 *   (organized/constrained) — constitutionally consented, procedurally
 *   sidelined majority; general_assembly_organ (institutional/mobile) —
 *   registers dissent, binds no one; un_secretariat (moderate/constrained) —
 *   absorbs operational consequences of stopped files;
 *   international_legal_scholarship (analytical/analytical) — observes the
 *   full structure. This file is one member of a three-story family
 *   decomposing the colloquial label 'P5 veto': the coordination sibling's
 *   referent prices a war-prevention service, the oligopoly sibling's
 *   referent prices entrenched authority rents, and this file's referent
 *   prices recognition of a capacity boundary — the epsilon values differ
 *   across the family by construction and are not averaged here.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.09).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.14).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.09).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.14).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "Security Council Permanent-Member Consent Guarantee (Westphalian Sovereignty Reading)").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "international_relations/institutional_design").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, '75858a2a-2b8c-46ea-ac57-f79c6abc6065').
narrative_ontology:cs_kernel_codification('75858a2a-2b8c-46ea-ac57-f79c6abc6065', fixed_text).
narrative_ontology:cs_authority_grounding('75858a2a-2b8c-46ea-ac57-f79c6abc6065', lineage).
narrative_ontology:cs_interpretation_layer_present('75858a2a-2b8c-46ea-ac57-f79c6abc6065').
narrative_ontology:cs_reading_relation('75858a2a-2b8c-46ea-ac57-f79c6abc6065', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('75858a2a-2b8c-46ea-ac57-f79c6abc6065', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_axiom('75858a2a-2b8c-46ea-ac57-f79c6abc6065', foundational, binding_requires_governed_consent).
narrative_ontology:cs_axiom_status(binding_requires_governed_consent, holdable).
narrative_ontology:cs_axiom_grounding('75858a2a-2b8c-46ea-ac57-f79c6abc6065', binding_requires_governed_consent, conventional).
narrative_ontology:cs_axiom('75858a2a-2b8c-46ea-ac57-f79c6abc6065', secondary, obligation_cannot_exceed_enforceable_capacity).
narrative_ontology:cs_axiom_status(obligation_cannot_exceed_enforceable_capacity, holdable).
narrative_ontology:cs_axiom_grounding('75858a2a-2b8c-46ea-ac57-f79c6abc6065', obligation_cannot_exceed_enforceable_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('75858a2a-2b8c-46ea-ac57-f79c6abc6065', westphalian_consent_bound_law).
narrative_ontology:cs_drift_state('75858a2a-2b8c-46ea-ac57-f79c6abc6065', contemporary_multipolarity, gap(stable, minor, true)).
narrative_ontology:cs_created_at('75858a2a-2b8c-46ea-ac57-f79c6abc6065', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, westphalian_consent_principle).
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, sovereign_equality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five states holding the concurrent right to block any non-procedural Security Council decision. Each obtained the right in the 1945 settlement and maintains it by declining to ratify any Charter amendment that would alter it. When the Council's direction conflicts with a vital interest, the blocking right converts disagreement into procedural stoppage; when the Council is unusable for a purpose, each can pursue that purpose through national instruments or ad hoc coalitions outside the organization, as several have done. Nothing obliges them to obtain from the Council what they cannot otherwise secure for themselves.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, p5_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% The large majority of the organization's membership, most admitted long after the founding bargain was sealed. Their consent to be bound entered once, at accession, covering the Charter scheme as a whole rather than each exercise of authority; they can be bound by Chapter VII resolutions they voted against. They debate, vote, finance, and staff the Council but cannot convert any majority into an outcome a permanent member opposes, and they hold no seat in the amendment process that could reopen the terms. Their outlets are Assembly resolutions, budget leverage, and normative campaigns.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, non_p5_member_states, excluded,
    organized, generational, constrained, global).

% The membership meeting as a body, wielding resolution power that recommends but does not bind. After each blocking decision it convenes mandated public debate under the 2022 veto initiative, and it has historically pulled deadlocked files to itself under the 1950 Uniting for Peace mechanism. It registers dissent the Council cannot absorb and keeps a public record of every cast, but its outputs carry no enforcement force against any member.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, general_assembly_organ, observer,
    institutional, biographical, mobile, global).

% The organization's career staff, who draft, mediate, and implement Council decisions. When a blocking decision halts a file, the Secretariat absorbs the operational consequence — missions planned against resolutions that never arrive, mandates stretched past their authorization — while continuing to serve every principal including the ones who blocked. It advocates quietly for workable process but holds no decision rights and no exit from the system it serves.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, un_secretariat, observer,
    moderate, biographical, constrained, global).

% Analysts of the Charter system in law faculties and research institutes, who observe the full structure: the consent logic of Article 27, the amendment lockout of Article 108, the difference in consent granularity between the five and the general membership, and the complete behavioral record of blocking decisions since 1946. They publish classifications and counterfactuals and supply the evidentiary base any renegotiation argument would rest on, with no stake in any particular outcome.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, international_legal_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__sovereignty_reading, diffuse).
narrative_ontology:fixing_cost_class(article_27_veto_power__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns the organization's binding authority with the consent of members capable of resisting enforcement: no resolution commanding a permanent member issues without that member's acquiescence, so the Council's legal form never outruns its executable reach. Whether this constitutes a service rendered to the membership or merely an honest registry of the capacity boundary is not asserted here; the arrangement's observable effect is that the body issues only commands its strongest members have consented to.
% TRANSFER_FUNCTION: Nothing material moves through this mechanism. What it allocates is decision rights: each permanent member holds an individual switch over the body's entire binding agenda, and the general membership receives in exchange a predictable boundary — files touching great-power vital interests stop at the procedural layer rather than surfacing as commands that would then be defied.
% ABSENT_VOICES: States bearing the consequences of blocked files — populations under attack where a blocking decision stalls a mandate or delays sanctions relief — have no procedural seat from which to contest a specific cast; they appear only as subjects of debate. Most of today's membership never faced the founding bargain and would renegotiate its consent terms if the amendment door opened; their objection is voiced in chambers that cannot reopen it. Seats that read the arrangement as insurance or as rent hold those positions in the sibling files; this file records only what the consent structure itself contains. Commentary-grade: none of these absences adjusts classification.
% DISAPPEARANCE_RATIONALE: Remove the concurrence requirement overnight and the body begins issuing binding commands its strongest members have not consented to; the commands would be disregarded, the institution's authority would erode along the League's path, and institutional form would churn for years. But the underlying boundary — great powers cannot be compelled — would be exactly where it stood before, which is this reading's reason for treating the arrangement as recognition of a fact rather than construction of one. Paper arrangements scramble; the governed reality does not move.
% FOUNDING_PROBLEM: The League of Nations collapsed when its covenant commanded collective action its great-power members declined to take, demonstrating that an institution which outruns its members' consent loses the members it cannot afford to lose. The Charter's drafters built the permanent-member concurrence term so the new organization would never again issue orders its indispensable members had not accepted, purchasing guaranteed great-power presence at the price of majority rule.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside any benefiting circle — and under this reading there is no benefiting circle positioned to attest for itself. The League's collapse is established diplomatic history written independently of the five; the San Francisco delegation records show non-great-power delegations accepting the concurrence term as the explicit, stated price of great-power participation; and every subsequent episode of great-power defiance of unconsented command, together with the fate of institutions that attempted to issue such commands, corroborates that the problem persists. No credible source attests the founding problem dead.
narrative_ontology:disappearance_verdict(article_27_veto_power__sovereignty_reading, world_unchanged).
narrative_ontology:founding_problem_status(article_27_veto_power__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_27_veto_power__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__sovereignty_reading, 0.09, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_27_veto_power__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.09: the veto transfers no resources and compels no one; the costs non-permanent states experience when a blocking decision halts a file they supported are the shadow of enforcement-capacity asymmetry that predates and would survive the Charter — the mechanism converts that asymmetry from open defiance into orderly procedure rather than imposing it. The residual value tracks the slow widening of the gap between majoritarian expectation and consent-bounded possibility as post-Cold War intervention norms raised what majorities expect the body to deliver. Suppression 0.14: Charter amendment requires ratification by all five (Article 108), foreclosing formal reform, but on consent logic that lockout is the principle applied consistently to its own revision rather than coercion aimed at available alternatives; no alternative to the veto exists that capacity leaves open. Theater 0.22: most casts perform their function — stopping binding action — while a growing minority are symbolic signals on files that would have failed ratification or implementation regardless, lifting the ratio gradually across the interval. Accessibility collapse 0.82: once the consent-bounding structure is understood, institutional alternatives — councils that bind the unwilling, enforcement without great-power acquiescence — collapse almost entirely; the League is the standing demonstration. Resistance 0.30: organized diplomatic resistance exists (Assembly veto-debate mandates, restraint-code campaigns) but aims at conduct around the mechanism rather than at the structure, and the structure absorbs it without strain. The claimed type and the metrics are authored independently: the mountain claim rests on the structural argument, the metric values on descriptive operation, and any engine divergence between them is retained as measurement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same two articles. From the permanent-member seat the arrangement is a self-evident constitutional feature: it guarantees the organization can never convert its majority against them, and their arbitrage exit — acting outside the body entirely, as several have — confirms it restrains nothing they could not already refuse. From the elected-member and general-membership seats the same text is a permanently locked door: they debate, vote, fund, and staff the institution yet cannot convert any majority into binding action over a permanent member's objection, nor reopen the bargain that set the terms. The analytical seat sees additionally the asymmetry the text encodes: ratification-level consent for the many, per-exercise consent for the five. The engine derives these divergences from the power and exit atoms authored on the stakeholder surface; this file's claim does not adjudicate between the seats' perceptions.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim declarations are authored — deliberately, because that absence IS this reading's structural content. The mechanism neither subsidizes nor taxes anyone: it recognizes a pre-existing capacity boundary, so no seat sits at either end of the directionality axis, and the derivation chain, finding no structural declarations to read, falls back toward per-atom defaults approximating symmetry. Explicit overrides were considered and withheld on purpose: pinning the permanent members toward the beneficiary end would smuggle the oligopoly reading's rent claim into this file; pinning the general membership toward the target end would smuggle the extraction claim the sovereignty reading denies. If the engine's per-seat computations diverge from symmetry, that divergence is measurement taken against this reading's claim — exactly the comparison the constraint family exists to support.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an institution that issues binding commands its indispensable members will not obey collapses — remains live: enforcement-capacity asymmetry has deepened since 1945 rather than faded, so mandate and function coincide and no sunset clause applies. The classification guards against mislabeling in both directions: reading the arrangement as pure extraction would require identifiable rents, which this reading's structural data deny exist; reading it as voluntary coordination service would require net-benefit participation the consent logic does not assert — the five remain because departure means institutional irrelevance, not because the arrangement delivers them a benefit they could decline. The receipt surface records a piton-shaped cell (no seat captures gains; fixing is prohibitive), and that cell is authored as descriptive fact: gains are diffuse-to-nonexistent because the mechanism transfers nothing, and fixing is prohibitive because revision requires the consent of exactly the states revision would constrain. Against the cell stand a continuously performed function and a sub-majority theater ratio, which argue against atrophy; if the engine weighs the receipt cell toward piton, the divergence is kept as corpus data rather than reconciled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Which structurally distinct constraint does the colloquial label ''P5 veto'' denote in a given analysis — consent-instantiation (this file), war-prevention mechanism (coordination reading), or entrenched oligopoly rent (oligopoly reading)?',
    'Cross-file selection within the constraint family: each sibling instantiates its own epsilon referent and its own beneficiary structure, and an analysis must adopt the reading whose referent matches the arrangement actually under evaluation rather than averaging across the kernel.',
    'Selecting a different reading changes epsilon, beneficiary/victim structure, and claimed type wholesale; this file''s near-zero epsilon and mountain claim are valid only under the sovereignty reading and are not estimates for the kernel as a whole.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: one kernel, three readings, three constraints; this file instantiates the sovereignty reading.').

omega_variable(
    veto_naturalness_counterfactual,
    'Is the concurrence requirement a genuine structural inevitability that would re-emerge in any institution empowered to command great powers, or a contingent drafting choice that happened to mirror the power distribution of 1945?',
    'Counterfactual institutional history and design-space analysis: examine the San Francisco drafting record (alternatives proposed and rejected — weighted voting, qualified majorities, no concurrence term), the League''s unanimity failure, and whether veto-free great-power bodies (G7/G20 consensus practice, Bretton Woods weighted shares) reproduce equivalent consent guarantees in different form.',
    'If the arrangement is contingent rather than inevitable, the mountain claim fails and the constraint recomputes as a durable authored construct with nonzero provenance and higher epsilon; if inevitable, emerges_naturally stands and epsilon stays near zero.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_naturalness_counterfactual, empirical, 'Naturality of the veto: structural inevitability versus contingent constitutional design.').

omega_variable(
    institutional_surplus_baseline,
    'Does formalizing great-power enforcement capacity into an explicit procedural blocking right confer institutional capability beyond what the underlying capacity asymmetry already dictates?',
    'Matched-body comparison controlling for material power: measure the five''s procedural weight, agenda control, and obstruction success in the veto-bearing Council against comparable great-power bodies lacking formalized blocking rights (League Council practice, G7/G20 consensus bodies, Bretton Woods voting architecture); a persistent procedural-weight premium indicates surplus.',
    'A measurable surplus falsifies this reading''s no-extraction corollary, raises epsilon, and shifts the file toward the oligopoly reading''s referent; absence of surplus vindicates the sovereignty reading and leaves epsilon near zero. This is the located disagreement with the oligopoly sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_surplus_baseline, empirical, 'The sovereignty-versus-oligopoly crux: whether formalization adds institutional surplus above raw enforcement capacity.').

omega_variable(
    consent_asymmetry_scope,
    'Does ratification-level consent satisfy the Westphalian principle for the general membership, or does granting per-exercise consent guarantees only to the five constitute selective application of the consent principle rather than its instantiation?',
    'Constitutional analysis of consent granularity across Charter mechanisms: Chapter VII obligations bind non-permanent states that voted against or abstained, while Article 27(3) reserves per-exercise consent for the five; determine whether the 1945 ratification bargain legitimately prices this asymmetry as the cost of universal membership or breaches the principle it invokes.',
    'If the asymmetry breaches rather than instantiates the consent principle, the constraint is a privileged carve-out rather than a natural-law recognition — epsilon rises, emerges_naturally weakens, and mountain certification is threatened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_asymmetry_scope, conceptual, 'Whether the veto applies the Westphalian principle universally or exempts the five from it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__sovereignty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t10, article_27_veto_power__sovereignty_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(arti_tr_t10, observed).
narrative_ontology:measurement(arti_tr_t20, article_27_veto_power__sovereignty_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement_basis(arti_tr_t20, observed).
narrative_ontology:measurement(arti_tr_t30, article_27_veto_power__sovereignty_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(arti_tr_t30, observed).
narrative_ontology:measurement(arti_tr_t40, article_27_veto_power__sovereignty_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement_basis(arti_tr_t40, observed).
narrative_ontology:measurement(arti_tr_t50, article_27_veto_power__sovereignty_reading, theater_ratio, 50, 0.19).
narrative_ontology:measurement_basis(arti_tr_t50, observed).
narrative_ontology:measurement(arti_tr_t60, article_27_veto_power__sovereignty_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement_basis(arti_tr_t60, observed).
narrative_ontology:measurement(arti_tr_t70, article_27_veto_power__sovereignty_reading, theater_ratio, 70, 0.21).
narrative_ontology:measurement_basis(arti_tr_t70, observed).
narrative_ontology:measurement(arti_tr_t80, article_27_veto_power__sovereignty_reading, theater_ratio, 80, 0.22).
narrative_ontology:measurement_basis(arti_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__sovereignty_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t10, article_27_veto_power__sovereignty_reading, base_extractiveness, 10, 0.06).
narrative_ontology:measurement_basis(arti_be_t10, observed).
narrative_ontology:measurement(arti_be_t20, article_27_veto_power__sovereignty_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement_basis(arti_be_t20, observed).
narrative_ontology:measurement(arti_be_t30, article_27_veto_power__sovereignty_reading, base_extractiveness, 30, 0.07).
narrative_ontology:measurement_basis(arti_be_t30, observed).
narrative_ontology:measurement(arti_be_t40, article_27_veto_power__sovereignty_reading, base_extractiveness, 40, 0.07).
narrative_ontology:measurement_basis(arti_be_t40, observed).
narrative_ontology:measurement(arti_be_t50, article_27_veto_power__sovereignty_reading, base_extractiveness, 50, 0.08).
narrative_ontology:measurement_basis(arti_be_t50, observed).
narrative_ontology:measurement(arti_be_t60, article_27_veto_power__sovereignty_reading, base_extractiveness, 60, 0.08).
narrative_ontology:measurement_basis(arti_be_t60, observed).
narrative_ontology:measurement(arti_be_t70, article_27_veto_power__sovereignty_reading, base_extractiveness, 70, 0.09).
narrative_ontology:measurement_basis(arti_be_t70, observed).
narrative_ontology:measurement(arti_be_t80, article_27_veto_power__sovereignty_reading, base_extractiveness, 80, 0.09).
narrative_ontology:measurement_basis(arti_be_t80, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(article_27_veto_power__sovereignty_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__oligopoly_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial concept 'the P5 veto' decomposes into three structurally distinct constraints per the epsilon-invariance principle. This file (sovereignty_reading) authors the consent-instantiation constraint: epsilon ~0.09, no beneficiary/victim structure, claimed mountain, because on this reading the veto recognizes a capacity boundary rather than creating one. article_27_veto_power__coordination_reading authors the war-prevention constraint: its referent is the insurance function keeping great powers inside the institution, with epsilon reflecting the premium structure of that service. article_27_veto_power__oligopoly_reading authors the rent-entrenchment constraint: its referent is the same text read as locked-in authority extraction, with substantially higher epsilon and named beneficiaries/victims. The three files share a kernel and a text but not an epsilon, a beneficiary structure, or a type; each links to the other two via affects_constraints, and cross-reading comparisons are made at the family level, never by averaging within a file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
