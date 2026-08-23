% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__broad_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__broad_copyleft_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__broad_copyleft_reading
 *   human_readable: Broad Copyleft Reading: Linking Creates a Derivative Work Triggering Source Disclosure
 *   domain: economic/legal/technological
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   gpl_derivative_work_trigger: the broad copyleft reading, under which the
 *   act of linking — expressly including dynamic linking — makes the combined
 *   program a derivative work of the GPL component, triggering the obligation
 *   to release the combiner's own source under GPL terms. The
 *   natural-language label 'does linking create a derivative work' covers at
 *   least three structurally distinct constraints: this broad reading, a
 *   narrow reading on which only modifications to GPL source trigger
 *   obligations (narrow_linking_permissive_reading), and an
 *   interface-boundary reading on which clean API separation breaks
 *   derivation even under tight coupling (interface_boundary_reading). Per
 *   the epsilon-invariance principle these are separate files with separate
 *   epsilon values, linked via network.affects_constraints; this file authors
 *   only the broad reading and does not hedge across its siblings. Interval
 *   mapping: t=0 is the 1991 GPLv2 publication, t=34 is 2025. The epsilon
 *   referent is the standing arrangement under contest — the operative
 *   practice and interpretive regime treating linked combinations as
 *   derivative works — assessed by this reading's own lights, which endorse
 *   the arrangement as the protection mechanism for the commons rather than
 *   as a tax on it. The claim (rope) and the metrics are authored
 *   independently: the metrics describe the arrangement's actual operation,
 *   including real costs borne by the vendor seat and an enforcement history
 *   with a genuine coercive arc.
 *
 * KEY AGENTS:
 *   - - fsf_as_license_custodian: Agenda setter (institutional/identity_locked) — interprets the license text, publishes the binding FAQ position that dynamic linking creates derivation, backstops enforcement; collects no material rents, returns are ideological and institutional
 *   - - downstream_users: Primary beneficiary (organized/mobile) — receive source access and auditability for combined works; individually mobile, collectively the constituency the constraint serves
 *   - - free_software_developers: Beneficiary (moderate/constrained) — contribution insurance against proprietary appropriation; exit constrained by reputation and project citizenship
 *   - - proprietary_vendors_linking_gpl_code: Primary target (powerful/constrained) — bear the disclosure obligation or the cost of engineering around it; entered voluntarily but face expensive unwinding once integrated
 *   - - courts_and_legislatures: Excluded voice (institutional/analytical) — hold adjudicative authority over the derivation question and have systematically declined to exercise it, leaving the custodian's interpretation unreviewed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.32).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.33).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.33).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "Broad Copyleft Reading: Linking Creates a Derivative Work Triggering Source Disclosure").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "economic/legal/technological").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, '6fc1ed5d-76ac-4b3e-9169-0d8d729310d5').
narrative_ontology:cs_kernel_codification('6fc1ed5d-76ac-4b3e-9169-0d8d729310d5', fixed_text).
narrative_ontology:cs_authority_grounding('6fc1ed5d-76ac-4b3e-9169-0d8d729310d5', lineage).
narrative_ontology:cs_interpretation_layer_present('6fc1ed5d-76ac-4b3e-9169-0d8d729310d5').
narrative_ontology:cs_reading_relation('6fc1ed5d-76ac-4b3e-9169-0d8d729310d5', gpl_derivative_work_trigger__narrow_linking_permissive_reading, forecloses).
narrative_ontology:cs_reading_relation('6fc1ed5d-76ac-4b3e-9169-0d8d729310d5', gpl_derivative_work_trigger__interface_boundary_reading, forecloses).
narrative_ontology:cs_axiom('6fc1ed5d-76ac-4b3e-9169-0d8d729310d5', foundational, linked_combined_works_inherit_gpl).
narrative_ontology:cs_axiom_status(linked_combined_works_inherit_gpl, holdable).
narrative_ontology:cs_axiom_grounding('6fc1ed5d-76ac-4b3e-9169-0d8d729310d5', linked_combined_works_inherit_gpl, conventional).
narrative_ontology:cs_axiom('6fc1ed5d-76ac-4b3e-9169-0d8d729310d5', secondary, software_freedom_outweighs_proprietary_convenience).
narrative_ontology:cs_axiom_status(software_freedom_outweighs_proprietary_convenience, holdable).
narrative_ontology:cs_axiom_grounding('6fc1ed5d-76ac-4b3e-9169-0d8d729310d5', software_freedom_outweighs_proprietary_convenience, deontological).
narrative_ontology:cs_reference_frame('6fc1ed5d-76ac-4b3e-9169-0d8d729310d5', combined_work_whole_inherits_gpl).
narrative_ontology:cs_drift_state('6fc1ed5d-76ac-4b3e-9169-0d8d729310d5', contemporary_cloud_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6fc1ed5d-76ac-4b3e-9169-0d8d729310d5', '2026-08-20T00:00:00Z').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, free_software_developers).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_vendors_linking_gpl_code).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__broad_copyleft_reading, copyleft_reciprocity_doctrine).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__broad_copyleft_reading, four_freedoms_software_freedom_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publishes and interprets the GPL license text, and maintains the FAQ positions that define this reading's operative rule — including the position that dynamic linking produces a derivative combined work. Backstops interpretation with copyright infringement enforcement support and movement advocacy. Collects no per-use fee; its return is movement vitality and institutional standing as custodian. Its organizational identity is constituted by the copyleft project, so abandoning the interpretive position would dissolve what the institution is.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, fsf_as_license_custodian, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Receive guaranteed access to the source of combined works that incorporate GPL components: the right to study, modify, rebuild, and redistribute the whole stack they run, not merely the GPL parts. Individually they can leave for proprietary alternatives at low cost; their leverage comes only from acting as a class and through the developers who speak for them.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_users, beneficiary,
    organized, biographical, mobile, global).

% Contribute work to GPL projects under the assurance that proprietary intermediaries cannot absorb their contributions into closed products without releasing source. Their exit is constrained by reputation, project citizenship, and in many cases employment tied to stewardship of specific codebases; some do relicense or move to permissive projects, at personal and communal cost.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, free_software_developers, beneficiary,
    moderate, generational, constrained, global).

% Build products that incorporate or link against GPL libraries and kernels. Under this reading the act of linking — static or dynamic — obligates them to release their own source under GPL terms, or to engineer around the dependency: reimplementing functionality, substituting permissively licensed components, negotiating commercial dual licenses, or keeping combinations out of shipped products. Choice is made largely at design time, but once a product line is built on GPL foundations, unwinding it is expensive; the constraint prices their continued participation in the commons ecosystem.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_vendors_linking_gpl_code, payer,
    powerful, biographical, constrained, global).

% Hold the authority that would settle whether linking constitutes derivation as a matter of law, and have overwhelmingly declined to reach the question on the merits — enforcement actions settle, dismiss, or die in procedural morasses. Their absence leaves the operative rule authored by the license custodian's interpretive layer, unreviewed by the institution nominally competent to adjudicate it.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, courts_and_legislatures, excluded,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__broad_copyleft_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__broad_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared code commons against enclosure: guarantees that anyone building on GPL foundations returns improvements to the pool, solving the free-rider problem in which proprietary intermediaries appropriate commons labor into closed products.
% TRANSFER_FUNCTION: Moves source-disclosure rights from entities that combine proprietary code with GPL code to the general public, and moves engineering cost onto vendors who must open their source or rebuild around the dependency; moves trust, auditability, and repair capability to downstream users.
% ABSENT_VOICES: Courts and legislatures, the bodies competent to settle the derivation question, are structurally absent — the operative rule runs on the custodian's interpretation precisely because adjudication never arrived. Proprietary vendors who object have largely exited rather than contested (avoidance over argument), so the visible consensus overstates agreement; end users who bear the outcomes rarely appear in licensing-policy forums at all.
% DISAPPEARANCE_RATIONALE: If the broad linking trigger vanished overnight — say courts universally held linked combinations to be mere aggregation — proprietary combinations would proliferate atop GPL stacks within product cycles, dual-licensing business models built on the compliance lever would collapse, permissive-ecosystem migration would accelerate among projects seeking contributor flow, and the commons would lose its principal anti-enclosure guarantee. Arrangements across the entire commercial open-source economy depend on the trigger's current shape.
% FOUNDING_PROBLEM: Early-1980s software enclosure: code that had circulated freely was being locked into proprietary products, exemplified by printers whose drivers could not be fixed or studied. The GPL was built so that freely shared code could not be proprietarized by downstream intermediaries.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: intellectual-property scholarship documents the historical enclosure wave the license answered; vendor conduct itself corroborates the trigger's operative force (decades of engineering-around behavior, corporate policies restricting GPL use in proprietary products, and the 2001 characterization of GPL as a competitive threat later reversed into strategic compliance); court filings in enforcement actions (BusyBox line, the German kernel case) attest the obligation is litigable and taken seriously by defendants spending real money on it. Critics who dispute the arrangement's justice nonetheless attest that it functions.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__broad_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__broad_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__broad_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).
:- end_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.32: the arrangement imposes real, asymmetric costs on the vendor seat (disclosure or avoidance engineering), but entry is consent-gated — no one is compelled to link GPL code — and the governed population is dominated by participants who are net beneficiaries. The temporal series shows realized extraction peaking during the enforcement-intensive era (t=12–20, the SCO antagonism through the BusyBox docket, when compliance carried litigation risk premiums) and easing afterward as norms absorbed, settlements standardized, and avoidance routes matured. Suppression 0.33 is a raw structural property, unscaled by power or scope: the obligation rests on the copyright backstop and an enforcement apparatus that visibly built up (dedicated enforcement organizations, a sustained lawsuit line circa t=14–20) and then partially stood down as litigation proved costly and courts unreceptive — hence the suppression_requirement series rises steeply then declines, which is why it is tracked at all here: the story's enforcement-capacity arc is a first-order dynamic, not a static picture. Theater_ratio 0.22 and rising slowly: the disclosure function remains predominantly real, but compliance theater (minimal-viable releases, open-source-washing around the edges of shipped combinations) grows as a share of activity in the mature era. Accessibility_collapse 0.2: understanding the constraint collapses almost no alternatives — permissive-licensed equivalents, proprietary stacks, commercial dual licenses, and abstention all remain fully viable, which is structural evidence against snare classification. Resistance 0.55: sustained vendor avoidance, jurisdictional arbitrage, SaaS routing around the trigger, and periodic open contestation of the custodian's interpretation. One shared time grid ([0,6,12,16,20,26,34]) carries all three metric series; every metric is authored at every point, so no scalar substitution injects end-state values into earlier times. No coercion_grid block is authored: level-resolved coercion dynamics is not this story's subject.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the structural data is arranged so they do. From the custodian's seat the arrangement is the protective membrane of a commons it is constitutionally committed to defending; from the downstream-user and developer seats it is a subsidy they receive; from the vendor seat the same structure operates as a compliance toll whose doctrinal basis (dynamic-linking derivation) is unsettled law enforced by an interested interpreter. The vendor seat's high directionality and constrained exit will amplify its computed effective extraction well above the beneficiary seats'; the engine owns that arithmetic. The authored rope claim describes the arrangement's overall structural character — consent-gated, alternatives-flourishing, net-coordinating — not any particular seat's experience of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows: downstream_users and free_software_developers are declared beneficiaries, driving their derived d toward the beneficiary end and damping (or inverting) their computed chi; proprietary_vendors_linking_gpl_code is declared victim with constrained exit options, placing it near the full-target end — trapped-after-integration targets sit nearer d=1 than mobile ones, and these vendors chose integration before the obligation priced in. The custodian is not declared a beneficiary because it collects no rents from the constraint's operation; its stake is ideological and institutional, which the structural data encodes by omission rather than by declaration. Courts_and_legislatures hold the excluded role: commentary-grade only, contributing no correction-grade weight to directionality or classification. No directionality_overrides are authored: the derivation chain from beneficiary/victim declarations plus exit options reproduces the true relationships for every seated agent, and an override keyed on the institutional power atom would misfire across both the custodian and the courts, which hold opposite relationships to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — proprietary enclosure of commons code — remains live, so this is not a mandate-outlived-function case; mandatrophy_resolved is not declared and the founding_problem_status x disappearance_verdict pair (live x world_rearranges) is coherent, raising no capture/zombie flag. The classification discipline earns its keep in both directions here. Against mislabeling as snare: the arrangement has enforcement machinery, an identified paying seat, and an interested interpreter — superficially snare-shaped — but entry is voluntary, exits are real and heavily used (the permissive ecosystem is the standing proof that alternatives are not suppressed), and no seat captures the extracted good; the receipt surface is diffuse. Against overclaiming rope purity: the vendor seat bears genuine asymmetric cost through the same structure that coordinates the commons, and the engine's per-seat computation will register elevated effective extraction there without forcing the global type off rope — which is the correct resolution of a hybrid experience produced by a non-hybrid structure. The piton risk is real but prospective, not current: the enforcement-decline omega tracks whether compliance is decaying toward theatrical maintenance, in which case a future revision of this story would re-author theater_ratio upward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'This constraint is the broad_copyleft_reading of kernel gpl_derivative_work_trigger; how would the sibling readings restructure the governed population, the beneficiary/victim surface, and epsilon?',
    'Author the sibling stories (narrow_linking_permissive_reading, interface_boundary_reading) as separate files and compare governed sets, victim seats, and per-seat classifications across the constraint family via network.affects_constraints edges.',
    'Under the narrow reading the vendor payer seat largely disappears (only source-modifiers are governed) and epsilon drops toward coordination-floor levels; under the interface reading API-clean combinations are exempt, shrinking the victim set and carving the middle out of this reading''s governed population. Classification consequences route per-seat through the family comparison rather than through this file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Committer structure: one of three readings of a contested kernel; disagreement located in the combined-work classification predicate, not in the facts of linking.').

omega_variable(
    judicial_settlement_of_linking_scope,
    'Would authoritative adjudication uphold the broad reading''s premise that even dynamic linking creates a derivative work, or would it restrict derivation to modification and static incorporation?',
    'A court reaching the merits of a GPL linking case (kernel module, linked library, or firmware combination) rather than settling, dismissing, or dying procedurally, as every case to date effectively has.',
    'Vindication converts the custodian''s interpretive layer into settled doctrine and stabilizes the rope with enforceable teeth; rejection collapses this reading''s governed set toward the narrow reading''s, leaving the broad position sustained only by license stipulation and voluntary compliance — a materially weaker constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_settlement_of_linking_scope, empirical, 'Whether the reading''s core doctrinal premise survives contact with the forum competent to decide it.').

omega_variable(
    enforcement_decline_interpretation,
    'Does the post-enforcement-era decline in suppression_requirement reflect successful norm absorption (compliance became routine, so less enforcement is needed) or obligation decay (ambiguity is exploited, avoidance normalized, the trigger quietly narrows in practice)?',
    'Longitudinal compliance auditing: source-availability quality and completeness across shipped products incorporating GPL components, tracked over the interval after the litigation peak.',
    'Norm absorption confirms stable-rope operation with a ratchet that no longer needs cranking; decay indicates drift toward theatrical compliance (rising theater_ratio, piton-ward motion) or de facto migration of practice toward the interface reading without any formal repudiation of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decline_interpretation, empirical, 'Two opposing readings of the falling enforcement series, with opposite classification consequences.').

omega_variable(
    saas_appropriation_residual,
    'How much of the founding anti-enclosure problem persists through the channel this reading cannot reach — network-resident use of GPL-derived code behind a service boundary, where no linking-triggered distribution occurs?',
    'Survey major SaaS offerings built on GPL-derived components for source practices; track AGPL-style remedial licensing adoption as the ecosystem''s own signal of where the trigger fails to bind.',
    'A large residual means this reading''s coordination function has a systematic hole: enclosure migrates to the uncovered channel rather than ceasing, pressuring license evolution and potentially admitting a new constraint-family member covering service-boundary appropriation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saas_appropriation_residual, empirical, 'Size of the appropriation channel left open by a distribution-and-linking-triggered obligation in a service-delivery era.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_broad_linking_tr_t0, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(gpl_broad_linking_tr_t0, observed).
narrative_ontology:measurement(gpl_broad_linking_tr_t6, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 6, 0.06).
narrative_ontology:measurement_basis(gpl_broad_linking_tr_t6, observed).
narrative_ontology:measurement(gpl_broad_linking_tr_t12, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 12, 0.09).
narrative_ontology:measurement_basis(gpl_broad_linking_tr_t12, observed).
narrative_ontology:measurement(gpl_broad_linking_tr_t16, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement_basis(gpl_broad_linking_tr_t16, observed).
narrative_ontology:measurement(gpl_broad_linking_tr_t20, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(gpl_broad_linking_tr_t20, observed).
narrative_ontology:measurement(gpl_broad_linking_tr_t26, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 26, 0.19).
narrative_ontology:measurement_basis(gpl_broad_linking_tr_t26, observed).
narrative_ontology:measurement(gpl_broad_linking_tr_t34, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 34, 0.22).
narrative_ontology:measurement_basis(gpl_broad_linking_tr_t34, observed).

% Extraction over time
narrative_ontology:measurement(gpl_broad_linking_be_t0, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(gpl_broad_linking_be_t0, observed).
narrative_ontology:measurement(gpl_broad_linking_be_t6, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 6, 0.27).
narrative_ontology:measurement_basis(gpl_broad_linking_be_t6, observed).
narrative_ontology:measurement(gpl_broad_linking_be_t12, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement_basis(gpl_broad_linking_be_t12, observed).
narrative_ontology:measurement(gpl_broad_linking_be_t16, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement_basis(gpl_broad_linking_be_t16, observed).
narrative_ontology:measurement(gpl_broad_linking_be_t20, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement_basis(gpl_broad_linking_be_t20, observed).
narrative_ontology:measurement(gpl_broad_linking_be_t26, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 26, 0.35).
narrative_ontology:measurement_basis(gpl_broad_linking_be_t26, observed).
narrative_ontology:measurement(gpl_broad_linking_be_t34, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 34, 0.32).
narrative_ontology:measurement_basis(gpl_broad_linking_be_t34, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl_broad_linking_su_t0, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(gpl_broad_linking_su_t0, observed).
narrative_ontology:measurement(gpl_broad_linking_su_t6, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 6, 0.12).
narrative_ontology:measurement_basis(gpl_broad_linking_su_t6, observed).
narrative_ontology:measurement(gpl_broad_linking_su_t12, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 12, 0.25).
narrative_ontology:measurement_basis(gpl_broad_linking_su_t12, observed).
narrative_ontology:measurement(gpl_broad_linking_su_t16, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement_basis(gpl_broad_linking_su_t16, observed).
narrative_ontology:measurement(gpl_broad_linking_su_t20, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(gpl_broad_linking_su_t20, observed).
narrative_ontology:measurement(gpl_broad_linking_su_t26, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 26, 0.38).
narrative_ontology:measurement_basis(gpl_broad_linking_su_t26, observed).
narrative_ontology:measurement(gpl_broad_linking_su_t34, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 34, 0.33).
narrative_ontology:measurement_basis(gpl_broad_linking_su_t34, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, interface_boundary_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'linking creates a derivative work under GPL' decomposes into three structurally distinct readings of one kernel (gpl_derivative_work_trigger). This file is the broad copyleft reading: maximal governed set (every linked combination), vendor seat fully in scope, highest epsilon of the family. narrow_linking_permissive_reading governs only source-modifiers and carries negligible vendor extraction; interface_boundary_reading exempts API-clean combinations and sits between. The upstream/downstream structure runs from this broad reading outward: the custodian's published FAQ positions are cited both as the position competitors must rebut and as the extreme against which narrower positions legitimate themselves, so this reading influences its siblings' operating environment even where it does not prevail.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
