% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__grievance_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-18
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__grievance_threshold_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__grievance_threshold_reading
 *   human_readable: Grievance-Threshold Condition on Secession Legitimacy
 *   domain: political/economic/federalism
 *
 * SUMMARY:
 *   The colloquial question 'when is secession legitimate?' decomposes, under
 *   the epsilon-invariance principle, into at least four structurally
 *   distinct constraints — four readings of one contested kernel
 *   (secession_legitimacy_boundary). This file instantiates exactly one of
 *   them: the grievance_threshold_reading, under which a region's exit
 *   becomes legitimate when federal conduct crosses a demonstrable threshold
 *   of structural injustice, whatever the constitutional text says. The
 *   standing arrangement under contest — and therefore the epsilon referent —
 *   is the threshold-gated legitimacy regime itself, assessed by this
 *   reading's own lights: the burden of proof it imposes on claimant regions,
 *   the evidentiary terrain it cedes to federal governments, and the
 *   protective default it grants them. The reading's own assessment finds a
 *   genuine service (an evidence-based route to exit that neither
 *   constitutional text nor federal consent fully controls) operating
 *   alongside real asymmetric costs (proof obligations fall on the weaker,
 *   evidence-controlled party, and the gate has certified almost no case).
 *   Sibling readings are separate constraints with separate epsilon values
 *   and victim sets; they are linked through network.affects_constraints, not
 *   averaged into this file. KEY AGENTS (by structural relationship): -
 *   federal_governments: Dual-positioned beneficiary/payer and partial
 *   administrator (institutional/constrained) — retains territory by default,
 *   bears conditional exposure - aggrieved_regions_meeting_threshold:
 *   Conditional beneficiary (organized/constrained) — gains a legitimacy
 *   route only after financing proof - regions_with_unprovable_grievances:
 *   Primary bearer of the gating (moderate/trapped) — bears the treatment and
 *   the unmeetable burden - majority_will_regions_without_violations: Bearer
 *   of displaced legitimacy (organized/constrained) — democratic majorities
 *   rendered non-currency - recognizing_states: Secondary beneficiary
 *   (institutional/arbitrage) — receives a discretionary criterion -
 *   international_recognition_bodies: Administrator
 *   (institutional/constrained) — operationalizes the threshold, collects no
 *   rents - regional_minorities_opposing_exit: Excluded voice
 *   (powerless/trapped) - comparative_constitutional_scholars: Analytical
 *   observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, 0.57).
domain_priors:suppression_score(secession_legitimacy_boundary__grievance_threshold_reading, 0.56).
domain_priors:theater_ratio(secession_legitimacy_boundary__grievance_threshold_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, extractiveness, 0.57).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__grievance_threshold_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__grievance_threshold_reading, "Grievance-Threshold Condition on Secession Legitimacy").
narrative_ontology:topic_domain(secession_legitimacy_boundary__grievance_threshold_reading, "political/economic/federalism").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__grievance_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__grievance_threshold_reading, 'a2e0afb8-4f2a-46c4-8561-f8531599a758').
narrative_ontology:cs_kernel_codification('a2e0afb8-4f2a-46c4-8561-f8531599a758', distributed).
narrative_ontology:cs_authority_grounding('a2e0afb8-4f2a-46c4-8561-f8531599a758', distributed).
narrative_ontology:cs_reading_relation('a2e0afb8-4f2a-46c4-8561-f8531599a758', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('a2e0afb8-4f2a-46c4-8561-f8531599a758', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('a2e0afb8-4f2a-46c4-8561-f8531599a758', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('a2e0afb8-4f2a-46c4-8561-f8531599a758', foundational, remedial_exit_right_of_oppressed_peoples).
narrative_ontology:cs_axiom_status(remedial_exit_right_of_oppressed_peoples, holdable).
narrative_ontology:cs_axiom_grounding('a2e0afb8-4f2a-46c4-8561-f8531599a758', remedial_exit_right_of_oppressed_peoples, deontological).
narrative_ontology:cs_axiom('a2e0afb8-4f2a-46c4-8561-f8531599a758', foundational, demonstrated_injustice_supersedes_constitutional_text).
narrative_ontology:cs_axiom_status(demonstrated_injustice_supersedes_constitutional_text, holdable).
narrative_ontology:cs_axiom_grounding('a2e0afb8-4f2a-46c4-8561-f8531599a758', demonstrated_injustice_supersedes_constitutional_text, deontological).
narrative_ontology:cs_reference_frame('a2e0afb8-4f2a-46c4-8561-f8531599a758', remedial_injustice_threshold_order).
narrative_ontology:cs_drift_state('a2e0afb8-4f2a-46c4-8561-f8531599a758', contemporary_recognition_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a2e0afb8-4f2a-46c4-8561-f8531599a758', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_regions_meeting_threshold).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, federal_governments).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, recognizing_states).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, regions_with_unprovable_grievances).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, majority_will_regions_without_violations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, federal_governments).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_regions_meeting_threshold).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__grievance_threshold_reading, remedial_secession_theory).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__grievance_threshold_reading, structural_injustice_threshold_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the territory and operate the constitutional order a secession claim aims at. Under this standard their territorial integrity is no longer unconditional: conduct toward a region is continuously priced against the possibility that it crosses the demonstrable-injustice line. They control much of the evidentiary terrain (archives, fiscal data, security files), fund the defense in any threshold proceeding, and in many jurisdictions staff the courts that would hear the claim. They cannot step outside the standard; their option is to keep conduct and litigation posture below the line.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__grievance_threshold_reading, federal_governments, agenda_setter).

% Regions whose documented treatment plausibly meets the injustice threshold. The standard gives their claim a legitimacy route that neither constitutional text nor federal consent fully controls, but only after they finance the evidentiary case, survive years of inquiry, and persuade adjudicators they did not manufacture the crisis. Their exit is the very thing being gated; until certification they remain inside the federation bearing the treatment they are documenting.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_regions_meeting_threshold, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_regions_meeting_threshold, payer).

% Regions suffering real harms — fiscal drain, cultural suppression, emergency-rule abuses — that fall short of what the evidentiary standard can certify, often because the records sit with the federal government or the harms are aggregate and slow-moving. For them the standard works as a closed door with a visible handle: the route exists, the proof does not. Waiting generations for documentation is the only strategy available from where they stand.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, regions_with_unprovable_grievances, payer,
    moderate, biographical, trapped, regional).

% Regions where a stable democratic majority votes for independence but the grievance is distributive or political rather than rights-based. Under this standard their referendum majorities carry no legitimating weight on their own; the standard tells them the vote is not the currency. Their realistic options are persuading the federation to negotiate, reframing the claim in rights language, or abandoning the project.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, majority_will_regions_without_violations, payer,
    organized, biographical, constrained, regional).

% Third-party governments deciding whether to recognize a secession. The standard hands them a criterion that limits destabilizing precedents while preserving discretion: they invoke the threshold where recognition suits their interests and decline it where it does not. Selective application is their structural advantage — inconsistency costs them almost nothing.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, recognizing_states, beneficiary,
    institutional, generational, arbitrage, global).

% Courts, UN organs, and fact-finding commissions that operationalize the standard: defining what counts as structural injustice, commissioning inquiries, issuing advisory opinions. They set the practical content of the threshold without a binding codified definition, and their dockets are shaped by which cases member states choose to refer. They run the gate but collect no revenue from it.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, international_recognition_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Communities inside a claimant region who would remain behind a new border they did not choose — ethnic minorities, federal employees, residents economically tied to the wider state. The threshold debate is conducted between the region's leadership and federal institutions; nobody convenes them, and the standard has no category for their consent. If the threshold is ever certified, they inherit the new arrangement as a settled fact.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, regional_minorities_opposing_exit, excluded,
    powerless, biographical, trapped, regional).

% Legal academics and comparative-law institutes mapping how the standard operates across cases, documenting the gap between doctrine and recognition practice, and proposing operationalizations of the threshold. They see the whole structure — the filtering function, the evidentiary asymmetry, the selective application — and publish on it while holding no seat in any proceeding.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__grievance_threshold_reading, federal_governments).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__grievance_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the recognition-coordination problem: federations, claimant regions, and third-party states need a shared, non-arbitrary criterion for when territorial exit deserves recognition, so that neither every grievance triggers dissolution nor no grievance ever does. The standard coordinates expectations on all sides by making legitimacy a matter of demonstrable fact rather than declared will.
% TRANSFER_FUNCTION: Moves proof burdens, litigation costs, and waiting time from the arrangement's protected parties to claimant regions; moves legitimacy and recognition to regions that certify threshold-crossing; and moves disciplinary risk onto federal governments, whose conduct toward regions now carries a priced possibility of losing territory.
% ABSENT_VOICES: Regional minorities inside claimant regions, and populations whose majorities favor exit on distributive rather than rights grounds, would both object that the standard speaks over them — the first because the threshold debate never convenes them, the second because it rules their votes non-currency. They sit outside the adjudicating forums, represented only indirectly by secession-movement leadership claiming to speak for the region as a unit.
% DISAPPEARANCE_RATIONALE: If the threshold standard vanished overnight, secession legitimacy would revert to a three-way contest among constitutional text, referendum results, and raw power; recognition decisions would lose their shared criterion and fragment along great-power lines; federal governments would lose the continuous disciplinary pricing on their conduct toward regions; and claimant regions would redirect resources from evidence-building to referendum campaigns and unilateral declarations. Every seated party's arrangements depend on the standard's existence.
% FOUNDING_PROBLEM: After the First World War, self-determination rhetoric collided with absolute territorial integrity: populations could be permanently locked inside states that oppressed them, with no exit however severe the treatment, while an unrestricted right of exit threatened endless fragmentation along every grievance line. The threshold standard was built to solve that double bind — make exit reachable for the demonstrably oppressed without making every distributive dispute a dissolution trigger.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the ICJ's Kosovo advisory-opinion proceedings record submissions from states across every bloc acknowledging cases of severe oppression; UN human-rights mechanisms have documented systemic violations in specific claimant regions; and comparative constitutional scholarship — written largely by authors with no stake in any particular secession — treats trapped-population cases as a continuing live problem. Federal governments dispute the doctrinal conclusion (that any exit ground exists), not the existence of the underlying problem.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__grievance_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__grievance_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__grievance_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__grievance_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 0.57, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   All three tracked series run on one shared eight-point grid (t=0..70) so no metric borrows another's end-state. Extractiveness rises from 0.30 to 0.57 as the standard hardened from interwar principle into operative recognition doctrine: the burden of proof is the reading's deliberate design, but its incidence is asymmetric — claimant regions must prove, federal governments control the archives, and the gate has effectively never opened (Kosovo proceeded on unique circumstances, not threshold certification). Suppression (0.56 at interval end) is authored as a raw structural property — the engine scales only extractiveness, by directionality and scope — and reflects the standard's displacement of rival legitimacy grounds: referendum majorities and negotiated-amendment routes are declared non-currency wherever the rights-violation condition goes unmet. Theater_ratio climbs to 0.38 because the arrangement's growth industry is documentation without certification: commissions of inquiry, fact-finding missions, and scholarly symposia that record injustice while certifying no exits. Accessibility_collapse 0.50: accepting the framework collapses unilateral-declaration and pure-majority routes, but negotiation and hybrid claims remain live. Resistance 0.62: claimant regions reject the burden ('oppression should not require litigation'), federal governments reject the discipline ('no such right exists'), and neither side accepts the other's framing. Coalition potential is real but unrealized: trapped regions share the same evidentiary problem and could pool documentation and legal capacity across cases, lowering each one's effective burden — the standard's cost profile depends partly on their failure to coordinate.
 *
 * PERSPECTIVAL GAP:
 *   The federal seat experiences the standard as a stability-preserving filter it can live with: the default protects it, and its control of evidence keeps the gate manageable. A trapped region with unprovable grievances experiences the same structure as a door with a handle welded on — the legitimate route visibly exists and is unreachable from where it stands. Recognizing_states occupy a third position: holding arbitrage-grade selectivity, they apply the criterion case-by-case and feel almost no constraint at all. Same-level lateral divergence is sharpest between the two claimant-region seats, which hold comparable nominal power (organized/moderate) but opposite evidentiary positions: the meeting-threshold region buys a live option with litigation; the unprovable-grievance region pays a comparable burden for a permanently closed door. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. aggrieved_regions_meeting_threshold (beneficiary, constrained exit) derives a mid-low d — subsidized where certified, taxed by the proof burden until then. federal_governments (listed beneficiary for the protective default, dual-positioned as payer and partial administrator) derive a beneficiary-leaning d that understates their conditional exposure; no override is authored because the correction is story-specific and a power-atom-wide override would also move international_recognition_bodies, whose position (administering without collecting) is genuinely near-symmetric. regions_with_unprovable_grievances and majority_will_regions_without_violations (victims, trapped/constrained) derive high d — nearest the full-target end. recognizing_states (beneficiaries with arbitrage-grade selectivity) derive the lowest d in the story. international_recognition_bodies run the gate without collecting from it and sit near the middle. No directionality_overrides are declared: the structural data already separates the seats, and the one imprecision (federal_governments' exposure) is documented here rather than forced through a shared power-atom override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — trapped populations under structural injustice with no lawful exit — remains live: the corroborating record (ICJ Kosovo proceedings, UN human-rights documentation, comparative scholarship from outside the benefiting parties) attests that the problem the standard was built for still generates cases. With founding_problem_status 'live' and disappearance_verdict 'world_rearranges', the R5 mismatch consumer finds no dead-mandate/zombie flag, and no mandatrophy_resolved flag is authored. The tangled_rope claim is what prevents mislabeling in both directions: reading the arrangement as pure extraction erases the real service it renders to regions that can meet the standard (a legitimacy route independent of federal consent and constitutional text); reading it as pure coordination erases the locked-out class — regions whose suffering is real but uncertifiable — for whom the coordination story functions as cover. The classification holds both truths because both are structurally present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is the grievance_threshold_reading of the secession_legitimacy_boundary kernel; how would the classification change under the sibling readings (constitutional_impossibility_reading, popular_sovereignty_reading, treaty_primacy_reading)?',
    'Generate the three sibling readings as separate stories and compare computed types, victim sets, and directionality profiles across the family.',
    'Under popular_sovereignty_reading the harmed class shifts to regions denied referendum weight and the cost profile concentrates on majoritarian claims; under treaty_primacy_reading it centers on Indigenous nations and the federal seat becomes a straightforward bearer; under constitutional_impossibility_reading the gate closes entirely and the arrangement trends toward pure extraction for trapped regions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Classification is indexed to one reading of a four-reading kernel; sibling files carry the deltas.').

omega_variable(
    adjudicator_capture_ambiguity,
    'Are threshold adjudications structurally independent of the federal governments they judge, or does appointment control, evidence custody, and funding dependence make the gate systematically federal-friendly?',
    'Compare certification rates across adjudicators with varying degrees of federal dependence; track reversal rates on appeal to less dependent bodies.',
    'If captured, effective costs on trapped regions rise sharply and the arrangement trends toward pure extraction; if independent, the burden-of-proof cost is closer to a neutral filter and the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjudicator_capture_ambiguity, empirical, 'Whether the gate''s adjudicators are structurally independent of the party being judged.').

omega_variable(
    burden_of_proof_filter_direction,
    'Does the objective evidentiary burden filter opportunistic claims (protective function) or systematically exclude genuine grievances whose documentation the federation controls (extractive function)?',
    'Audit rejected secession claims for documentary feasibility: could a well-resourced, honest claimant have met the standard with evidence obtainable from non-federal sources?',
    'A protective reading supports the coordination half of the tangled_rope classification; an extractive reading supports drift toward the extraction-dominant pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_of_proof_filter_direction, empirical, 'Direction of the evidentiary filter: shields against opportunism or locks out the genuinely aggrieved.').

omega_variable(
    structural_injustice_operationalization,
    'Where is the line between ''structural injustice'' and ordinary distributive conflict — do fiscal exploitation, cultural suppression, or emergency governance count, and who decides?',
    'Comparative doctrinal analysis of which harm categories adjudicators have accepted versus refused; any legislative or treaty codification attempt would resolve it directly.',
    'A narrow operationalization locks out most claimant regions and raises effective costs on the trapped; a broad one destabilizes the federation''s default protection and pushes outcomes toward the popular-sovereignty pole.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_injustice_operationalization, conceptual, 'The threshold''s content is undefined; its width determines who the gate admits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__grievance_threshold_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(sece_tr_t0, observed).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(sece_tr_t10, observed).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(sece_tr_t20, observed).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(sece_tr_t30, observed).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement_basis(sece_tr_t40, observed).
narrative_ontology:measurement(sece_tr_t50, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement_basis(sece_tr_t50, observed).
narrative_ontology:measurement(sece_tr_t60, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 60, 0.36).
narrative_ontology:measurement_basis(sece_tr_t60, observed).
narrative_ontology:measurement(sece_tr_t70, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 70, 0.38).
narrative_ontology:measurement_basis(sece_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(sece_be_t0, observed).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement_basis(sece_be_t10, observed).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(sece_be_t20, observed).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(sece_be_t30, observed).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement_basis(sece_be_t40, observed).
narrative_ontology:measurement(sece_be_t50, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 50, 0.51).
narrative_ontology:measurement_basis(sece_be_t50, observed).
narrative_ontology:measurement(sece_be_t60, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 60, 0.54).
narrative_ontology:measurement_basis(sece_be_t60, observed).
narrative_ontology:measurement(sece_be_t70, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 70, 0.57).
narrative_ontology:measurement_basis(sece_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(sece_su_t0, observed).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement_basis(sece_su_t10, observed).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement_basis(sece_su_t20, observed).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement_basis(sece_su_t30, observed).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement_basis(sece_su_t40, observed).
narrative_ontology:measurement(sece_su_t50, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement_basis(sece_su_t50, observed).
narrative_ontology:measurement(sece_su_t60, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 60, 0.53).
narrative_ontology:measurement_basis(sece_su_t60, observed).
narrative_ontology:measurement(sece_su_t70, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 70, 0.56).
narrative_ontology:measurement_basis(sece_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__grievance_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one member of a four-story constraint family decomposing the colloquial label 'secession legitimacy.' The decomposition follows the epsilon-invariance principle: each reading assigns legitimacy to a different determinant (demonstrated injustice / constitutional process / provincial majority / Indigenous consent), so each yields a distinct epsilon, victim set, and classification. The grievance-threshold reading is upstream of the others only in discourse: its evidentiary vocabulary supplies the terms in which the sibling readings' disputes are argued. Cross-family comparison requires generating the three sibling files; this file links them without absorbing them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
