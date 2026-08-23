% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__treaty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__treaty_primacy_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__treaty_primacy_reading
 *   human_readable: Treaty-Primacy Consent Gate on Secession Legitimacy
 *   domain: political economy/federalism/resource politics
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the treaty_primacy_reading — of the
 *   contested kernel secession_legitimacy_boundary. The kernel asks what
 *   makes secession legitimate; this reading answers that Indigenous treaty
 *   rights predate and supersede both federal and provincial authority, so no
 *   secession is legitimate without treaty-holder consent, and neither order
 *   of government can unilaterally alter treaty relationships. The three
 *   sibling readings (constitutional impossibility, popular sovereignty,
 *   grievance threshold) are separate constraints authored in separate files;
 *   they are NOT folded into this story's epsilon, beneficiaries, or
 *   classification. Epsilon's referent is the standing arrangement this story
 *   is about — the consent gate and supersession doctrine as they actually
 *   operate in Canadian constitutional practice since the 1982 entrenchment
 *   of treaty rights — assessed by this reading's own lights; the fully
 *   realized arrangement the reading endorses is not the referent. The
 *   claim/metrics split is deliberate: claimed_type records my structural
 *   belief (genuine coordination function plus asymmetric cost-bearing plus
 *   enforcement dependence), while the metrics record descriptive operation,
 *   and the engine computes each seat's classification from the data rather
 *   than from the claim.
 *
 * KEY AGENTS:
 *   - indigenous_treaty_holders: primary beneficiary (organized / identity_locked) — consent-gate authority and negotiation leverage accrue here; conditionally exposed if a secession proceeds without consultation
 *   - federal_executive: dual-positioned target-administrator (institutional / constrained) — loses unilateral discretion under the gate while still running the machinery that processes treaty obligations
 *   - provincial_secession_movements: primary target (organized / constrained) — their core legitimacy claim is invalidated unless they obtain consent they cannot self-supply
 *   - provincial_governments: target of supersession (institutional / constrained) — barred from unilateral alteration of treaty relationships on lands they administer
 *   - non_treaty_indigenous_nations: excluded seat (organized / identity_locked) — bound by the consent architecture but not seated by it
 *   - supreme_court_judiciary: agenda_setter administering the gate (institutional / analytical) — its doctrine determines whether the gate binds
 *   - resource_development_industry: diffuse-cost bearer (powerful / mobile) — bears project delays and cancellations; nominal mobility undercut by sunk assets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, 0.55).
domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, 0.45).
domain_priors:theater_ratio(secession_legitimacy_boundary__treaty_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__treaty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__treaty_primacy_reading, "Treaty-Primacy Consent Gate on Secession Legitimacy").
narrative_ontology:topic_domain(secession_legitimacy_boundary__treaty_primacy_reading, "political economy/federalism/resource politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__treaty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__treaty_primacy_reading, '535c15d0-d5f8-4468-b8a3-91b1f0fd40ec').
narrative_ontology:cs_kernel_codification('535c15d0-d5f8-4468-b8a3-91b1f0fd40ec', distributed).
narrative_ontology:cs_authority_grounding('535c15d0-d5f8-4468-b8a3-91b1f0fd40ec', lineage).
narrative_ontology:cs_interpretation_layer_present('535c15d0-d5f8-4468-b8a3-91b1f0fd40ec').
narrative_ontology:cs_reading_relation('535c15d0-d5f8-4468-b8a3-91b1f0fd40ec', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('535c15d0-d5f8-4468-b8a3-91b1f0fd40ec', secession_legitimacy_boundary__constitutional_impossibility_reading, influences).
narrative_ontology:cs_reading_relation('535c15d0-d5f8-4468-b8a3-91b1f0fd40ec', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('535c15d0-d5f8-4468-b8a3-91b1f0fd40ec', foundational, preconfederation_treaty_covenants_are_supreme_law).
narrative_ontology:cs_axiom_status(preconfederation_treaty_covenants_are_supreme_law, holdable).
narrative_ontology:cs_axiom_grounding('535c15d0-d5f8-4468-b8a3-91b1f0fd40ec', preconfederation_treaty_covenants_are_supreme_law, conventional).
narrative_ontology:cs_axiom('535c15d0-d5f8-4468-b8a3-91b1f0fd40ec', foundational, territorial_legitimacy_requires_affected_nation_consent).
narrative_ontology:cs_axiom_status(territorial_legitimacy_requires_affected_nation_consent, holdable).
narrative_ontology:cs_axiom_grounding('535c15d0-d5f8-4468-b8a3-91b1f0fd40ec', territorial_legitimacy_requires_affected_nation_consent, deontological).
narrative_ontology:cs_reference_frame('535c15d0-d5f8-4468-b8a3-91b1f0fd40ec', crown_treaty_covenant_supremacy).
narrative_ontology:cs_drift_state('535c15d0-d5f8-4468-b8a3-91b1f0fd40ec', contemporary_post_undrip_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('535c15d0-d5f8-4468-b8a3-91b1f0fd40ec', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, provincial_secession_movements).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, federal_executive).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, resource_development_industry).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__treaty_primacy_reading, honour_of_the_crown_fiduciary_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__treaty_primacy_reading, successor_state_treaty_inheritance_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nations holding numbered, modern, and historic treaties signed with the Crown before and after Confederation. They maintain the treaty relationship as a continuing covenant, assert consultation and accommodation rights over lands inside treaty territories, and organize through treaty alliances and national political assemblies. Consent authority over territorial change accrues to them under this framing; their exposure is conditional in the other direction, since a secession that proceeds without consultation would visit its harms directly on their members and lands. Leaving the relationship would mean dissolving the nationhood the treaties record, so exit is not a live option.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders, payer).

% Sets federal policy on treaty implementation, funds and staffs consultation processes, appoints the judges who interpret treaty obligations, and would lead any response to a provincial secession bid. Under the treaty-primacy framing it loses the ability to alter treaty terms or answer secession on its own authority, yet it continues to run the administrative machinery through which treaty obligations are processed day to day.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_executive, payer,
    institutional, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, federal_executive, agenda_setter).

% Organized campaigns for provincial independence, strongest in Quebec, that seek a path from referendum result to recognized statehood. The treaty-primacy framing invalidates their central claim unless they obtain consent from nations they did not create, cannot represent, and whose territories overlap the province they propose to take out. Their realistic options are persuading treaty holders, reframing the question, or abandoning the bid.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_secession_movements, payer,
    organized, biographical, constrained, regional).

% Administer Crown lands, approve resource development, and legislate within provincial boundaries that cut across treaty territories. The framing bars unilateral alteration of treaty relationships and subjects approvals and land dispositions to consultation duties they have repeatedly contested in court and occasionally defied on the ground.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments, payer,
    institutional, biographical, constrained, regional).

% Nations whose territories were never covered by historic treaties, together with Metis communities and non-status and off-reserve Indigenous people. Their claims rest on Aboriginal title, kinship, and community membership rather than treaty covenants. A consent architecture built on treaties leaves them outside the table while decisions taken inside it still touch their lands and members.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, non_treaty_indigenous_nations, excluded,
    organized, generational, identity_locked, regional).

% Interprets the constitutional status of treaty obligations through the honour-of-the-Crown doctrine and related case law, deciding when consultation is owed, when consent is required, and what weight treaty commitments carry against provincial and federal legislation. Its rulings determine whether the consent gate binds in practice or remains a principle awaiting enforcement.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, supreme_court_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Firms building pipelines, mines, hydro projects, and forestry operations on treaty-adjacent lands. Consultation and consent requirements lengthen timelines, add equity and benefit-sharing costs, and have cancelled major projects outright. Capital can relocate between jurisdictions, but sunk assets in contested corridors cannot, so firms with fixed infrastructure absorb the costs their mobility was supposed to let them dodge.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, resource_development_industry, payer,
    powerful, immediate, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__treaty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates legitimate authority over territorial change among overlapping polities: provincial boundaries were drawn across pre-existing treaty territories, so a boundary-altering decision made by provincial majority alone imposes costs on nations that never consented to the provincial unit. The consent gate converts a purely majoritarian boundary question into a multi-party negotiation in which the peoples holding prior covenants with the larger polity must be accounted for.
% TRANSFER_FUNCTION: Moves decision authority over secession legitimacy and over treaty-term alteration from federal and provincial executives and from secessionist majorities to Indigenous treaty-holding nations; moves negotiation, consultation, and accommodation costs onto any project that would rearrange territorial jurisdiction.
% ABSENT_VOICES: Nations outside the treaty frame would object if seated: unceded-territory nations whose claims rest on Aboriginal title rather than covenants, Metis communities without historic treaties, and non-status and off-reserve Indigenous people, all of whom are bound by decisions of a consent architecture that does not seat them. Settler minorities inside a would-be seceding province would also object that the gate addresses nations but leaves local demographic minorities without comparable protection.
% DISAPPEARANCE_RATIONALE: If the consent gate and the supersession doctrine vanished overnight, secession projects would treat provincial lines as the only relevant units; successor states would claim freedom to alter or abandon inherited treaty obligations; the duty-to-consult and benefit-sharing settlements that resource governance currently rests on would collapse into open litigation; and the federation's legitimacy narrative would rearrange around whichever of the rival criteria — constitutional procedure, referendum majorities, or injustice thresholds — filled the vacancy.
% FOUNDING_PROBLEM: The arrangement answers a legitimacy defect built into the federation's constituent units: provincial boundaries and federal authority were constituted over territories already governed by treaty covenants, so any boundary-changing move threatens to dispose of obligations the moving parties never created and cannot unilaterally extinguish — the successor-state free-rider problem applied to secession.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by the international-law succession tradition and its scholarly literature, which holds that successor states inherit treaty obligations; by imperial and Crown archival records in which negotiators recorded the promises later invoked; and by judicial statements in the honour-of-the-Crown line of cases authored by an institution standing apart from the beneficiary nations' advocacy. No corroborating source outside the benefiting parties attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__treaty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__treaty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__treaty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55): the gate removes real discretionary authority from three kinds of actor and imposes genuine process costs, but what it transfers is decision power and negotiation leverage rather than stripped wealth, and the coordination it purchases is substantive. Suppression (0.45) is a raw structural property, unscaled by power or scope: the constraint forecloses the unilateral path entirely and its maintenance now involves injunctions, police deployments at rail and pipeline blockades, and escalating court enforcement — machinery that has grown steadily across the interval, hence the tracked suppression_requirement series rather than reliance on the static scalar. Theater (0.40) reflects the proliferation of land acknowledgments and symbolic consultation alongside binding litigation; the performative share has risen faster than the functional share since roughly the mid-interval. Accessibility collapse is LOW (0.30): the three rival readings remain live, negotiation and constitutional-amendment routes stay open, and understanding the gate does not close alternatives the way a natural law closes them. Resistance is HIGH (0.60): referendum campaigns have proceeded as if the gate did not exist, provinces have contested consultation duties in court, and land-defense blockades meet physical enforcement. The temporal picture is a secular rise with episodic crisis spikes (1990 and 2020 blockade crises are the sharpest); the shared six-point grid records the trend, and every tracked metric is authored at every shared time point so no end-state value leaks backward into early rows.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the treaty-holders' seat the structure reads as a restored priority — a protective ordering of authority that merely returns obligations to their original position; from the secessionist and executive seats the same structure reads as a veto they cannot appeal past, imposed on claims they regard as democratically grounded; from the excluded seat (non-treaty nations) it reads as a coordination scheme that settles who is inside the room by leaving them outside it. The judiciary's seat differs again: it experiences the structure as interpretive workload and doctrinal choice rather than as burden or benefit. Same-power divergence is visible between the federal executive and provincial governments — both institutional, both constrained — because provinces meet the supersession doctrine most directly in land and resource approval, while the federal order meets it in secession response and fiduciary obligation, giving them different stakes in the same rule.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. indigenous_treaty_holders sit at the beneficiary end (low effective extraction; their identity_locked exit deepens subsidy rather than trapping, since what locks them in is also what the gate protects). provincial_secession_movements derive near the full-target end: victim-declared, constrained exit, no compensating access. federal_executive and provincial_governments derive high-but-not-full target positions: they bear removed discretion, but both retain agenda leverage (consultation design, litigation strategy, legislative counter-moves) that keeps them short of trapped targets. One override is authored: resource_development_industry at power_atom powerful, d_value 0.60. The derivation would push a mobile, powerful payer toward the beneficiary end via arbitrage-grade exit, but the story's facts contradict that — capital mobility is partial, sunk corridor assets are immovable, and firms have absorbed cancelled-project losses outright — so the override corrects an exit-option artifact, not a structural misdeclaration. The judiciary derives mid-range as administrator: it neither collects the gate's gains nor bears its costs, but its doctrine choices set how hard the gate bites.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here cuts both ways. Naming the genuine coordination function (boundary-externalities: no polity should be able to vote itself out of obligations to peoples inside its claimed territory who hold prior agreements with the larger polity) prevents mislabeling the gate as pure obstruction — a snare reading would erase the fact that the gate solves a real collective-action problem nobody else in the dispute is positioned to solve. Conversely, refusing the mountain framing matters equally: the reading's own language ('predates and supersedes') invites natural-law presentation, but the constraint demonstrably requires active enforcement — courts, injunctions, political mobilization — and meets sustained resistance, which is the signature of a constructed and defended order, not a fixed one. The mandate is not atrophied and the founding problem is corroborated as live, so no mandatrophy resolution is declared; the rising theater_ratio is watched as a degradation signal rather than treated as a verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the secession_legitimacy_boundary kernel governs: treaty primacy, constitutional procedure, referendum majorities, or injustice thresholds?',
    'Authoritative adjudication (a reference-case ruling or negotiated constitutional settlement) that fixes the legitimacy criterion; until then the four readings persist as rival constraints held by different parties.',
    'Adoption of popular sovereignty would relocate the victim set to Indigenous nations excluded from the referendum franchise and dissolve this story''s consent gate; adoption of constitutional impossibility would remove treaty consent from the legitimacy test while keeping negotiated exit; this story''s classification holds only while the treaty-primacy criterion remains a live contender.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This constraint is one reading of a contested kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    conditional_victim_exposure,
    'Are indigenous_treaty_holders structurally beneficiaries of this constraint, or contingent victims whose victim-set entry activates whenever a secession proceeds without consultation?',
    'Observe the gate''s behavior under a live secession bid: whether consent mechanisms hold under referendum stress or are bypassed, and whether nations that withhold consent suffer the projected harms.',
    'If the gate fails under stress, the treaty-holding seat flips toward the full-target end of directionality and the constraint''s coordination function computes as failed rather than operative, shifting per-seat classifications materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditional_victim_exposure, empirical, 'Dual-position encoding (beneficiary with conditional payer exposure) resolves only when the gate is tested.').

omega_variable(
    gate_enforcement_theater_risk,
    'Does the consent gate bind when compliance is expensive, or does it degrade into performative consultation — acknowledgments, studies, and meetings without veto force?',
    'Outcome tracking of major projects and boundary decisions opposed by every affected treaty-holding nation: if unanimous Indigenous opposition reliably stops nothing, the functional share is collapsing beneath the performative share.',
    'A sustained theater_ratio climb above 0.5 would signal piton-direction drift — a gate maintained ceremonially while unilateral practice resumes underneath it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gate_enforcement_theater_risk, empirical, 'Whether rising performative activity masks declining enforcement of the consent condition.').

omega_variable(
    nontreaty_nation_coverage_gap,
    'Does centering treaty covenants leave unceded-territory nations, Metis communities, and non-status Indigenous people outside the coordination architecture the constraint claims to provide?',
    'Comparative analysis of how the consent gate treats title-based versus covenant-based claims in litigation and negotiation, and whether excluded nations gain comparable seating through adjacent instruments.',
    'If the gap is real, the constraint''s coordination function covers less territory than its supersession doctrine claims, strengthening the excluded-seat objection and weakening the pure-coordination component of the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nontreaty_nation_coverage_gap, conceptual, 'Whether the treaty-centered frame under-covers the Indigenous polities it implicitly binds.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__treaty_primacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(slb_tpr_tr_t0, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(slb_tpr_tr_t0, observed).
narrative_ontology:measurement(slb_tpr_tr_t8, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement_basis(slb_tpr_tr_t8, observed).
narrative_ontology:measurement(slb_tpr_tr_t16, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement_basis(slb_tpr_tr_t16, observed).
narrative_ontology:measurement(slb_tpr_tr_t24, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement_basis(slb_tpr_tr_t24, observed).
narrative_ontology:measurement(slb_tpr_tr_t32, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 32, 0.34).
narrative_ontology:measurement_basis(slb_tpr_tr_t32, observed).
narrative_ontology:measurement(slb_tpr_tr_t40, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement_basis(slb_tpr_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(slb_tpr_be_t0, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(slb_tpr_be_t0, observed).
narrative_ontology:measurement(slb_tpr_be_t8, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 8, 0.25).
narrative_ontology:measurement_basis(slb_tpr_be_t8, observed).
narrative_ontology:measurement(slb_tpr_be_t16, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement_basis(slb_tpr_be_t16, observed).
narrative_ontology:measurement(slb_tpr_be_t24, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement_basis(slb_tpr_be_t24, observed).
narrative_ontology:measurement(slb_tpr_be_t32, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 32, 0.48).
narrative_ontology:measurement_basis(slb_tpr_be_t32, observed).
narrative_ontology:measurement(slb_tpr_be_t40, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement_basis(slb_tpr_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(slb_tpr_su_t0, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(slb_tpr_su_t0, observed).
narrative_ontology:measurement(slb_tpr_su_t8, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 8, 0.21).
narrative_ontology:measurement_basis(slb_tpr_su_t8, observed).
narrative_ontology:measurement(slb_tpr_su_t16, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 16, 0.29).
narrative_ontology:measurement_basis(slb_tpr_su_t16, observed).
narrative_ontology:measurement(slb_tpr_su_t24, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 24, 0.36).
narrative_ontology:measurement_basis(slb_tpr_su_t24, observed).
narrative_ontology:measurement(slb_tpr_su_t32, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 32, 0.41).
narrative_ontology:measurement_basis(slb_tpr_su_t32, observed).
narrative_ontology:measurement(slb_tpr_su_t40, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement_basis(slb_tpr_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__treaty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__grievance_threshold_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial question 'when is secession legitimate?' covers four structurally distinct claims, each locating legitimacy in a different source (procedure, majority will, injustice threshold, prior covenant). Each yields a distinct epsilon, a distinct beneficiary/victim structure, and a distinct enforcement profile, so the label decomposes into four linked stories. This file is the treaty_primacy member. The edges are peer relations among readings of one kernel rather than upstream/downstream dependency; contamination analysis should treat a legitimacy shift in any member as altering the operating environment of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__treaty_primacy_reading, powerful, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
