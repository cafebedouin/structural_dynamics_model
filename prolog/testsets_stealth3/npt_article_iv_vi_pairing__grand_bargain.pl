% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__grand_bargain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__grand_bargain, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__grand_bargain
 *   human_readable: NPT Article IV / Article VI Reciprocal Bargain — Grand Bargain Reading
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   The Nuclear Non-Proliferation Treaty pairs Article IV — an 'inalienable
 *   right' of non-weapon parties to peaceful nuclear technology — with
 *   Article VI — a good-faith undertaking by weapon parties to pursue
 *   disarmament. The grand_bargain reading holds these limbs reciprocal: NNWS
 *   restraint is consideration paid against WS performance, so persistent
 *   Article VI non-performance corrodes the legitimacy of everything Article
 *   IV-side demands. The standing arrangement under contest — the regime as
 *   it actually operates, with indefinite extension since 1995,
 *   supplier-gated enrichment access, and no negotiated multilateral
 *   disarmament in over fifty years — is assessed here by that reading's own
 *   lights, which is what fixes this story's epsilon: the bargain as
 *   breached, not the bargain as it would run if honored. KEY AGENTS (by
 *   structural relationship): nuclear_weapon_states_p5: agenda-setter
 *   (institutional/arbitrage) — administers the regime, retains arsenals at
 *   no binding cost; nsg_supplier_states: beneficiary
 *   (institutional/constrained) — collects gatekeeping leverage over
 *   fuel-cycle access; us_extended_deterrence_allies: beneficiary
 *   (powerful/identity_locked) — deterrence without weapons, invested in the
 *   arsenals' retention; nonaligned_nnws_parties: primary target
 *   (organized/trapped) — restraint banked against unkept promises;
 *   enr_aspirant_nnws: secondary target (moderate/constrained) — Article IV
 *   narrowed by conditions beyond treaty text; iaea_secretariat:
 *   administering executor (institutional/constrained);
 *   tpnw_coalition_states: excluded challenger (organized/mobile) — built the
 *   parallel instrument; arms_control_legal_scholars: analytical observer.
 *   Per the claim/metric independence rule: the claimed_type (tangled_rope)
 *   states what this seat believes structurally true — a real reciprocal
 *   design currently operating with asymmetric extraction under active
 *   enforcement — and the metrics state what is descriptively true of its
 *   operation; neither was tuned toward the other or toward a predicted
 *   engine output.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, 0.68).
domain_priors:suppression_score(npt_article_iv_vi_pairing__grand_bargain, 0.66).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__grand_bargain, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Article IV / Article VI Reciprocal Bargain — Grand Bargain Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, 'f05062e8-25f6-47ae-8a52-dc9f23b01ce8').
narrative_ontology:cs_kernel_codification('f05062e8-25f6-47ae-8a52-dc9f23b01ce8', fixed_text).
narrative_ontology:cs_authority_grounding('f05062e8-25f6-47ae-8a52-dc9f23b01ce8', lineage).
narrative_ontology:cs_interpretation_layer_present('f05062e8-25f6-47ae-8a52-dc9f23b01ce8').
narrative_ontology:cs_reading_relation('f05062e8-25f6-47ae-8a52-dc9f23b01ce8', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('f05062e8-25f6-47ae-8a52-dc9f23b01ce8', npt_article_iv_vi_pairing__abolitionist, influences).
narrative_ontology:cs_axiom('f05062e8-25f6-47ae-8a52-dc9f23b01ce8', foundational, nnws_restraint_is_conditional_consideration).
narrative_ontology:cs_axiom_status(nnws_restraint_is_conditional_consideration, holdable).
narrative_ontology:cs_axiom_grounding('f05062e8-25f6-47ae-8a52-dc9f23b01ce8', nnws_restraint_is_conditional_consideration, conventional).
narrative_ontology:cs_axiom('f05062e8-25f6-47ae-8a52-dc9f23b01ce8', foundational, article_vi_nonperformance_vitiates_article_iv_basis).
narrative_ontology:cs_axiom_status(article_vi_nonperformance_vitiates_article_iv_basis, holdable).
narrative_ontology:cs_axiom_grounding('f05062e8-25f6-47ae-8a52-dc9f23b01ce8', article_vi_nonperformance_vitiates_article_iv_basis, conventional).
narrative_ontology:cs_reference_frame('f05062e8-25f6-47ae-8a52-dc9f23b01ce8', reciprocal_exchange_equilibrium).
narrative_ontology:cs_drift_state('f05062e8-25f6-47ae-8a52-dc9f23b01ce8', post_revcon_stalemate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f05062e8-25f6-47ae-8a52-dc9f23b01ce8', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states_p5).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, nsg_supplier_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, us_extended_deterrence_allies).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, nonaligned_nnws_parties).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, enr_aspirant_nnws).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, enr_aspirant_nnws).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, iaea_secretariat).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, pacta_sunt_servanda).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, treaty_reciprocity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Depositary governments and permanent Security Council members. They convene and shape the review conferences, hold consensus leverage over export-control decisions, define what counts as compliance in public discourse, and retain their arsenals while reaffirming their disarmament commitments. Treaty amendment requires their ratification and Council action requires their assent, so continuing present policy imposes no binding cost on them; adjusting posture between professed commitment and actual deployment is always available.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states_p5, agenda_setter,
    institutional, generational, arbitrage, global).

% Industrialized exporter states that operate the coordinated export-control arrangements deciding which civil nuclear transfers proceed. They gain commercial leverage over importing states and shelter for domestic fuel-cycle industries. Their governing rule is consensus, so loosening terms unilaterally would break the arrangement that gives them their gatekeeping position, and exiting it would mean surrendering that position to others.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nsg_supplier_states, beneficiary,
    institutional, biographical, constrained, global).

% Allied governments under extended deterrence that obtain security against nuclear attack without fielding their own weapons. They support retention of the arsenals backing the umbrella and resist timetables that would unsettle it. Their defense institutions, public opinion, and budgets are built around the umbrella's continuation; replacing it would mean constructing deterrent options their postwar settlements deliberately forwent.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, us_extended_deterrence_allies, beneficiary,
    powerful, biographical, identity_locked, continental).

% Non-weapon parties that renounced the weapons option, accept comprehensive safeguards on their civil programs, and forgo independent deterrence. In exchange they hold a treaty guarantee of peaceful-use cooperation and a standing disarmament commitment that has yielded no negotiated multilateral reductions across five decades of membership. Formal withdrawal carries heavy penalty, as the experience of the sole withdrawing party shows; remaining means financing review processes whose outcome documents their delegations cannot convert into performance.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nonaligned_nnws_parties, payer,
    organized, generational, trapped, global).

% Developing parties pursuing enrichment or reprocessing capability for energy autonomy. They receive some peaceful-use cooperation and technical assistance, yet meet supplier-imposed conditions beyond the treaty's text when they press for fuel-cycle rights, and prominent cases of pressing have drawn Board referral, sanctions, and years of suspended negotiation.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, enr_aspirant_nnws, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, enr_aspirant_nnws, beneficiary).

% The verification agency implementing the safeguard measures on which the arrangement runs. It executes mandates handed down by member states, inspects declared material, reports noncompliance findings, and absorbs criticism from every camp. Its workload and budget scale with the regime, but the reciprocity terms it polices are not its to set — those rest with the member states.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, iaea_secretariat, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, iaea_secretariat, beneficiary).

% States aligned with the humanitarian initiative that judged the internal route to enforceable reciprocity closed and negotiated a parallel prohibition treaty outside this arrangement. Inside its review process their enforceability proposals are repeatedly dropped under consensus rules; outside it they demonstrated the exit is exercisable by building and bringing into force an alternative instrument.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, tpnw_coalition_states, excluded,
    organized, generational, mobile, global).

% International lawyers and arms-control analysts outside the negotiating rooms who compile compliance records, draft model verification protocols, and publish assessments of whether promised performance has occurred. Their seat watches the whole structure and owes nothing to any camp.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, arms_control_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states_p5).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__grand_bargain, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates mutual restraint among states to prevent cascade proliferation while keeping civil nuclear commerce open: non-weapon parties submit to common verification standards, weapon parties pledge eventual disarmament, exporters pool transfer decisions, and all parties share a standing forum for reviewing the whole exchange.
% TRANSFER_FUNCTION: Moves security restraint — renounced weapons options, accepted inspections, forgone deterrence — from non-weapon parties to the collective nonproliferation good; moves peaceful-use technology and assistance, gated by supplier discretion, from exporters to importers; moves political legitimacy for retained arsenals from the general adherence of non-weapon parties to the weapon states; moves unsecured disarmament promises from weapon states onto the treaty record.
% ABSENT_VOICES: Populations exposed to historic atmospheric and underground testing never received a seat in the review machinery; the three non-parties whose arsenals the arrangement brackets are governed by its effects without membership; the humanitarian-initiative majority holds enforceability positions that consensus procedure removes from outcome documents before adoption.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would unwind the comprehensive safeguards web, dissolve the coordinated export-control cartel, remove the legal basis for extended-deterrence burden-sharing, and leave non-weapon parties with no forum in which their restraint is banked against anything — regional hedging cascades and emergency renegotiation would follow within years, not decades.
% FOUNDING_PROBLEM: Built against the 1960s forecast that dozens of states would acquire nuclear weapons within years: halt horizontal spread among non-weapon states, preserve the existing arsenals' legality for their holders, and open civil nuclear commerce so restraint had something in it for those giving it up.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: SIPRI yearbooks and UNIDIR/UNODA records attest that state-level horizontal spread slowed dramatically among adherents while arsenals of the five grew qualitatively throughout; the published treaty histories (e.g., Müller) corroborate that reciprocity was written in as inducement, not enforced as obligation; humanitarian-initiative states parties attest the founding problem persists in altered form. Weapon states themselves do not corroborate the reciprocity-failure characterization — they affirm performance in good faith — and no neutral body adjudicates the dispute, so corroboration is partial and contested rather than settled.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__grand_bargain, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__grand_bargain, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__grand_bargain, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__grand_bargain, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__grand_bargain, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.68 because the restraint side of the exchange is fully collected while the performance side has gone undelivered across the whole interval — yet the arrangement does deliver real goods (state-level cascade prevention among adherents, a working verification web, some technology flow), which keeps it below snare territory from this seat. Suppression is 0.66 and rising: the arrangement's persistence depends on comprehensive safeguards backed by referral politics, a consensus-gated supplier cartel, and the demonstrated penalty on formal exit, not on voluntary renewal. Theater_ratio reaches 0.46 because the disarmament-review machinery has become substantially performative — the 2000 outcome's thirteen practical steps were adopted then shelved, and successive review conferences have ended in stalemate over the very reciprocity this reading names — while the safeguards function beneath stays real. Accessibility_collapse is 0.45: exits exist (Article X withdrawal, the parallel prohibition treaty, regional zones) but each is heavily priced or incomplete. Resistance is 0.58: the non-aligned bloc's recurring demands, the humanitarian initiative's exodus into a rival instrument, and fuel-cycle challengers are sustained, organized pushback. The temporal series run on one shared grid (t = 0, 5, 10, 20, 25, 35, 45, 55) with every tracked metric authored at every point; all values are observed historical assessment, not projection. Cyclical note: the five-year review rhythm generates a sawtooth — bargain rhetoric revived at each conference, decaying between — and that oscillation is itself load-bearing: each revival defuses exit pressure without transferring performance, an intermittent-legitimation dynamic rather than noise.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the agenda-setter seat (weapon states), the arrangement is a successfully maintained coordination achievement they defend and staff — low experienced extraction, enforcement aimed outward. From the trapped payer seat (non-aligned NNWS), the identical structure operates as collected restraint against unkept consideration — high experienced extraction with suppressed exit. From the excluded seat (humanitarian-initiative states), the arrangement is a closed conversation whose consensus rule manufactures unanimity by removing dissent before adoption. The engine computes these per-seat classifications from the authored structural data; this story's claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation and no overrides are needed: weapon states are beneficiaries with arbitrage-grade exit (they can reinterpret, modernize, and stall at negligible cost), placing them near the beneficiary pole; supplier states collect gatekeeping rents under constrained exit, slightly less extreme; umbrella allies are identity-locked beneficiaries — the identity lock amplifies attachment to the arrangement rather than extraction borne from it. Non-aligned NNWS are trapped payers, pushed toward the full-target end; fuel-cycle aspirants pay with a secondary beneficiary offset, moderating their d. The IAEA secretariat sits near-symmetric: it executes mandates and scales with the regime but sets none of the terms and absorbs criticism from all camps — the structural derivation captures this from its dual declaration without intervention. The excluded coalition sits mostly outside the chi surface: it pays little into and collects little from this arrangement's daily operation, having moved its activity to the parallel instrument.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both failure directions. Reading the pairing as a pure rope launders the uncollected half of the exchange — restraint fully taken, disarmament never delivered — into benign coordination cost. Reading it as a snare erases the genuine goods (cascade prevention, verification infrastructure, a banking place for restraint) that even the payer seats renew rather than abandon. Tangled rope holds both facts: a real coordination function with extraction layered through the same structure, requiring active enforcement to persist. On genealogy: the founding problem (horizontal cascade) is partially addressed but transformed rather than solved, so founding_problem_status is contested, not dead — no mismatch flag fires, correctly, because the arrangement is degraded rather than zombified. Forward migration paths are visible in the data: performed Article VI would pull the structure toward rope; openly repudiated Article VI would strip the bargain cover and force either renegotiation or collapse; continued drift along the current trajectory pushes theater past 0.5, at which the review machinery's proxy function (document production) fully displaces the bargain function it simulates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Does the reciprocity-failure structure described here belong to the treaty arrangement itself, or only to one contestable reading of it — this file instantiates the grand_bargain reading of kernel npt_article_iv_vi_pairing, and sibling readings (nonproliferation_primary, abolitionist) would locate the breach, the victims, and epsilon elsewhere?',
    'Track adoption: if a review cycle adopts enforceable-reciprocity language with verification machinery, the grand bargain hardens into operative law; if weapon states maintain the nonproliferation_primary reading indefinitely, the structure remains contested and per-seat classification diverges permanently.',
    'From the nonproliferation_primary seat the same standing arrangement computes nearer a defended coordination mechanism with modest excess; from the abolitionist seat it computes as a perpetuated dual-use hazard with maximal victim set. This file''s epsilon of 0.68 is valid only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which reading of the IV/VI kernel governs the classification surface.').

omega_variable(
    article_vi_justiciability,
    'Is Article VI legally enforceable — justiciable as reciprocal consideration — or interpretively non-justiciable, as the weapon states maintain?',
    'An ICJ advisory opinion, an arbitral proceeding, or a review-cycle decision conferring justiciability on Article VI performance records.',
    'Enforceable Article VI converts weapon states from agenda-setters into breach actors, redistributes effective extraction sharply upward onto them, and licenses the remedial moves this reading anticipates; non-justiciable Article VI collapses this reading toward the sibling''s and lowers measured extraction on the weapon-state side to zero.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability, empirical, 'Whether the disarmament limb of the bargain is law or aspiration.').

omega_variable(
    withdrawal_licensing_precedent,
    'Does demonstrated Article VI non-performance actually license NNWS withdrawal under Article X or expansion of Article IV entitlements, or does the enforcement system punish any exercise of that license regardless of justification?',
    'Comparative analysis of withdrawal and hedging episodes against the documented compliance record of the withdrawing party — the sole completed withdrawal was sanctioned despite its stated reciprocity rationale; observe whether any future claim of Article VI breach meets recognition or punishment.',
    'If the license is real, the arrangement is a genuine conditional bargain whose enforcement gap is transitional; if the license is nominal and always punished, the conditionality is decorative and the arrangement''s suppression component is doing all the load-bearing work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_licensing_precedent, empirical, 'Whether the bargain''s exit clause functions as advertised.').

omega_variable(
    verification_reciprocity_feasibility,
    'Can weapon-state warhead material be accounted for under safeguards symmetric to what NNWS already accept, at acceptable cost and without unacceptable security disclosure?',
    'IPFM and bilateral managed-access demonstrations (e.g., the UK-Norway initiative, Trilateral Initiative archives), scaled to a multilateral protocol.',
    'Feasible verification removes the standard weapon-state excuse and makes nonperformance a naked choice — supporting the breach framing and raising the legitimacy cost of the standing asymmetry; infeasibility would shift blame from will to capacity and soften this reading''s classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_reciprocity_feasibility, empirical, 'Whether symmetric verification is technically achievable.').

omega_variable(
    umbrella_ally_position_ambiguity,
    'Are extended-deterrence allies beneficiaries of the standing arrangement, or covert payers whose hosting, subsidy dependence, and foregone sovereignty are costs disguised by the umbrella?',
    'Counterfactual costing: price the allies'' host-nation contributions, basing exposure, and lost autonomous options against the insurance value of the umbrella under credible US-retrenchment scenarios.',
    'If the umbrella is net subsidy, allies belong at the beneficiary pole and their identity-locked exit merely stabilizes the arrangement; if net payment, they are a third victim class and the arrangement''s extraction base widens materially.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(umbrella_ally_position_ambiguity, conceptual, 'Net position of umbrella states within the bargain.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of NNWS dissent primarily structural (export denial, sanction exposure, referral politics, withdrawal penalties) or partly internalized (socialization of restraint as the marker of responsible statehood that survives removal of the barriers)?',
    'Post-exit trajectory analysis: track states that left or defied the arrangement (the withdrawn party, threshold states) and measure whether their restraint norms persisted after the enforcement pressure lifted; survey NAM delegations on whether restraint would continue absent enforcement.',
    'If substantially internalized, the arrangement''s coercive overhead is lower than the structural measure suggests and part of its stability is preference convergence — moving the classification toward rope; if purely structural, removal of enforcement collapses the bargain immediately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, conceptual, 'Mechanism composition of the arrangement''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__grand_bargain, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_gb_tr_t0, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(npt_gb_tr_t0, observed).
narrative_ontology:measurement(npt_gb_tr_t5, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(npt_gb_tr_t5, observed).
narrative_ontology:measurement(npt_gb_tr_t10, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(npt_gb_tr_t10, observed).
narrative_ontology:measurement(npt_gb_tr_t20, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(npt_gb_tr_t20, observed).
narrative_ontology:measurement(npt_gb_tr_t25, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 25, 0.36).
narrative_ontology:measurement_basis(npt_gb_tr_t25, observed).
narrative_ontology:measurement(npt_gb_tr_t35, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(npt_gb_tr_t35, observed).
narrative_ontology:measurement(npt_gb_tr_t45, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 45, 0.44).
narrative_ontology:measurement_basis(npt_gb_tr_t45, observed).
narrative_ontology:measurement(npt_gb_tr_t55, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 55, 0.46).
narrative_ontology:measurement_basis(npt_gb_tr_t55, observed).

% Extraction over time
narrative_ontology:measurement(npt_gb_be_t0, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(npt_gb_be_t0, observed).
narrative_ontology:measurement(npt_gb_be_t5, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 5, 0.46).
narrative_ontology:measurement_basis(npt_gb_be_t5, observed).
narrative_ontology:measurement(npt_gb_be_t10, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(npt_gb_be_t10, observed).
narrative_ontology:measurement(npt_gb_be_t20, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(npt_gb_be_t20, observed).
narrative_ontology:measurement(npt_gb_be_t25, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(npt_gb_be_t25, observed).
narrative_ontology:measurement(npt_gb_be_t35, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 35, 0.64).
narrative_ontology:measurement_basis(npt_gb_be_t35, observed).
narrative_ontology:measurement(npt_gb_be_t45, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 45, 0.67).
narrative_ontology:measurement_basis(npt_gb_be_t45, observed).
narrative_ontology:measurement(npt_gb_be_t55, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 55, 0.68).
narrative_ontology:measurement_basis(npt_gb_be_t55, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt_gb_su_t0, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(npt_gb_su_t0, observed).
narrative_ontology:measurement(npt_gb_su_t5, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 5, 0.38).
narrative_ontology:measurement_basis(npt_gb_su_t5, observed).
narrative_ontology:measurement(npt_gb_su_t10, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(npt_gb_su_t10, observed).
narrative_ontology:measurement(npt_gb_su_t20, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(npt_gb_su_t20, observed).
narrative_ontology:measurement(npt_gb_su_t25, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 25, 0.55).
narrative_ontology:measurement_basis(npt_gb_su_t25, observed).
narrative_ontology:measurement(npt_gb_su_t35, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 35, 0.6).
narrative_ontology:measurement_basis(npt_gb_su_t35, observed).
narrative_ontology:measurement(npt_gb_su_t45, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 45, 0.63).
narrative_ontology:measurement_basis(npt_gb_su_t45, observed).
narrative_ontology:measurement(npt_gb_su_t55, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 55, 0.66).
narrative_ontology:measurement_basis(npt_gb_su_t55, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__grand_bargain, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__abolitionist).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iii_comprehensive_safeguards).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, tpnw_nuclear_weapons_prohibition).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the NPT bargain' conflates three structurally distinct claims about one kernel text. This story authors the grand_bargain reading (reciprocal obligations, breach-corrosion of legitimacy, weapon states as potential breach actors); the nonproliferation_primary reading (verification-conditioned Article IV, non-justiciable Article VI) and the abolitionist reading (mandated complete disarmament, humanitarian-law authority) are separate files with separate epsilon values, victim sets, and classifications. Family edges run through network.affects_constraints in all three files; the upstream established claim (nonproliferation_primary, highest empirical confidence among the siblings) is cited as evidence within this reading's dispute, and this reading's breach-framing in turn feeds the abolitionist coalition's downstream legitimacy case. Any evaluation that changes observable — asking whether the treaty 'works' simpliciter — crosses stories and yields inconsistent epsilon; the decomposition exists precisely to prevent that.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
