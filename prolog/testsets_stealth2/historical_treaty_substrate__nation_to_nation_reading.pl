% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__nation_to_nation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__nation_to_nation_reading, []).

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
 *   constraint_id: historical_treaty_substrate__nation_to_nation_reading
 *   human_readable: Historical Crown-Indigenous Treaties Administered Under the Nation-to-Nation Construal
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   Historical treaties between the Crown and Indigenous nations — concluded
 *   in the nineteenth and early twentieth centuries and administered ever
 *   since inside settler-state constitutional orders — form the
 *   land-relations substrate over which three incompatible readings compete.
 *   This story instantiates ONE of them, the nation-to-nation reading:
 *   treaties as international agreements between sovereign equals, requiring
 *   ongoing consent and subject to modern treaty-law principles. Per the
 *   epsilon-referent rule, epsilon here is authored for the STANDING
 *   ARRANGEMENT — the treaties as actually administered (domesticated into
 *   statute and common law, unilaterally interpreted, imperfectly performed)
 *   — assessed by this reading's own lights. By those lights the standing
 *   arrangement is deeply extractive: parties the agreements entitle as
 *   co-sovereigns with consent authority over territorial changes are
 *   administered as domestic dependents, and development proceeds on treaty
 *   territories where this construal of the agreements requires nation-level
 *   consent. The claim and the metrics are authored independently:
 *   claimed_type records what this reading takes the standing arrangement's
 *   structure to be (a hybrid that genuinely coordinates coexistence while
 *   conveying value asymmetrically), and the metrics record its descriptive
 *   operation. The sibling readings (extinguishment_reading,
 *   stewardship_reading) instantiate DIFFERENT constraints from the same
 *   kernel and are linked via network.affects_constraints; the contest itself
 *   lives in the omega variables, not inside this constraint. KEY AGENTS (by
 *   structural relationship): - indigenous_treaty_nations: primary target
 *   (organized/identity_locked) — entitled co-sovereigns administered as
 *   dependents; bear the arrangement's costs - settler_state_governments:
 *   agenda-setter and principal beneficiary (institutional/arbitrage) — sets
 *   interpretation, collects the land base and revenues -
 *   settler_resource_industries: secondary beneficiary (powerful/mobile) —
 *   receives licensed access to treaty territories -
 *   international_treaty_bodies: analytical observer
 *   (institutional/analytical) — names the compliance gap without enforcement
 *   power - non_signatory_indigenous_nations: excluded voice
 *   (organized/trapped) — bound by the template's precedent without having
 *   signed
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.71).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.66).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Historical Crown-Indigenous Treaties Administered Under the Nation-to-Nation Construal").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, '3135c413-4d55-434b-aef4-b4c5922bce25').
narrative_ontology:cs_kernel_codification('3135c413-4d55-434b-aef4-b4c5922bce25', fixed_text).
narrative_ontology:cs_authority_grounding('3135c413-4d55-434b-aef4-b4c5922bce25', lineage).
narrative_ontology:cs_interpretation_layer_present('3135c413-4d55-434b-aef4-b4c5922bce25').
narrative_ontology:cs_reading_relation('3135c413-4d55-434b-aef4-b4c5922bce25', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('3135c413-4d55-434b-aef4-b4c5922bce25', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('3135c413-4d55-434b-aef4-b4c5922bce25', foundational, indigenous_sovereignty_never_ceased).
narrative_ontology:cs_axiom_status(indigenous_sovereignty_never_ceased, holdable).
narrative_ontology:cs_axiom_grounding('3135c413-4d55-434b-aef4-b4c5922bce25', indigenous_sovereignty_never_ceased, deontological).
narrative_ontology:cs_axiom('3135c413-4d55-434b-aef4-b4c5922bce25', foundational, treaties_bind_as_international_law).
narrative_ontology:cs_axiom_status(treaties_bind_as_international_law, holdable).
narrative_ontology:cs_axiom_grounding('3135c413-4d55-434b-aef4-b4c5922bce25', treaties_bind_as_international_law, conventional).
narrative_ontology:cs_reference_frame('3135c413-4d55-434b-aef4-b4c5922bce25', sovereign_equals_founding_compact).
narrative_ontology:cs_drift_state('3135c413-4d55-434b-aef4-b4c5922bce25', contemporary_domestication_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('3135c413-4d55-434b-aef4-b4c5922bce25', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, settler_resource_industries).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, indigenous_treaty_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, indigenous_treaty_nations).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__nation_to_nation_reading, pacta_sunt_servanda).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__nation_to_nation_reading, sovereign_equality_of_treaty_parties).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__nation_to_nation_reading, free_prior_informed_consent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Signatory nations hold treaty entitlements — annuities, reserve lands, protected hunting and fishing livelihoods, promised schooling and medical care — and, under the agreements' own logic as sovereign parties, consent authority over territorial changes. In practice the administering state interprets the texts through its own courts and legislation, licenses development on treaty territories without nation-level consent, and delivers consideration whose real value has shrunk for over a century. The nations cannot exit: their membership, territory, and legal standing are constituted through the treaty relationship itself. Litigation, negotiation, public mobilization, and international advocacy are the levers that remain.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_treaty_nations, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, indigenous_treaty_nations, beneficiary).

% Federal and provincial Crowns administer the treaties through their own statutes, courts, and departments: they survey and hold reserve lands, pay annuities at nominal values fixed in the signing era, legislate over matters the oral accounts record as promised away, and authorize forestry, mining, hydro, and settlement across treaty territories. They set the interpretive agenda — deciding what the agreements mean and what honour requires — while collecting the land base, resource revenues, and tax capacity the arrangements convey. They cannot leave the relationship, but they arbitrage between domestic frames, judicial doctrines, and legislative amendments to manage its costs.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments, beneficiary).

% Forestry, mining, energy, and agricultural operators receive permits and leases on treaty territories issued by the settler state without the nation-level consent this reading's construal of the agreements requires. They take resource value directly and move capital to the next jurisdiction when local resistance or regulation raises costs; their exposure to the treaty relationship runs entirely through the state's licensing decisions.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_resource_industries, beneficiary,
    powerful, immediate, mobile, global).

% United Nations treaty bodies and special rapporteurs review the administering state's conduct against international human-rights and treaty obligations, including free, prior, and informed consent standards. They issue concluding observations and country reports that name the gap between treaty text and administration, but hold no direct enforcement power inside the settler constitutional order.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, international_treaty_bodies, observer,
    institutional, generational, analytical, global).

% Nations whose territories were absorbed by the settler state without any treaty — much of the Pacific slope among others — watch the treaty substrate supply the legal template applied to everyone. They pursue title claims from outside the treaty framework, would contest the premise that signature and payment settle the land question, and are absent from the bilateral interpretive arena the treaties construct.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, non_signatory_indigenous_nations, excluded,
    organized, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__nation_to_nation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The treaties solved a real collective problem: two sovereign peoples sharing a continent needed a framework that ended recurrent warfare, secured alliance during imperial competition, and structured coexistence — defined land-sharing, payment relationships, protected livelihoods, and mutual obligations — instead of leaving every encounter to force.
% TRANSFER_FUNCTION: Moves land, resources, and jurisdictional precedence from Indigenous treaty nations to the settler state and its licensed industries, against annuity payments, reserve allocations, and promised services whose real value has fallen far below what was conveyed; also moves interpretive authority over the agreements' meaning to the settler state's courts and departments.
% ABSENT_VOICES: Non-signatory Indigenous nations, whose territories were absorbed without any treaty and who sit outside the bilateral arena the agreements construct; future generations of treaty nations, bound by ancestral signatures they did not give; and the oral-record tradition of the negotiations, whose promises never entered the written texts. They stand in international fora, in oral history, and in litigation from the margins, with limited traction on how the administering state reads the agreements.
% DISAPPEARANCE_RATIONALE: If the treaty substrate vanished overnight, the land-title order of the treaty territories would lose its foundation: reserve boundaries, annuity obligations, and the settler state's assumed underlying title would all need reconstruction, and every permit issued on treaty lands would hang from an illegitimated chain. The settler state's jurisdictional map, municipal existence, and resource economy across the treaty areas would rearrange around whatever replaced the agreements.
% FOUNDING_PROBLEM: Ending cycles of warfare and insecure coexistence between expanding settler polities and Indigenous nations, and securing alliance and safe passage amid imperial competition, by exchanging defined land-sharing, annual payments, and protected ways of life for peace and ordered relations on shared territory.
% FOUNDING_PROBLEM_CORROBORATION: Both parties' own records attest the founding problem: the treaty texts and commissioners' dispatches on the settler side, and the orally transmitted accounts of the negotiations on the nations' side. Outside the beneficiary set, the Royal Commission on Aboriginal Peoples, academic treaty-history scholarship, and the church archives that hosted the negotiations corroborate that the founding problem was real and that its resolution remains disputed; no source outside the benefiting parties attests that the founding bargain was fully performed.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__nation_to_nation_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__nation_to_nation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__nation_to_nation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(historical_treaty_substrate__nation_to_nation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__nation_to_nation_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.71 at interval end) because the value conveyed through the treaties — land, resources, jurisdictional precedence — accrued overwhelmingly to the settler side while the returned consideration (annuities fixed at nineteenth-century nominal values, a diminishing reserve base, unevenly delivered services) fell far below it; under this reading's construal the shortfall measures breach, not market price. Suppression (0.66) is structural: the pass system, the legislative overlay governing reserve life, surrender machinery, and the modern injunction-and-court-order apparatus exist to hold the unilateral interpretation in place against objection; suppression is authored as a raw structural property and is not scaled by power or scope. Theater rises past 0.5 by interval end: honour-of-the-Crown rhetoric, commemoration, formal apologies, and consultation processes increasingly perform fidelity and consent while substantive consent authority remains unexercised — proxy goals replacing the real function. The three series share one time grid; the 1990 dip in extraction reflects constitutional recognition of treaty rights and specific-claims settlements returning real value, before renewed resource pressure and implementation gaps push the trajectory higher again. The rising suppression_requirement series traces enforcement intensification across the interval — from police-enforced movement controls through legislative consolidation to the contemporary injunction era — which is why suppression_requirement is tracked rather than left static.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the settler-state seat the treaties are burdens honorably administered — obligations it funds, constraints it accepts, a relationship it manages — and the arrangement looks like costly good faith. From the Indigenous-nation seat the same structure operates as administered dispossession: entitlements read down, consent bypassed, consideration eroded. The nations' dual position (secondary_role beneficiary) encodes the pivot: under the agreements' own terms they collect; under the administration they pay. The engine computes per-seat classifications from the structural data; this story does not adjudicate which seat is right — the sibling stories carry the competing construals.
 *
 * DIRECTIONALITY LOGIC:
 *   Settler-state governments derive near the beneficiary end: they set the rules, capture the land base and revenues, and face chiefly the obligations they choose to fund — though not at the pure-beneficiary pole, since annuity and service obligations are real costs they cannot fully shed. Settler resource industries sit nearest the beneficiary pole: licensed access without consent obligations, capital mobile across jurisdictions. Indigenous treaty nations derive near the full-target end: they bear the costs and cannot exit — identity_locked, because nationhood, territory, and legal personhood are constituted through the very relationship being administered against them, and trapped or identity-locked targets sit nearer the full-target end than mobile ones. International bodies occupy the analytical seat, and non-signatory nations are excluded — outside the beneficiary/victim flow but positioned by the template's precedential force over the untreatied portions of the territory.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the standing arrangement as tangled_rope rather than snare preserves the genuine coordination core — the treaties really did end warfare, structure coexistence, and deliver real if inadequate consideration — while forcing the asymmetric transfer into the open. Reading it as a snare would erase the coordination function the nations themselves invoke when they demand implementation rather than abolition; reading it as a rope would launder the transfer as the price of peace. The mandatrophy question is live rather than resolved: the founding problem (stable coexistence) persists in contested form, so the arrangement has not outlived its function — but the founding bargain has been so altered by unilateral administration that the genealogy interview flags the arrangement for scrutiny rather than closure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the historical_treaty_substrate kernel governs the substrate''s classification — this nation-to-nation construal, the extinguishment construal, or the stewardship construal?',
    'Comparative evaluation across the three sibling stories: whichever construal the relevant legal-political community adopts reassigns every seat — extinguishment removes Indigenous consent rights and closes the entitlement ledger; stewardship replaces the sovereignty frame with relational coexistence obligations. Adoption is observable in constitutional doctrine, court rulings on treaty status, or negotiated recognition instruments.',
    'Switching readings changes the beneficiary/victim structure wholesale: under extinguishment the nations are former sellers with no live entitlements and measured extraction collapses toward administration cost; under stewardship both parties become mutual obligors and the asymmetry is reframed as relational betrayal rather than sovereign breach.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which of three rival construals of the treaty kernel is authoritative.').

omega_variable(
    ongoing_consent_scope,
    'Does the ongoing-consent requirement extend to all resource development on treaty territories, or only to changes of territorial boundary and jurisdiction?',
    'Comparative treaty-law analysis together with the nations'' own articulated positions: if consent scope covers subsurface and renewable-resource licensing, most current development on treaty lands counts as proceeding without required consent; if limited to boundary change and jurisdictional transfer, only alienations and overrides count.',
    'Determines how much of the measured shortfall is breach versus agreed sharing — a wide consent scope sustains high extraction; a narrow scope lowers the effective extraction attributable to the standing arrangement materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ongoing_consent_scope, conceptual, 'Scope of the consent right this reading asserts over treaty territories.').

omega_variable(
    international_recognition_pathway,
    'Will the treaties ever be adjudicated as international instruments inside the settler constitutional order, or does this construal''s enforceability depend entirely on external pressure and voluntary domestic reinterpretation?',
    'Track court doctrine on treaty status and federalism, legislative adoption of free-prior-informed-consent standards, and any acceptance of international mechanism jurisdiction over treaty disputes.',
    'If no domestic pathway opens, the reading''s obligations operate only as external critique — enforcement stays outside the order and the unilateral administration persists unopposed within it; if a pathway opens, enforcement migrates inward and the shortfall becomes actionable breach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_recognition_pathway, empirical, 'Enforceability pathway for the international-law construal of the treaties.').

omega_variable(
    oral_written_term_divergence,
    'Which record constitutes the agreement — the written texts or the orally transmitted promises recorded in commissioners'' accounts — where the two diverge?',
    'Oral-history adjudication alongside textual scholarship; commissions and courts have begun weighing oral tradition as primary evidence of treaty meaning.',
    'The divergence is large (protected lifeways, medical care, schooling, and non-interference promises appear variably across the two records); resolving it shifts both the entitlement baseline and the size of the measured shortfall.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oral_written_term_divergence, empirical, 'Constitutive-text ambiguity between written and oral treaty records.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 1871, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1871, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1871, 0.15).
narrative_ontology:measurement_basis(hist_tr_t1871, observed).
narrative_ontology:measurement(hist_tr_t1900, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1900, 0.22).
narrative_ontology:measurement_basis(hist_tr_t1900, observed).
narrative_ontology:measurement(hist_tr_t1930, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1930, 0.3).
narrative_ontology:measurement_basis(hist_tr_t1930, observed).
narrative_ontology:measurement(hist_tr_t1960, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1960, 0.38).
narrative_ontology:measurement_basis(hist_tr_t1960, observed).
narrative_ontology:measurement(hist_tr_t1990, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1990, 0.45).
narrative_ontology:measurement_basis(hist_tr_t1990, observed).
narrative_ontology:measurement(hist_tr_t2025, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2025, 0.52).
narrative_ontology:measurement_basis(hist_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(hist_be_t1871, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1871, 0.42).
narrative_ontology:measurement_basis(hist_be_t1871, observed).
narrative_ontology:measurement(hist_be_t1900, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1900, 0.55).
narrative_ontology:measurement_basis(hist_be_t1900, observed).
narrative_ontology:measurement(hist_be_t1930, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1930, 0.63).
narrative_ontology:measurement_basis(hist_be_t1930, observed).
narrative_ontology:measurement(hist_be_t1960, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1960, 0.66).
narrative_ontology:measurement_basis(hist_be_t1960, observed).
narrative_ontology:measurement(hist_be_t1990, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1990, 0.64).
narrative_ontology:measurement_basis(hist_be_t1990, observed).
narrative_ontology:measurement(hist_be_t2025, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2025, 0.71).
narrative_ontology:measurement_basis(hist_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1871, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1871, 0.4).
narrative_ontology:measurement_basis(hist_su_t1871, observed).
narrative_ontology:measurement(hist_su_t1900, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1900, 0.55).
narrative_ontology:measurement_basis(hist_su_t1900, observed).
narrative_ontology:measurement(hist_su_t1930, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1930, 0.6).
narrative_ontology:measurement_basis(hist_su_t1930, observed).
narrative_ontology:measurement(hist_su_t1960, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1960, 0.58).
narrative_ontology:measurement_basis(hist_su_t1960, observed).
narrative_ontology:measurement(hist_su_t1990, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1990, 0.62).
narrative_ontology:measurement_basis(hist_su_t1990, observed).
narrative_ontology:measurement(hist_su_t2025, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2025, 0.66).
narrative_ontology:measurement_basis(hist_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__nation_to_nation_reading, resource_allocation).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__stewardship_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the historical treaties' covers three structurally distinct claims with different epsilon values, different beneficiary/victim sets, and different failure modes. The extinguishment reading (upstream in institutional practice — the settler state's administration cites completed-transaction logic) treats the agreements as closed exchanges; this nation-to-nation reading treats them as live international compacts whose breach is measurable; the stewardship reading reframes the whole as relational obligation without sovereignty transfer. Each story links the other two via network.affects_constraints; the family exists so contamination propagates — a doctrinal shift in one reading's status changes the operating environment of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
