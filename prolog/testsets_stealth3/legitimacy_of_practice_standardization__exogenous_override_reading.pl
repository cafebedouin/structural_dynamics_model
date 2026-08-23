% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__exogenous_override_reading
 *   human_readable: State-Decreed Practice Standardization Regime (Exogenous Override Reading)
 *   domain: political_history/modernization_studies
 *
 * SUMMARY:
 *   Across the interwar modernization waves (Republican Anatolia, Pahlavi
 *   Iran, and cognate cases), governments decreed wholesale replacement of
 *   customary timekeeping and dress with civil standards — a Gregorian-style
 *   calendar, hat and clothing codes — justified publicly by fiscal
 *   synchronization, administrative legibility, and alignment with treaty
 *   partners. Implementation was abrupt and enforced: special tribunals,
 *   fines, and police action against visible noncompliance, heaviest in the
 *   first decade and a half. Compliance arrived quickly in cities and on
 *   paper; in the countryside the lunar-agrarian reckoning and ritual dress
 *   persisted privately for forty years and more, producing a stable double
 *   life rather than a completed transition. This story instantiates the
 *   exogenous_override_reading of the legitimacy_of_practice_standardization
 *   kernel: whatever legitimacy practice standardization carries flows from
 *   sovereign decree aimed at collective benefit, and the constraint under
 *   assessment is the decree-and-enforcement regime itself — epsilon is
 *   scored against that standing arrangement, not against the
 *   voluntary-displacement counterfactual the endogenous sibling prefers.
 *   Claim and metrics are independent: this reading CLAIMS the arrangement is
 *   a legitimate (if costly) coordination instrument; the metrics describe
 *   its actual operation, including asymmetric costs and a growing
 *   performative share. KEY AGENTS (by structural relationship): -
 *   modernizing_state_executive: agenda-setter (institutional/arbitrage) —
 *   decrees the standardization and commands its enforcement -
 *   central_fiscal_administration: primary beneficiary (institutional/mobile)
 *   — receives synchronized fiscal order; the seat the gains land on -
 *   urban_reformist_elites: secondary beneficiary (organized/mobile) —
 *   champion and client of the standard - international_alignment_partners:
 *   external beneficiary (powerful/arbitrage) — collects alignment without
 *   bearing enforcement - rural_agricultural_communities: primary bearer of
 *   costs (powerless/trapped) — decades-long double life -
 *   clergy_and_ritual_specialists: bearer of costs with identity lock
 *   (organized/identity_locked) - observant_dress_wearers: bearer of costs
 *   (powerless/identity_locked) - village_women_under_veil_decrees: excluded
 *   voice (powerless/trapped) - modernization_historians: analytical observer
 *   — sees both ledgers
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, 0.63).
domain_priors:suppression_score(legitimacy_of_practice_standardization__exogenous_override_reading, 0.38).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__exogenous_override_reading, 0.56).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0.56).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__exogenous_override_reading, "State-Decreed Practice Standardization Regime (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__exogenous_override_reading, "political_history/modernization_studies").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__exogenous_override_reading, '29f118ef-c28d-4593-b78a-fd81c050c4d0').
narrative_ontology:cs_kernel_codification('29f118ef-c28d-4593-b78a-fd81c050c4d0', formalized).
narrative_ontology:cs_authority_grounding('29f118ef-c28d-4593-b78a-fd81c050c4d0', practice).
narrative_ontology:cs_interpretation_layer_present('29f118ef-c28d-4593-b78a-fd81c050c4d0').
narrative_ontology:cs_reading_relation('29f118ef-c28d-4593-b78a-fd81c050c4d0', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('29f118ef-c28d-4593-b78a-fd81c050c4d0', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, influences).
narrative_ontology:cs_axiom('29f118ef-c28d-4593-b78a-fd81c050c4d0', foundational, state_decree_confers_legitimacy).
narrative_ontology:cs_axiom_status(state_decree_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('29f118ef-c28d-4593-b78a-fd81c050c4d0', state_decree_confers_legitimacy, conventional).
narrative_ontology:cs_axiom('29f118ef-c28d-4593-b78a-fd81c050c4d0', secondary, collective_benefit_vindicates_enforcement_cost).
narrative_ontology:cs_axiom_status(collective_benefit_vindicates_enforcement_cost, holdable).
narrative_ontology:cs_axiom_grounding('29f118ef-c28d-4593-b78a-fd81c050c4d0', collective_benefit_vindicates_enforcement_cost, instrumental).
narrative_ontology:cs_reference_frame('29f118ef-c28d-4593-b78a-fd81c050c4d0', modernizing_state_decree_order).
narrative_ontology:cs_drift_state('29f118ef-c28d-4593-b78a-fd81c050c4d0', post_double_life_stabilization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('29f118ef-c28d-4593-b78a-fd81c050c4d0', '2026-06-12T14:20:00Z').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, central_fiscal_administration).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, urban_reformist_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, international_alignment_partners).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, rural_agricultural_communities).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, clergy_and_ritual_specialists).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, observant_dress_wearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, rural_agricultural_communities).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__exogenous_override_reading, top_down_modernization_doctrine).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__exogenous_override_reading, administrative_legibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the calendar and dress decrees in the name of fiscal stability, administrative legibility, and alignment with treaty partners; commands the police, courts, and provincial governors who carry them out. Measures progress through enrollment and inspection statistics. Bears the early enforcement bill and can redirect or relax enforcement as politics allow; its commitment is to the standard, not to any particular method of reaching it.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, modernizing_state_executive, agenda_setter,
    institutional, generational, arbitrage, national).

% Runs taxation, budgeting, and debt service on the unified civil year introduced by decree. Gains predictable revenue timing, comparable accounts across provinces, and unambiguous treaty and loan dating. Its ledgers, rolls, and procedures are rebuilt around the new calendar within the first decade, after which reverting would mean re-dating every account and contract it holds.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, central_fiscal_administration, beneficiary,
    institutional, biographical, mobile, national).

% Officers, professionals, merchants, and students in the capital cities who championed standardization and adopted the new calendar and dress readily. Staff the ministries and schools that transmit the standards, gain standing at home and credibility abroad from visible modernity, and pay little of the adjustment cost that falls on the countryside.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, urban_reformist_elites, beneficiary,
    organized, biographical, mobile, national).

% Foreign governments, lenders, and trading houses whose treaties, loans, shipping schedules, and correspondence presuppose the Gregorian-style civil year. Benefit from the alignment without contributing to or bearing any part of the enforcement that produced it, and their satisfaction is cited domestically as proof the sacrifices were worthwhile.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, international_alignment_partners, beneficiary,
    powerful, generational, arbitrage, global).

% Villages whose liturgical and agrarian time runs on the lunar and seasonal reckoning. Register births, pay taxes, and attend official markets on the civil calendar while continuing to marry, mourn, fast, and sow by the old one — keeping two books of time. Punishment for visible noncompliance fell hardest here in the early decades; distance from inspectors made quiet dual practice sustainable afterward. Leaving is not a realistic option: land, kin, and worship are all here. They use the civil year for tax days and market weeks because commerce and officials run on it.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, rural_agricultural_communities, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__exogenous_override_reading, rural_agricultural_communities, beneficiary).

% Keepers of the prayer cycle, festival dates, and rites of passage that structure village and neighborhood religious life. The decrees strike at the calendar their office exists to serve; several were prosecuted or stripped of posts for preaching against the changes. Their standing depends on the old reckoning remaining alive, so maintaining it quietly — teaching it at home, marking it in private — is inseparable from who they are.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, clergy_and_ritual_specialists, payer,
    organized, biographical, identity_locked, national).

% Townspeople and villagers whose headwear and garments carry religious obligation and family honor. Under the dress decrees they faced fines, public shaming, and in some periods prosecution for what they wore to prayer or market. Many complied in streets and offices and reverted at home and in worship — a wardrobe split along the public/private line. Giving up the dress outright would mean abandoning a covenant they understand themselves to have made.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, observant_dress_wearers, payer,
    powerless, biographical, identity_locked, national).

% Subject to unveiling campaigns in some of these reform waves, decided entirely by male officials and reform councils; no consultation, no petition channel that reached the center. Compliance was enforced in public space; household practice adjusted around the inspectors. Their preferences enter the record only secondhand, in travelers' accounts and later oral history.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, village_women_under_veil_decrees, excluded,
    powerless, biographical, trapped, regional).

% Reconstruct the sequence from gazette archives, tribunal caseloads, fiscal records, and village memory. The seat that can see both ledgers — the compliance statistics and the underground continuity — and whose reconstructions are the main corrective to the official success narrative. Holds no stake in either calendar.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, modernization_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__exogenous_override_reading, central_fiscal_administration).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: One civil calendar and one public dress standard synchronize taxation, debt service, conscription, market weeks, treaty dating, and official identification across the territory and with foreign partners, replacing a patchwork of lunar, seasonal, and regional reckonings that made centralized bookkeeping and diplomacy error-prone.
% TRANSFER_FUNCTION: Moves adjustment costs — relabeled festivals, shifted rest days, discarded wardrobes, punishment risk, and the labor of running two calendars — from the decreeing center onto rural and observant populations, and moves fiscal predictability, administrative comparability, and diplomatic credibility to the central state, its urban clients, and foreign counterparts.
% ABSENT_VOICES: Rural households, the clergy whose offices the old calendar constituted, and (in the unveiling campaigns) village women had no seat in the reform councils drafting the decrees; their objection — that ritual time and religious dress are not administrable objects — appears in the record only as enforcement friction, dismissed petitions, and prosecuted preachers. The consensus behind 'collective benefit' arose in rooms those parties never entered.
% DISAPPEARANCE_RATIONALE: If the decree-and-enforcement regime vanished overnight, provinces would drift back toward plural reckonings for ritual life while keeping the civil year for anything touching the state and export trade — the fiscal year, treaty dates, and market schedules would need renegotiating, and the public/private wardrobe boundary would harden into an open, lawful dual practice instead of a concealed one. Nothing snaps back to pre-decree life, but the equilibrium everyone already lives in would finally be acknowledged and reorganized around.
% FOUNDING_PROBLEM: Multi-calendar fragmentation: tax years misaligned with harvests and with each other across provinces, treaty and loan dates ambiguous between jurisdictions, census and conscription rolls kept on irreconcilable reckonings, and a sartorial patchwork the center read as political unreliability. The arrangement was built to install one civil standard fast enough to anchor the fiscal and diplomatic program.
% FOUNDING_PROBLEM_CORROBORATION: Foreign chancelleries' correspondence and lender audits from the period attest the dating chaos was real and pressing — attestation independent of the modernizing coalition's own account. Administrative historians attest the broader aim (extinguishing traditional practice) was not achieved: village registers, tribunal caseloads, and the ethnographic record show the old reckoning alive decades on, and no source outside the beneficiary coalition attests that the enforcement phase accomplished what voluntary diffusion would not have. The narrow fiscal problem is corroborated solved; the broad transformation problem is corroborated abandoned-in-place.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (epsilon 0.63 at endpoint) is substantial but reading-discounted: the transfer is real (compliance obtained under sanction; identity, ritual, and wardrobe costs borne by populations who did not consent, while fiscal and diplomatic gains concentrate in the center), yet this reading's own warrant credits part of the burden as the price of a genuine collective good. Suppression (0.38 at endpoint) is the raw structural coercion left after the enforcement decay the series traces — statutes still forbid the old forms, but active punishment became rare; per the framework, suppression is authored unscaled, and only extractiveness is scaled by directionality and scope downstream. Accessibility collapse is 0.42, deliberately not high: the decrees drove alternative practice underground rather than extinguishing it, so alternatives remained available at the price of concealment — the opposite profile from a natural law. Resistance 0.58: no successful revolt, but sustained quiet refusal (dual calendars, wardrobe switching, clerical networks) that the enforcement record documents. All three series share one six-point grid. Suppression_requirement is authored deliberately: the story's traced dynamic IS enforcement-capacity change — buildup to a peak near T=16, then decay as prosecutions became politically embarrassing and practically pointless. Base extractiveness declines only modestly from its peak and plateaus at 0.63, because the double-life burden and the fiscal-administrative asymmetry persist after coercion fades — an equilibrium, not a resolution. Theater_ratio rises monotonically (0.18 to 0.56): as transformation stalled, compliance statistics became proxies for it, and ceremony, registration, and inspection increasingly measured performance rather than change (Goodhart drift).
 *
 * PERSPECTIVAL GAP:
 *   Seats should diverge sharply. The rural and identity-locked payer seats face the maximum effective-extraction profile: trapped or identity-fused agents with no arbitrage sit nearest the full-target end, and the engine amplifies chi accordingly — from those seats the arrangement computes as enforced extraction wearing a coordination costume. The executive and fiscal seats sit near the beneficiary pole: they authored the decree, collect its synchronization dividends, and can shed enforcement when convenient — from there the same structure computes as coordination they successfully administered. The excluded seat (village women under veil decrees) experiences pure imposition: costs without representation or even acknowledgment. The international partner seat is the purest beneficiary in the story — alignment arrives free of enforcement cost, arbitrage-grade position, directionality near the beneficiary extreme. These are computed divergences from the structural data, not reconciled by the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation chain. central_fiscal_administration, urban_reformist_elites, and international_alignment_partners are declared beneficiaries (low directionality; the partners' arbitrage-grade exit places them at the extreme beneficiary end). rural_agricultural_communities, clergy_and_ritual_specialists, and observant_dress_wearers are declared victims, with trapped and identity_locked exits pushing them toward full-target directionality and amplifying effective extraction; identity_locked is derived from the victim declarations plus exit modulation, not authored separately. No directionality_overrides are needed: every seat's relationship is captured by its declaration plus exit posture, and the one potentially confusing seat (rural communities, who also use the civil calendar commercially) is handled as a secondary beneficiary role on the stakeholder rather than an override, keeping the structural source authoritative.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is refusing both available mislabels. Not a rope despite delivering real fiscal synchronization: the delivery is asymmetric (gains concentrate; adjustment and punishment costs fall on the non-consenting countryside) and holds only under active enforcement. Not a snare despite the coercion: the coordination good is genuinely produced and consumed — even resisters use the civil year for taxes and markets — so the coordination story is not mere cover. Not a scaffold: the transitional rhetoric ('until habits form') never carried a sunset clause; enforcement was designed to last indefinitely until transformation completed, and transformation never completed. What remains by interval end is a mandate that has outlived its function: surface compliance saturated, enforcement decayed to routine, theater_ratio past half — the mandate is resolved in substance even though the statutes stand, and the rising theater series marks the zombie phase the engine's drift detection exists to catch. The classification also guards against reading the double life either as failed policy (a piton reading — but the fiscal function remains real and someone demonstrably profits, so the cost-asymmetry test fails) or as secret vindication of the state (a rope reading — but the extraction is asymmetric and enforced, so the purity test fails). It is the equilibrium signature of a tangled rope whose extraction persists without completing its coordination promise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the exogenous_override_reading of the legitimacy_of_practice_standardization kernel: how would classification change under the endogenous_displacement_reading''s warrant applied to the same decree-and-enforce arrangement?',
    'Generate the sibling file over the identical referent (the enforced decree regime) but warrant-indexed scoring under voluntary-adoption legitimacy; compare victim sets, epsilon, and computed types across the pair.',
    'Under the endogenous warrant, coerced compliance loses its legitimacy cover and the arrangement scores toward pure extraction; under this warrant the same structure retains coordination standing (tangled_rope). The pair locates the dispute in warrant, not in the facts on the ground.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer routing: one reading of the practice-legitimacy kernel; the sibling warrant would reclassify the same referent.').

omega_variable(
    sibling_domain_partition_delta,
    'What structurally changes under the dual_practice_equilibrium_reading, which partitions legitimacy by domain (public/administrative versus private/ritual)?',
    'Author the sibling reading over the same referent; observe that its victim set excludes underground practitioners (re-read as rightful private-domain autonomy) and that its enforcement object reduces to the public-domain boundary alone.',
    'The victim set contracts and epsilon falls toward the coordination floor; the double life stops counting as suppressed residue and starts counting as the equilibrium itself. This story''s influences edge on the sibling encodes the fact that the enforcement regime created the very partition the sibling theorizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_domain_partition_delta, conceptual, 'The dual-practice sibling would re-scope victims to public-domain intrusions only.').

omega_variable(
    compliance_statistics_validity,
    'Do administrative compliance counts measure practice transformation or its concealment?',
    'Triangulate registry, enrollment, and inspection counts against village-level ethnographic and court records; sample tribunal caseloads against observed private practice decades on.',
    'If the counts measure concealment, the arrangement''s coordination achievement is overstated, theater_ratio understates the gap, and the unmeasured double-life burden belongs inside epsilon — raising effective extraction above the authored value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_statistics_validity, empirical, 'Whether official uniformity statistics reflect real practice change or its hiding.').

omega_variable(
    enforcement_necessity_counterfactual,
    'Was the enforcement component necessary to reach even surface uniformity, or would voluntary displacement (commerce, schooling, media exposure) have converged on the civil standard?',
    'Compare jurisdictions where analogous calendar and metrology shifts propagated without decree (trade-driven Gregorian adoption) against decree cases, holding literacy and commerce intensity constant.',
    'If voluntary convergence was feasible on the relevant timescale, the enforcement phase was gratuitous cost layered on a self-solving coordination problem (supports the endogenous sibling); if infeasible, part of the measured suppression is the price of the collective good (supports this reading).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_necessity_counterfactual, empirical, 'Counterfactual necessity of coercion for standardization.').

omega_variable(
    persistence_driver_identity_vs_utility,
    'Does the decades-long rural retention of lunar reckoning reflect identity fusion (ritual time constitutive of community selfhood) or continuing agrarian utility (seasonal markers the civil calendar lacks)?',
    'Distinguish sacred-cycle adherence from planting-cycle adherence in interview and parish-register evidence; test whether adoption accelerates where agrarian utilities are reproduced inside the civil calendar.',
    'If utility-driven, the rural seats are less identity-locked than modeled, their directionality sits nearer symmetric, and the constraint is milder than classified; if identity-driven, the lock stands and effective extraction on those seats is higher than surface compliance suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_driver_identity_vs_utility, empirical, 'Driver of underground persistence: identity lock versus practical utility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement_basis(legi_tr_t8, observed).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(legi_tr_t16, observed).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement_basis(legi_tr_t24, observed).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 32, 0.51).
narrative_ontology:measurement_basis(legi_tr_t32, observed).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 40, 0.56).
narrative_ontology:measurement_basis(legi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 8, 0.73).
narrative_ontology:measurement_basis(legi_be_t8, observed).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement_basis(legi_be_t16, observed).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement_basis(legi_be_t24, observed).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement_basis(legi_be_t32, observed).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement_basis(legi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(legi_su_t8, observed).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement_basis(legi_su_t16, observed).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement_basis(legi_su_t24, observed).
narrative_ontology:measurement(legi_su_t32, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 32, 0.47).
narrative_ontology:measurement_basis(legi_su_t32, observed).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(legi_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__exogenous_override_reading, information_standard).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'legitimacy of practice standardization' decomposes into three readings of one kernel with distinct warrants and distinct victim sets. This file (exogenous_override_reading) scores the decree-and-enforce regime under a decree-confers-legitimacy warrant. endogenous_displacement_reading scores the same referent under a voluntary-adoption warrant, under which coerced compliance loses legitimacy cover. dual_practice_equilibrium_reading re-partitions the referent into public/private domains and re-reads underground practice as rightful autonomy rather than suppressed residue. Structural edges: the exogenous regime's enforcement CREATED the dual-practice structure the equilibrium reading theorizes (encoded as influences from this reading); the endogenous reading competes with this one on the source of legitimacy without either ruling the other out (coexists). Epsilon differs across the family by warrant, not by referent — same standing arrangement, reading-indexed scores.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
