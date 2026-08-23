% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Endogenous-Displacement Criterion for Practice-Change Legitimacy
 *   domain: political_history/modernization_studies
 *
 * SUMMARY:
 *   Since the mid-twentieth century the operative test for whether a practice
 *   standardization — a calendar, a script, a dress code, a system of weights
 *   — counts as legitimate reform has been emergent character: change
 *   qualifies as legitimate when adoption spreads voluntarily, driven by
 *   perceived utility or cultural evolution, rather than by decree. This
 *   story instantiates that criterion as a standing arrangement: multilateral
 *   agencies, development banks, heritage bodies, and rights mechanisms
 *   require demonstrated community demand before financing standardization;
 *   courts and heritage registries treat continuity as a protected interest;
 *   historiography reads adoption curves as revelations of preference. The
 *   arrangement subsidizes incumbent practice-holders, taxes imposed change,
 *   and concentrates its costs on two seats its aggregate evidence cannot see
 *   — populations trapped in cross-jurisdiction friction that no single actor
 *   may remove by decree, and dissenters inside traditional communities whose
 *   public conformity is recorded as preference. The epsilon referent is this
 *   standing voluntariness-test arrangement itself, assessed by the
 *   endogenous reading's own lights; the authored metrics describe its actual
 *   operation and are deliberately not reconciled to the reading's
 *   self-understanding. This file is one member of a three-story constraint
 *   family decomposing the colloquial kernel; the sibling readings are
 *   separate constraints linked through network.affects_constraints. KEY
 *   AGENTS (by structural relationship): - traditional_practice_communities:
 *   sheltered beneficiary (organized / identity_locked) -
 *   incumbent_cultural_elites: primary rent-collecting beneficiary (organized
 *   / identity_locked) - commercial_elite_early_adopters: mobile beneficiary
 *   (powerful / mobile) - coordination_trapped_populations: primary payer
 *   (powerless / trapped) - intra_community_dissenters: concealed payer
 *   (powerless / trapped) - modernizing_state_reformers: constrained payer
 *   (institutional / constrained) - development_agencies_and_norms_bodies:
 *   agenda-setter with secondary beneficiary position (institutional /
 *   constrained) - historiographic_profession: analytical observer with
 *   secondary beneficiary position (analytical / analytical)
 *
 * KEY AGENTS:
 *   - - traditional_practice_communities: sheltered beneficiary (organized / identity_locked) — continuity certified as legitimate; exit constitutive, not optional
 *   - - incumbent_cultural_elites: primary rent-collecting beneficiary (organized / identity_locked) — clerical, gerontocratic, and guild authority compounds with every failed displacement
 *   - - commercial_elite_early_adopters: mobile beneficiary (powerful / mobile) — switch first for transaction advantage; reversibility damps exposure
 *   - - coordination_trapped_populations: primary payer (powerless / trapped) — recurring cross-jurisdiction friction no actor may remove by decree; latent coalition power only
 *   - - intra_community_dissenters: concealed payer (powerless / trapped) — private preference erased by aggregate adoption evidence; double-life conformity recorded as consent
 *   - - modernizing_state_reformers: constrained payer (institutional / constrained) — fiscal and interoperability goals pursued through subsidy and persuasion alone; decree counted illegitimate
 *   - - development_agencies_and_norms_bodies: agenda-setter and secondary beneficiary (institutional / constrained) — operationalize the test and collect mandate and budget from running it
 *   - - historiographic_profession: analytical observer with secondary beneficiary position (analytical / analytical) — adjudicates which changes count as endogenous; careers ride on the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.46).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.38).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Endogenous-Displacement Criterion for Practice-Change Legitimacy").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/modernization_studies").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__endogenous_displacement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'fa54d45f-51b4-458f-8634-7b19529f067f').
narrative_ontology:cs_kernel_codification('fa54d45f-51b4-458f-8634-7b19529f067f', distributed).
narrative_ontology:cs_authority_grounding('fa54d45f-51b4-458f-8634-7b19529f067f', expertise).
narrative_ontology:cs_interpretation_layer_present('fa54d45f-51b4-458f-8634-7b19529f067f').
narrative_ontology:cs_reading_relation('fa54d45f-51b4-458f-8634-7b19529f067f', legitimacy_of_practice_standardization__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('fa54d45f-51b4-458f-8634-7b19529f067f', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('fa54d45f-51b4-458f-8634-7b19529f067f', foundational, cultural_self_determination_against_imposed_change).
narrative_ontology:cs_axiom_status(cultural_self_determination_against_imposed_change, holdable).
narrative_ontology:cs_axiom_grounding('fa54d45f-51b4-458f-8634-7b19529f067f', cultural_self_determination_against_imposed_change, deontological).
narrative_ontology:cs_axiom('fa54d45f-51b4-458f-8634-7b19529f067f', foundational, aggregate_adoption_curves_reveal_preference).
narrative_ontology:cs_axiom_status(aggregate_adoption_curves_reveal_preference, holdable).
narrative_ontology:cs_axiom_grounding('fa54d45f-51b4-458f-8634-7b19529f067f', aggregate_adoption_curves_reveal_preference, empirically_contingent).
narrative_ontology:cs_reference_frame('fa54d45f-51b4-458f-8634-7b19529f067f', organic_diffusion_baseline).
narrative_ontology:cs_drift_state('fa54d45f-51b4-458f-8634-7b19529f067f', contemporary_participatory_turn, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fa54d45f-51b4-458f-8634-7b19529f067f', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_practice_communities).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, incumbent_cultural_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, commercial_elite_early_adopters).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, coordination_trapped_populations).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernizing_state_reformers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, development_agencies_and_norms_bodies).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, historiographic_profession).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, spontaneous_order_doctrine).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, diffusion_of_innovations_model).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, revealed_preference_aggregation).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_self_determination_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities whose calendrical, linguistic, dress, and ritual practices predate standardization campaigns. The criterion converts their non-adoption from obstruction into legitimate persistence: outside actors lose the warrant to compel change, and continuity acquires standing in courts, heritage registries, and funding formulas. They do not administer the test; they are sheltered by it. Leaving the practice would mean dissolving the community's shared identity, so exit is not experienced as an option.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_practice_communities, beneficiary,
    organized, generational, identity_locked, regional).

% Clergy, elders, lineage heads, craft-guild masters, and language authorities whose social rank depends on the old practice remaining authoritative. Every year the imposed alternative fails to take root, their position compounds. They supply authenticity testimony to heritage bodies, narrate voluntary continuity, and lobby funding formulas toward preservation; they bear none of the friction costs that nonstandardized practice imposes on traders and administrators.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, incumbent_cultural_elites, beneficiary,
    organized, generational, identity_locked, regional).

% Merchant and professional strata who switch to the incoming practice first — trading calendars, foreign dress, new measures — because their counterparties already use them. Their switching costs are repaid in transaction advantages, and the criterion certifies their move as the legitimate vanguard of diffusion. If diffusion stalls they can revert or code-switch between systems; their exposure is reversible.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, commercial_elite_early_adopters, beneficiary,
    powerful, biographical, mobile, regional).

% Farmers, small traders, and borderland households who transact across jurisdictions running mismatched calendars, measures, scripts, or market days. Each bears a small recurring friction — converted dates, duplicated bookkeeping, missed markets — that no single actor can remove by acting alone and that the criterion rules out removing by decree. Individually immobile, they hold only latent coalition power: where merchant leagues or fair associations have formed, joint voluntary adoption has opened the trap.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, coordination_trapped_populations, payer,
    powerless, biographical, trapped, national).

% Members of traditional communities who privately prefer the incoming practice — young adults, women constrained by dress codes, converts, wage migrants — but face familial and congregational sanction if they deviate openly. Many live a double life: public conformity, private divergence. Because legitimacy is read off aggregate adoption curves, their unvoiced dissent never registers; the curve records their pressured public conformity as preference.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, intra_community_dissenters, payer,
    powerless, biographical, trapped, local).

% Bureaucracies and reform ministries pursuing standardization for fiscal, administrative, or military ends. Under the criterion their instruments are limited to subsidy, demonstration, and persuasion: a decree issued before uptake matures is counted illegitimate, so programs stretch across decades and consume budgets manufacturing visible demand. Exit exists — abandoning the standardization goal — but the interoperability and fiscal problems that motivated it do not disappear.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernizing_state_reformers, payer,
    institutional, generational, constrained, national).

% Multilateral agencies, development banks, heritage bodies, and rights mechanisms that operationalize the criterion: conditioning finance on demonstrated community demand, certifying locally led change, drafting free-prior-informed-consent protocols. Running the test yields programmatic control, staff, and moral authority; abandoning it for decree-based methods would forfeit the participatory legitimacy their charters now require, so their exit from the criterion is effectively closed even where uptake data look staged.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, development_agencies_and_norms_bodies, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, development_agencies_and_norms_bodies, beneficiary).

% Historians, anthropologists, and diffusion researchers who adjudicate which past and present changes count as endogenous. Their adoption-curve methodologies supply the criterion's evidence base; journals, curricula, and careers are organized around reading diffusion as revelation of preference. They collect professional standing from the adjudication role while bearing none of the arrangement's costs.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, historiographic_profession, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, historiographic_profession, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__endogenous_displacement_reading, incumbent_cultural_elites).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__endogenous_displacement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a shared, publicly checkable test for distinguishing legitimate from coerced practice change — demonstrated voluntary adoption — so that reformers, communities, funders, and courts can coordinate expectations about cultural change without repeated open conflict over each proposal; it converts a recurring legitimacy war into an evidentiary procedure.
% TRANSFER_FUNCTION: Moves decision rights over practice change from decree-wielding states to distributed adopter populations; moves legitimacy, and the budgets and mandates that ride on it, toward the institutions that administer the test; leaves the recurring costs of nonstandardized practice — date conversion, duplicated bookkeeping, fragmented markets — with third-party transactors, and the costs of silence with intra-community dissenters.
% ABSENT_VOICES: Intra-community dissenters are demographically present but evidentially absent: the criterion's unit of observation is the aggregate adoption curve, in which their pressured public conformity is indistinguishable from preference, so the seat most affected by the test has no voice the test can register. Cross-jurisdiction transactors likewise have no seat in legitimacy adjudication — the friction they bear is booked as the price of authenticity by the seats that run the test.
% DISAPPEARANCE_RATIONALE: If the voluntariness test vanished overnight, ministries would standardize calendars, measures, scripts, and administrative dress by decree wherever interoperability pays, adoption curves would steepen discontinuously, incumbent cultural elites would lose the shield that converts their persistence into legitimacy, and the participatory-certification apparatus — consultation protocols, consent conditionality, authenticity review — would lose its function within a funding cycle. Communities resisting change would resist without legitimating cover, and the historiography of modernization would reorganize around outcomes rather than mode of emergence.
% FOUNDING_PROBLEM: The criterion was articulated to stop states from laundering coercion as progress: after a century of forced assimilation — residential schooling, compelled sedentarization, dress and name decrees, banned languages — reformers needed a test that would distinguish emancipatory standardization from homogenization imposed on unwilling populations, and whether the change emerged voluntarily became that test.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: truth-commission and tribunal documentation of forced-assimilation programs, United Nations special-rapporteur reporting on contemporary coercive homogenization, and the archival record of decree-era standardizations that collapsed or required sustained gendarmerie support. Traditional-practice communities also attest the problem, but the attestation does not depend on them — the documentary record stands independently of the beneficiary seat.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate and rising (0.34 to 0.46 across the interval): the criterion's costs are real but concentrated — recurring friction for coordination-trapped transactors, erased voice for intra-community dissenters, decades-long program drag for reformers — while its benefits are broad, which is why epsilon sits well below snare territory yet clearly above rope. Suppression (0.38) is discursive and procedural rather than physical: the test suppresses by refusing to register certain evidence (private dissent, decree outcomes) and by conditioning finance on performed demand; it is authored as a raw structural property and left unscaled — the engine applies directionality and scope scaling to extractiveness only. Theater_ratio (0.42 at interval end) is the fastest-rising series: once demonstrated community demand became the price of funding, staged consultations, pilot theatrics, and astroturfed uptake became a rational investment, so a growing share of the activity that certifies legitimacy is performance. Accessibility_collapse is low (0.32): the rival criteria — decree-based legitimation and domain-partitioned legitimacy — remain live and operative in other polities and literatures, so understanding this criterion does not close the alternatives. Resistance is moderate-high (0.55): modernizing bureaucracies, development economists, and dissenting community members all actively contest the test. The three temporal series share one grid (t = 0,10,...,60, corresponding roughly to 1955-2015) so every metric is authored at every examined point; suppression_requirement is tracked because the story specifically traces enforcement-capacity build-up — consent protocols, participation conditionality, and certification regimes hardened materially from the 1980s onward. The trajectories are monotonic, not cyclical: accumulation, not oscillation, is this arrangement's dynamic. Claimed type is tangled_rope on the structural facts: a genuine coordination function (a shared legitimacy test that lowered the temperature of cultural conflict) operating through active enforcement, with named beneficiaries and named victims extracted through the same structure.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the incumbent-elite and traditional-community seats the arrangement is shelter: their continuity is certified, external pressure loses warrant, and the criterion looks like a hard-won civilizational achievement — expect a rope-or-better reading from those seats. From the coordination-trapped and dissenter seats the same structure operates as a tax on the voiceless: friction that could be removed by a stroke no one may strike, and conformity recorded as consent — expect snare-flavored readings there. The agenda-setting agencies occupy a third position: they experience the criterion as mandate and budget, and its costs as externalities booked elsewhere. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d toward zero: traditional_practice_communities and incumbent_cultural_elites are subsidized directly, and identity_locked exit deepens their subsidy — they cannot leave the practice that shelters them, so the arrangement's protection is worth more, not less, over time. commercial_elite_early_adopters sit near the beneficiary pole with mobile exit damping exposure further. development_agencies_and_norms_bodies derive a low d from their agenda-setter-plus-beneficiary position: they run the test and collect from running it. Among payers, intra_community_dissenters sit nearest the full-target pole: trapped exit plus total evidential invisibility means the arrangement draws from them with maximal efficiency and zero registered resistance. coordination_trapped_populations are similarly near-full targets, their powerlessness partially offset by latent coalition capacity — merchant leagues and fair associations have historically opened the trap through joint voluntary adoption, the criterion's own remedy working as designed. modernizing_state_reformers carry high d, but their institutional power and genuine option to abandon the goal place them slightly below the trapped seats. The historiographic profession sits near symmetric as analytical observer, collecting standing rather than rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — stopping coercion dressed as progress — is live, so nothing here is mandatorophic: the criterion has not outlived its function, and no sunset logic applies. The tangled_rope claim earns its keep by blocking both mislabeling directions. Reading the arrangement as pure protection (rope) would erase the two victim classes the aggregate test cannot see and would license indefinite extension of performed-voluntariness certification; reading it as pure cover (snare) would erase the documented conflict-reduction function that made the test worth building after the forced-assimilation era. The classification keeps both faces on the table and routes the residual question — how much of the extraction is removable without destroying the protection — into the omega variables rather than into the type label.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    one_reading_of_legitimacy_kernel,
    'This constraint is the endogenous_displacement_reading of the kernel legitimacy_of_practice_standardization — what would classification look like under the sibling readings, and does this file''s epsilon travel?',
    'Author and compile the sibling stories (exogenous_override_reading, dual_practice_equilibrium_reading) over the same historical episodes and compare per-seat classifications; resolve by cross-reading comparison, never by retuning this file.',
    'Under the exogenous sibling the decree route becomes the legitimate baseline and this criterion''s protections register as obstruction costs; under the dual sibling legitimacy partitions by domain and no whole-arrangement epsilon applies. Seat-level victims and beneficiaries reshuffle accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(one_reading_of_legitimacy_kernel, conceptual, 'Committer structure: reading-of-kernel status and sibling structural deltas.').

omega_variable(
    disagreement_location_evidentiary_test,
    'Where exactly do the three readings disagree — which structural element does each modify?',
    'Locate each sibling''s edit: the exogenous reading replaces the evidentiary test (decree plus collective-benefit accounting replaces adoption curves); the dual reading replaces the scope (domain jurisdiction replaces a universal test); this reading holds test and scope fixed.',
    'Fixes which metric each sibling must re-author and which seats change directionality; prevents false comparison of epsilon across differently scoped arrangements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_evidentiary_test, conceptual, 'The readings disagree on the legitimacy test itself (adoption curves versus decree accounting) and on its scope (universal versus domain-partitioned).').

omega_variable(
    aggregate_adoption_masking,
    'Does the aggregate adoption curve measure preference or pressure — how much recorded voluntary uptake is coerced public conformity?',
    'Individual-level panel data disaggregated by sanction exposure and economic dependence, plus natural experiments where enforcement intensity varied across otherwise similar regions.',
    'If a large share of recorded adoption is pressure-driven, the criterion certifies coercion as consent: epsilon is understated, intra_community_dissenters become the primary victim class rather than a residual, and the theater ratio understates the arrangement''s reliance on performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_adoption_masking, empirical, 'Whether the criterion''s central evidence — adoption curves — can distinguish preference from fear.').

omega_variable(
    double_life_phase_vs_equilibrium,
    'Is the public/private practice split a transitional phase on the way to convergence, as this reading holds, or a stable equilibrium, as the dual-practice sibling holds?',
    'Multi-generation cohort tracking of public and private practice divergence in communities under standardization pressure.',
    'If the split is an equilibrium, this reading''s transitional verdict systematically misreads durable pluralism as pending convergence, licensing indefinite extension of the arrangement and crediting it with successes that never arrive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(double_life_phase_vs_equilibrium, conceptual, 'Phase versus equilibrium status of the double-life phenomenon.').

omega_variable(
    coordination_failure_counterfactual,
    'How much of the friction borne by coordination_trapped_populations would a decree have removed, weighed against the coercion cost the criterion exists to prevent?',
    'Paired cross-country comparison of calendar, measure, and script adoptions by route (decree versus voluntary), netting welfare effects against enforcement violence and backlash.',
    'Bounds whether the criterion''s blocking is protective or obstructive; a large net-friction residue against low decree-violence counterfactuals would push effective extraction for the trapped seats sharply upward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_failure_counterfactual, preference, 'Counterfactual cost of the decree route the criterion forecloses.').

omega_variable(
    constructed_norm_or_discovered_law,
    'Is the voluntariness test a discovered regularity of durable practice change, or a constructed norm whose codification served identifiable beneficiaries — incumbent elites and the adjudicating professions?',
    'Trace the criterion''s codification history for beneficiary advocacy; comparative institutional analysis of where adoption-based tests outperform decree-based ones across polity types.',
    'If constructed-with-beneficiaries, false-summit pressure toward stronger extraction classifications strengthens and the criterion''s protective self-description loses standing; if discovered, the norm itself approaches mountain-like stability even while its application extracts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_norm_or_discovered_law, conceptual, 'Natural-law versus constructed-norm status of the voluntariness criterion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement(legi_tr_t60, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 30, 0.41).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(legi_be_t60, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 60, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0, 0.24).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 10, 0.26).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 20, 0.29).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 30, 0.31).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 40, 0.33).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 50, 0.36).
narrative_ontology:measurement(legi_su_t60, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, information_standard).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'legitimate practice standardization' covers three structurally distinct legitimacy criteria with different epsilon values, victim sets, and enforcement modes. This file authors the endogenous-displacement criterion alone; the exogenous-override and dual-practice-equilibrium criteria are separate stories linked here. The endogenous reading sits upstream of the dual reading, whose private-domain half borrows this reading's evidentiary test, and in direct logical opposition to the exogenous reading, whose sufficiency-of-decree premise this reading's necessity-of-voluntary-emergence premise negates. Epsilon values across the family are not comparable without seat-level recomputation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
