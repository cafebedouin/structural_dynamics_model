% ============================================================================
% CONSTRAINT STORY: cbdr_principle__historical_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cbdr_principle__historical_responsibility_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: cbdr_principle__historical_responsibility_reading
 *   human_readable: CBDR — Historical Responsibility Reading (Binding Reductions + Loss/Damage Finance)
 *   domain: international climate governance / treaty law / development economics
 *
 * SUMMARY:
 *   This story instantiates the historical-responsibility reading of the
 *   Common But Differentiated Responsibilities (CBDR) kernel: CBDR requires
 *   BINDING emissions reductions from developed nations, scaled to their
 *   cumulative historical emissions share, plus binding loss/damage
 *   financing. This is a treaty-law claim with real (if imperfectly enforced)
 *   legal traction — the Paris Agreement's differentiation language and the
 *   2022 loss-and-damage fund decision are its institutional artifacts. Under
 *   this reading, developed nations sit in the victim set for both
 *   financial-transfer and emissions-constraint obligations, and
 *   developing/vulnerable nations exit the victim set entirely, becoming
 *   beneficiaries of a formula-anchored claim rather than supplicants for
 *   discretionary aid. This is a genuinely different constraint from the
 *   sibling voluntary-commitment reading, which keeps developed-nation
 *   obligations non-binding and centers technology transfer — the two
 *   readings have different beneficiary/victim sets and would be measured
 *   with different ε if forced into one story, which is exactly the
 *   decomposition trigger.
 *
 * KEY AGENTS:
 *   - small_island_developing_states: primary intended beneficiary (powerless/trapped) — receives loss/damage claims under the formula
 *   - developed_nation_treasuries: primary target (institutional/constrained) — bears binding financial and mitigation obligations
 *   - developed_nation_carbon_intensive_industries: secondary target (powerful/constrained) — bears domestic decarbonization costs flowing from the international obligation
 *   - emerging_economy_high_emitters: contested beneficiary-by-exclusion (organized/mobile) — classified as recipient despite rising cumulative share, the central fairness contest
 *   - unfccc_secretariat_and_treaty_negotiators: agenda-setter (institutional/analytical) — administers the accounting methodology that determines obligation size
 *   - future_generations: excluded (powerless/trapped) — inherit the mitigation trajectory with no negotiating voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, 0.58).
domain_priors:suppression_score(cbdr_principle__historical_responsibility_reading, 0.35).
domain_priors:theater_ratio(cbdr_principle__historical_responsibility_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__historical_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__historical_responsibility_reading, "CBDR — Historical Responsibility Reading (Binding Reductions + Loss/Damage Finance)").
narrative_ontology:topic_domain(cbdr_principle__historical_responsibility_reading, "international climate governance / treaty law / development economics").

domain_priors:requires_active_enforcement(cbdr_principle__historical_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__historical_responsibility_reading, '51b79359-caa5-42ca-ae0a-c6357b21a6b5').
narrative_ontology:cs_kernel_codification('51b79359-caa5-42ca-ae0a-c6357b21a6b5', fixed_text).
narrative_ontology:cs_authority_grounding('51b79359-caa5-42ca-ae0a-c6357b21a6b5', distributed).
narrative_ontology:cs_reading_relation('51b79359-caa5-42ca-ae0a-c6357b21a6b5', cbdr_principle__voluntary_commitment_reading, coexists_with).
narrative_ontology:cs_axiom('51b79359-caa5-42ca-ae0a-c6357b21a6b5', foundational, cumulative_historical_causation_grounds_binding_liability).
narrative_ontology:cs_axiom_status(cumulative_historical_causation_grounds_binding_liability, holdable).
narrative_ontology:cs_axiom_grounding('51b79359-caa5-42ca-ae0a-c6357b21a6b5', cumulative_historical_causation_grounds_binding_liability, deontological).
narrative_ontology:cs_axiom('51b79359-caa5-42ca-ae0a-c6357b21a6b5', secondary, loss_and_damage_finance_is_enforceable_entitlement_not_aid).
narrative_ontology:cs_axiom_status(loss_and_damage_finance_is_enforceable_entitlement_not_aid, holdable).
narrative_ontology:cs_axiom_grounding('51b79359-caa5-42ca-ae0a-c6357b21a6b5', loss_and_damage_finance_is_enforceable_entitlement_not_aid, conventional).
narrative_ontology:cs_reference_frame('51b79359-caa5-42ca-ae0a-c6357b21a6b5', rio_1992_differentiation_principle).
narrative_ontology:cs_drift_state('51b79359-caa5-42ca-ae0a-c6357b21a6b5', post_paris_2015_ndc_regime, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('51b79359-caa5-42ca-ae0a-c6357b21a6b5', '').
narrative_ontology:cs_kernel_id(cbdr_principle__historical_responsibility_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, small_island_developing_states).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, least_developed_countries).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, climate_vulnerable_coastal_populations).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nation_treasuries).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nation_carbon_intensive_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, emerging_economy_high_emitters).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, polluter_pays_doctrine).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, cumulative_historical_liability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face existential sea-level and storm-intensity threats from cumulative emissions they did not produce. Under this reading, they receive loss/damage financing and can point to a binding legal formula tying developed-nation obligations to historical emissions share, rather than relying on discretionary pledges. Their exit from the arrangement would mean returning to a voluntary-pledge regime that has historically underdelivered.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, small_island_developing_states, beneficiary,
    powerless, generational, trapped, global).

% Depend on adaptation and loss/damage transfers to manage climate impacts while pursuing development. This reading formalizes a claim on developed-nation resources proportional to historical emissions, converting what was a moral appeal into an enforceable (if imperfectly enforced) financial and mitigation obligation.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, least_developed_countries, beneficiary,
    powerless, generational, trapped, global).

% Directly experience displacement, crop loss, and infrastructure damage attributable to warming. They have no seat at treaty negotiations themselves; they benefit indirectly if national governments successfully claim loss/damage transfers on their behalf, but delivery depends on intermediating state institutions.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, climate_vulnerable_coastal_populations, beneficiary,
    powerless, biographical, trapped, regional).

% Bear the binding financial transfer and emissions-reduction obligations under a formula indexed to cumulative historical emissions since industrialization. They can negotiate the formula's parameters, delay ratification, or contest the historical accounting, but full exit would mean abandoning the treaty framework and its diplomatic and reputational benefits — a costly but not impossible move.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developed_nation_treasuries, payer,
    institutional, generational, constrained, global).

% Face domestic decarbonization mandates and carbon pricing driven by the international obligation their governments accepted. They can lobby for softer domestic implementation, relocate production to jurisdictions with looser rules, or absorb compliance costs; full insulation from the obligation is not available once ratified.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developed_nation_carbon_intensive_industries, payer,
    powerful, biographical, constrained, national).

% Large current-year emitters (having industrialized later) are formally classified as developing under this reading and thus excluded from binding reduction obligations despite rising cumulative shares. They benefit from continued classification as recipients rather than payers, but this exclusion is precisely what developed-nation negotiators contest as the reading's central fairness gap.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, emerging_economy_high_emitters, excluded,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, emerging_economy_high_emitters, beneficiary).

% Administer the negotiation process, draft the historical-emissions accounting methodology, and broker the binding formula. They do not personally pay or collect the transfers but control which accounting baseline (1850? 1990? 1992?) and which financing mechanism becomes operative, which determines the size of the obligation.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, unfccc_secretariat_and_treaty_negotiators, agenda_setter,
    institutional, generational, analytical, global).

% Will inherit whatever mitigation trajectory and adaptation infrastructure current negotiations produce, but have no representation in the negotiating rooms where the historical-responsibility formula is set or watered down.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, future_generations, excluded,
    powerless, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cbdr_principle__historical_responsibility_reading, diffuse).
narrative_ontology:fixing_cost_class(cbdr_principle__historical_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, internationally legible formula for allocating the mitigation and finance burden of a genuinely global collective-action problem (atmospheric carbon is a shared sink), preventing the free-rider dynamic where every state waits for others to act first.
% TRANSFER_FUNCTION: Moves binding emissions-reduction obligations and loss/damage finance from developed nations (indexed to cumulative historical emissions since industrialization) to climate-vulnerable developing nations and populations bearing impacts disproportionate to their own emissions share.
% ABSENT_VOICES: Future generations who inherit the mitigation trajectory have no negotiating seat. Climate-vulnerable coastal populations are represented only through intermediating state governments, which may not fully pass through loss/damage transfers. Emerging-economy high emitters are present but resist being reclassified into the payer set as their cumulative share rises.
% DISAPPEARANCE_RATIONALE: If binding historical-responsibility obligations vanished, developed nations would revert to discretionary pledges (as under the voluntary reading), loss/damage financing would lose its legal anchor, and vulnerable states would lose their strongest lever for compelling transfers — the negotiating landscape and finance flows would materially reorganize toward the voluntary-commitment baseline.
% FOUNDING_PROBLEM: Post-industrial atmospheric carbon loading was overwhelmingly produced by a small set of early-industrializing states, while climate impacts fall disproportionately on states that contributed least to the stock of emissions and lack resources to adapt — a mismatch between causal responsibility and impact burden that ad hoc aid could not resolve.
% FOUNDING_PROBLEM_CORROBORATION: Independent carbon-accounting researchers (e.g. Global Carbon Project historical emissions datasets) and IPCC assessment reports, both outside the negotiating parties, corroborate the underlying cumulative-emissions asymmetry. Developed-nation treasuries and industry associations contest that the problem, so measured, justifies a binding formula rather than differentiated voluntary action; developing-nation blocs and independent legal scholars corroborate that the founding asymmetry remains live and unresolved by voluntary mechanisms to date.
narrative_ontology:disappearance_verdict(cbdr_principle__historical_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__historical_responsibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__historical_responsibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cbdr_principle__historical_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__historical_responsibility_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cbdr_principle__historical_responsibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cbdr_principle__historical_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (moderate-high, rising over the interval) because binding obligations under this reading do transfer real resources and impose real mitigation costs on developed nations, and this burden has intensified as loss/damage financing has moved from aspirational language (Rio 1992) to an operationalized fund (COP27 2022) with real payment expectations. Suppression is moderate (0.35) — developed nations retain meaningful exit routes (delayed ratification, contested accounting baselines, non-compliance without severe sanction) unlike a hard-enforcement regime, but suppression has risen as the loss/damage fund has moved from principle to institution with monitoring and reporting requirements. Theater ratio starts high (0.6 in 1992, when CBDR was largely rhetorical) and falls over the interval (0.42 by 2024) as the framework has partially operationalized — this is a rare declining-theater trajectory, reflecting genuine institutional hardening rather than pure symbolic drift. Accessibility collapse is moderate (0.4): voluntary and hybrid alternatives to the binding formula remain politically live (see the sibling reading), so alternatives have not collapsed. Resistance is high (0.72): developed nations have contested the historical-accounting methodology, delayed ratification of loss/damage mechanisms, and periodically threatened treaty exit (e.g. Paris withdrawal episodes) — this is a constraint that must be actively defended, not one that persists by inertia.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (UNFCCC secretariat/negotiators), the arrangement is functioning coordination: a shared accounting standard preventing free-riding on a genuine collective-action problem. From the developed-nation-treasury payer seat, the same structure is an enforced, asymmetric transfer obligation whose historical-accounting baseline is itself contestable (why 1850? why not per-capita adjusted? why not consumption-based rather than production-based accounting?). From the small-island-state beneficiary seat, the arrangement is the only mechanism that has ever converted a moral claim into something resembling an enforceable entitlement, and its central complaint is under-delivery, not over-extraction. The engine should compute meaningfully different seat classifications from these three positions given the same structural data — that divergence is the analytical payload, not a flaw to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Small island states, least developed countries, and vulnerable coastal populations are structural beneficiaries — the formula exists to move resources and obligations toward them, so their derived directionality sits near the full-beneficiary end despite their low raw power (their power is powerless, but the constraint's design compensates by making claims enforceable regardless of their individual bargaining leverage). Developed-nation treasuries and carbon-intensive industries are structural targets: they bear the transfer and mitigation costs the formula assigns, placing them near the full-target end even though their institutional/powerful status gives them real (if imperfect) exit and negotiation leverage — this is why they sit at 'constrained' rather than 'trapped' exit, moderating but not eliminating their high derived extraction exposure. Emerging-economy high emitters are the structurally interesting case: classified as developing (hence excluded from binding obligations) despite organized power and mobile exit, they occupy an anomalous low-d position that is the central object of developed-nation grievance against this reading — the derivation here would put them near the beneficiary end on formal classification alone, which is precisely the distortion the sibling reading's proponents point to.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a mismatch between historical emissions causation and current climate impact distribution — remains empirically live (corroborated by independent carbon-accounting data), which argues against mandatrophy: this is not a dead mandate persisting by inertia. However, the accounting methodology and financing mechanisms have been renegotiated repeatedly (Kyoto binding targets → Paris nationally-determined pledges → COP27 loss/damage fund) without ever fully operationalizing the binding-formula version this reading claims, raising the question of whether the 'binding' character of the obligation is itself more aspirational than the reading asserts. The tangled_rope classification captures this: there is a genuine coordination function (shared accounting standard, avoiding a global collective-action failure) coexisting with genuine asymmetric extraction (developed nations bear costs proportional to a formula they did not fully accept as binding), and active enforcement is required — through diplomatic pressure, treaty conditionality, and reputational cost — to keep developed nations from reverting to the voluntary reading's much lower obligation floor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cbdr_reading_disagreement_locus,
    'Does the CBDR principle, as codified in UNFCCC Article 3.1 and the Paris Agreement, actually establish BINDING obligations proportional to historical emissions, or only a normative expectation of differentiation that nations are free to satisfy through voluntary means?',
    'This is the precise locus of the kernel contest between the historical_responsibility_reading (this story) and the voluntary_commitment_reading (sibling story). Resolution would require either a binding ICJ/arbitral ruling interpreting the treaty text as creating enforceable obligations, or continued state practice confirming the voluntary character (Paris Agreement''s explicit rejection of binding NDCs in 2015 is evidence for the voluntary reading; the 2022 loss-and-damage fund''s institutionalization is evidence for a hardening historical-responsibility reading). No single ruling has settled this; the two readings currently coexist as live legal-political positions held by different treaty parties.',
    'If the binding reading prevails, developed-nation treasuries face enforceable liability exposure and the tangled_rope classification with active-enforcement gate is structurally correct. If the voluntary reading prevails, the same treaty text describes a much lower-ε rope-like coordination mechanism with no true victim set among developed nations — this story and its sibling would diverge further in their engine-computed classifications, which is the expected and correct outcome of decomposition rather than a defect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cbdr_reading_disagreement_locus, conceptual, 'The kernel-level disagreement between binding and voluntary readings of CBDR, routed here per Rule 2 rather than folded into this story''s own classification.').

omega_variable(
    historical_baseline_selection,
    'What baseline year for ''cumulative historical emissions'' is normatively defensible — 1850 (start of industrialization), 1950 (post-WWII acceleration), or 1990 (UNFCCC founding era) — and does the choice of baseline itself constitute a hidden policy lever inside the ''historical responsibility'' formula?',
    'Comparative analysis of emissions-share rankings under each baseline; negotiating-history review of which baseline proposals developed vs. developing blocs have favored (developed nations have historically favored later baselines that reduce their relative share; developing nations have favored earlier baselines).',
    'A later baseline (e.g. 1990) substantially reduces the calculated obligation of early industrializers and increases the relative share attributed to rapidly industrializing states — this could shift emerging-economy high emitters from the excluded/beneficiary seat into the payer seat, materially changing the victim set this story declares.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_baseline_selection, empirical, 'Whether the choice of historical baseline is itself an extraction lever within the formula this reading treats as settled.').

omega_variable(
    emerging_economy_reclassification_pressure,
    'As emerging-economy high emitters (e.g. current top-3 cumulative emitters by some baselines) approach or exceed the cumulative emissions share of some ''developed'' nations, does the historical-responsibility formula require their reclassification into the payer set, and if so, on what threshold?',
    'Track cumulative emissions crossover points against UNFCCC Annex I/non-Annex I classification; observe whether any formal treaty revision proposes moving specific states between classifications.',
    'If reclassification occurs, several states currently coded as beneficiaries/excluded in this story''s stakeholder list would shift into the payer role, and the story''s beneficiary/victim declarations would need revision to remain accurate to the reading''s own logic rather than to a frozen 1992-era classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(emerging_economy_reclassification_pressure, empirical, 'Whether static developed/developing classification is compatible with a genuinely cumulative-emissions-indexed formula over a multi-decade interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__historical_responsibility_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_tr_t1992, cbdr_principle__historical_responsibility_reading, theater_ratio, 1992, 0.6).
narrative_ontology:measurement(cbdr_tr_t1997, cbdr_principle__historical_responsibility_reading, theater_ratio, 1997, 0.58).
narrative_ontology:measurement(cbdr_tr_t2005, cbdr_principle__historical_responsibility_reading, theater_ratio, 2005, 0.55).
narrative_ontology:measurement(cbdr_tr_t2015, cbdr_principle__historical_responsibility_reading, theater_ratio, 2015, 0.5).
narrative_ontology:measurement(cbdr_tr_t2020, cbdr_principle__historical_responsibility_reading, theater_ratio, 2020, 0.46).
narrative_ontology:measurement(cbdr_tr_t2024, cbdr_principle__historical_responsibility_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(cbdr_be_t1992, cbdr_principle__historical_responsibility_reading, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(cbdr_be_t1997, cbdr_principle__historical_responsibility_reading, base_extractiveness, 1997, 0.38).
narrative_ontology:measurement(cbdr_be_t2005, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(cbdr_be_t2015, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(cbdr_be_t2020, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(cbdr_be_t2024, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_su_t1992, cbdr_principle__historical_responsibility_reading, suppression_requirement, 1992, 0.2).
narrative_ontology:measurement(cbdr_su_t1997, cbdr_principle__historical_responsibility_reading, suppression_requirement, 1997, 0.22).
narrative_ontology:measurement(cbdr_su_t2005, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2005, 0.26).
narrative_ontology:measurement(cbdr_su_t2015, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2015, 0.28).
narrative_ontology:measurement(cbdr_su_t2020, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2020, 0.32).
narrative_ontology:measurement(cbdr_su_t2024, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__historical_responsibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(cbdr_principle__historical_responsibility_reading, 0.12).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, cbdr_principle__voluntary_commitment_reading).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, loss_and_damage_fund_operationalization).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, paris_agreement_ndc_ratchet_mechanism).

% DUAL FORMULATION NOTE:
% This constraint and cbdr_principle__voluntary_commitment_reading are sibling readings of the single cbdr_principle kernel. They share the same underlying treaty text (UNFCCC Art. 3.1, Paris Agreement preamble and Art. 9) but diverge on whether CBDR creates binding, historically-indexed obligations (this story) or voluntary, technology-transfer-centered obligations (sibling). The two stories carry different ε (this story: 0.58, substantially extractive of developed-nation resources; sibling: expected lower ε, closer to rope, since voluntary commitments carry weaker enforcement and a smaller true victim set) and different beneficiary/victim declarations (this story removes developing nations from any victim role; the sibling reading leaves a residual adaptation-financing-gap victim exposure for developing nations since voluntary pledges are chronically underdelivered). Both stories link to each other via affects_constraints per the kernel-decomposition rule; neither is authored as 'the' correct reading of CBDR.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
