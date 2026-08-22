% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__resource_sovereignty_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__resource_sovereignty_primacy, []).

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
 *   constraint_id: provincial_sovereignty_boundary__resource_sovereignty_primacy
 *   human_readable: Provincial Resource Ownership as Territorial Sovereignty (s.92A Primacy Reading)
 *   domain: political economy/federalism/resource governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading — resource_sovereignty_primacy — of
 *   the contested kernel provincial_sovereignty_boundary. On this reading,
 *   provincial ownership of natural resources under s.92A of the Constitution
 *   Act 1982 grounds full territorial sovereignty: what lies beneath
 *   provincial soil belongs to the province as a matter of right, federal
 *   climate and fiscal instruments that reach the resource base are
 *   illegitimate takings, and unilateral exit is a constitutional
 *   entitlement. The standing arrangement under contest is the actual
 *   federal-provincial resource governance order — s.92A ownership together
 *   with the federal instruments that press against it — and epsilon below is
 *   authored for that arrangement by this reading's own lights. The sibling
 *   readings (constitutional_subordination, compact_federalism) are separate
 *   constraint stories over the same referent and are deliberately NOT
 *   described or averaged here; per the epsilon-invariance principle, one
 *   reading, one constraint, one epsilon. KEY AGENTS (by structural
 *   relationship): - producing_provincial_governments: Primary beneficiary
 *   and administrator (institutional/constrained) — own the resource base,
 *   collect royalties, defend the line - extraction_industry_leaseholders:
 *   Secondary beneficiary (powerful/mobile) — hold leases and permits under
 *   provincial regimes - indigenous_nations_with_unsurrendered_title: Primary
 *   target (organized/identity_locked) — title subordinated to provincial
 *   Crown ownership - federal_government: Target of the boundary's fencing
 *   (institutional/constrained) — climate and fiscal capacity stopped at the
 *   line - climate_cost_bearers: Diffuse target (powerless/trapped) — carry
 *   emission and reclamation costs with no seat - supreme_court_of_canada:
 *   Analytical observer (institutional/analytical) — fixes the line case by
 *   case - resource_region_municipalities: Excluded voice (powerless/trapped)
 *   — bear local costs with no constitutional role
 *
 * KEY AGENTS:
 *   - producing_provincial_governments: Primary beneficiary/agenda-setter (institutional/constrained) — owns and administers the resource base, collects royalties, litigates the line
 *   - extraction_industry_leaseholders: Secondary beneficiary (powerful/mobile) — holds leases and permits; returns depend on provincial permitting control persisting
 *   - indigenous_nations_with_unsurrendered_title: Primary target (organized/identity_locked) — unsurrendered title overridden by asserted provincial Crown ownership; exit impossible
 *   - federal_government: Target of the fencing (institutional/constrained) — national climate and fiscal instruments repeatedly defeated or narrowed at the line
 *   - climate_cost_bearers: Diffuse target (powerless/trapped) — unorganized bearers of emission and liability costs
 *   - supreme_court_of_canada: Analytical observer (institutional/analytical) — adjudicates the line without collecting or paying
 *   - resource_region_municipalities: Excluded voice (powerless/trapped) — absorb local liabilities with no seat in the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.62).
domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.68).
domain_priors:theater_ratio(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, extractiveness, 0.62).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__resource_sovereignty_primacy, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__resource_sovereignty_primacy, "Provincial Resource Ownership as Territorial Sovereignty (s.92A Primacy Reading)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__resource_sovereignty_primacy, "political economy/federalism/resource governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__resource_sovereignty_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__resource_sovereignty_primacy, '49d5bac9-6492-4b08-badc-76e967bb5caa').
narrative_ontology:cs_kernel_codification('49d5bac9-6492-4b08-badc-76e967bb5caa', fixed_text).
narrative_ontology:cs_authority_grounding('49d5bac9-6492-4b08-badc-76e967bb5caa', lineage).
narrative_ontology:cs_interpretation_layer_present('49d5bac9-6492-4b08-badc-76e967bb5caa').
narrative_ontology:cs_reading_relation('49d5bac9-6492-4b08-badc-76e967bb5caa', provincial_sovereignty_boundary__constitutional_subordination, coexists_with).
narrative_ontology:cs_reading_relation('49d5bac9-6492-4b08-badc-76e967bb5caa', provincial_sovereignty_boundary__compact_federalism, influences).
narrative_ontology:cs_axiom('49d5bac9-6492-4b08-badc-76e967bb5caa', foundational, resource_ownership_grounds_territorial_sovereignty).
narrative_ontology:cs_axiom_status(resource_ownership_grounds_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('49d5bac9-6492-4b08-badc-76e967bb5caa', resource_ownership_grounds_territorial_sovereignty, deontological).
narrative_ontology:cs_axiom('49d5bac9-6492-4b08-badc-76e967bb5caa', secondary, unilateral_secession_is_constitutional_right).
narrative_ontology:cs_axiom_status(unilateral_secession_is_constitutional_right, holdable).
narrative_ontology:cs_axiom_grounding('49d5bac9-6492-4b08-badc-76e967bb5caa', unilateral_secession_is_constitutional_right, conventional).
narrative_ontology:cs_reference_frame('49d5bac9-6492-4b08-badc-76e967bb5caa', proprietary_provincial_sovereignty).
narrative_ontology:cs_drift_state('49d5bac9-6492-4b08-badc-76e967bb5caa', post_carbon_pricing_reference_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('49d5bac9-6492-4b08-badc-76e967bb5caa', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, producing_provincial_governments).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, extraction_industry_leaseholders).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_nations_with_unsurrendered_title).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, climate_cost_bearers).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_proprietary_rights_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__resource_sovereignty_primacy, s92a_resource_ownership_clause).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Govern resource-producing jurisdictions (of the Alberta, Saskatchewan, and Newfoundland-and-Labrador type). Own and administer Crown resource lands within their borders under s.92A: set royalty rates, auction leases, approve or refuse projects, and collect resource revenue that funds a large share of provincial budgets. Defend the ownership line against federal instruments through litigation, intergovernmental councils, and, recently, declaratory sovereignty statutes. Leaving the federation is not available in practice; their leverage runs entirely through control of the resource base itself.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, producing_provincial_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__resource_sovereignty_primacy, producing_provincial_governments, agenda_setter).

% Oil and gas producers, mining companies, and pipeline proponents holding provincial leases and permits. Face one principal regulator per province, negotiate royalty and access terms with provincial ministries, and can in principle redeploy capital to other basins or countries, though sunk infrastructure ties much of it in place. Their returns depend on provincial control of permitting remaining where it is.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, extraction_industry_leaseholders, beneficiary,
    powerful, biographical, mobile, global).

% Nations whose territories were never covered by land-surrender treaties, principally in British Columbia and parts of the Prairies and North. Provincial Crown ownership was asserted over their lands without purchase or agreement; resource decisions on those lands run through provincial permitting regimes in which they hold consultation rights but neither ownership nor a veto. Their recourse is decades-long litigation; their communities, economies, and self-conception are tied to the territories at issue and cannot be relocated.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_nations_with_unsurrendered_title, payer,
    organized, generational, identity_locked, regional).

% Sets national climate targets, prices carbon through a backstop where provincial systems fall short, funds equalization, and claims authority over interprovincial works and transboundary project effects. Its instruments repeatedly collide with provincial resource jurisdiction and are tested at the Supreme Court; it cannot move the ownership line without provincial consent under the amending formula, and it cannot leave the federation either.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, payer,
    institutional, generational, constrained, national).

% Canadian households and, indirectly, global populations exposed to climate damage from emissions released under provincially permitted development. They are diffuse and unorganized, hold no seat in intergovernmental negotiation, and receive their share of the costs as insurance premiums, disaster recovery bills, and long-term warming regardless of which government made the original decision.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, climate_cost_bearers, payer,
    powerless, generational, trapped, global).

% Adjudicates the recurring references and appeals that fix where the jurisdictional line falls — secession (1998), carbon pricing (2021), impact assessment (2023). Its reasons bind both orders of government; it collects no resource revenue and bears none of the costs it allocates.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, supreme_court_of_canada, observer,
    institutional, generational, analytical, national).

% Towns and cities hosting wells, mines, and pipelines. They absorb boom-bust fiscal swings, orphan-well and reclamation liabilities, and infrastructure wear, but hold no constitutional role in resource decisions and depend on provincial grants that rise and fall with the same cycles.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_region_municipalities, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__resource_sovereignty_primacy, producing_provincial_governments).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__resource_sovereignty_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates ownership and regulatory authority over natural resources to the government closest to the resource. It solves a real collective-action problem: without a settled allocation, every well, mine, and forest would sit in overlapping federal-provincial jurisdiction, developers would face dueling regulators, and provincial electorates would govern endowments they do not control.
% TRANSFER_FUNCTION: Moves resource rents — royalties, leasing revenue, and regulatory discretion — to producing-provincial treasuries and, through purchased access, to leaseholders. Moves climate and reclamation costs outward onto diffuse bearers with no seat. Blocks federal fiscal and climate instruments from reaching the resource base behind the line.
% ABSENT_VOICES: Indigenous nations were absent from both the 1867 and 1982 tables where the ownership rules were written; s.35 arrived alongside s.92A with no provincial consent requirement attached to resource decisions. Resource-region municipalities and climate-exposed populations likewise hold no seat and would object to decisions taken wholly within provincial jurisdiction.
% DISAPPEARANCE_RATIONALE: Royalty regimes, provincial budget structures, industry permitting, equalization politics, and indigenous-title litigation all presuppose the ownership boundary. Overnight removal would force renegotiation of who owns and regulates the resource base, repricing leases and shifting billions in annual revenue and liability between orders of government and onto new holders.
% FOUNDING_PROBLEM: Settle ownership and jurisdiction over natural resources so that resource development is not paralyzed by intergovernmental conflict: at Confederation the lands belonged to the centre; the 1930 natural-resources transfer agreements moved Prairie Crown lands to the provinces; s.92A (1982) confirmed provincial ownership after offshore disputes. The founding problem is jurisdictional allocation, not sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Supreme Court of Canada reference judgments (Secession 1998; Greenhouse Gas Pricing 2021; Impact Assessment 2023) repeatedly restate the allocation question as unsettled; constitutional-law scholarship and the Rowell-Sirois Royal Commission attest the same. Producing-province governments also attest it, but they sit inside the beneficiary set, so the judicial and scholarly attestation is the load-bearing one.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__resource_sovereignty_primacy, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__resource_sovereignty_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62 is reading-indexed over the standing arrangement: by this reading's own lights the current order is substantially extractive, because federal climate and fiscal instruments reach past the ownership line (backstop carbon pricing applied in provinces, impact-assessment authority over designated projects, equalization conditionality debates), and the reading counts that reach as taking from the producing side. The metric aggregates the reading's assessment; it does not endorse it. Suppression 0.68 is a raw structural property, unscaled by power or scope: the line is held by constitutional supremacy plus a permanent litigation machine, and alternatives (amendment, negotiated delegation, federal withdrawal) exist on paper but are prohibitively slow or require the very consent being contested. Theater_ratio 0.38: routine royalty administration and permitting are fully functional, but a growing share of activity is declaratory — sovereignty motions, symbolic statutes that purport to nullify federal law without machinery to do so — and that share rises as substantive control erodes. Accessibility_collapse 0.55: once the allocation is understood, alternatives partly collapse (the amending formula makes unilateral federal repossession practically unavailable) but federal workarounds persist and are partially validated by the courts, so collapse is real but incomplete. Resistance 0.70: the line meets continuous, organized resistance — federal references, indigenous title actions, interprovincial pipeline conflicts — and must be re-won case by case. The claim/metric pair is independent: tangled_rope is claimed from structure (a genuine allocation function carrying asymmetric costs under active enforcement), not tuned to the numbers. The measurement series share one grid (1980-2025, seven points) across all three tracked metrics; the extractiveness series is two-humped rather than monotonic — peaks at the National Energy Program (1980) and the carbon-pricing/backstop era (2019-2021), troughs during the devolution and free-trade years — driven by the federal policy cycle rather than by intermittent reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the producing-province seat the arrangement is rightful ownership under siege: the boundary is experienced as protection, and the extraction is located entirely on the far side of the line, in Ottawa. From the indigenous-nation seat the same boundary is a wall: the decisive fact is that decisions over their territories are made in a room they entered only as consultees. From the federal seat it is a ceiling on national policy capacity; from the climate-bearer seat it is an invisible subsidy paid by people who were never asked. Notably, this reading is authored from a seat adjacent to the primary beneficiary, yet it still rates the standing arrangement substantially extractive — because its referent includes the federal instruments it condemns. That divergence between seats, computed by the engine from the structural data, is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: producing_provincial_governments and extraction_industry_leaseholders sit near the beneficiary end (low d, damped or inverted effective extraction) — the provinces as residual claimants of the line, the leaseholders as purchasers of access under it. Victim declarations drive the target end: indigenous_nations_with_unsurrendered_title combine payer status with identity_locked exit (land-based identity, immovable territory), placing them nearest the full-target pole; federal_government is a payer with constrained exit and institutional power, high d but not maximal; climate_cost_bearers are payers with trapped exit and no organization — individually powerless, and coalition formation is structurally difficult because the harm is diffuse, delayed, and global, so their effective weight arrives mainly through the federal seat that nominally represents them. The supreme_court_of_canada holds the analytical seat and contributes no directional pull. Spatial scopes amplify accordingly: the global scope carried by industry and climate bearers raises verification difficulty where the engine applies it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Against rope: the arrangement does solve a real allocation problem, but the same fence that allocates jurisdiction also shelters rents and externalizes costs onto parties with no seat, and holding it requires active enforcement — that is the tangled-rope signature, not pure coordination. Against piton: the founding problem (jurisdictional allocation) is still live, corroborated by the courts from outside the beneficiary set, and the arrangement is vigorously maintained rather than inertially performed — though the rising theater ratio is the early-warning series to watch, since declaratory sovereignty statutes are precisely what maintenance looks like as substance drains. Against snare: the coordination function is genuine and historically prior to the extraction; the victims are real but the structure would not dissolve into pure taking if they were compensated. The R5 mismatch check runs clean here — founding problem live, world rearranges on removal — so no zombie flag is warranted on current data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates one reading (resource_sovereignty_primacy) of the kernel provincial_sovereignty_boundary; what would the sibling readings (constitutional_subordination, compact_federalism) change structurally if instantiated instead?',
    'Author and classify the two sibling stories over the same referent, then compare beneficiary/victim sets, exit treatment, and epsilon; the divergence across the three files maps the contest.',
    'Under constitutional_subordination the same arrangement reads as ordinary delegated jurisdiction with no inherent sovereignty and no exit right, and the producing provinces drop out of the victim set entirely; under compact_federalism the victim set shifts to compact-breached provinces and exit becomes negotiable rather than unilateral. Classification of this file is conditional on the reading, not on the topic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this constraint is one of three readings of the provincial sovereignty kernel; siblings are separate files.').

omega_variable(
    unilateral_exit_legal_status,
    'Is unilateral provincial exit actually a constitutional right, as this reading asserts, or does the 1998 Secession Reference''s framework (clear majority on a clear question triggers a negotiation obligation, not a departure right) defeat it?',
    'Doctrinal analysis of the Reference re Secession of Quebec and subsequent practice; any actual exit attempt would force immediate adjudication and settle the question.',
    'If no unilateral right exists, the reading''s sovereignty claim is internally incomplete — an authority that cannot leave is holding jurisdiction, not sovereignty — and the constraint downgrades from sovereignty-grounding to ownership-shielding, lowering its claimed reach.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unilateral_exit_legal_status, empirical, 'Legal status of the exit right the reading depends on.').

omega_variable(
    aboriginal_title_priority,
    'Does s.35 Aboriginal title — confirmed in Tsilhqot''in (2014) to include the right to decide how titled lands are used — override provincial resource jurisdiction on declared-title lands?',
    'Litigation testing provincial permitting against declared title, and remedial rulings on consultation depth and consent standards on titled territories.',
    'If title carries jurisdictional priority, the absolute character of provincial resource sovereignty fails across a growing land base, the victim set gains a seat with effective veto power, and the extraction asymmetry narrows on titled lands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aboriginal_title_priority, empirical, 'Whether indigenous title caps the sovereignty claim geographically.').

omega_variable(
    settlement_vs_political_construct,
    'Is the ownership boundary a settled constitutional allocation, or a political construct continuously re-manufactured by regional mobilization?',
    'Observe boundary behavior across federal-provincial truces versus confrontations: a settlement holds steady without mobilization; a construct tracks mobilization intensity (compare the quiet devolution years against the NEP and carbon-pricing confrontations).',
    'If construct, the constraint''s stability is contingent on enforcement coalitions and its effective enforcement dependence rises; if settlement, persistence requires no active defense and the suppression series overstates ongoing coercive need.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_vs_political_construct, conceptual, 'Persistence basis of the boundary: settlement or manufactured consensus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__resource_sovereignty_primacy, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1980, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1980, 0.15).
narrative_ontology:measurement_basis(prov_tr_t1980, observed).
narrative_ontology:measurement(prov_tr_t1990, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1990, 0.18).
narrative_ontology:measurement_basis(prov_tr_t1990, observed).
narrative_ontology:measurement(prov_tr_t2000, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2000, 0.2).
narrative_ontology:measurement_basis(prov_tr_t2000, observed).
narrative_ontology:measurement(prov_tr_t2010, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2010, 0.24).
narrative_ontology:measurement_basis(prov_tr_t2010, observed).
narrative_ontology:measurement(prov_tr_t2018, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2018, 0.3).
narrative_ontology:measurement_basis(prov_tr_t2018, observed).
narrative_ontology:measurement(prov_tr_t2021, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2021, 0.34).
narrative_ontology:measurement_basis(prov_tr_t2021, observed).
narrative_ontology:measurement(prov_tr_t2025, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2025, 0.38).
narrative_ontology:measurement_basis(prov_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(prov_be_t1980, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement_basis(prov_be_t1980, observed).
narrative_ontology:measurement(prov_be_t1990, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement_basis(prov_be_t1990, observed).
narrative_ontology:measurement(prov_be_t2000, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement_basis(prov_be_t2000, observed).
narrative_ontology:measurement(prov_be_t2010, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement_basis(prov_be_t2010, observed).
narrative_ontology:measurement(prov_be_t2018, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement_basis(prov_be_t2018, observed).
narrative_ontology:measurement(prov_be_t2021, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2021, 0.66).
narrative_ontology:measurement_basis(prov_be_t2021, observed).
narrative_ontology:measurement(prov_be_t2025, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(prov_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1980, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1980, 0.74).
narrative_ontology:measurement_basis(prov_su_t1980, observed).
narrative_ontology:measurement(prov_su_t1990, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement_basis(prov_su_t1990, observed).
narrative_ontology:measurement(prov_su_t2000, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2000, 0.54).
narrative_ontology:measurement_basis(prov_su_t2000, observed).
narrative_ontology:measurement(prov_su_t2010, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2010, 0.56).
narrative_ontology:measurement_basis(prov_su_t2010, observed).
narrative_ontology:measurement(prov_su_t2018, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2018, 0.61).
narrative_ontology:measurement_basis(prov_su_t2018, observed).
narrative_ontology:measurement(prov_su_t2021, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2021, 0.65).
narrative_ontology:measurement_basis(prov_su_t2021, observed).
narrative_ontology:measurement(prov_su_t2025, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2025, 0.68).
narrative_ontology:measurement_basis(prov_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_allocation).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary__constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary__compact_federalism).

% DUAL FORMULATION NOTE:
% The colloquial label 'provincial sovereignty over natural resources' decomposes into three structurally distinct readings of one kernel (provincial_sovereignty_boundary). This file instantiates resource_sovereignty_primacy alone: s.92A ownership grounds absolute sovereignty, federal climate/fiscal reach is illegitimate taking, unilateral exit is a right. The sibling files instantiate constitutional_subordination (provinces as creatures of the federal constitution; no inherent sovereignty; exit requires federal consent) and compact_federalism (Confederation as a compact among sovereign provinces; residual sovereignty; negotiated exit). All three share the referent — the standing federal-provincial resource arrangement — and author different epsilon over it by their own lights; they are linked here rather than merged because a single story averaging across readings would violate epsilon-invariance. Upstream/downstream: this reading supplies the material, property-based grounding that compact_federalism arguments cite, so its fortunes influence the compact reading's legitimacy conditions; it merely coexists with the subordination reading, whose premise it contradicts at full strength but which remains reconcilable with a weaker functional-sovereignty version.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
