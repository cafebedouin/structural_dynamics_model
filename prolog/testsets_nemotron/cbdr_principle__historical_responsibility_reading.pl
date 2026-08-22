% ============================================================================
% CONSTRAINT STORY: cbdr_principle__historical_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: cbdr_principle__historical_responsibility_reading
 *   human_readable: CBDR Historical Responsibility Reading — Binding Emissions Reductions & Loss/Damage Finance
 *   domain: international_climate_governance/treaty_law/development_economics
 *
 * SUMMARY:
 *   The Common But Differentiated Responsibilities and Respective
 *   Capabilities (CBDR-RC) principle, read through the historical
 *   responsibility lens, requires developed nations to accept binding
 *   emissions reductions proportional to their cumulative historical
 *   emissions (since industrialization) and to provide loss/damage financing
 *   commensurate with that historical share. This reading treats the
 *   atmosphere's finite carbon budget as a distributive justice problem:
 *   those who filled the budget owe both the remaining mitigation effort and
 *   compensation for harms already locked in. The constraint is structurally
 *   a tangled rope — it performs genuine coordination (a single equity metric
 *   for burden-sharing, a dedicated loss/damage channel) while extracting
 *   substantial transfers from identifiable payers (Annex I/OECD) through
 *   active enforcement (treaty reporting, transparency framework, financial
 *   mechanism governance). The voluntary commitment reading is the sibling
 *   constraint, treating CBDR as a dynamic, self-differentiated process
 *   without historical anchoring.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, 0.68).
domain_priors:suppression_score(cbdr_principle__historical_responsibility_reading, 0.45).
domain_priors:theater_ratio(cbdr_principle__historical_responsibility_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__historical_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__historical_responsibility_reading, "CBDR Historical Responsibility Reading — Binding Emissions Reductions & Loss/Damage Finance").
narrative_ontology:topic_domain(cbdr_principle__historical_responsibility_reading, "international_climate_governance/treaty_law/development_economics").

domain_priors:requires_active_enforcement(cbdr_principle__historical_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__historical_responsibility_reading, 'e8ed939c-3536-4788-86fd-6cbad471d902').
narrative_ontology:cs_kernel_codification('e8ed939c-3536-4788-86fd-6cbad471d902', formalized).
narrative_ontology:cs_authority_grounding('e8ed939c-3536-4788-86fd-6cbad471d902', lineage).
narrative_ontology:cs_interpretation_layer_present('e8ed939c-3536-4788-86fd-6cbad471d902').
narrative_ontology:cs_reading_relation('e8ed939c-3536-4788-86fd-6cbad471d902', cbdr_principle__voluntary_commitment_reading, influences).
narrative_ontology:cs_axiom('e8ed939c-3536-4788-86fd-6cbad471d902', foundational, cumulative_historical_emissions_as_obligation_anchor).
narrative_ontology:cs_axiom_status(cumulative_historical_emissions_as_obligation_anchor, holdable).
narrative_ontology:cs_axiom_grounding('e8ed939c-3536-4788-86fd-6cbad471d902', cumulative_historical_emissions_as_obligation_anchor, deontological).
narrative_ontology:cs_axiom('e8ed939c-3536-4788-86fd-6cbad471d902', foundational, loss_damage_as_separate_financial_obligation_distinct_from_adaptation).
narrative_ontology:cs_axiom_status(loss_damage_as_separate_financial_obligation_distinct_from_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('e8ed939c-3536-4788-86fd-6cbad471d902', loss_damage_as_separate_financial_obligation_distinct_from_adaptation, deontological).
narrative_ontology:cs_reference_frame('e8ed939c-3536-4788-86fd-6cbad471d902', unfccc_1992_equity_architecture).
narrative_ontology:cs_drift_state('e8ed939c-3536-4788-86fd-6cbad471d902', post_paris_loss_damage_fund, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e8ed939c-3536-4788-86fd-6cbad471d902', '').
narrative_ontology:cs_kernel_id(cbdr_principle__historical_responsibility_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, developing_nations_group).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, least_developed_countries).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, small_island_developing_states).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, vulnerable_communities_global_south).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nations_group).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, historical_emitters_oecd).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, fossil_fuel_producing_annex_i).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developing_nations_group).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, historical_responsibility_doctrine).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, common_but_differentiated_respective_capabilities_principle).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, polluter_pays_principle_international).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, climate_justice_framework).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, loss_and_damage_as_separate_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Annex I / OECD nations bearing binding emissions reduction targets proportional to cumulative historical emissions (pre-1990 onward) and obligated to provide loss/damage financing. They control the treaty architecture and financial mechanisms but face domestic political resistance to transfer magnitudes. Exit would mean withdrawing from UNFCCC/Paris framework — legally possible but diplomatically costly and would forfeit influence over global carbon markets and technology transfer rules.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developed_nations_group, payer,
    institutional, generational, constrained, global).

% Non-Annex I nations receiving adaptation finance, technology transfer, and policy space for development. They also bear mitigation costs from domestic action (secondary payer role). Their leverage comes from G77+China bloc cohesion and moral authority of historical responsibility narrative. Exit from the framework is structurally constrained — climate impacts hit them first/ hardest, and alternative financing channels (bilateral, MDBs) are insufficient and conditional.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developing_nations_group, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, developing_nations_group, payer).

% LDC Group and African Group — minimal historical emissions, maximum vulnerability, least adaptive capacity. They are the primary intended recipients of loss/damage financing and grant-based adaptation. Their exit options are near-zero: they cannot meaningfully withdraw from the only multilateral channel for climate finance, and domestic resources are structurally insufficient for adaptation.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, least_developed_countries, beneficiary,
    powerless, biographical, trapped, global).

% AOSIS — existential threat from sea-level rise makes loss/damage not just financial but territorial survival. They championed the loss/damage agenda through decades of resistance. Exit is not an option — the framework is the only venue where their existential claim has procedural standing.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, small_island_developing_states, beneficiary,
    powerless, biographical, trapped, global).

% Frontline communities (coastal, arid, high-mountain, informal urban) within developing nations who bear disproportionate impacts. They are the ultimate beneficiaries of loss/damage flows but have no direct representation in treaty negotiations — mediated through national governments and civil society. Exit is individual migration (increasingly blocked) or local adaptation (resource-constrained).
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, vulnerable_communities_global_south, beneficiary,
    powerless, immediate, trapped, local).

% Subset of developed nations with highest cumulative emissions (USA, EU historic members, UK, Japan, Canada, Australia, Russia). They possess the financial capacity and technological means to meet obligations but use policy discretion, carbon market design, and domestic veto points to modulate transfer magnitude. Their exit options include unilateral carbon clubs, border adjustments (CBAM), and plurilateral agreements that could bypass UNFCCC equity architecture.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, historical_emitters_oecd, payer,
    powerful, generational, arbitrage, global).

% OPEC+ Annex I members (Russia, Norway, USA, Canada, Australia) and Gulf states with observer status. They simultaneously bear payer obligations under historical responsibility and exercise agenda-setting power through energy security narratives, fossil fuel subsidy defense, and obstruction of loss/damage finance operationalization. Exit from the framework is constrained by their need for legitimate market access and technology cooperation (CCUS, hydrogen).
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, fossil_fuel_producing_annex_i, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, fossil_fuel_producing_annex_i, agenda_setter).

% GCF, GEF, Adaptation Fund, Loss and Damage Fund Board, MDBs (World Bank, regional development banks). They administer the financial transfers, set access modalities, and define 'additionality' and 'vulnerability' criteria that determine flow allocation. Their institutional survival depends on the constraint's persistence — they are the operational layer that makes the historical responsibility reading material.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, climate_finance_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Climate Action Network, LDC Watch, Pan African Climate Justice Alliance, youth movements, indigenous peoples' organizations. They monitor compliance, amplify vulnerable voices, and sustain the normative pressure that keeps the historical responsibility reading alive in negotiations. Their analytical seat is grounded in direct engagement with affected communities — not detached academic observation.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, civil_society_climate_justice_networks, observer,
    organized, generational, analytical, global).

% IPCC, UNEP Emissions Gap Report, WMO, scientific academies. They provide the carbon budget accounting, attribution science, and cumulative emissions calculations that ground the 'proportional to historical emissions' metric. Their authority is epistemic — they do not negotiate but their assessments define the constraint's factual parameters.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, ipcc_and_science_assessment_bodies, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global mitigation burden-sharing by anchoring national obligations to a single, measurable metric (cumulative historical emissions) that resolves the free-rider problem in a climate system where all emissions mix globally. Simultaneously coordinates loss/damage finance by establishing a liability-like obligation from historical emitters to those suffering irreversible impacts, creating a channel for redress that pure adaptation finance does not.
% TRANSFER_FUNCTION: Moves binding emissions reduction obligations (tonnes CO2e/yr) from developed nations' domestic economies to the global carbon budget, and moves financial resources (USD/yr) from developed nations' public finance + carbon market revenues to developing nations' adaptation/loss-damage response — with the transfer magnitude indexed to cumulative historical emissions share.
% ABSENT_VOICES: Future generations (who inherit the carbon budget depletion and unpaid loss/damage debt), climate-displaced persons without state representation (cross-border migrants, stateless communities), and fossil-fuel-dependent workers/communities in developed nations (who bear transition costs but are not a negotiated party). These voices would object to: (1) the discount rate applied to future damages, (2) the absence of legal status for climate refugees, (3) the just-transition gap in Annex I domestic policy.
% DISAPPEARANCE_RATIONALE: If the historical responsibility reading vanished overnight, the Paris Agreement would revert to purely voluntary NDCs with no equity anchor — developed nations' obligations would become purely self-determined, loss/damage finance would collapse to voluntary humanitarian aid, and the G77+China coalition would lose its primary normative lever. The global climate regime would reorganize around capability-only (voluntary commitment reading) or fragment into plurilateral carbon clubs.
% FOUNDING_PROBLEM: The climate regime's founding problem (UNFCCC 1992, Art. 3.1) was how to allocate mitigation burden and support for vulnerable nations in a system where: (a) historical emissions are highly concentrated in industrialized nations, (b) impacts fall disproportionately on those who contributed least, (c) the atmosphere is a single well-mixed commons, and (d) developing nations need carbon space for development. CBDR was the negotiated solution — differentiated obligations anchored in historical responsibility and respective capabilities.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as still live by: IPCC AR6 WGIII (historical emissions inequality persists — top 10% households = 40-45% of emissions, bottom 50% = 13-15%), UNFCCC Standing Committee on Finance (adaptation finance gap $194-366bn/yr vs $28bn delivered), and LDC Group submissions. It is attested as substantially solved / overtaken by: OECD DAC (climate finance goal 'likely met' in 2022 per provider reporting, though additionality contested), EU/US negotiators (Paris NDC architecture supersedes static Annex I/non-Annex I), and some development economists (capability-based approaches more operationally relevant than historical accounting). No consensus — the contest IS the constraint's current state.
narrative_ontology:disappearance_verdict(cbdr_principle__historical_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__historical_responsibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__historical_responsibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(cbdr_principle__historical_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__historical_responsibility_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the magnitude of binding transfers required: ~$400-600bn/yr in new loss/damage finance alone by 2030, plus mitigation cost differential, indexed to historical emissions shares that concentrate 79% of cumulative CO2 in Annex I (1850-2019). Suppression (0.45) is moderate — the constraint operates through treaty consent and peer pressure, not coercion, but exit is structurally costly (diplomatic isolation, loss of carbon market access, climate club exclusion). Theater ratio (0.38) captures the growing gap between negotiated text (Paris Art. 8 on loss/damage, Art. 9 on finance) and delivery — elaborate governance structures (Fund Board, Santiago Network) exist while actual flows remain a fraction of obligation. Accessibility collapse (0.35) is low — alternative burden-sharing metrics (capability-only, per-capita, hybrid) remain viable and are actively negotiated. Resistance (0.62) is high — developed nations resist historical quantification, loss/damage liability language, and mandatory finance; they exercise veto through consensus rules, definitional ambiguity, and domestic ratification requirements.
 *
 * PERSPECTIVAL GAP:
 *   From the developed nation payer seat, the constraint appears as extractive overreach — a fixed historical ledger that ignores current emissions trajectories (China now #1 annual emitter) and capability shifts. From the LDC/SIDS beneficiary seat, it appears as the only structural guarantee of survival finance — without historical anchoring, finance becomes voluntary charity. From the climate finance institution seat, it appears as mission justification — the constraint creates their mandate. The engine computes these divergences from the structural data; the authored claim (tangled_rope) does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed nations (institutional/powerful, constrained/arbitrage exit) sit at the target end (d → 1.0) — they bear the binding financial and mitigation transfers. Developing nations (organized/powerless, constrained/trapped exit) sit at the beneficiary end (d → 0.0) — they receive finance and policy space. Vulnerable communities (powerless, trapped) are deep beneficiaries with zero exit. Fossil fuel producers in Annex I are dual-positioned: payer for historical emissions, agenda setter obstructing operationalization — their effective d is modulated upward by agenda-setting power. Climate finance institutions are agenda setters with analytical exit — their institutional interest aligns with constraint persistence. Observers (civil society, science bodies) have analytical exit and zero extraction exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (equitable burden-sharing for a global commons with unequal history) remains live — cumulative emissions inequality has worsened, not improved, since 1992. However, the specific institutional form (static Annex I/non-Annex I bifurcation with binding targets) has atrophied: the Kyoto Protocol's binding-target architecture collapsed, Paris replaced it with NDCs, and the loss/damage finance obligation was only recognized in 2022 (COP27) after 30 years of resistance. The historical responsibility reading persists as a normative anchor and negotiating position but its operational form has shifted from binding targets (Kyoto) to voluntary contributions indexed to a contested equity metric (Paris + Loss & Damage Fund). This is not pure mandatrophy — the coordination function (equity anchor) remains live — but the enforcement mechanism has degraded from treaty law to political pressure. The theater ratio rise (0.1 → 0.38) tracks this: more institutional performance (Fund Boards, Networks, Dialogues) per unit of actual transfer delivered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_emissions_accounting_boundary,
    'Where does the historical responsibility ledger start and end — pre-industrial (1750), post-WWII (1950), UNFCCC baseline (1990), or a moving window? Does it include land-use change, colonial-era emissions allocated to colonizer or colony, and emissions embedded in trade?',
    'IPCC methodological guidance on historical emissions accounting; UNFCCC technical dialogue on common metrics; eventual negotiation of a common accounting rulebook under Paris Art. 13 transparency framework.',
    'The accounting boundary determines the numerical obligation shares. A 1750 start with colonial allocation increases OECD shares to ~85%; a 1990 start with production-based accounting reduces to ~60%. This directly scales extractiveness (epsilon) and determines which nations enter the payer set. If unresolved, the constraint''s extraction magnitude is indeterminate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_emissions_accounting_boundary, conceptual, 'The historical emissions accounting boundary is unsettled and changes the constraint''s quantitative extraction profile.').

omega_variable(
    loss_damage_liability_vs_finance,
    'Does the loss/damage obligation under this reading constitute legal liability (compensation for harm, implying fault and enforceable claims) or structured finance (solidarity-based grants, no admission of liability)?',
    'COP27 decision (loss/damage fund) explicitly avoids ''liability and compensation'' language per developed nation red line. Future litigation (ICJ advisory opinion, ITLOS, national courts) may establish customary law liability. The Fund Board''s operational modalities (grants vs. loans, eligibility criteria, trigger mechanisms) will reveal the de facto character.',
    'If liability, suppression increases (legal enforceability, court-ordered payments) and extractiveness becomes legally determined rather than politically negotiated. If finance, the constraint remains in the political realm — extraction depends on periodic replenishment cycles and donor discretion. The theater ratio is higher under finance (elaborate governance for voluntary flows).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(loss_damage_liability_vs_finance, conceptual, 'Whether loss/damage is liability or finance changes the constraint''s enforcement character and extraction certainty.').

omega_variable(
    developed_nation_coalition_fragmentation,
    'Will the developed nation payer coalition hold as a unified block, or will differential exposure (US vs EU vs Japan vs Australia vs Canada) fracture the payer side into variable-geometry coalitions?',
    'Track negotiation blocs: Umbrella Group (US, Japan, Australia, Canada, Norway, etc.) vs EU vs EIG vs individual positions. Watch for carbon club formation (CBAM, climate clubs) that create alternative burden-sharing outside UNFCCC. Domestic political cycles (US elections, EU Green Deal politics) drive fragmentation.',
    'If the payer coalition fragments, the constraint''s enforcement degrades (suppression falls) but extractiveness may rise for remaining payers (EU carrying disproportionate share). If it holds, the constraint maintains coordinated pressure but risks systemic breakdown if a major payer (US) withdraws. The tangled rope classification depends on active enforcement — fragmentation tests whether enforcement is collective or bilateral.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developed_nation_coalition_fragmentation, empirical, 'Coalition cohesion among historical emitters determines whether the constraint''s enforcement is collective or fragmenting.').

omega_variable(
    kernel_reading_boundary,
    'Is the historical_responsibility_reading a distinct constraint from the voluntary_commitment_reading, or are they observably the same constraint measured at different time horizons (historical = long-term equity anchor, voluntary = short-term operational form)?',
    'Apply the epsilon-invariance test: if measuring the constraint via ''binding historical share of mitigation+finance'' yields ε≈0.68 while ''voluntary NDC ambition + technology transfer'' yields ε≈0.35, they are distinct constraints. If both readings converge on the same operational obligations (e.g., Paris NDCs with equity assessment converge to historical shares), they are one constraint with measurement ambiguity.',
    'If distinct, the kernel decomposition is valid and both stories should exist with network.affects_constraints linking them. If same constraint, the historical reading is a framing overlay on the voluntary constraint — the epsilon-invariance principle would require merging into one story with the lower ε (voluntary) as the operational reality, and the historical reading becomes a vindicated proposition, not a beneficiary structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the two CBDR readings are structurally distinct constraints or framings of one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__historical_responsibility_reading, 1992, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_hist_tr_t1992, cbdr_principle__historical_responsibility_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(cbdr_hist_tr_t1997, cbdr_principle__historical_responsibility_reading, theater_ratio, 1997, 0.18).
narrative_ontology:measurement(cbdr_hist_tr_t2001, cbdr_principle__historical_responsibility_reading, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(cbdr_hist_tr_t2009, cbdr_principle__historical_responsibility_reading, theater_ratio, 2009, 0.32).
narrative_ontology:measurement(cbdr_hist_tr_t2015, cbdr_principle__historical_responsibility_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(cbdr_hist_tr_t2022, cbdr_principle__historical_responsibility_reading, theater_ratio, 2022, 0.35).
narrative_ontology:measurement(cbdr_hist_tr_t2030, cbdr_principle__historical_responsibility_reading, theater_ratio, 2030, 0.37).
narrative_ontology:measurement(cbdr_hist_tr_t2035, cbdr_principle__historical_responsibility_reading, theater_ratio, 2035, 0.38).

% Extraction over time
narrative_ontology:measurement(cbdr_hist_be_t1992, cbdr_principle__historical_responsibility_reading, base_extractiveness, 1992, 0.15).
narrative_ontology:measurement(cbdr_hist_be_t1997, cbdr_principle__historical_responsibility_reading, base_extractiveness, 1997, 0.22).
narrative_ontology:measurement(cbdr_hist_be_t2001, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2001, 0.18).
narrative_ontology:measurement(cbdr_hist_be_t2009, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2009, 0.35).
narrative_ontology:measurement(cbdr_hist_be_t2015, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(cbdr_hist_be_t2022, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2022, 0.58).
narrative_ontology:measurement(cbdr_hist_be_t2030, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2030, 0.65).
narrative_ontology:measurement(cbdr_hist_be_t2035, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2035, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_hist_su_t1992, cbdr_principle__historical_responsibility_reading, suppression_requirement, 1992, 0.2).
narrative_ontology:measurement(cbdr_hist_su_t1997, cbdr_principle__historical_responsibility_reading, suppression_requirement, 1997, 0.3).
narrative_ontology:measurement(cbdr_hist_su_t2001, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2001, 0.25).
narrative_ontology:measurement(cbdr_hist_su_t2009, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2009, 0.4).
narrative_ontology:measurement(cbdr_hist_su_t2015, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(cbdr_hist_su_t2022, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2022, 0.43).
narrative_ontology:measurement(cbdr_hist_su_t2030, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2030, 0.44).
narrative_ontology:measurement(cbdr_hist_su_t2035, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2035, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__historical_responsibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(cbdr_principle__historical_responsibility_reading, 0.12).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, cbdr_principle__voluntary_commitment_reading).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, paris_agreement_ndc_architecture).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, loss_and_damage_fund_operationalization).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, global_carbon_budget_allocation).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, climate_finance_goal_post_2025).

% DUAL FORMULATION NOTE:
% This constraint and voluntary_commitment_reading form the cbdr_principle constraint family. They share the kernel (CBDR principle) but instantiate different structural constraints: this reading anchors obligations in historical emissions (high ε, binding transfers); the sibling anchors in current capability and voluntary pledges (lower ε, coordination via transparency). The historical reading influences the sibling by providing the equity benchmark against which NDCs are assessed in the Global Stocktake. Both are needed to model the actual negotiation dynamics — parties switch readings strategically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cbdr_principle__historical_responsibility_reading, powerful, 0.75).
constraint_indexing:directionality_override(cbdr_principle__historical_responsibility_reading, institutional, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
