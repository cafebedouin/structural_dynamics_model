% ============================================================================
% CONSTRAINT STORY: cbdr_principle__historical_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: cbdr_principle__historical_responsibility_reading
 *   human_readable: CBDR Historical Responsibility — Binding Emissions and Loss/Damage Transfer
 *   domain: international/environmental/economic
 *
 * SUMMARY:
 *   The historical responsibility reading of CBDR frames climate obligation
 *   allocation around cumulative historical emissions: developed nations,
 *   responsible for ~80% of atmospheric CO2 since industrialization, must
 *   commit to binding emissions reductions (typically 45-50% by 2030) and
 *   finance loss/damage and adaptation for developing nations. This reading
 *   is ONE of two contested interpretations of the Common But Differentiated
 *   Responsibilities kernel. The alternative reading
 *   (voluntary_commitment_reading) reframes CBDR as voluntary nationally
 *   determined contributions with technology transfer primary. This story
 *   instantiates the historical-responsibility version in its structural
 *   form: developed nations enter the victim set (bearing binding cuts and
 *   financial transfers); developing nations move into beneficiary position
 *   (receiving binding pledges and financing); the justification is causal
 *   responsibility for historical emissions and equity in per-capita rights.
 *   The constraint operates as tangled rope: genuine coordination function
 *   (allocates responsibility without free-riding via the responsibility
 *   principle) fused with asymmetric extraction (developed nations'
 *   sovereignty over domestic climate policy is reduced, fiscal transfers are
 *   imposed, vulnerable populations remain the moral center but are
 *   structurally excluded from negotiation). Resistance is high (0.72):
 *   developed-nation governments and carbon-intensive sectors constantly push
 *   toward voluntary framing, weaker targets, and lower financing; G77+China
 *   and AOSIS coalition resist backsliding.
 *
 * KEY AGENTS:
 *   - developed_nations: institutional power, high time horizon, constrained exit — bear binding reduction targets and financial obligations; sovereignty-limited by binding targets; exit (withdrawal from treaty) carries diplomatic cost
 *   - developing_nations (coalition): organized power, high time horizon, mobile exit — benefit from binding developed-nation commitments and access financing; coalition leverage (G77+China, African Union) provides threat capacity; exit available if financing inadequate
 *   - vulnerable_populations_sids_ldc: powerless, immediate time horizon, trapped exit — the moral center of the claim (their existential vulnerability justifies the redistribution); structurally excluded from negotiation; exit means displacement or ruin
 *   - fossil_fuel_exporters: powerful, biographical time horizon, constrained exit — structurally excluded from formal treaty seats but economically affected (reduced coal/oil demand); their interests shape developed-nation resistance via lobbying
 *   - least_developed_countries_coalition: organized power, high time horizon, mobile exit — sets the negotiating agenda (frames the historical-responsibility claim, concentrates vulnerable-state voting power); threat capacity (withdrawal, non-compliance)
 *   - industrial_agriculture_oil_sectors_developed_north: powerful, biographical time horizon, constrained exit — bear compliance costs of emissions reduction (capital retooling, input costs, revenue pressure); excluded from formal seats; resist via sector lobbying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, 0.68).
domain_priors:suppression_score(cbdr_principle__historical_responsibility_reading, 0.55).
domain_priors:theater_ratio(cbdr_principle__historical_responsibility_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__historical_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__historical_responsibility_reading, "CBDR Historical Responsibility — Binding Emissions and Loss/Damage Transfer").
narrative_ontology:topic_domain(cbdr_principle__historical_responsibility_reading, "international/environmental/economic").

domain_priors:requires_active_enforcement(cbdr_principle__historical_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__historical_responsibility_reading, '3ab499a3-3b36-4ae0-ad60-5f0739d010e6').
narrative_ontology:cs_kernel_codification('3ab499a3-3b36-4ae0-ad60-5f0739d010e6', fixed_text).
narrative_ontology:cs_authority_grounding('3ab499a3-3b36-4ae0-ad60-5f0739d010e6', lineage).
narrative_ontology:cs_interpretation_layer_present('3ab499a3-3b36-4ae0-ad60-5f0739d010e6').
narrative_ontology:cs_reading_relation('3ab499a3-3b36-4ae0-ad60-5f0739d010e6', cbdr_principle__voluntary_commitment_reading, coexists_with).
narrative_ontology:cs_axiom('3ab499a3-3b36-4ae0-ad60-5f0739d010e6', foundational, historical_emissions_create_binding_obligation).
narrative_ontology:cs_axiom_status(historical_emissions_create_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('3ab499a3-3b36-4ae0-ad60-5f0739d010e6', historical_emissions_create_binding_obligation, deontological).
narrative_ontology:cs_axiom('3ab499a3-3b36-4ae0-ad60-5f0739d010e6', foundational, cumulative_responsibility_principle).
narrative_ontology:cs_axiom_status(cumulative_responsibility_principle, holdable).
narrative_ontology:cs_axiom_grounding('3ab499a3-3b36-4ae0-ad60-5f0739d010e6', cumulative_responsibility_principle, deontological).
narrative_ontology:cs_axiom('3ab499a3-3b36-4ae0-ad60-5f0739d010e6', secondary, loss_damage_finance_non_negotiable).
narrative_ontology:cs_axiom_status(loss_damage_finance_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('3ab499a3-3b36-4ae0-ad60-5f0739d010e6', loss_damage_finance_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('3ab499a3-3b36-4ae0-ad60-5f0739d010e6', differentiated_responsibility_framework_1992).
narrative_ontology:cs_drift_state('3ab499a3-3b36-4ae0-ad60-5f0739d010e6', contemporary_2030, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3ab499a3-3b36-4ae0-ad60-5f0739d010e6', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(cbdr_principle__historical_responsibility_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, developing_nations).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, vulnerable_populations_sids_ldc).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nations).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, fossil_fuel_exporters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, global_north_scientists_ngos).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, climate_justice_advocacy_coalition).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, industrial_agriculture_oil_sectors_developed_north).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, common_but_differentiated_responsibility).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, polluter_pays_principle).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, intergenerational_justice_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically responsible for ~80% of cumulative CO2 emissions; under this reading face binding emissions reduction targets (typically 45-50% by 2030 from 2010 levels) plus obligatory loss/damage financing to developing nations. Their options: comply with negotiated targets, fund adaptation/mitigation in the global South, or withdraw from treaty (with diplomatic and trade consequences). Exit is theoretically available but high cost.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developed_nations, payer,
    institutional, generational, constrained, global).

% Receive binding commitments from developed nations on emissions cuts and receive loss/damage financing (~$100B annually pledged, actual delivery contested). Retain right to grow emissions toward per-capita equity. Can exit or renegotiate terms if financing is insufficient or enforcement lax; AOSIS, African Union, and G77+China provide coalition power.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developing_nations, beneficiary,
    organized, generational, mobile, global).

% Small Island Developing States and Least Developed Countries face existential climate impacts (sea-level rise, drought, cyclones) that developed-nation emissions created. Under this reading they are the moral center of the claim: the constraint is justified by their vulnerability and the developed world's causal responsibility. Their exit options are minimal — displacement, migration, or climate ruin.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, vulnerable_populations_sids_ldc, beneficiary,
    powerless, immediate, trapped, local).

% High-carbon-export economies (Saudi Arabia, Russia, Australia) are not formally named in the constraint but face indirect pressure: developed-nation emissions cuts reduce demand for fossil fuels, depressing prices and export revenues. Their formal role is excluded from the negotiation, though their economic interests shape developed-nation resistance.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, fossil_fuel_exporters, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, fossil_fuel_exporters, excluded).

% Collective voice demanding binding developed-nation targets and loss/damage financing. Sets the negotiating agenda by (a) framing the moral claim around historical responsibility and climate justice, (b) threatening non-compliance or withdrawal if financing is inadequate, (c) allying with vulnerable small states to concentrate voting power. AOSIS, LDC Group, and African Union are the institutional seats.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, least_developed_countries_coalition, agenda_setter,
    organized, generational, mobile, global).

% Research institutions and advocacy NGOs in developed nations that frame and promote the historical responsibility narrative. They benefit from the constraint via legitimacy, funding for climate research, and institutional positioning on the 'right side' of climate justice. Their exit is rhetorical defection or institutional defunding; threat level is low.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, global_north_scientists_ngos, beneficiary,
    organized, generational, mobile, global).

% Industrial sectors in developed nations that depend on high-carbon inputs (fossil fuels, synthetic fertilizers, long-distance logistics). Bear the compliance costs of reduced emissions targets: capital retooling, input cost increases, revenue pressure. Their exclusion from formal negotiation means they lobby through domestic governments; their constraint-specific exit is limited.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, industrial_agriculture_oil_sectors_developed_north, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, industrial_agriculture_oil_sectors_developed_north, excluded).

% Multilateral development banks, Green Climate Fund, UNFCCC secretariat, bilateral aid agencies. Administer and report on loss/damage financing. Their agency is contested: developing nations see them as accountable to the historical responsibility principle; developed nations and beneficiary-sector actors in the Global South see them as neutral technical bodies. They navigate both framings.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, climate_finance_administrators, agenda_setter,
    institutional, biographical, analytical, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, climate_finance_administrators, observer).

% Transnational networks of climate-justice advocates, frontline-community representatives, and global-South NGOs that frame the constraint around intergenerational justice and the rights of those bearing climate impacts. They benefit from the constraint's legitimacy but are systematically excluded from formal treaty negotiation (the seats are nation-states, not movements).
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, climate_justice_advocacy_coalition, beneficiary,
    moderate, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, climate_justice_advocacy_coalition, excluded).

% IPCC and independent climate science institutions that measure, assess, and validate climate impacts and responsibility allocations. They provide the empirical ground for the historical responsibility claim but maintain institutional neutrality; their role is epistemic validation, not advocacy.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, academic_observers_ipcc, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cbdr_principle__historical_responsibility_reading, developed_nations).
narrative_ontology:fixing_cost_class(cbdr_principle__historical_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global emissions-reduction system in which responsibility is allocated proportional to historical contribution to the problem (cumulative CO2 concentration from industrialization through present), enabling differentiated targets without free-riding: developed nations bear steeper cuts, financing obligations drive developed-nation investment in developing-nation mitigation/adaptation, and developing nations retain growth equity. Solves the allocation problem via responsibility principle rather than equal-percentage cuts.
% TRANSFER_FUNCTION: Transfers three things: (1) emissions reduction obligations from developed to global atmosphere (quantified in nationally determined contribution targets), (2) financial resources from developed nations to developing nations for adaptation/mitigation and loss/damage compensation (~$100B annually pledged, actual: $70-80B), (3) technology transfer in clean energy and climate-resilient agriculture. The constraint specifies BOTH the obligation structure (developed nations pay, developing nations receive) AND the justification (historical responsibility).
% ABSENT_VOICES: Fossil fuel exporters and carbon-intensive domestic industries in the Global North are structurally excluded from formal negotiation — they have no voting seat, no agenda-setting power. They appear only as pressure (via lobbying governments) to weaken developed-nation targets and reduce financing. Workers in stranded carbon sectors in the Global North are also absent — transition costs are borne by them but not priced into the constraint's design. Indigenous frontline communities in the Global South are excluded from formal seats despite being the highest-impact populations.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared overnight, the global climate governance system would revert to the voluntary-commitment reading (no binding targets, technology transfer primary, no loss/damage fund) or to no international climate structure at all. Developed-nation emissions would face no binding obligation, loss/damage financing would collapse, and developing nations would either build separate adaptation systems or face uncompensated climate impacts. The world's emissions trajectory, capital flows, and climate-impact distribution would shift materially.
% FOUNDING_PROBLEM: Historical emissions from industrialization (1870–present, concentrated in North America and Europe) have driven atmospheric CO2 concentration to unsafe levels (~420ppm), creating climate impacts disproportionately borne by developing nations and island states that contributed minimally to the problem. The constraint exists to allocate responsibility and remediation to those causally and financially capable of bearing them.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports (AR6) document historical responsibility: cumulative emissions through 2021 show developed nations responsible for ~80% of CO2 added to atmosphere since 1850, while developing nations face disproportionate climate impacts (IPCC, 2023). Small island developing states and Least Developed Countries formally attest the founding problem through COP statements and the Paris Agreement preamble. Developed-nation governments acknowledge historical emissions but contest the causal responsibility frame (they dispute whether historical emissions were 'foreseeable' or create binding obligations). Fossil fuel export interests and carbon-intensive sectors in the North do NOT attest the founding problem — their position is that current emissions matter, not past emissions.
narrative_ontology:disappearance_verdict(cbdr_principle__historical_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__historical_responsibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__historical_responsibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cbdr_principle__historical_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__historical_responsibility_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is 0.68 (high) because the constraint transfers substantial financial resources and emissions-cut sovereignty from developed to developing nations; developed-nation fiscal capacity is mobilized not by their current emissions level but by their historical contribution. This is not compensation for a service rendered in the past — it is reallocation of current and future capacity based on a historical responsibility principle. Suppression is 0.55 (moderate) because the constraint persists despite strong resistance: developed nations actively resist binding targets and financing (push toward voluntary framing, delay implementation, condition aid on performance metrics), and this resistance must be suppressed via coalition voting power, legal obligation under UNFCCC, and reputational pressure. Theater is 0.42 (moderate-high) because loss/damage financing is repeatedly pledged but under-delivered (~$70-80B actual vs. $100B pledged); many developed-nation climate actions are framed as compliance but are minimal relative to 1.5C trajectory; implementation theater masks enforcement gaps. Accessibility alternatives have partially collapsed (individual level 0.32 by 2030): developing nations cannot easily exit or opt for unilateral adaptation; developed nations cannot easily renegotiate without treaty breach. The measurement series show extractiveness rising 1992–2030 (+0.33): the constraint's scope deepened post-2009 (Copenhagen Accord), sharpened post-2015 (Paris Agreement), and is being operationalized post-2022 (Enhanced NDCs, loss/damage fund). Theater also rises steadily, indicating growing performance/enforcement divergence — the 2030 projections assume current trajectory (no breakthrough in financing or enforcement).
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (developed nations) and the beneficiary seat (developing-nation coalition) should compute to different constraint types and experience divergent effective extraction. From developed-nation governmental seats, the constraint reads as rope or light tangled-rope (genuine coordination justified by climate physics and emissions equity, with some extraction asymmetry but not predatory). From developing-nation coalition seats, it reads as tangled-rope with snare-flavored elements (genuine coordination [responsibility-based allocation] fused with asymmetric extraction [financing under-delivered, developed nations retain implementation control, conditioning power]). From vulnerable-population seats (powerless, trapped exit), it reads as snare-with-protective-rhetoric: the constraint's operation (financing delays, adaptation underfunding, exclusion from negotiation) contradicts its stated purpose (protecting vulnerable populations from climate impacts). The engine computes these divergences independently from the structural data — the authored metrics and beneficiary/victim declarations produce seat-specific types.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed nations carry high d (near 0.80) because they are binding-target payers with constrained exit and institutional power that is insufficient to escape the obligation once ratified. Their exit (treaty withdrawal) carries high diplomatic and trade costs, and domestic ratification makes reversal difficult. Developing nations carry low d (near 0.25–0.35) because they are beneficiaries with high coalition power and mobile exit (they can threaten withdrawal, renegotiation, non-compliance, and have voting power in UNFCCC). Vulnerable populations carry very low d (near 0.10–0.15) as beneficiaries but with identity-locked exit (geography-bound, climate-vulnerable by definition, cannot leave the region). This identity lock is structural, not psychological: a SIDS citizen cannot simply move to higher elevation; climate vulnerability is an identity fact, not a choice subject to re-evaluation. The directionality override for developed nations (+0.02 from structural derivation) reflects that their institutional power to reshape the constraint domestically is constrained by the treaty's binding character — ratification is intentionally difficult to reverse, which reduces their effective power relative to their abstract institutional power atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The historical-responsibility reading avoids classic mandatrophy because its founding problem (emissions already in the atmosphere creating current/future climate impacts) is LIVE and will remain so for decades. Vulnerable populations will face climate impacts through 2100+ regardless of mitigation; developed-nation historical responsibility is not erased by current low-carbon transitions. However, the constraint exhibits a weaker mandatrophy dynamic: the FOUNDING OBLIGATION (developed nations reduce emissions proportional to historical responsibility) is live, but the IMPLEMENTATION OBLIGATION (deliver loss/damage financing) is degrading into performance theater. Loss/damage financing was promised at $100B annually; actual delivery is $70-80B and contested (some flows are relabeled development aid, not climate-specific transfers). The theater_ratio rise (0.18 → 0.42) indicates this performance/function divergence. If loss/damage financing collapses entirely while developed-nation emissions targets remain binding, the constraint would bifurcate: binding-cut obligations persist (harder to escape politically and legally) while transfer obligations become piton-like (administered, reported, under-delivered, maintained mostly through rhetoric and annual COP performance theater). The constraint as specified avoids full mandatrophy because the COORDINATION function (allocate responsibility without free-riding) remains live and unsolved; if that function atrophies (e.g., if responsibility-based allocation is replaced by equal-percentage cuts or per-capita caps), then mandatrophy would set in (the constraint would persist as institutional theater while the real decision-making moved elsewhere).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_responsibility_vs_moral_responsibility,
    'Does historical emissions responsibility from 1870–1992 (pre-UNFCCC) create binding LEGAL obligations on current developed-nation governments, or only moral/political responsibility?',
    'International law scholarship and ICJ precedent on treaty interpretation (does a text written in 1992 retroactively bind parties to pre-1992 emissions?). The answer determines whether the constraint creates enforceable legal obligations or aspirational political commitments.',
    'If causal responsibility creates legal obligation, the constraint''s enforcement mechanism is strengthened (parties cannot simply withdraw); if only moral responsibility, enforcement depends on political will and coalition pressure, and the constraint degrades toward voluntary commitment or piton. This is the core ambiguity the reading does not resolve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_responsibility_vs_moral_responsibility, conceptual, 'Whether historical emissions entail binding legal obligations or only moral responsibility.').

omega_variable(
    developed_north_coalition_endurance,
    'Will developed nations maintain cohesion around binding emissions targets and loss/damage financing, or will their coalition fragment into national interest-maximizers?',
    'Observation of ratification patterns, domestic compliance, financing flows, and climate diplomacy through 2030. If developed nations begin unilaterally renegotiating (opting for weaker targets, conditioning aid, defecting to voluntary frameworks), the constraint''s persistence degrades.',
    'If cohesion breaks, the constraint bifurcates: binding targets remain on paper (high legal exit cost) but are renegotiated downward and stalled in implementation (performance theater rises, effective extraction falls). If cohesion holds, the constraint operates as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(developed_north_coalition_endurance, empirical, 'Whether developed-nation coalition stays unified on binding targets and financing.').

omega_variable(
    fossil_fuel_stranded_asset_feedback,
    'Will stranded-asset losses from reduced coal/oil demand create sufficient domestic political pressure in developed nations to weaken the constraint''s emissions targets, or will renewable-energy sector gains offset that pressure?',
    'Political-economy analysis of sectoral lobbying intensity, investment flows into renewable vs. carbon-intensive sectors, and electoral dynamics in developed nations through 2030. If carbon-sector political power rises, pressure to weaken targets increases.',
    'If fossil-fuel political power remains high, the constraint faces increasing pressure toward voluntary framing and lower targets (pushing toward the sibling voluntary reading). If renewable-sector power rises, the constraint is more likely to harden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fossil_fuel_stranded_asset_feedback, empirical, 'Whether stranded-asset feedback weakens developed-nation commitment to binding targets.').

omega_variable(
    reading_foreclosure_empirical_test,
    'If loss/damage financing collapses entirely (falls to near-zero by 2030) while developed-nation emissions targets remain binding, does the constraint foreclose its sibling voluntary reading, or do the two readings continue coexisting in different jurisdictions?',
    'Observation of financing delivery and treaty interpretation through 2030. If financing collapse produces explicit renegotiation or treaty amendment (rejecting loss/damage obligations), that is foreclosure evidence. If financing collapse produces only performance theater (targets on paper, financing flows continue to be reported as higher than reality), coexistence persists.',
    'If foreclosure occurs, the constraint hardened its structural boundary (the voluntary reading becomes internally contradictory). If coexistence persists, both readings remain live in different jurisdictions and forums.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_empirical_test, empirical, 'Whether financing collapse forecloses the voluntary-commitment reading or leaves it coexisting.').

omega_variable(
    suppression_internalization_ambiguity,
    'For developed-nation governmental actors, is suppression of resistance to binding targets primarily STRUCTURAL (economic incentives, legal constraints, coalition pressure) or INTERNALIZED (genuine belief in the historical responsibility principle, acceptance of obligation)?',
    'Private communications, leaked negotiation transcripts, post-retirement political statements by climate negotiators. If suppression is structural, targets reverse when incentives shift (e.g., fossil-fuel price spike, coalition collapse). If suppression is internalized, commitment persists across incentive changes.',
    'If suppression is structural, the constraint''s persistence is fragile (depends on maintaining coalition and legal architecture). If suppression is internalized, persistence is more robust (beliefs are stickier than incentives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Whether developed-nation acceptance of binding targets is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__historical_responsibility_reading, 1992, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_tr_t1992, cbdr_principle__historical_responsibility_reading, theater_ratio, 1992, 0.18).
narrative_ontology:measurement_basis(cbdr_tr_t1992, observed).
narrative_ontology:measurement(cbdr_tr_t2000, cbdr_principle__historical_responsibility_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement_basis(cbdr_tr_t2000, observed).
narrative_ontology:measurement(cbdr_tr_t2009, cbdr_principle__historical_responsibility_reading, theater_ratio, 2009, 0.32).
narrative_ontology:measurement_basis(cbdr_tr_t2009, observed).
narrative_ontology:measurement(cbdr_tr_t2015, cbdr_principle__historical_responsibility_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement_basis(cbdr_tr_t2015, observed).
narrative_ontology:measurement(cbdr_tr_t2022, cbdr_principle__historical_responsibility_reading, theater_ratio, 2022, 0.4).
narrative_ontology:measurement_basis(cbdr_tr_t2022, observed).
narrative_ontology:measurement(cbdr_tr_t2030, cbdr_principle__historical_responsibility_reading, theater_ratio, 2030, 0.42).
narrative_ontology:measurement_basis(cbdr_tr_t2030, projected).

% Extraction over time
narrative_ontology:measurement(cbdr_be_t1992, cbdr_principle__historical_responsibility_reading, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement_basis(cbdr_be_t1992, observed).
narrative_ontology:measurement(cbdr_be_t2000, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2000, 0.41).
narrative_ontology:measurement_basis(cbdr_be_t2000, observed).
narrative_ontology:measurement(cbdr_be_t2009, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2009, 0.52).
narrative_ontology:measurement_basis(cbdr_be_t2009, observed).
narrative_ontology:measurement(cbdr_be_t2015, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement_basis(cbdr_be_t2015, observed).
narrative_ontology:measurement(cbdr_be_t2022, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2022, 0.66).
narrative_ontology:measurement_basis(cbdr_be_t2022, observed).
narrative_ontology:measurement(cbdr_be_t2030, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2030, 0.68).
narrative_ontology:measurement_basis(cbdr_be_t2030, projected).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_su_t1992, cbdr_principle__historical_responsibility_reading, suppression_requirement, 1992, 0.28).
narrative_ontology:measurement_basis(cbdr_su_t1992, observed).
narrative_ontology:measurement(cbdr_su_t2000, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement_basis(cbdr_su_t2000, observed).
narrative_ontology:measurement(cbdr_su_t2009, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2009, 0.42).
narrative_ontology:measurement_basis(cbdr_su_t2009, observed).
narrative_ontology:measurement(cbdr_su_t2015, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement_basis(cbdr_su_t2015, observed).
narrative_ontology:measurement(cbdr_su_t2022, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2022, 0.53).
narrative_ontology:measurement_basis(cbdr_su_t2022, observed).
narrative_ontology:measurement(cbdr_su_t2030, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2030, 0.55).
narrative_ontology:measurement_basis(cbdr_su_t2030, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1992, tn=2030
narrative_ontology:measurement(cbdr_grid_01, cbdr_principle__historical_responsibility_reading, accessibility_collapse(class), 1992, 0.48).
narrative_ontology:measurement(cbdr_grid_02, cbdr_principle__historical_responsibility_reading, accessibility_collapse(class), 2030, 0.58).
narrative_ontology:measurement(cbdr_grid_03, cbdr_principle__historical_responsibility_reading, accessibility_collapse(individual), 1992, 0.15).
narrative_ontology:measurement(cbdr_grid_04, cbdr_principle__historical_responsibility_reading, accessibility_collapse(individual), 2030, 0.32).
narrative_ontology:measurement(cbdr_grid_05, cbdr_principle__historical_responsibility_reading, accessibility_collapse(organizational), 1992, 0.35).
narrative_ontology:measurement(cbdr_grid_06, cbdr_principle__historical_responsibility_reading, accessibility_collapse(organizational), 2030, 0.54).
narrative_ontology:measurement(cbdr_grid_07, cbdr_principle__historical_responsibility_reading, accessibility_collapse(structural), 1992, 0.52).
narrative_ontology:measurement(cbdr_grid_08, cbdr_principle__historical_responsibility_reading, accessibility_collapse(structural), 2030, 0.62).
narrative_ontology:measurement(cbdr_grid_09, cbdr_principle__historical_responsibility_reading, resistance(class), 1992, 0.65).
narrative_ontology:measurement(cbdr_grid_10, cbdr_principle__historical_responsibility_reading, resistance(class), 2030, 0.78).
narrative_ontology:measurement(cbdr_grid_11, cbdr_principle__historical_responsibility_reading, resistance(individual), 1992, 0.42).
narrative_ontology:measurement(cbdr_grid_12, cbdr_principle__historical_responsibility_reading, resistance(individual), 2030, 0.68).
narrative_ontology:measurement(cbdr_grid_13, cbdr_principle__historical_responsibility_reading, resistance(organizational), 1992, 0.58).
narrative_ontology:measurement(cbdr_grid_14, cbdr_principle__historical_responsibility_reading, resistance(organizational), 2030, 0.75).
narrative_ontology:measurement(cbdr_grid_15, cbdr_principle__historical_responsibility_reading, resistance(structural), 1992, 0.72).
narrative_ontology:measurement(cbdr_grid_16, cbdr_principle__historical_responsibility_reading, resistance(structural), 2030, 0.82).
narrative_ontology:measurement(cbdr_grid_17, cbdr_principle__historical_responsibility_reading, stakes_inflation(class), 1992, 0.45).
narrative_ontology:measurement(cbdr_grid_18, cbdr_principle__historical_responsibility_reading, stakes_inflation(class), 2030, 0.68).
narrative_ontology:measurement(cbdr_grid_19, cbdr_principle__historical_responsibility_reading, stakes_inflation(individual), 1992, 0.22).
narrative_ontology:measurement(cbdr_grid_20, cbdr_principle__historical_responsibility_reading, stakes_inflation(individual), 2030, 0.58).
narrative_ontology:measurement(cbdr_grid_21, cbdr_principle__historical_responsibility_reading, stakes_inflation(organizational), 1992, 0.38).
narrative_ontology:measurement(cbdr_grid_22, cbdr_principle__historical_responsibility_reading, stakes_inflation(organizational), 2030, 0.72).
narrative_ontology:measurement(cbdr_grid_23, cbdr_principle__historical_responsibility_reading, stakes_inflation(structural), 1992, 0.51).
narrative_ontology:measurement(cbdr_grid_24, cbdr_principle__historical_responsibility_reading, stakes_inflation(structural), 2030, 0.75).
narrative_ontology:measurement(cbdr_grid_25, cbdr_principle__historical_responsibility_reading, suppression(class), 1992, 0.32).
narrative_ontology:measurement(cbdr_grid_26, cbdr_principle__historical_responsibility_reading, suppression(class), 2030, 0.64).
narrative_ontology:measurement(cbdr_grid_27, cbdr_principle__historical_responsibility_reading, suppression(individual), 1992, 0.18).
narrative_ontology:measurement(cbdr_grid_28, cbdr_principle__historical_responsibility_reading, suppression(individual), 2030, 0.42).
narrative_ontology:measurement(cbdr_grid_29, cbdr_principle__historical_responsibility_reading, suppression(organizational), 1992, 0.28).
narrative_ontology:measurement(cbdr_grid_30, cbdr_principle__historical_responsibility_reading, suppression(organizational), 2030, 0.58).
narrative_ontology:measurement(cbdr_grid_31, cbdr_principle__historical_responsibility_reading, suppression(structural), 1992, 0.35).
narrative_ontology:measurement(cbdr_grid_32, cbdr_principle__historical_responsibility_reading, suppression(structural), 2030, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__historical_responsibility_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(cbdr_principle__historical_responsibility_reading, 0.18).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, cbdr_principle__voluntary_commitment_reading).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, paris_agreement_enforcement_mechanisms).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, loss_damage_financing_fund).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, climate_reparations_discourse).

% DUAL FORMULATION NOTE:
% The cbdr_principle kernel instantiates two structurally distinct constraints: historical_responsibility_reading (this story) treats developed nations as binding payers and developing nations as binding beneficiaries; voluntary_commitment_reading treats both parties as voluntary actors with technology transfer primary. The ε values differ substantially (this reading: 0.68; sibling: estimated 0.35–0.45 on similar observables). They share the same formal text (UNFCCC, Paris Agreement) but instantiate different reading/authority structures (lineage grounding with interpretation-layer bifurcation: developed-nation interpreters favor voluntary framing; developing-nation interpreters favor historical-responsibility framing). Network edges represent causal influence: this reading's enforcement failures (theater_ratio rise, financing under-delivery) create political pressure that INFLUENCES the voluntary_commitment_reading (makes it more attractive to developed nations); strengthened loss/damage financing would influence the piton status (reduces theater_ratio, hardens the constraint).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cbdr_principle__historical_responsibility_reading, institutional, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
