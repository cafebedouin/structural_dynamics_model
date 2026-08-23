% ============================================================================
% CONSTRAINT STORY: market_naturalization__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__hybrid_reading, []).

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
 *   constraint_id: market_naturalization__hybrid_reading
 *   human_readable: Market Dominance Persistence: Composite Lapse-and-Maintenance Arrangement (Hybrid Reading)
 *   domain: political economy / economic history / institutional analysis
 *
 * SUMMARY:
 *   This story instantiates the HYBRID READING of the market-naturalization
 *   kernel: the persistence of market dominance combines genuinely lapsed
 *   elements (structures that now run on inertia — installed bases, consumer
 *   habit, brand salience, sunk learning) with actively maintained elements
 *   (exclusive dealing, patent thickets, killer acquisitions, doctrine-shaped
 *   litigation, narrative defense). The ε referent is the standing
 *   arrangement under contest — the existing dominance-persistence structure
 *   as the hybrid reading assesses it — never the fully-contested or
 *   fully-lapsed counterfactual the sibling readings endorse. Interval
 *   mapping: time 0 = 1980 (ascendance of the consumer-welfare standard),
 *   time 44 = 2024; all metric series share this single grid. KEY AGENTS (by
 *   structural relationship): - incumbent_capital_holders: Agenda-setter and
 *   primary beneficiary (institutional/arbitrage) — funds and directs the
 *   maintenance machinery, collects the rents - dominant_firm_executives:
 *   Beneficiary (powerful/identity_locked) — operates the firms,
 *   professionally fused with the meritocracy narrative - potential_entrants:
 *   Target (moderate/trapped) — bears exclusion at the point of attempted
 *   entry - dependent_small_suppliers: Target (powerless/constrained) — bears
 *   forced terms annually - mass_end_consumers: Dual-positioned
 *   (moderate/constrained) — pays embedded margins, receives scale benefits -
 *   antitrust_enforcement_agencies: Analytical observer
 *   (institutional/analytical) — polices the arrangement's excesses under the
 *   very doctrine the arrangement shaped - would_be_competing_founders:
 *   Excluded seat (powerless/trapped) — deterred before constituting any
 *   record
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.66).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.64).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Dominance Persistence: Composite Lapse-and-Maintenance Arrangement (Hybrid Reading)").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "political economy / economic history / institutional analysis").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, '29e17b24-04b2-496a-8681-ef6c9f88b20c').
narrative_ontology:cs_kernel_codification('29e17b24-04b2-496a-8681-ef6c9f88b20c', formalized).
narrative_ontology:cs_authority_grounding('29e17b24-04b2-496a-8681-ef6c9f88b20c', expertise).
narrative_ontology:cs_interpretation_layer_present('29e17b24-04b2-496a-8681-ef6c9f88b20c').
narrative_ontology:cs_reading_relation('29e17b24-04b2-496a-8681-ef6c9f88b20c', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('29e17b24-04b2-496a-8681-ef6c9f88b20c', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('29e17b24-04b2-496a-8681-ef6c9f88b20c', foundational, dominance_persistence_is_composite).
narrative_ontology:cs_axiom_status(dominance_persistence_is_composite, holdable).
narrative_ontology:cs_axiom_grounding('29e17b24-04b2-496a-8681-ef6c9f88b20c', dominance_persistence_is_composite, empirically_contingent).
narrative_ontology:cs_axiom('29e17b24-04b2-496a-8681-ef6c9f88b20c', secondary, maintenance_intensity_varies_by_domain).
narrative_ontology:cs_axiom_status(maintenance_intensity_varies_by_domain, holdable).
narrative_ontology:cs_axiom_grounding('29e17b24-04b2-496a-8681-ef6c9f88b20c', maintenance_intensity_varies_by_domain, empirically_contingent).
narrative_ontology:cs_reference_frame('29e17b24-04b2-496a-8681-ef6c9f88b20c', partial_lapse_partial_defense_baseline).
narrative_ontology:cs_drift_state('29e17b24-04b2-496a-8681-ef6c9f88b20c', contemporary_platform_polarization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('29e17b24-04b2-496a-8681-ef6c9f88b20c', '').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, dominant_firm_executives).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, potential_entrants).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, dependent_small_suppliers).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, mass_end_consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, mass_end_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and control the dominant firms. Fund trade associations, university chairs, think tanks, and litigation that shape competition doctrine; board decisions allocate budget between operational defense (exclusive dealing, patent thickets, acquisitions of nascent rivals) and narrative defense (efficiency studies, policy op-eds). Capital is redeployable across sectors and jurisdictions, so their commitment to any single market's structure is portfolio-level rather than existential.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, incumbent_capital_holders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, incumbent_capital_holders, beneficiary).

% Run the dominant firms day to day; compensation rides on equity and on sustaining the growth story. Their professional self-conception is bound up with the claim that the firm leads because it is better; moving to a smaller rival would mean trading the industry's center for its periphery. They commission, repeat, and personally believe the efficiency narratives that circulate in policy debate.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, dominant_firm_executives, beneficiary,
    powerful, biographical, identity_locked, global).

% Firms attempting to enter markets where an incumbent holds the customer base, the supply relationships, and the standards. Entry means accepting terms the incumbent sets: platform fees, discretionary interoperability access, or an acquisition offer that arrives with an implied alternative. Realistic paths are niche subsistence below the incumbent's attention threshold or sale.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, potential_entrants, payer,
    moderate, biographical, trapped, national).

% Sell inputs or distribution services to the dominant buyer. Terms renew annually under threat of delisting; switching means rebuilding volume in thin alternative channels. Many depend on the dominant firm for the majority of revenue, which makes collective refusal to accept terms possible in principle but ruinous for any single defector.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, dependent_small_suppliers, payer,
    powerless, immediate, constrained, regional).

% Buy from the dominant firms because they are convenient, cheap at the point of sale, and familiar. They bear embedded margins invisibly and simultaneously receive real scale benefits: low headline prices, wide selection, interoperable defaults. Individual switching costs are small but rarely exercised, so the aggregate choice set narrows year over year.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, mass_end_consumers, payer,
    moderate, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, mass_end_consumers, beneficiary).

% Investigate mergers and conduct of dominant firms and bring cases under prevailing doctrine, losing more often than they win under the current standard. Leadership rotates among government, academia, and the regulated firms' own bar, which shapes which theories of harm staff find actionable and which cases survive review.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, antitrust_enforcement_agencies, observer,
    institutional, generational, analytical, national).

% People with viable ideas who survey the concentrated landscape and choose employment or unrelated ventures instead of founding. They never enter the record: no complaint is filed, no lobby is formed, no data trail exists, because the deterrent operated before they became market participants at all.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, would_be_competing_founders, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__hybrid_reading, incumbent_capital_holders).
narrative_ontology:fixing_cost_class(market_naturalization__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrated market structures coordinate at scale: interoperability standards, reliable bulk supply, quality assurance, and payment/discovery infrastructure are solved once inside the incumbent rather than negotiated anew among many rivals. This coordination is real and partially passed through to consumers as lower headline prices and wider selection.
% TRANSFER_FUNCTION: Moves margin above competitive cost from suppliers (renewed under duress), consumers (embedded in prices), and foregone entrants (surplus never created) to incumbent shareholders and executives; separately, moves doctrinal authority to economic experts who certify concentrated outcomes as efficient.
% ABSENT_VOICES: Would-be founders deterred before entry never organize to object; the workers and communities of defunct regional competitors are dispersed and voiceless; consumers appear nowhere as a seated party, only aggregated through agencies. They are absent because the closure completed before they could constitute themselves as constituencies — the deterrent precedes the grievance.
% DISAPPEARANCE_RATIONALE: If the maintenance machinery and the naturalization narrative vanished overnight, incumbent margins would compress as entry responded, supplier terms would renegotiate within contract cycles, and the doctrine certifying dominance as efficient would lose its enforcement arm. Markets would reorganize around contestable positions — though the lapsed residue (installed bases, habit, brand salience, sunk consumer learning) would decay slowly rather than disappear, which is precisely the hybrid signature this reading asserts.
% FOUNDING_PROBLEM: After the trust-busting era, large firms and their allies faced a legitimacy problem: how to reconcile visible economic concentration with a political economy committed to competition. The naturalization apparatus — the consumer-welfare standard, the market-selection narrative, business-school pedagogy of competitive fitness — was built to solve that legitimacy problem by recasting dominance as the output of a neutral competitive process rather than a maintained position.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians of antitrust, documenting the deliberate mid-century migration to the consumer-welfare standard, and current enforcement leadership pursuing the neo-Brandeisian correction attest, from outside the benefiting parties, that the doctrine was consciously constructed and that its legitimating function persists. Recurring legislative hearings on platform concentration provide further external attestation. Incumbent testimony exists but is self-interested and is not relied on here.
narrative_ontology:disappearance_verdict(market_naturalization__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__hybrid_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66: rents are real and have accumulated since 1980, but they are bounded by residual niche competition and by genuine scale benefits passed through to consumers — moderate-to-high, matching the reading's 'varies by domain' delta. Suppression is 0.64: active exclusion machinery operates in digital and platform domains while legacy-sector suppression has largely lapsed into self-executing habit. Theater ratio 0.44 and rising: an increasing share of maintenance activity defends elements that inertia already secures — annual competition statements, compliance rituals, efficiency studies re-proving what no longer needs proving — which is the measurable fingerprint of the lapsed component. Accessibility_collapse 0.48: alternatives are half-collapsed; niche entry remains viable while core-position entry is blocked. Resistance 0.55: the neo-Brandeisian revival, the EU Digital Markets Act, worker organizing at dominant firms, and open-source substitutes constitute real, ongoing resistance. The claimed type (tangled_rope) and the metrics were authored independently: I believe the structure genuinely coordinates (scale economies, standards, reliability) AND asymmetrically extracts AND requires active enforcement for its maintained portion — the metrics report what I believe descriptively true, and the engine computes per-seat types from the structural data. On the suppression series: the U-shaped trajectory tracks enforcement-capacity change, not extraction drift — heavy direct political suppression at t0, a dip as doctrinal internalization made suppression cheaply self-executing (the naturalization narrative itself did the enforcing), then renewed machinery build-out as contractual and platform-based exclusion tools deployed. Suppression is authored as a raw structural property throughout; only extractiveness is scaled downstream by directionality and scope. Coalition note: dependent_small_suppliers are individually powerless but a coordinated refusal to accept terms is their latent lever; the arrangement's annual-renewal design fragments exactly that coordination.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From incumbent_capital_holders the arrangement is a defended asset and a policy achievement; from dominant_firm_executives it is earned merit — their identity_locked exit means the meritocracy narrative is not cover they cynically deploy but a belief their careers are constituted by. From potential_entrants and dependent_small_suppliers the same structure is enforced exclusion with no exit. Mass_end_consumers sit nearest symmetric: genuine convenience subsidy, invisible rent. The excluded founders' seat is distinctive — their objection was never voiced because the deterrent preceded participation, so the apparent consensus around 'markets chose the winners' is partly an artifact of who was never in the room. Identity-lock dynamics: if the executive meritocracy frame broke (e.g., a wave of admitted maintenance disclosures), maintenance effort would drop sharply, the lapsed share would grow, and the structure would tilt toward the lapsed-alternative sibling's picture — the hybrid classification is contingent on the identity frame holding.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. incumbent_capital_holders derive near the beneficiary pole (declared beneficiary, arbitrage-grade exit, generational horizon — they can leave any single market's structure behind). dominant_firm_executives derive low d as beneficiaries, with identity_lock deepening their commitment to maintenance rather than raising their extracted burden. potential_entrants and dependent_small_suppliers derive near the full-target pole (declared victims, trapped/constrained exit). mass_end_consumers carry secondary_role beneficiary alongside payer, which pulls their derived d toward symmetric rather than full-target — the intended result, since they genuinely receive scale benefits. No directionality_overrides are authored: the declarations plus exit options already produce the correct qualitative ordering, and the schema's override mechanism keys on power atoms, which would collide (consumers and entrants share the moderate atom but need different directions). Scope amplification applies modestly: the arrangement operates globally, so verification difficulty scales effective extraction upward somewhat for the trapped seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline matters unusually here because the two sibling errors are both live temptations. Reading the arrangement as pure lapse (the lapsed_alternative sibling) would mislabel it piton and erase the documented enforcement machinery — lobbying budgets, exclusive-contract litigation, acquisition pipelines — that demonstrably operates. Reading it as pure maintenance (the beneficiary_maintained sibling) would mislabel it snare and erase the genuinely self-running residue: no one actively enforces consumer habit or installed-base lock-in, yet those do real persistence work. Tangled_rope preserves both halves: a real coordination function (scale, standards, reliability — vindicated in consumer prices in some domains), asymmetric extraction through the same structure, and enforcement that is partial rather than total. The founding problem (legitimating concentration) is still live — the naturalization mandate has not outlived its function, so mandatrophy is unresolved and the dead-mandate mismatch flag correctly does not fire: status=live with verdict=world_rearranges is the coherent pairing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lapse_maintenance_proportion,
    'What fraction of observed dominance persistence is genuinely lapsed (inertial) versus actively maintained?',
    'Domain-level process tracing: compare sectors where enforcement machinery lapsed and dominance nonetheless eroded against sectors where dominance persisted despite equivalent lapse pressure; audit maintenance budgets (lobbying, litigation, acquisition spend) against measured persistence.',
    'A predominantly lapsed structure tilts classification toward the lapsed-alternative sibling''s picture (piton-flavored); a predominantly maintained structure tilts toward the beneficiary-maintained sibling''s (snare-flavored). The tangled_rope claim stands only in the middle band.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapse_maintenance_proportion, empirical, 'The composite proportion is the hybrid reading''s load-bearing empirical claim and is currently unmeasured.').

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the market_naturalization kernel; would instantiating the lapsed_alternative or beneficiary_maintained reading instead change the structural classification?',
    'Compare the sibling files'' epsilon values, beneficiary/victim sets, and enforcement declarations once authored; locate the disagreement precisely — the siblings differ on the maintenance share, not on the existence of dominance persistence.',
    'The lapsed_alternative reading drops requires_active_enforcement and shrinks the victim set to diffuse consumer surplus loss; the beneficiary_maintained reading raises epsilon toward the snare band and names incumbent capital as sole capturer. Classification, gain_flow, and fixing_cost all move with the reading chosen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer-frame omega: the kernel supports three structurally distinct instantiations; this file authors exactly one.').

omega_variable(
    domain_extractiveness_variance,
    'Extractiveness varies sharply by domain (digital platforms versus grocery retail versus pharmaceuticals) — does a single scalar epsilon misrepresent the composite?',
    'Decompose into per-domain constraint stories sharing this kernel reading, linked by network edges; compare their independently authored epsilon values against this aggregate.',
    'The aggregate understates platform-domain extraction and overstates legacy-sector extraction; per-domain stories would likely split into distinct classifications, with the platform branch testing the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_extractiveness_variance, empirical, 'Aggregation risk: the ''varies by domain'' clause in the reading''s expected delta is asserted, not yet modeled.').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel best framed as formalized-in-doctrine (the consumer-welfare standard as the operative kernel, adjudicated by economic expertise) or as distributed-across-discourse (an ambiguous claim with no single adjudicating authority)?',
    'Trace whether doctrinal texts actually govern enforcement behavior or merely decorate practice that follows incumbent interests regardless of doctrine.',
    'Under the formalized-plus-expertise framing adopted here, an interpretable authority structure exists and drift computations route through it; under the distributed framing, no designated interpreter exists, interpretation_layer_present becomes invalid, and foreclosure/drift analysis changes shape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Two coherent framings of the same kernel yield different commitment-system classifications; signals guiding the choice here were the CWS''s codification in case law and the operational role of agency economics shops.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mnat_hybrid_tr_t0, market_naturalization__hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(mnat_hybrid_tr_t8, market_naturalization__hybrid_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(mnat_hybrid_tr_t16, market_naturalization__hybrid_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(mnat_hybrid_tr_t24, market_naturalization__hybrid_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(mnat_hybrid_tr_t32, market_naturalization__hybrid_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(mnat_hybrid_tr_t40, market_naturalization__hybrid_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(mnat_hybrid_tr_t44, market_naturalization__hybrid_reading, theater_ratio, 44, 0.44).

% Extraction over time
narrative_ontology:measurement(mnat_hybrid_be_t0, market_naturalization__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mnat_hybrid_be_t8, market_naturalization__hybrid_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(mnat_hybrid_be_t16, market_naturalization__hybrid_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(mnat_hybrid_be_t24, market_naturalization__hybrid_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(mnat_hybrid_be_t32, market_naturalization__hybrid_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(mnat_hybrid_be_t40, market_naturalization__hybrid_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(mnat_hybrid_be_t44, market_naturalization__hybrid_reading, base_extractiveness, 44, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(mnat_hybrid_su_t0, market_naturalization__hybrid_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(mnat_hybrid_su_t8, market_naturalization__hybrid_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(mnat_hybrid_su_t16, market_naturalization__hybrid_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(mnat_hybrid_su_t24, market_naturalization__hybrid_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(mnat_hybrid_su_t32, market_naturalization__hybrid_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(mnat_hybrid_su_t40, market_naturalization__hybrid_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement(mnat_hybrid_su_t44, market_naturalization__hybrid_reading, suppression_requirement, 44, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'market dominance is natural' decomposes, per the epsilon-invariance principle, into three structurally distinct claims — pure lapse (lapsed_alternative_reading), pure maintenance (beneficiary_maintained_reading), and the composite (this file). Each is authored as a separate story with its own epsilon, beneficiary/victim structure, and claimed type; all three link one another via affects_constraints. Epsilon differs across the family because the referent assessment differs: the lapsed reading finds negligible active extraction, the maintenance reading finds high extraction under continuous defense, and this hybrid reading finds moderate, accumulating extraction with a growing inertial residue. Upstream/downstream structure: documented maintenance evidence (lobbying records, acquisition histories) is the evidentiary input the maintenance reading cites against the lapsed reading; this hybrid file mediates the dispute by asserting the composite. The sibling files carry reciprocal links and their own notes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
