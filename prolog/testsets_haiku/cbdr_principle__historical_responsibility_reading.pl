% ============================================================================
% CONSTRAINT STORY: cbdr_principle__historical_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cbdr_principle__historical_responsibility_reading
 *   human_readable: CBDR Historical Responsibility Reading: Binding Emissions Reductions and Loss/Damage Finance from Developed Nations
 *   domain: international/environmental/development
 *
 * SUMMARY:
 *   This constraint instantiates the 'historical responsibility' reading of
 *   the CBDR principle embedded in the UNFCCC (1992) and operationalized
 *   through the Paris Agreement (2015) and Loss and Damage mechanisms. Under
 *   this reading, developed nations (high cumulative historical emissions)
 *   enter the victim/payer set for binding emissions reductions indexed to
 *   their responsibility and mandatory loss-and-damage financing. Developing
 *   nations (low historical emissions but high vulnerability) exit the
 *   adaptation-financing victim set and enter the beneficiary set for
 *   financial transfers and emissions allowances. This reading is contested
 *   against the 'voluntary commitment' reading, which frames CBDR as
 *   requiring voluntary NDCs with technology transfer as the developed
 *   nation's primary obligation, rather than binding, historically-indexed
 *   targets. The constraint's persistence depends on active enforcement
 *   through UN processes, technical working groups, and climate justice
 *   advocacy networks pushing developed nations to honor binding commitments
 *   despite domestic political resistance. The measuring period (0–35 years)
 *   spans the UNFCCC era through near-present.
 *
 * KEY AGENTS:
 *   - Developed nations with high cumulative emissions (USA, EU, Japan, Australia): payers, institutional power, constrained exit — face binding targets and mandatory finance
 *   - Vulnerable developing nations (AOSIS, LDC coalition, SIDS): beneficiaries, organized power, constrained exit — receive transfers and lower-responsibility targets but trapped in dependence on developed-nation compliance
 *   - Least developed countries: beneficiaries but structurally powerless, trapped — positioned as rightful recipients but cannot enforce obligation
 *   - UNFCCC secretariat and climate conferences: agenda-setter, institutional power — administers the principle operationally, translates to technical rules and fund architecture
 *   - Climate justice advocates and NGOs: beneficiary advocates, organized, mobile — defend the binding reading against watering-down
 *   - Fossil fuel industry: excluded, powerful — maintains counter-narratives but structurally absent from binding negotiation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, 0.68).
domain_priors:suppression_score(cbdr_principle__historical_responsibility_reading, 0.52).
domain_priors:theater_ratio(cbdr_principle__historical_responsibility_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__historical_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__historical_responsibility_reading, "CBDR Historical Responsibility Reading: Binding Emissions Reductions and Loss/Damage Finance from Developed Nations").
narrative_ontology:topic_domain(cbdr_principle__historical_responsibility_reading, "international/environmental/development").

domain_priors:requires_active_enforcement(cbdr_principle__historical_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__historical_responsibility_reading, 'b5af3227-c561-4ffc-a1e9-faa85fbccd69').
narrative_ontology:cs_kernel_codification('b5af3227-c561-4ffc-a1e9-faa85fbccd69', fixed_text).
narrative_ontology:cs_authority_grounding('b5af3227-c561-4ffc-a1e9-faa85fbccd69', extraction).
narrative_ontology:cs_interpretation_layer_present('b5af3227-c561-4ffc-a1e9-faa85fbccd69').
narrative_ontology:cs_reading_relation('b5af3227-c561-4ffc-a1e9-faa85fbccd69', cbdr_principle__voluntary_commitment_reading, coexists_with).
narrative_ontology:cs_axiom('b5af3227-c561-4ffc-a1e9-faa85fbccd69', foundational, historical_cumulative_responsibility_principle).
narrative_ontology:cs_axiom_status(historical_cumulative_responsibility_principle, holdable).
narrative_ontology:cs_axiom_grounding('b5af3227-c561-4ffc-a1e9-faa85fbccd69', historical_cumulative_responsibility_principle, deontological).
narrative_ontology:cs_axiom('b5af3227-c561-4ffc-a1e9-faa85fbccd69', foundational, binding_indexed_obligation).
narrative_ontology:cs_axiom_status(binding_indexed_obligation, holdable).
narrative_ontology:cs_axiom_grounding('b5af3227-c561-4ffc-a1e9-faa85fbccd69', binding_indexed_obligation, conventional).
narrative_ontology:cs_reference_frame('b5af3227-c561-4ffc-a1e9-faa85fbccd69', unfccc_1992_cbdr_founding).
narrative_ontology:cs_drift_state('b5af3227-c561-4ffc-a1e9-faa85fbccd69', contemporary_2024, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b5af3227-c561-4ffc-a1e9-faa85fbccd69', '').
narrative_ontology:cs_kernel_id(cbdr_principle__historical_responsibility_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, vulnerable_developing_nations).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, least_developed_countries).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, small_island_developing_states).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nations_with_high_cumulative_emissions).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, industrial_economies).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__historical_responsibility_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(cbdr_principle__historical_responsibility_reading, 'none', 1).

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
 *   Extractiveness is high (0.68) because the constraint transfers substantial financial resources and imposes binding emissions cuts on developed nations beyond what they chose. Suppression is moderate (0.52) because the constraint is actively defended through soft enforcement (naming and shaming, scientific attestation, coalition pressure) rather than hard coercion, and developed nations can resist through treaty withdrawal, cap-and-trade gaming, or financing alternative readings. Theater ratio rises slightly (0.28→0.41) over the interval as the gap widens between binding rhetoric and actual implemented transfers — many developed nations publicly commit to loss-and-damage finance but deliver minimal amounts. The plateau at t=25 onward reflects stabilization: the principle is institutionalized but under-enforced, with theater replacing active fulfillment. Accessibility collapse is moderate (0.58) because developed nations retain exit options (treaty withdrawal, bilateral deals, alternative framings), though the cost is high reputationally and economically. Resistance is high (0.71) because developed nations actively contest the principle through subsidiary negotiations, cost-sharing proposals, and private-sector alternatives. All measurements are on one shared time grid so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The developed-nation seat and the vulnerable-nation seat compute entirely differently. From a developed-nation payer perspective, the constraint is overreach: it imposes backward-looking responsibility for emissions they can't undo, violates state sovereignty, and extracts wealth without consent. From the vulnerable-nation beneficiary perspective, it is inadequate: the promised transfers don't materialize at scale, the binding language is rhetorical, and they remain trapped in climate vulnerability despite the principle's acknowledgment. The agenda-setter (UNFCCC secretariat) sits between: administering the principle operationally, constrained by the need to keep all parties at the table, so translating binding language into soft enforcement. The engine computes these seat-specific types from the structural data (power, exit, beneficiary/victim declarations) without reconciling the divergent experiences — that divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed nations are structural targets of this reading (high d, near 1.0): they are named as responsible, obligated to reduce and finance, and trapped in institutional and reputational constraints. Their exit is constrained by economic interdependence and climate physics (unilateral withdrawal doesn't reduce their atmospheric responsibility). Vulnerable developing nations are beneficiaries (low d, near 0.0): they receive transfers, lower targets, and legitimacy for claiming more. However, their exit is also constrained by dependence on transfers and reliance on the principle's legitimacy framework for their claims — a beneficiary trapped is still trapped. The least developed countries face a special directionality: positioned as zero-responsibility beneficiaries (d≈0.0 nominally) but structurally powerless to enforce their claimed rights, they are functionally co-victims with developed nations in the constraint's under-enforcement (they don't receive promised transfers). The principle's directionality asymmetry — developed nations bear costs they contest, developing nations collect transfers that don't materialize — is the source of the tangled-rope classification: genuine coordination function (global emissions reduction requires burden-sharing) plus asymmetric extraction (developed nations fund adaptation while remaining constrained; vulnerable nations receive legitimacy but not resources).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint faces incipient mandatrophy: the founding problem (climate impacts from developed-nation historical emissions, insufficient adaptation capital in vulnerable nations) is live and growing more severe. Yet the mechanism's actual output (development of loss-and-damage funds) is decoupled from the mechanism's stated obligation (binding, indexed transfers). The measured theater ratio rise (0.28→0.41) indicates growing performativity: countries ratify the principle, announce climate finance pledges, but deliver through voluntary channels rather than binding transfers. The extractiveness plateau (0.68 from t=20 onward) reflects a constraint that collects rents (developed nations finance something) but no longer drives its founding function (global emissions reduction proportional to historical responsibility). The principle persists because the coordination need is real and the beneficiary narratives are institutionalized, but the enforcement gap (developed nations can game compliance through credits, offsets, and subsidiary agreements) reveals a zombie constraint: the founding problem lives but the mechanism's mechanism is rotting. This is not quite a piton (the principle still extracts and still coordinates somewhat) but shows early mandatrophy symptoms. The constraint classification remains tangled_rope (the coordination function is genuine, the extraction is real, the enforcement is active even if soft) — but the rising theater and the plateau in extractiveness suggest a trajectory toward piton-hood if enforcement continues to decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_accounting_boundary,
    'What emissions count as ''historical responsibility''? Pre-industrial baseline (1800), industrialization onset (1850), atmosphere concentration onset (1950), or treaty baseline (1992)? How are colonial-era and outsourced emissions attributed?',
    'Scientific consensus on cumulative radiative forcing attribution plus negotiated agreement on accounting cutoff. Climate databases (EDGAR, Global Carbon Project) document different baselines; negotiation records show developed nations arguing for post-1990 cutoff while vulnerable nations argue for 1800 or full cumulative.',
    'Different baselines shift the developed-nation responsibility set (USA and EU include full 200 years; China and India responsibility appears only post-1980). If outsourced emissions are attributed to consumer nations rather than production locations, responsibility concentration shifts northward, deepening developed-nation obligation; if attributed to production, responsibility dilutes across middle-income nations, weakening the binding principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_accounting_boundary, empirical, 'The historical baseline and attribution methodology determine who counts as ''developed'' and thus liable.').

omega_variable(
    binding_vs_soft_enforcement_ambiguity,
    'Does ''binding'' mean legally enforceable through international courts, or politically binding through reputational pressure and conference decisions?',
    'Dispute resolution mechanisms in Paris Agreement Article 15; if cases are brought and adjudicated with material consequences (penalties, sanctions, compensation), binding is enforced legally; if cases are filed but settlements remain diplomatic, binding is soft. Post-2020 DS records will clarify.',
    'Hard enforcement (legal teeth) would significantly increase suppression and extractiveness — developed nations face material penalty for non-compliance, driving actual emissions reductions. Soft enforcement (current state) sustains theater: the principle is binding rhetorically but weak in practice. Reclassification to Piton occurs if enforcement remains soft and transfers continue to decline relative to pledges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binding_vs_soft_enforcement_ambiguity, empirical, 'Whether ''binding commitment'' has legal force or is a normative aspiration.').

omega_variable(
    developing_nation_heterogeneity_in_responsibility,
    'Should responsibility tier with current emissions (making China and India higher-responsibility developing nations) or remain indexed to historical cumulative (exempting emerging economies)?',
    'Renegotiation of tiering in future COPs or through subsidiary agreements. Current NDCs show China/India applying lower targets than developed nations despite higher current emissions; contestation is live in 2024–2025 COP negotiations.',
    'If responsibility tiers shift to current emissions, emerging economies enter the victim/payer set, deepening the principle''s bite on total global emissions but narrowing the developed-nation concentration. This would strengthen the principle''s ecological function (more aggregate reduction) but weaken its equity function (responsibility no longer correlates with development inequality). The beneficiary set (LDCs, AOSIS) would shrink, and the constraint would shift from tangled_rope toward pure coordination (more symmetric burden-sharing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_nation_heterogeneity_in_responsibility, conceptual, 'The scope of ''developed nation'' responsibility in a world of rising emerging economies.').

omega_variable(
    loss_and_damage_finance_materialization,
    'Will loss-and-damage funds materialize as promised ($100 billion+ annual), as pledged but under-delivered (~$10 billion actual), or remain rhetorical (near-zero)?',
    'Annual UNFCCC finance tracking and GCF/LDCF disbursement records. If pledges remain unmet, the gap between binding rhetoric and actual transfer indicates the constraint is increasingly performative (theater_ratio rises further).',
    'If finance materializes, extractiveness from developed nations remains high but the coordination function is genuinely served (vulnerable nations can adapt). If finance stalls, extractiveness remains nominal (developed nations avoid real sacrifice) and the constraint approaches Piton status (theatrical binding without material effect). The theater_ratio plateau at 0.41 suggests this is the current trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(loss_and_damage_finance_materialization, empirical, 'Whether the principle''s transfer mechanism actually operates or remains aspirational.').

omega_variable(
    reading_foreclosure_via_alternative_framings,
    'Will the voluntary-commitment reading (sibling) eventually foreclose this historical-responsibility reading through institutional capture of COP presidencies and UNFCCC priorities?',
    'Sequence of COP decisions and UNFCCC operational directives. If successive COPs downgrade loss-and-damage in favor of NDC-voluntary language, or if the secretariat''s technical guidance shifts from binding-indexed language to capacity-based language, foreclosure is underway. If vulnerable-nation coalitions maintain majority voice in main negotiations, foreclosure does not occur.',
    'Foreclosure of historical-responsibility reading means the principle becomes voluntary, developed-nation obligations are no-longer-binding, and the constraint''s classification flips to Rope (pure coordination without asymmetric extraction). Vulnerable nations lose their legitimacy framework for claiming transfers. This is the likely long-term trajectory if soft enforcement continues and developed-nation cost-shifting accelerates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_via_alternative_framings, conceptual, 'Whether this reading survives institutional pressure toward voluntary framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__historical_responsibility_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_tr_t0, cbdr_principle__historical_responsibility_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cbdr_tr_t5, cbdr_principle__historical_responsibility_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(cbdr_tr_t10, cbdr_principle__historical_responsibility_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(cbdr_tr_t15, cbdr_principle__historical_responsibility_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(cbdr_tr_t20, cbdr_principle__historical_responsibility_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(cbdr_tr_t25, cbdr_principle__historical_responsibility_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(cbdr_tr_t30, cbdr_principle__historical_responsibility_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(cbdr_tr_t35, cbdr_principle__historical_responsibility_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(cbdr_be_t0, cbdr_principle__historical_responsibility_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(cbdr_be_t5, cbdr_principle__historical_responsibility_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement(cbdr_be_t10, cbdr_principle__historical_responsibility_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(cbdr_be_t15, cbdr_principle__historical_responsibility_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(cbdr_be_t20, cbdr_principle__historical_responsibility_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(cbdr_be_t25, cbdr_principle__historical_responsibility_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(cbdr_be_t30, cbdr_principle__historical_responsibility_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(cbdr_be_t35, cbdr_principle__historical_responsibility_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_su_t0, cbdr_principle__historical_responsibility_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cbdr_su_t5, cbdr_principle__historical_responsibility_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(cbdr_su_t10, cbdr_principle__historical_responsibility_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(cbdr_su_t15, cbdr_principle__historical_responsibility_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement(cbdr_su_t20, cbdr_principle__historical_responsibility_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(cbdr_su_t25, cbdr_principle__historical_responsibility_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(cbdr_su_t30, cbdr_principle__historical_responsibility_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(cbdr_su_t35, cbdr_principle__historical_responsibility_reading, suppression_requirement, 35, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__historical_responsibility_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(cbdr_principle__historical_responsibility_reading, 0.18).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, cbdr_principle__voluntary_commitment_reading).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, paris_agreement_ndcs_gate).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, loss_and_damage_financing_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the CBDR kernel. The sibling reading 'voluntary_commitment_reading' instantiates CBDR with voluntary NDCs and technology transfer as primary developed-nation obligation, making all nations symmetric in their choice of commitment level and removing developed nations from the victim set. The two readings are not alternative measurements of one constraint — they are structurally distinct constraints with different ε values, different beneficiary/victim structures, and different classification outcomes. Both readings are authoritatively referenced in UNFCCC documents but compete for operational primacy through successive COPs. Network link is bidirectional: a shift toward the voluntary reading would reduce this constraint's extractiveness and suppress its enforcement; the historical-responsibility reading's institutional persistence maintains pressure on the voluntary reading to remain subordinate in official doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cbdr_principle__historical_responsibility_reading, powerless, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
