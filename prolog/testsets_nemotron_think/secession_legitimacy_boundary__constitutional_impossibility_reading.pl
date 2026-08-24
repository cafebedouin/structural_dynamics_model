% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__constitutional_impossibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__constitutional_impossibility_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: secession_legitimacy_boundary__constitutional_impossibility_reading
 *   human_readable: Constitutional Impossibility of Unilateral Secession
 *   domain: political/federalism/constitutional
 *
 * SUMMARY:
 *   This constraint story instantiates the
 *   constitutional_impossibility_reading of the secession_legitimacy_boundary
 *   kernel. The reading holds that the written constitution categorically
 *   prohibits unilateral secession; exit is legitimate only through the
 *   constitution's own amendment procedure, which requires federal
 *   participation and typically supermajority thresholds. The federal
 *   government is the agenda-setter and primary beneficiary (territorial
 *   integrity, resource control, geopolitical standing). Secessionist
 *   provinces and movements are the structural payers — they bear the cost of
 *   denied exit — though this reading denies their victim status by declaring
 *   their claims categorically illegitimate. Indigenous nations are
 *   structurally excluded: the treaty_primacy_reading asserts their consent
 *   is a precondition no federal or provincial authority can override, but
 *   this reading's absolute federal authority renders that claim invisible.
 *   The constraint presents as a Mountain (emerges_naturally: true,
 *   near-total accessibility_collapse, minimal resistance) but declares
 *   federal beneficiaries, making it an FSM candidate. The claimed_type is
 *   Mountain; the authored metrics reflect the reading's self-understanding
 *   (low extractiveness, high suppression as constitutional enforcement, low
 *   theater). The engine will compute per-seat types from the structural
 *   data; the divergence between the federal seat (Mountain) and secessionist
 *   seats (likely Snare/Tangled Rope) is the measurement.
 *
 * KEY AGENTS:
 *   - federal_government: Primary agenda_setter (institutional/arbitrage) — sets and enforces the constitutional interpretation, collects territorial integrity benefits
 *   - non_secessionist_provinces: Beneficiary (organized/constrained) — gain stability and federal transfers from unity
 *   - secessionist_provinces: Payer (powerful/identity_locked) — bear denied exit, resource capture, political autonomy loss; exit fused to identity
 *   - secessionist_movements: Payer (moderate/identity_locked) — civil society actors whose political project is declared illegitimate
 *   - indigenous_nations: Excluded (powerless/trapped) — treaty rights superseded without consent; not in the constitutional conversation
 *   - international_observers: Observer (institutional/analytical) — monitor self-determination compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.15).
domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.88).
domain_priors:theater_ratio(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__constitutional_impossibility_reading, mountain).
narrative_ontology:human_readable(secession_legitimacy_boundary__constitutional_impossibility_reading, "Constitutional Impossibility of Unilateral Secession").
narrative_ontology:topic_domain(secession_legitimacy_boundary__constitutional_impossibility_reading, "political/federalism/constitutional").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__constitutional_impossibility_reading).
domain_priors:emerges_naturally(secession_legitimacy_boundary__constitutional_impossibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__constitutional_impossibility_reading, '01747e20-ab5b-44a2-be83-9a31031094cc').
narrative_ontology:cs_kernel_codification('01747e20-ab5b-44a2-be83-9a31031094cc', fixed_text).
narrative_ontology:cs_authority_grounding('01747e20-ab5b-44a2-be83-9a31031094cc', lineage).
narrative_ontology:cs_interpretation_layer_present('01747e20-ab5b-44a2-be83-9a31031094cc').
narrative_ontology:cs_reading_relation('01747e20-ab5b-44a2-be83-9a31031094cc', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('01747e20-ab5b-44a2-be83-9a31031094cc', secession_legitimacy_boundary__grievance_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('01747e20-ab5b-44a2-be83-9a31031094cc', secession_legitimacy_boundary__treaty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('01747e20-ab5b-44a2-be83-9a31031094cc', foundational, federal_constitutional_authority_absolute).
narrative_ontology:cs_axiom_status(federal_constitutional_authority_absolute, holdable).
narrative_ontology:cs_axiom_grounding('01747e20-ab5b-44a2-be83-9a31031094cc', federal_constitutional_authority_absolute, conventional).
narrative_ontology:cs_axiom('01747e20-ab5b-44a2-be83-9a31031094cc', foundational, secession_categorically_impermissible).
narrative_ontology:cs_axiom_status(secession_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('01747e20-ab5b-44a2-be83-9a31031094cc', secession_categorically_impermissible, conventional).
narrative_ontology:cs_reference_frame('01747e20-ab5b-44a2-be83-9a31031094cc', written_constitutional_compact).
narrative_ontology:cs_drift_state('01747e20-ab5b-44a2-be83-9a31031094cc', contemporary_self_determination_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('01747e20-ab5b-44a2-be83-9a31031094cc', '2026-08-04T12:00:00Z').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, non_secessionist_provinces).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, secessionist_provinces).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, secessionist_movements).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_indivisibility_principle).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, rule_of_law_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and enforces the constitutional interpretation that prohibits unilateral secession. Controls the amendment process (typically requiring federal legislature supermajority plus provincial consents). Collects benefits: territorial integrity, control of natural resources in secessionist regions, geopolitical standing, fiscal transfers from resource-rich provinces. Exit is arbitrage-grade: the federal government can concede secession (as UK did for Scotland referendum) but chooses not to because benefits of unity exceed costs.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from federal stability, equalization transfers, common market, and shared defense. Their exit is constrained — they could theoretically seek secession but have no political will to do so; the constraint serves their interest. They participate in the amendment formula that gates any secession, giving them veto power over secessionist provinces' exit.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, non_secessionist_provinces, beneficiary,
    organized, biographical, constrained, national).

% Bear the costs of denied exit: resource revenues flow to federal center, political autonomy limited by federal jurisdiction, cultural/linguistic policies set nationally. Their exit is identity_locked — secession is not a calculable policy option but an existential identity claim (nationhood, self-determination). The constitutional impossibility reading declares their claim categorically illegitimate, fusing their political identity with the denied exit. They possess significant power (resources, population, administrative capacity) but cannot convert it into exit without federal consent.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, secessionist_provinces, payer,
    powerful, biographical, identity_locked, national).

% Civil society and political parties organizing for secession. Bear costs: legal persecution, political marginalization, denial of democratic mandate (referendums declared unconstitutional). Their exit is identity_locked — the movement's existence is constituted by the secession claim; abandoning it dissolves the movement. The reading's categorization of their claim as 'illegitimate' delegitimizes their political speech and assembly.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, secessionist_movements, payer,
    moderate, biographical, identity_locked, national).

% Hold treaty rights that predate the federal constitution and (per treaty_primacy_reading) supersede it. The constitutional_impossibility_reading's absolute federal authority renders their consent irrelevant to secession decisions affecting their territories. They are structurally excluded from the constitutional conversation — not a party to the amendment formula, not consulted on unity decisions. Their exit is trapped: no legal path to withdraw from federal jurisdiction, no international recognition pathway, and the constitutional order treats their sovereignty as extinguished.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, indigenous_nations, excluded,
    powerless, generational, trapped, national).

% UN human rights bodies, international courts, foreign governments monitoring self-determination compliance. They observe the constraint's operation but have no enforcement power within the federal system. Their analytical seat sees the full structure: federal benefit, secessionist extraction, Indigenous exclusion. They can generate diplomatic pressure but cannot alter the constitutional mechanics.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, international_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains federal unity against fragmentation, external aggression, and internal conflict by establishing a single, non-negotiable constitutional order. Solves the collective action problem of federation: no unit can defect, so all invest in shared institutions.
% TRANSFER_FUNCTION: Moves political autonomy and resource control from secessionist provinces and Indigenous nations to the federal center. The federal government collects the gains of territorial integrity (resources, strategic depth, population) while secessionist units bear the opportunity cost of denied self-government.
% ABSENT_VOICES: Indigenous nations are structurally excluded — their treaty-based sovereignty claims are not recognized in the constitutional amendment formula. Secessionist movements are delegitimized — their democratic mandates (referendums, elections) are declared unconstitutional. Both would object if present; their absence is enforced by the constraint itself.
% DISAPPEARANCE_RATIONALE: If the constitutional impossibility constraint vanished overnight, secessionist provinces would initiate negotiated exit processes (some would likely succeed), Indigenous nations would assert treaty-based veto over any territorial change affecting their lands, and the federal government would lose control of resource revenues and geopolitical standing. The federation would either reorganize as a looser confederation or dissolve.
% FOUNDING_PROBLEM: The federation was built to solve the problem of small, vulnerable colonies uniting against external threats (imperial powers, US expansion) and internal fragmentation (regional conflict, economic dislocation). The constitutional impossibility of secession was the binding commitment that made the union credible — no unit could defect when the arrangement became inconvenient.
% FOUNDING_PROBLEM_CORROBORATION: Federal government and non-secessionist provinces attest the founding problem (external threat, internal stability) remains live, citing contemporary geopolitical risks. Secessionist provinces, Indigenous nations, and independent historians attest the founding problem is dead — the original threats have vanished, and the constraint now serves to lock in federal resource control and political dominance. No single corroborating source outside the federal beneficiary set attests the problem as unequivocally live.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__constitutional_impossibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__constitutional_impossibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__constitutional_impossibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, ExtMetricName, E),
    domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(secession_legitimacy_boundary__constitutional_impossibility_reading),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) from the reading's perspective: the constitutional order is framed as coordination, not extraction — the federation provides security, market integration, and conflict resolution. But structurally, the federal government controls resource revenues and political authority that secessionist units would otherwise access; the low ε reflects the reading's denial of extraction. Suppression is high (0.88) because the constraint's persistence depends on active legal and, if necessary, military enforcement against secession attempts — the constitutional text alone does not hold; enforcement machinery maintains it. Theater is low (0.12): the constitutional courts and amendment processes are functional, not performative. Accessibility_collapse is near-total (0.92): once the constitutional impossibility premise is accepted, no alternative exit path exists within the framework. Resistance is low (0.18) from the reading's view: successful secession is structurally impossible, so resistance manifests as political protest not effective challenge. The measurement grid tracks a 200-year interval (e.g., 1820–2020) showing slight extractiveness creep (federal power accumulation) and rising theater (amendment process increasingly performative as supermajorities become unattainable).
 *
 * PERSPECTIVAL GAP:
 *   The federal seat (agenda_setter, institutional, arbitrage exit) experiences this as Mountain — the constitutional order is the natural law of the polity. Secessionist province seats (payer, powerful, identity_locked) experience it as Snare: high effective extraction (denied resource control, political autonomy), high suppression (legal/military barrier), identity_locked exit (secession is not a policy choice but an existential identity claim). Non-secessionist provinces (beneficiary, organized, constrained) experience it as Rope — genuine coordination benefit with symmetric costs. Indigenous nations (excluded, powerless, trapped) experience it as Snare with an additional dimension: their prior sovereign authority is extinguished without consent. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government: beneficiary (collects territorial integrity, resource control, geopolitical weight) — d near 0.0. Non-secessionist provinces: beneficiary (stability, transfers) with some payer aspects (federal oversight) — d ~0.3. Secessionist provinces/movements: payers (bear denied exit, resource capture) with identity_locked exit — d near 1.0. Indigenous nations: excluded payers (treaty rights overridden) with trapped exit — d near 1.0. International observers: analytical — d = 0.5. The reading declares 'no victim set from federal extraction because extraction claim is invalid' — this is the reading's axiom, not the structural reality. The base_properties.victims array names the structural payers; the reading's denial is recorded in commentary and omegas.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal unity against fragmentation, external threat, internal conflict) is contested: federalists attest it remains live; secessionists and Indigenous nations attest it is dead or never applied to them. The constraint persists with high suppression and near-zero accessibility_collapse — classic mandatrophy markers. But the reading's Mountain claim (emerges_naturally) blocks mandatrophy resolution: if the constraint is natural law, it cannot have outlived its mandate. The FSM omega captures this tension. The theater_ratio rise suggests the amendment process is becoming performative — a piton drift signal — but the constitutional_impossibility_reading treats this as stability, not decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine natural law of constitutional order (Mountain) or a constructed federal claim that benefits the federal government (Snare/Tangled Rope)?',
    'Comparative analysis of federal systems: if multiple independent federations converge on the same categorical prohibition without common origin, natural-law evidence strengthens; if prohibition correlates with federal power consolidation, constructed claim strengthens.',
    'If natural law, classification holds as Mountain across all seats. If constructed with identifiable beneficiaries, False Summit Mountain triggers reclassification to Tangled Rope via FSM signature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Natural-law vs. constructed status of the constitutional impossibility claim').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, military enforcement) or internalized (secessionist movements accept illegitimacy, political culture treats unity as sacred)?',
    'Post-referendum trajectory analysis: if suppression persists after legal barriers are removed (e.g., UK/Scotland, Canada/Quebec frameworks), reclassify as partially internalized.',
    'If internalized, effective suppression is higher than structural measure — the constraint travels with agents after formal exit options open.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in secession prevention').

omega_variable(
    indigenous_exclusion_legitimacy,
    'Does the constitutional impossibility reading''s absolute federal authority structurally exclude Indigenous treaty-holders whose consent the treaty_primacy_reading requires?',
    'Legal-theoretical audit: if the reading''s axioms logically require Indigenous consent to be overridden, the exclusion is structural; if the reading can accommodate treaty rights as a separate layer, exclusion is contingent.',
    'If structural exclusion, the reading''s beneficiary set expands to include federal extraction from Indigenous nations, triggering victim declaration and potential Snare reclassification for those seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_exclusion_legitimacy, conceptual, 'Whether constitutional impossibility structurally forecloses treaty primacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__constitutional_impossibility_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(secession_const_imposs_tr_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(secession_const_imposs_tr_t50, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 50, 0.07).
narrative_ontology:measurement(secession_const_imposs_tr_t100, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 100, 0.09).
narrative_ontology:measurement(secession_const_imposs_tr_t150, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 150, 0.11).
narrative_ontology:measurement(secession_const_imposs_tr_t200, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 200, 0.12).

% Extraction over time
narrative_ontology:measurement(secession_const_imposs_be_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(secession_const_imposs_be_t50, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 50, 0.1).
narrative_ontology:measurement(secession_const_imposs_be_t100, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 100, 0.12).
narrative_ontology:measurement(secession_const_imposs_be_t150, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 150, 0.14).
narrative_ontology:measurement(secession_const_imposs_be_t200, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 200, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(secession_const_imposs_su_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(secession_const_imposs_su_t50, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 50, 0.86).
narrative_ontology:measurement(secession_const_imposs_su_t100, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 100, 0.87).
narrative_ontology:measurement(secession_const_imposs_su_t150, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 150, 0.88).
narrative_ontology:measurement(secession_const_imposs_su_t200, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 200, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__constitutional_impossibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.1).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__treaty_primacy_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_resource_distribution).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, intergovernmental_transfer_system).

% DUAL FORMULATION NOTE:
% This reading and its three siblings decompose the colloquial 'secession legitimacy' label into four structurally distinct constraints with different ε values, beneficiary/victim structures, and enforcement logics. The constitutional_impossibility_reading has the lowest ε (0.15) — it frames the constraint as coordination. The popular_sovereignty_reading and grievance_threshold_reading have higher ε (extraction from federal center to provinces). The treaty_primacy_reading has a distinct victim set (Indigenous nations) and ε reflecting colonial extraction. All four are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__constitutional_impossibility_reading, powerful, 0.95).
constraint_indexing:directionality_override(secession_legitimacy_boundary__constitutional_impossibility_reading, moderate, 0.92).
constraint_indexing:directionality_override(secession_legitimacy_boundary__constitutional_impossibility_reading, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
