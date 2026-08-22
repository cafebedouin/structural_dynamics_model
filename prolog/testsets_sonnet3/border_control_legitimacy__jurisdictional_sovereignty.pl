% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__jurisdictional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__jurisdictional_sovereignty, []).

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
 *   constraint_id: border_control_legitimacy__jurisdictional_sovereignty
 *   human_readable: Border Control as Bounded Jurisdictional Authority (Balancing Reading)
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This story instantiates the jurisdictional_sovereignty reading of the
 *   border_control_legitimacy kernel: sovereignty grants jurisdictional
 *   authority to regulate rights and obligations within territory, but this
 *   authority does not automatically include unconstrained border closure,
 *   and its legitimacy depends on balancing protection obligations, labor
 *   market needs, and public consent. This is a distinct constraint from the
 *   sovereignty_primary reading (absolute discretion to exclude, border
 *   control as constitutive of statehood) and the freedom_of_movement_primary
 *   reading (movement as a fundamental right that sovereignty cannot
 *   override). Under this reading, both excluded migrants AND displaced
 *   citizens are named as potential victims, and enforcement itself becomes
 *   illegitimate when it fails proportionality/necessity tests OR when
 *   unmanaged admission erodes public consent. The rising extractiveness and
 *   theater-ratio trajectory reflects a real historical pattern: balancing
 *   rhetoric has increasingly accompanied enforcement infrastructure that
 *   outpaces the protection and labor-needs analysis it claims to be grounded
 *   in.
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus: sets and enforces admission policy, claims bounded jurisdictional authority
 *   - receiving_state_citizens: primary beneficiary of controlled admission, source of the consent constraint
 *   - excluded_asylum_seekers: bear enforcement costs, protected in principle by proportionality tests they cannot themselves invoke from outside
 *   - undocumented_labor_migrants: fill labor demand while denied legal standing
 *   - displaced_citizens_in_underserved_regions: the second, less visible victim class this reading insists on naming
 *   - international_human_rights_bodies: analytical observer checking proportionality claims against practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.58).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.62).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Border Control as Bounded Jurisdictional Authority (Balancing Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, 'b96d0c3c-b171-4efe-9419-f030bed6fa14').
narrative_ontology:cs_kernel_codification('b96d0c3c-b171-4efe-9419-f030bed6fa14', distributed).
narrative_ontology:cs_authority_grounding('b96d0c3c-b171-4efe-9419-f030bed6fa14', distributed).
narrative_ontology:cs_reading_relation('b96d0c3c-b171-4efe-9419-f030bed6fa14', border_control_legitimacy__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('b96d0c3c-b171-4efe-9419-f030bed6fa14', border_control_legitimacy__freedom_of_movement_primary, coexists_with).
narrative_ontology:cs_axiom('b96d0c3c-b171-4efe-9419-f030bed6fa14', foundational, jurisdiction_does_not_entail_closure_authority).
narrative_ontology:cs_axiom_status(jurisdiction_does_not_entail_closure_authority, holdable).
narrative_ontology:cs_axiom_grounding('b96d0c3c-b171-4efe-9419-f030bed6fa14', jurisdiction_does_not_entail_closure_authority, conventional).
narrative_ontology:cs_axiom('b96d0c3c-b171-4efe-9419-f030bed6fa14', foundational, legitimacy_requires_tripartite_balancing_test).
narrative_ontology:cs_axiom_status(legitimacy_requires_tripartite_balancing_test, holdable).
narrative_ontology:cs_axiom_grounding('b96d0c3c-b171-4efe-9419-f030bed6fa14', legitimacy_requires_tripartite_balancing_test, instrumental).
narrative_ontology:cs_reference_frame('b96d0c3c-b171-4efe-9419-f030bed6fa14', post_war_bounded_sovereignty_framework).
narrative_ontology:cs_drift_state('b96d0c3c-b171-4efe-9419-f030bed6fa14', contemporary_enforcement_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b96d0c3c-b171-4efe-9419-f030bed6fa14', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, receiving_state_citizens).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, domestic_labor_market_incumbents).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, state_administrative_apparatus).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, undocumented_labor_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens_in_underserved_regions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, domestic_labor_market_incumbents).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, proportionality_constrained_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces admission, asylum, and removal policy under domestic and international law. Claims authority to regulate entry as an incident of jurisdiction over territory, but is bound (in law and in this reading's own terms) by non-refoulement, proportionality, and necessity tests. Administers the border, collects the political and fiscal benefits of border legitimacy, and bears the reputational cost when enforcement is found disproportionate.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, state_administrative_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from controlled admission through preserved wage floors in some sectors, public service capacity, and a felt sense of democratic control over who joins the polity. Their consent is treated as a legitimacy input the state must track, not override; they can exit the debate through the franchise but cannot exit the polity's exposure to migration pressure.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, receiving_state_citizens, beneficiary,
    organized, biographical, mobile, national).

% Benefit where restricted entry protects wages and job access in low-skill sectors, but pay indirectly where restricted labor migration raises costs in sectors dependent on migrant labor (agriculture, care work, construction) that citizens do not fill. Their situation is genuinely double-edged under this reading, which is why labor needs are named as a legitimacy input rather than ignored.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, domestic_labor_market_incumbents, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__jurisdictional_sovereignty, domestic_labor_market_incumbents, payer).

% Seek protection from persecution or violence and are turned away, detained, or returned under enforcement measures justified as jurisdictional prerogative. Under this reading their claim is not simply overridden by sovereignty — enforcement against them is only legitimate if it survives a proportionality and necessity test grounded in actual protection obligations, which they can invoke but rarely can enforce from outside the territory.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, excluded_asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Work inside the territory without full legal status, filling labor demand the domestic workforce does not meet, while remaining subject to removal at the state's discretion. They bear the cost of a border regime that both needs their labor and denies them the legal standing that would let them bargain over its terms.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, undocumented_labor_migrants, payer,
    powerless, biographical, trapped, national).

% Citizens in regions where enforcement and admission resources are diverted toward border infrastructure rather than local services bear a displaced cost of the border regime even though they are not migrants — the dual-victim structure this reading insists on naming: enforcement can fail its own citizens' welfare needs while claiming to protect them.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens_in_underserved_regions, payer,
    powerless, biographical, constrained, regional).

% Monitor whether enforcement measures satisfy proportionality, necessity, and non-refoulement obligations; issue findings and rulings that can constrain state practice without directly controlling it. Their assessments are the primary external check this reading relies on to distinguish legitimate jurisdictional enforcement from disguised exclusion.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Operate in the gap between restricted legal channels and persistent migration demand, profiting precisely because enforcement raises the cost and risk of movement without eliminating the underlying pressure. Not part of the legitimacy conversation, but their existence is evidence this reading treats seriously: enforcement without addressing labor demand and protection need generates informal, more dangerous substitutes.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, cross_border_smuggling_networks, excluded,
    organized, immediate, arbitrage, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__jurisdictional_sovereignty, diffuse).
narrative_ontology:fixing_cost_class(border_control_legitimacy__jurisdictional_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a polity's capacity to regulate who enters its territory, allocate scarce public resources (housing, services, labor market protections), and honor international protection obligations, while keeping admission policy answerable to democratic consent rather than either unlimited discretion or unconditional openness.
% TRANSFER_FUNCTION: Moves the costs of exclusion onto asylum seekers and undocumented workers (denied entry, legal status, or bargaining power) and moves some benefits (wage protection, service capacity, felt control) to citizens and incumbent workers — while also transferring resources away from underserved citizen communities toward enforcement infrastructure, and transferring labor-cost burdens onto sectors that depend on migrant labor the legal channels do not supply.
% ABSENT_VOICES: Excluded asylum seekers and undocumented migrants are structurally outside the polity that debates and legitimizes the policy; they can be represented by advocates and international bodies but cannot vote or directly contest enforcement decisions made about them. Smuggling networks are excluded from the legitimacy conversation entirely, though their existence is a direct product of the gap this reading tries to manage.
% DISAPPEARANCE_RATIONALE: If jurisdictional border authority disappeared overnight, citizens and labor-market incumbents would experience real disruption to wage structures, service capacity, and the felt legitimacy of the polity's self-governance (world_rearranges from their seat); but excluded migrants and rights bodies would argue the underlying protection and labor-need problems were never solved by border closure in the first place, only displaced onto them, and that removing enforcement mainly exposes rather than creates the world's actual condition (world_unchanged from their seat). This reading holds both readings of the disappearance test as live, which is precisely its claim to occupy the balancing position between the two more absolute sibling readings.
% FOUNDING_PROBLEM: States needed a way to regulate membership, allocate finite public resources, and control labor market entry while also honoring accumulated post-WWII obligations (refugee protection, non-refoulement) that constrain unlimited discretion over exclusion — the founding problem is reconciling jurisdictional self-governance with binding protection commitments and economic interdependence.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies and independent migration scholars attest that the protection component of the founding problem remains live and is frequently violated in practice (disproportionate enforcement, pushbacks, prolonged detention) — corroboration from outside the enforcing states themselves. Labor economists studying sectors dependent on migrant labor corroborate that the labor-needs component is also live and poorly matched by current legal channels. The state administrative apparatus itself attests the problem is being managed responsibly; that attestation alone would not count as external corroboration under this reading's own standard, which is why independent monitoring bodies are named as the relevant outside check.
narrative_ontology:disappearance_verdict(border_control_legitimacy__jurisdictional_sovereignty, contested).
narrative_ontology:founding_problem_status(border_control_legitimacy__jurisdictional_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__jurisdictional_sovereignty, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) sits meaningfully below what a pure sovereignty_primary reading would author (which treats exclusion as costless discretion) but well above what a freedom_of_movement_primary reading would author (which treats most border enforcement as illegitimate extraction). This reading holds that some extraction is genuinely justified by protection-balancing and labor-market coordination, but that current enforcement has drifted past what proportionality would license — hence the rising trajectory. Suppression (0.62) reflects real coercive machinery (detention, removal, interdiction) but is authored below the sovereignty_primary ceiling because this reading insists that legitimate suppression is bounded by necessity and proportionality tests, not unconstrained. Theater ratio (0.42) captures a growing gap between the balancing rhetoric officials use to justify policy and enforcement decisions that no longer track the labor-need and protection analysis that would make the balancing claim honest.
 *
 * PERSPECTIVAL GAP:
 *   The state administrative apparatus experiences this constraint as a legitimate, bounded coordination function it is actively balancing. Excluded asylum seekers and undocumented migrants experience the same structure as extraction enforced against them with limited real recourse to the proportionality test that is supposed to protect them. Displaced citizens in underserved regions experience a third, often invisible position: bearing costs of an enforcement apparatus that primarily serves a legitimacy narrative rather than their own welfare. The engine should compute divergent seat types from this same structural data — the claim (tangled_rope, genuinely balancing) versus what victims experience (closer to snare-like extraction) is the measurement gap this reading is built to expose without resolving.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving-state citizens and labor-market incumbents (partially) sit toward the beneficiary end: they gain wage protection, service capacity, and felt democratic control, with mobile or constrained exit options reflecting their embeddedness in the polity but real capacity to contest policy through voice. Excluded asylum seekers and undocumented migrants sit at the target end: trapped exit options, powerless structural position, and the state's compliance obligations (non-refoulement, proportionality) exist precisely because their structural vulnerability is otherwise total. Displaced citizens in underserved regions are victims without being migrants at all — their inclusion in the victim set is this reading's distinguishing structural move, refusing to let 'citizen' and 'beneficiary' collapse into synonyms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling jurisdictional self-governance with protection obligations and labor interdependence) remains genuinely live by the state's own admission and by independent monitoring bodies' assessment — this blocks a clean mandatrophy verdict of pure obsolescence. But the founding_problem_status is authored as contested rather than live because current enforcement practice, per the temporal measurements, has drifted toward extraction and theater beyond what the founding balancing rationale would license. This reading resists collapsing into either 'pure coordination, ignore the victims' or 'pure extraction, the balancing language is only cover' — it holds that the coordination function is real AND that its administration has become increasingly extractive over time, which is exactly what the tangled_rope classification with a rising extraction trajectory is meant to capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_test_operationalization,
    'Is the proportionality/necessity test this reading relies on to bound legitimate enforcement actually operationalized and enforced by any body with power to compel compliance, or is it aspirational language with no binding mechanism?',
    'Track outcomes of international human rights body rulings against enforcing states: compliance rate, remedy implementation, and whether findings of disproportionate enforcement produce actual policy change versus being absorbed without consequence.',
    'If the proportionality test has no real enforcement mechanism, this reading''s claim to occupy a bounded middle ground collapses toward the sovereignty_primary reading in practice even while retaining different rhetoric — the classification should then trend toward snare rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_test_operationalization, empirical, 'Whether the balancing test constraining sovereignty is actually binding or merely rhetorical.').

omega_variable(
    labor_need_versus_labor_capture,
    'Is the ''labor needs'' component of legitimacy genuinely about matching admission policy to demonstrated labor market gaps, or is it primarily a vehicle for employer interests to secure a controllable, precarious workforce lacking full legal status?',
    'Compare legal migration channel design against independently measured labor shortages; examine whether legal pathways expand in shortage sectors or whether undocumented status is functionally preserved to suppress wages and bargaining power.',
    'If labor needs functions primarily as employer capture rather than genuine coordination, the domestic_labor_market_incumbents beneficiary declaration is too narrow — a distinct beneficiary class (employers of undocumented labor) should be separated out, likely pushing the constraint toward a more extractive classification for that sub-population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_need_versus_labor_capture, conceptual, 'Whether labor-needs balancing is genuine coordination or employer rent extraction disguised as policy coordination.').

omega_variable(
    dual_victim_weighting,
    'When enforcement resource allocation trades off protection obligations to migrants against service provision to displaced citizens in underserved regions, is there any principled way to weigh these dual victim classes against each other, or does this reading''s dual-victim structure just relocate rather than resolve the distributional conflict?',
    'Track actual budget allocation data comparing border enforcement spending to underserved-region service spending over time, and survey whether policy debates treat these as genuinely comparable claims or whether one systematically crowds out the other.',
    'If displaced citizens'' claims are consistently and structurally subordinated to enforcement spending regardless of rhetorical acknowledgment, the dual-victim framing may function as legitimacy cover rather than a real balancing commitment, which would push the classification toward snare for that victim class specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_victim_weighting, conceptual, 'Whether naming displaced citizens as a second victim class produces real distributional weighing or only rhetorical inclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0, 0.22).
narrative_ontology:measurement(bord_tr_t8, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 8, 0.27).
narrative_ontology:measurement(bord_tr_t16, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 16, 0.32).
narrative_ontology:measurement(bord_tr_t24, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 24, 0.36).
narrative_ontology:measurement(bord_tr_t32, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 32, 0.4).
narrative_ontology:measurement(bord_tr_t40, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bord_be_t8, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(bord_be_t16, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(bord_be_t24, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(bord_be_t32, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(bord_be_t40, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(bord_su_t8, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(bord_su_t16, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(bord_su_t24, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(bord_su_t32, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(bord_su_t40, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, freedom_of_movement_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the border_control_legitimacy kernel, decomposed per the epsilon-invariance principle: sovereignty_primary authors near-unconstrained exclusion discretion as a high-extraction, low-proportionality-check constraint; freedom_of_movement_primary authors most border enforcement as illegitimate extraction against a fundamental right; this jurisdictional_sovereignty reading occupies a structurally distinct middle position — bounded jurisdictional authority conditioned on a proportionality/necessity test and dual victim recognition — with its own epsilon (0.58), its own beneficiary/victim structure, and a rising-extraction temporal trajectory reflecting drift away from its own founding balance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
