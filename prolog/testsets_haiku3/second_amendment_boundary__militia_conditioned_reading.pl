% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__militia_conditioned_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__militia_conditioned_reading, []).

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
 *   constraint_id: second_amendment_boundary__militia_conditioned_reading
 *   human_readable: Second Amendment Militia-Conditioned Reading: State Regulatory Authority
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The Second Amendment contains two clauses: 'A well regulated Militia,
 *   being necessary to the security of a free State, the right of the people
 *   to keep and bear Arms, shall not be infringed.' The militia-conditioned
 *   reading interprets the prefatory militia clause as definitional—it bounds
 *   the operative clause's scope to collective defense context, permitting
 *   comprehensive state regulation of private firearms possession. This is
 *   one of three structurally distinct readings of the same constitutional
 *   text. Under this reading, state regulatory authority is presumed
 *   legitimate; private firearms possession is subject to means-end scrutiny
 *   tied to militia function; and the firearms market is exposed to
 *   democratic restriction. This constraint story models the extractive
 *   structure that this reading instantiates: who benefits from this
 *   interpretation, who bears costs, what enforcement mechanisms sustain it,
 *   and what contested premises remain irreducibly uncertain. The reading's
 *   authority derives from judicial adoption (particularly recent decisions
 *   like District of Columbia v. Heller, 2008, as narrowly construed) and
 *   democratic constituencies supporting gun regulation; that authority is
 *   actively contested by advocates of alternative readings.
 *
 * KEY AGENTS:
 *   - State regulatory authority (agenda setter): interprets prefatory militia clause as binding; grounds legitimate regulation in collective defense purpose
 *   - Public safety constituencies (beneficiary): advocacy organizations, legislators, and public opinion supporting restrictions on firearms access
 *   - Private firearms owners (payer/victim): individuals whose possession rights are subordinated to state collective defense authority; bear costs of licensing, registration, permits, waiting periods
 *   - Gun rights advocates (payer/observer): organizations and scholars arguing for individual-right reading; mount continuous resistance through litigation and legislation
 *   - Constitutional scholars (militia faction, beneficiary): provide intellectual justification for the reading; influence judicial interpretation
 *   - Federal judiciary (agenda setter/observer): adjudicates constitutional meaning; upholds regulations as consistent with militia-conditioned reading
 *   - Gun manufacturers/retailers (payer): face market segmentation and compliance burdens from regulated access
 *   - Insurrectionist tradition holders (excluded): voices structurally foreclosed by the reading's core premise that ties the right to state-defined collective defense, not individual revolutionary capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, 0.68).
domain_priors:suppression_score(second_amendment_boundary__militia_conditioned_reading, 0.72).
domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__militia_conditioned_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Second Amendment Militia-Conditioned Reading: State Regulatory Authority").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional/political").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__militia_conditioned_reading, 'eb092518-03de-4f58-b699-5a99393b14c6').
narrative_ontology:cs_kernel_codification('eb092518-03de-4f58-b699-5a99393b14c6', fixed_text).
narrative_ontology:cs_authority_grounding('eb092518-03de-4f58-b699-5a99393b14c6', lineage).
narrative_ontology:cs_interpretation_layer_present('eb092518-03de-4f58-b699-5a99393b14c6').
narrative_ontology:cs_reading_relation('eb092518-03de-4f58-b699-5a99393b14c6', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb092518-03de-4f58-b699-5a99393b14c6', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('eb092518-03de-4f58-b699-5a99393b14c6', foundational, prefatory_clause_binds_operative_scope).
narrative_ontology:cs_axiom_status(prefatory_clause_binds_operative_scope, holdable).
narrative_ontology:cs_axiom_grounding('eb092518-03de-4f58-b699-5a99393b14c6', prefatory_clause_binds_operative_scope, deontological).
narrative_ontology:cs_axiom('eb092518-03de-4f58-b699-5a99393b14c6', foundational, collective_defense_purpose_grounds_regulation).
narrative_ontology:cs_axiom_status(collective_defense_purpose_grounds_regulation, holdable).
narrative_ontology:cs_axiom_grounding('eb092518-03de-4f58-b699-5a99393b14c6', collective_defense_purpose_grounds_regulation, deontological).
narrative_ontology:cs_reference_frame('eb092518-03de-4f58-b699-5a99393b14c6', militia_centered_armed_capacity).
narrative_ontology:cs_drift_state('eb092518-03de-4f58-b699-5a99393b14c6', contemporary_litigation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('eb092518-03de-4f58-b699-5a99393b14c6', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_regulatory_authority).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, public_safety_constituencies).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, private_firearms_owners).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, gun_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, constitutional_scholars_militia_faction).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, gun_manufacturers_retailers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the prefatory militia clause as defining the operative clause's scope, thereby grounding its authority to regulate private firearms possession through licensing, registration, permit systems, and capacity restrictions. Justifies regulations as furthering collective defense and public safety. Enforces compliance through criminal penalties and civil liability.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_regulatory_authority, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from regulatory frameworks that restrict access to firearms for individuals deemed dangerous, mentally unstable, or convicted of violence. The reading legitimizes regulations that would be unconstitutional under alternative readings. They organize through lobbying, legislative testimony, and litigation support for regulatory regimes.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, public_safety_constituencies, beneficiary,
    organized, biographical, constrained, national).

% Bear the costs of compliance: permits, licenses, registration fees, waiting periods, background checks, storage requirements, and capacity limitations. Their possession rights are subordinated to the state's collective defense authority under this reading. Exit options are constrained by federal geography and the uniform interpretation across states; compliance is mandatory wherever they reside.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, private_firearms_owners, payer,
    moderate, biographical, constrained, national).

% Organizations and individuals who read the operative clause as establishing an individual right independent of militia service. They bear the costs of this reading through lost political battles, restricted market access for firearms, and the need to contest regulations in court. They mount continuous resistance through constitutional litigation and legislative advocacy.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, gun_rights_advocates, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__militia_conditioned_reading, gun_rights_advocates, observer).

% Academic and legal interpreters whose scholarship, judicial opinions, and amicus briefs vindicate the militia-conditioning reading. They provide intellectual infrastructure for regulatory legitimacy and influence judicial interpretation of constitutional scope.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, constitutional_scholars_militia_faction, beneficiary,
    analytical, generational, analytical, national).

% Adjudicates the meaning of the prefatory clause and its relationship to the operative clause. Under this reading, courts uphold regulatory statutes as constitutional so long as they serve the militia purpose. Their doctrinal choices determine which regulations survive strict scrutiny and which readings gain institutional authority.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__militia_conditioned_reading, federal_judiciary, observer).

% Bear costs through regulatory compliance burdens, market segmentation where high-regulation jurisdictions restrict sales of certain firearms, and reduced consumer access. They have higher exit mobility than individual owners (can relocate operations or product lines), but face a regulated market wherever the militia-conditioning reading is in effect.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, gun_manufacturers_retailers, payer,
    powerful, biographical, mobile, national).

% Would argue that the amendment's true purpose is to preserve armed resistance capacity against tyrannical government. The militia-conditioning reading explicitly excludes this interpretation by tying the right to state-defined collective defense, not individual revolutionary capacity. Their voice is structurally foreclosed by the reading's core premise.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, insurrectionist_tradition_holders, excluded,
    moderate, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__militia_conditioned_reading, state_regulatory_authority).
narrative_ontology:fixing_cost_class(second_amendment_boundary__militia_conditioned_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state defensive capacity and democratic regulation of weapons: the prefatory militia clause anchors the operative clause to a collective defense function, permitting legislatures to align firearms access with defensive necessity rather than individual preference.
% TRANSFER_FUNCTION: Transfers regulatory authority from individual choice to state-administered collective defense systems: private owners surrender unilateral possession decisions in exchange for participation in state-coordinated armed structures (National Guard, militia reserve) and public safety infrastructure.
% ABSENT_VOICES: Insurrectionist interpreters and those who read the operative clause as establishing an individual right independent of collective defense purpose are structurally excluded by the reading's core premise. They would argue the amendment protects an individual's capacity to resist tyrannical government, a function the militia-conditioning reading explicitly subordinates to state authority.
% DISAPPEARANCE_RATIONALE: If this reading's institutional authority vanished, alternative readings would immediately claim the constitutional field. The world would not rearrange its baseline arrangements—firearms law would reorganize around the dominant alternative reading (individual right, insurrectionist potential, or other framings), and the specific regulatory structures justified by this reading (licensing, registration, capacity restrictions grounded in collective defense) would lose constitutional foundation and face immediate legal challenge.
% FOUNDING_PROBLEM: The militia clause was drafted to ensure states retained armed capacity independent of federal control and to preserve armed resistance capacity against tyranny. Early republic readers tied individual right to bear arms to the necessity of a trained militia for collective defense and potential resistance.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars aligned with this reading (Saul Cornell, Michael Waldman) attest the founding problem was collective defense and militia sufficiency, not individual self-defense or unrestricted ownership. Scholars aligned with individual-right reading (David Kopel, Eugene Volokh) attest the founding problem was individual liberty against tyranny. Contemporary gun-rights organizations and gun-control organizations offer competing corroborations, each grounded in their reading's legitimacy. No neutral historical source exists; the corroboration is entirely reading-indexed.
narrative_ontology:disappearance_verdict(second_amendment_boundary__militia_conditioned_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_boundary__militia_conditioned_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__militia_conditioned_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_boundary__militia_conditioned_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__militia_conditioned_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__militia_conditioned_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__militia_conditioned_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.68 at interval end) reflects the substantial costs imposed on private owners through regulatory compliance, permit systems, licensing fees, waiting periods, and possession restrictions. The measurement trajectory (0.38 → 0.68 over the interval) models the accumulation of state regulatory authority and expanding regulatory scope over time—as the militia-conditioned reading gained judicial and legislative adoption, more restrictions were justified by appeal to collective defense authority. Suppression is high (0.72) because the constraint's persistence depends on active enforcement: licensing and registration systems, criminal penalties for non-compliance, civil liability for unauthorized possession, and litigation to defend regulatory statutes against constitutional challenge. The suppression requirement has risen (0.52 → 0.72) as resistance intensified—gun-rights advocates have mounted increasingly coordinated litigation campaigns and legislative pushback, requiring more enforcement intensity to sustain the reading's dominance. Theater ratio is moderate-low (0.28) because the constraint has genuine coordination content (states do conduct militia readiness assessment and public safety regulation) but an increasing share of regulatory activity defends the reading's constitutional authority against legal challenge rather than optimizing actual collective defense. The measurement grid is aligned: every metric is authored at the same six time points (0, 10, 20, 30, 40, 50), enabling temporal analysis without imputation. The claim (tangled_rope) matches the structure: genuine coordination function (collective defense, public safety) coupled with asymmetric extraction (private owners bear costs, state authority and public safety constituencies benefit).
 *
 * PERSPECTIVAL GAP:
 *   The state regulatory authority and public safety constituencies experience this constraint as legitimate coordination—they see themselves as preventing dangerous individuals from accessing weapons, thereby accomplishing a public safety function. Private firearms owners experience the same structure as enforced extraction—they see themselves as having their constitutional rights subordinated to state authority claims without compelling empirical justification, and they experience the regulatory burden as an asymmetric cost imposed by those who benefit from firearm restrictions. The gun-rights advocates sit at the widest perspectival gap: they read the operative clause as establishing an individual right independent of militia service, so they experience the entire regulatory structure as a usurpation of constitutional authority, not a legitimate coordination mechanism. The federal judiciary's position is structurally ambiguous—it is both an agenda-setter (through doctrinal choices) and an observer (of constitutional meaning), with perceptual asymmetry depending on which role dominates at any moment. Scholars aligned with the militia-conditioned reading experience the constraint as vindicating constitutional text and historical founding intent; scholars aligned with alternative readings experience the same text as misinterpreted, with the constraint representing an intellectual capture by one scholarly faction.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulatory authority sits at the beneficiary end of directionality (d ≈ 0.1-0.2): it gains institutional power, expanded regulatory scope, and deference in constitutional interpretation. Public safety constituencies sit near beneficiary (d ≈ 0.25-0.35): they benefit from possession restrictions without bearing the compliance burden. Private firearms owners sit firmly at the target end (d ≈ 0.75-0.85): they bear compliance costs, foregone possession options, and constrained exit (they cannot easily relocate to avoid regulation, and identity-locked gun enthusiasts cannot abandon the practice). Gun-rights advocates sit at d ≈ 0.65-0.75: they pay through lost political battles and restricted market access, but they retain partial exit (relocating to more permissive jurisdictions, though this is mobile/constrained, not arbitrage). Gun manufacturers and retailers sit at d ≈ 0.55-0.65: they face market constraints and compliance burdens, but retain more mobile exit than individual owners. The insurrectionist tradition holders sit at d ≈ 0.80-0.90: their entire interpretive framework is structurally excluded, making exit impossible—they cannot adopt the militia-conditioned reading without abandoning their core premise.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (militia sufficiency and collective defense) is contested. State authorities and public-safety constituencies argue it remains live: they claim regulatory authority is still necessary to keep firearms away from dangerous individuals and to maintain state defensive capacity. Gun-rights advocates argue the founding problem is dead: they contend that modern state militaries and law enforcement make the militia function obsolete, so regulations cannot be justified by appeal to a defunct founding purpose. Constitutional scholars aligned with alternative readings argue the founding problem was never about collective militia but about individual resistance to tyranny, so the militia-conditioned reading misidentifies the problem entirely. This mandatrophy profile (contested founding problem + contested disappearance verdict) is precisely where a tangled-rope classification holds: the constraint has genuine coordination content (public safety, collective defense) but its persistence depends on which reading of the founding problem and its status prevails. If the gun-rights reading gains institutional authority and the founding problem is redefined as individual liberty against tyranny, the constraint's justification collapses and it becomes a snare (pure extraction with discredited coordination cover). If the militia-conditioned reading remains dominant, the constraint persists as tangled rope. The mandatrophy risk is high because the founding problem's status is fundamentally contested and the alternative readings have institutional momentum.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_binding_force,
    'Does the prefatory militia clause grammatically and constitutionally bind the operative clause''s scope, or does it merely state purpose without limiting scope?',
    'Comparative textual analysis against other constitutional provisions with prefatory and operative structure; review of founding-era usage of ''prefatory clause'' in legal documents; originalist and living-constitutionalist interpretive methodologies applied to the same text.',
    'If the prefatory clause binds scope, the militia-conditioned reading is structurally sound and regulatory authority is legitimate. If the clause states purpose only, the operative clause stands independent and individual-right readings gain structural plausibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prefatory_clause_binding_force, conceptual, 'Grammatical and constitutional binding force of prefatory militia clause to operative clause scope.').

omega_variable(
    militia_definition_boundary,
    'What constitutes ''well regulated Militia'' and ''collective defense'' in the reading? Does it include only state-controlled National Guard, or does it extend to all armed citizens organized for defense, or something else?',
    'Judicial determination of how far the militia concept stretches; examination of founding-era militia law and organization; assessment of whether private gun clubs, private security, or armed citizens outside state command structure qualify as ''well regulated Militia.''',
    'A narrow militia definition (state-only, National Guard-only) maximizes regulatory authority and extractiveness. A broader militia definition (all armed citizens in some organizational capacity) reduces regulatory scope and extractiveness. The measured extractiveness assumes a state-narrow reading; a broader reading would lower the epsilon substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_definition_boundary, empirical, 'Scope and institutional meaning of ''well regulated Militia'' as the binding purpose.').

omega_variable(
    judicial_legitimacy_vs_democratic_mandate,
    'Does this reading''s institutional authority derive from judicial adoption (court opinions grounding it as constitutional law) or from democratic constituencies that prefer regulations and support lawmakers enacting them?',
    'Examine the causal sequence: did courts establish the reading first and legislatures follow, or did legislatures enact regulations first and courts ratify them? Assess which authority structure—judicial or democratic—sustains the reading''s enforcement.',
    'If judicial authority is primary, the reading is vulnerable to future judicial reinterpretation; if democratic authority is primary, the reading is resilient to judicial narrowing but depends on continued legislative will. This affects the long-term sustainability of the extraction regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_legitimacy_vs_democratic_mandate, conceptual, 'Source of institutional legitimacy for the militia-conditioned reading: judicial doctrine vs. democratic mandate.').

omega_variable(
    alternative_reading_institutional_pressure,
    'Are the individual-right and insurrectionist readings gaining institutional force (through litigation, legislative movement, or scholarly influence) such that the militia-conditioned reading faces displacement?',
    'Monitor litigation outcomes, legislative activity in individual-right jurisdictions, scholarly publication patterns, and public opinion shifts. Track whether courts are narrowing or expanding the militia-conditioned reading in recent decisions.',
    'Rising institutional pressure on alternative readings would indicate the militia-conditioned reading''s authority is eroding. The long-term extractiveness of this constraint depends on whether state regulatory authority remains institutionally dominant; if alternative readings gain force, the constraint''s foundation weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_institutional_pressure, empirical, 'Institutional trajectory of sibling readings and threat to militia-conditioned reading dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t10, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(seco_tr_t10, observed).
narrative_ontology:measurement(seco_tr_t20, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(seco_tr_t20, observed).
narrative_ontology:measurement(seco_tr_t30, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(seco_tr_t30, observed).
narrative_ontology:measurement(seco_tr_t40, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement_basis(seco_tr_t40, observed).
narrative_ontology:measurement(seco_tr_t50, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(seco_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t10, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(seco_be_t10, observed).
narrative_ontology:measurement(seco_be_t20, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(seco_be_t20, observed).
narrative_ontology:measurement(seco_be_t30, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement_basis(seco_be_t30, observed).
narrative_ontology:measurement(seco_be_t40, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement_basis(seco_be_t40, observed).
narrative_ontology:measurement(seco_be_t50, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(seco_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(seco_su_t0, observed).
narrative_ontology:measurement(seco_su_t10, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(seco_su_t10, observed).
narrative_ontology:measurement(seco_su_t20, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement_basis(seco_su_t20, observed).
narrative_ontology:measurement(seco_su_t30, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement_basis(seco_su_t30, observed).
narrative_ontology:measurement(seco_su_t40, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(seco_su_t40, observed).
narrative_ontology:measurement(seco_su_t50, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(seco_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__militia_conditioned_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__militia_conditioned_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__insurrectionist_reading).

% DUAL FORMULATION NOTE:
% The second_amendment_boundary kernel admits three structurally distinct readings: militia_conditioned_reading (this story), individual_right_reading, and insurrectionist_reading. Each reading instantiates a different constraint with different epsilon values, beneficiary/victim structures, and type classifications. The readings compete for institutional dominance; no single reading can be 'correct' in the sense of settling the constitutional meaning. The constraint family must be analyzed as three separate stories linked by network effects. This militia-conditioned reading produces institutional pressure on the individual_right_reading through judicial opinions and legislative statutes; it coexists with insurrectionist_reading across different political communities. The individual_right_reading influences this reading by providing alternative authority structures that constrain the militia-conditioned reading's scope where courts have adopted individual-right doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_boundary__militia_conditioned_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
