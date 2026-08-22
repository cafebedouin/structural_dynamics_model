% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__substantial_effects_limited_reading, []).

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
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Commerce Clause Substantial Effects Doctrine with Jurisdictional Nexus Limit
 *   domain: constitutional/federalism
 *
 * SUMMARY:
 *   The substantial effects doctrine is one reading of the Commerce Clause
 *   kernel: federal power extends to intrastate activity when it
 *   substantially affects interstate commerce, but the reading insists on a
 *   jurisdictional nexus (the regulated activity or party must have a direct
 *   connection to interstate commerce) and a non-pretextual purpose (the
 *   regulation must be genuinely economic, not police power regulation
 *   disguised as commerce regulation). This reading sits between the
 *   originalist narrow reading (interstate commerce = trade crossing borders
 *   only) and the expansive federal reading (all economic activity with
 *   aggregate effects). The constraint's structure is a hybrid: it
 *   coordinates national standard-setting for genuinely economic activities
 *   while extracting state regulatory autonomy, and it polices its own
 *   boundary by requiring courts to prevent federal overreach into
 *   non-economic domains. The claim/metric gap is intentional: the reading is
 *   CLAIMED as tangled_rope (coordination + asymmetric extraction + active
 *   enforcement) while the authored metrics show substantial extraction,
 *   significant theater as the boundary-policing function becomes
 *   performative, and rising suppression as the doctrine encounters
 *   resistance from states and originalist interpreters defending the
 *   non-economic sphere.
 *
 * KEY AGENTS:
 *   - Federal regulatory agencies (EPA, OSHA, FDA, NLRB, etc.): set and enforce economic standards under Commerce Clause authority; benefit from broad jurisdictional reach
 *   - State legislatures and local government: cede authority over activities classified as having substantial effects; trapped in shrinking regulatory space
 *   - National market participants (multinational firms, national commerce): benefit from uniform federal standards preempting 50 different state regimes
 *   - Courts defining the boundary: police the economic/non-economic distinction; institutional stake in maintaining jurisdiction
 *   - Originalist interpreters: excluded from the reading's construction; contest the jurisdictional scope
 *   - Non-economic activity sovereigns (states protecting family law, criminal justice, cultural sovereignty): pay the cost of boundary policing; identity-locked against exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.62).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.58).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Commerce Clause Substantial Effects Doctrine with Jurisdictional Nexus Limit").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, '4e989363-7254-48ab-aaaa-569ba3949e8c').
narrative_ontology:cs_kernel_codification('4e989363-7254-48ab-aaaa-569ba3949e8c', formalized).
narrative_ontology:cs_authority_grounding('4e989363-7254-48ab-aaaa-569ba3949e8c', lineage).
narrative_ontology:cs_interpretation_layer_present('4e989363-7254-48ab-aaaa-569ba3949e8c').
narrative_ontology:cs_reading_relation('4e989363-7254-48ab-aaaa-569ba3949e8c', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e989363-7254-48ab-aaaa-569ba3949e8c', commerce_clause_text__expansive_federal_reading, coexists_with).
narrative_ontology:cs_axiom('4e989363-7254-48ab-aaaa-569ba3949e8c', foundational, intrastate_economic_activity_federally_regulable).
narrative_ontology:cs_axiom_status(intrastate_economic_activity_federally_regulable, holdable).
narrative_ontology:cs_axiom_grounding('4e989363-7254-48ab-aaaa-569ba3949e8c', intrastate_economic_activity_federally_regulable, deontological).
narrative_ontology:cs_axiom('4e989363-7254-48ab-aaaa-569ba3949e8c', foundational, jurisdictional_nexus_constrains_federal_reach).
narrative_ontology:cs_axiom_status(jurisdictional_nexus_constrains_federal_reach, holdable).
narrative_ontology:cs_axiom_grounding('4e989363-7254-48ab-aaaa-569ba3949e8c', jurisdictional_nexus_constrains_federal_reach, conventional).
narrative_ontology:cs_axiom('4e989363-7254-48ab-aaaa-569ba3949e8c', secondary, non_economic_regulatory_sovereignty_preserved).
narrative_ontology:cs_axiom_status(non_economic_regulatory_sovereignty_preserved, holdable).
narrative_ontology:cs_axiom_grounding('4e989363-7254-48ab-aaaa-569ba3949e8c', non_economic_regulatory_sovereignty_preserved, deontological).
narrative_ontology:cs_reference_frame('4e989363-7254-48ab-aaaa-569ba3949e8c', new_deal_commerce_power_framework).
narrative_ontology:cs_drift_state('4e989363-7254-48ab-aaaa-569ba3949e8c', contemporary_boundary_contestation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4e989363-7254-48ab-aaaa-569ba3949e8c', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, national_market_participants).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, centralized_standard_setting_bodies).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, state_legislative_authority).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, local_regulatory_autonomy).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, non_economic_activity_sovereigns).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, enumerated_federal_powers_supremacy).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, necessary_and_proper_clause_scope).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% EPA, OSHA, FDA, NLRB, FTC, and similar agencies enforce federal economic regulation by invoking the substantial effects doctrine. They set compliance standards for labor, environment, product safety, and finance, conduct enforcement, and defend their jurisdictional reach in litigation. Their authority depends on the doctrine's persistence and the courts' acceptance that the activities they regulate have substantial effects on interstate commerce.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% State legislatures cede authority over activities courts determine have substantial effects on interstate commerce. They retain power over intrastate activity courts classify as non-economic (family law, criminal justice, land use for local purposes) but must defend that classification against federal recharacterization. They cannot easily exit or redefine the boundary once courts establish it.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, state_legislative_authority, payer,
    institutional, generational, constrained, regional).

% Local communities (via county and city government) lose control over labor practices, environmental management, business licensing, and land use when courts classify those activities as having substantial effects on interstate commerce. Once federal jurisdiction attaches under the doctrine, local alternatives are preempted; they cannot negotiate, compromise, or opt out of federal standards.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, local_regulatory_autonomy, payer,
    moderate, biographical, trapped, local).

% National and multinational firms benefit from uniform federal regulation that preempts 50 different state standards. They gain regulatory predictability, lower compliance costs relative to multi-state variation, and federal enforcement that eliminates state-level competitive divergence. They can lobby federal agencies directly, fund litigation, and relocate operations across state boundaries in response to regulatory change.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, national_market_participants, beneficiary,
    powerful, biographical, mobile, global).

% Federal agencies and courts that interpret the Commerce Clause consolidate power to set national baselines for economic regulation. Their interpretations define what counts as 'economic' and what degree of effect is 'substantial' — the categories that determine their jurisdiction. They benefit from the doctrine's operation and expansion because it expands their institutional role and authority.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, centralized_standard_setting_bodies, beneficiary,
    institutional, generational, arbitrage, national).

% States and Native nations defending regulations rooted in non-economic purposes (family law, criminal justice, cultural sovereignty, health and welfare, environmental protection) face increasing pressure to prove those regulations do NOT substantially affect interstate commerce. The doctrine's economic/non-economic boundary determines whether they retain authority. Their identity as sovereign regulators of local social life is directly implicated; exit is ideologically incoherent and institutionally unthinkable.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, non_economic_activity_sovereigns, payer,
    moderate, generational, identity_locked, regional).

% Originalist judges and scholars argue the substantial effects doctrine extends federal power beyond the text's original scope, which they read as commerce among the states (boundary-crossing activity and interstate instrumentalities only). They would overrule the doctrine if they controlled the courts, but find their interpretation treated as minority/illegitimate even when they hold power on the bench. They are excluded from the substantial-effects reading's construction.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, originalist_interpreters, excluded,
    institutional, generational, constrained, national).

% Policy advocates and scholars arguing for even broader federal authority (any economic activity with effects however indirect and diffuse, or even non-economic activity with sufficient aggregate spillovers) find the jurisdictional nexus requirement and non-pretextual regulation limits frustrating constraints. They influence the direction of doctrinal drift but are not at the table when the substantial-effects boundary is drawn.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, expansive_federal_advocates, excluded,
    institutional, generational, constrained, national).

% Federal courts (especially the Supreme Court) perform the boundary-policing function: they determine whether an activity is 'economic,' whether it has 'substantial effects,' whether there is jurisdictional nexus, and whether the regulation is pretextual. Their decisions define the operational content of the doctrine. They maintain the boundary by rejecting regulations framed as commerce regulation when the court determines non-economic purposes predominate, but their own institutional stake (authority to define boundaries) creates incentive pressure to preserve jurisdiction.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, courts_defining_boundary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, courts_defining_boundary, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies).
narrative_ontology:fixing_cost_class(commerce_clause_text__substantial_effects_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables uniform national regulatory standards for genuinely economic activities whose effects cross state lines, solving the collective-action problem of 50 states imposing incompatible requirements on labor, environment, product safety, and finance. Permits federal agencies to internalize externalities (pollution, unsafe working conditions, unfair competition) that individual states cannot address without being economically undercut by regulatory arbitrage in neighboring jurisdictions.
% TRANSFER_FUNCTION: Moves regulatory authority from state legislatures and local government to federal agencies and federal courts. Extracts state capacity to define their own economic and social policy boundaries. States retain authority over activities determined to be non-economic (family law, criminal justice, land use for non-commercial purposes) but must defend that classification against federal recharacterization as activity with substantial effects on interstate commerce.
% ABSENT_VOICES: Originalist judges and scholars who read the Commerce Clause as bounded by the text's historical meaning (commerce among the states = direct interstate trade and movement, not intrastate activity with aggregate effects) are excluded from this reading's construction. Expansive federal advocates arguing the doctrine should extend further, without jurisdictional nexus or non-pretextual limits, are also absent — their objection is that the constraints are unnecessary and should be abandoned.
% DISAPPEARANCE_RATIONALE: If the substantial effects doctrine and its enforcement disappeared overnight, federal regulatory authority would collapse to the originalist narrow scope (interstate instrumentalities only). Within weeks, state and local regulatory authority over labor standards, environmental protection, product safety, and financial markets would expand into the vacuum. Divergent state standards would fragment national markets; interstate commerce would face 50 different regulatory regimes; firms would intensify regulatory arbitrage by relocating production to low-regulation states; pollution and workplace hazards would cross state boundaries unchecked. The federal administrative state would cease to function in its current form.
% FOUNDING_PROBLEM: Before the New Deal, the Constitution's enumeration of federal commerce power was interpreted narrowly: Congress could regulate goods crossing state borders and the instrumentalities of interstate movement (railroads, ships), but not intrastate economic activity even if it had spillover effects on national markets. This created a regulatory gap: states could not unilaterally regulate their own economic activity if it created interstate spillovers (pollution, unfair labor competition); each state had incentive to hold standards low to attract business. The founding problem: how to authorize federal regulation of intrastate economic activity necessary to internalize interstate spillovers, without allowing federal government to regulate local activity under false commerce pretexts.
% FOUNDING_PROBLEM_CORROBORATION: Federal agencies and courts defending the substantial effects doctrine attest the founding problem remains live: spillover dynamics persist, arbitrage pressures mount when state standards diverge, and national markets require national baseline standards to prevent competitive degradation. States and originalist interpreters attest the founding problem was substantially solved by the New Deal regulatory regime itself; the doctrine now enables federal regulation of genuinely local activity (gun possession near schools, family medical decisions, state land-use policy) through recharacterization as commercial. Economic research from independent sources (university economists, cross-partisan think tanks, international comparative studies) shows the founding spillover problem is addressed by current federal regulations and the doctrine now captures non-economic activity in its expansion.
narrative_ontology:disappearance_verdict(commerce_clause_text__substantial_effects_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__substantial_effects_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__substantial_effects_limited_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (1937, newly established) to 0.62 (2026, mature doctrine) as federal agencies accumulate regulatory power and states adapt to shrinking authority. Theater rises from 0.12 to 0.41 as the doctrine's boundary-policing function becomes increasingly performative: courts must reject applications that recharacterize non-economic activity as commercial (Gonzales v. Raich for medical marijuana, United States v. Lopez for gun possession near schools), but the recharacterizations persist and the doctrine's categorization work grows without substantive limitation. Suppression requirement rises from 0.32 to 0.58 as resistance hardens: originalist judges resist the expansive reading, states defend non-economic regulatory domains, and legal scholarship contests whether the doctrine prevents the recharacterization problem it was designed to prevent. The measurements are authored on a single shared time grid (every metric at every examined year) to track the three dynamics: extraction accumulation (federal power grows), theater growth (categorical boundary work overtakes functional coordination), and suppression increase (defensive enforcement hardens as the boundary-policing work fails to contain scope).
 *
 * PERSPECTIVAL GAP:
 *   From the federal institutional seat, the doctrine enables necessary coordination: one national standard for labor, environment, product safety is more efficient than 50 divergent standards. From the state seat, the doctrine enables federal institutional expansion disguised as coordination: the 'economic/non-economic' distinction is the only boundary, and courts have shown they will recharacterize non-economic activity (gun possession, family medical decisions, land use) as economic when federal regulation is desired. From the originalist interpreter seat, the doctrine has already failed its core function: if it were constraining federal power, there would be fewer recharacterization cases and more rejections. The theater ratio's rise tracks this divergence: courts perform boundary maintenance (deny recharacterization, uphold the economic/non-economic distinction) while federal agencies accumulate regulatory authority anyway, and the constraint persists through formalism rather than functional limitation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the beneficiary/victim structure: federal agencies are beneficiaries (collect authority, face no exit, analytical power); states are victims (lose authority, trapped by jurisdictional attachment). National market participants are beneficiaries (gain from uniform standards, mobile, powerful). Non-economic sovereigns are victims (lose regulatory space, identity-locked, moderate power, trapped). Courts are dual-positioned: they are agenda-setters (define the boundary) and beneficiaries (authority to interpret 'economic' and 'substantial' expands their institutional role). Directionality for states: high d (near target) because they lose authority and are constrained. Directionality for national participants: low d (near beneficiary) because they gain and have arbitrage options. Directionality for non-economic sovereigns: highest d (full target) because they lose identity-constituting regulatory space and cannot exit ideologically.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint sits between pure coordination (if the boundary held and prevented recharacterization) and pure extraction (if the boundary were purely performative). The tangled_rope classification depends on: (1) a real coordination function — federal standards do internalize spillovers from genuinely economic activity and reduce arbitrary variation; (2) asymmetric extraction — states and localities lose authority they cannot recover; (3) active enforcement — courts must continuously perform the economic/non-economic policing to prevent the doctrine's collapse into pure federal override. The theater_ratio rise (0.12 to 0.41) suggests the enforcement is increasingly theatrical: the recharacterization cases (Raich, Lopez, Gonzales) show courts rejecting applications while the doctrine's scope continues to expand, indicating the boundary is being defended performatively but not actually limiting federal reach. This is the mandatrophy signature: the founding problem (spillover internalization) is substantially solved; the doctrine now persists partly as institutional maintenance (courts need to define boundaries to justify their role) and partly as imperial overreach (federal agencies use 'economic' categorization to reach non-economic activity). The constraint is not yet a piton because the coordination function is real and the extraction remains bound by the doctrine's stated limits; but the rising theater and persistent recharacterization attempts suggest the constraint is drifting toward that state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_non_economic_boundary_stability,
    'Is the economic/non-economic distinction a stable, justiciable boundary that actually constrains federal power, or is it a performative categorization that expands and contracts to accommodate whatever federal regulation is desired?',
    'Audit the trajectory of recharacterization attempts: if courts increasingly reject recharacterizations and the federal power stabilizes at a fixed scope, the boundary is stable; if recharacterizations succeed despite performative rejections, and federal reach continues expanding, the boundary is performative.',
    'If stable, the constraint is a genuine tangled_rope with effective boundary maintenance. If performative, the constraint is degrading toward piton: the coordination function is real, but the extraction persists through institutional inertia and the boundary-policing becomes theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_non_economic_boundary_stability, empirical, 'Whether the economic/non-economic boundary functions as a substantive constraint or merely a legitimating category.').

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (interstate spillovers requiring federal internalization) still live, or has the federal regulatory regime substantially solved it, making the doctrine now serve only imperial expansion?',
    'Compare spillover dynamics in regulated vs. unregulated domains: if regulated domains show stable spillover internalization and unregulated domains show significant competitive arbitrage, the founding problem is live; if both show similar patterns, the doctrine persists beyond functional necessity.',
    'If foundational problem is live, the constraint is tangled_rope (coordination + extraction). If dead, the constraint is snare with mandatrophy (the founding justification is gone but enforcement persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether federal commerce regulation still solves a genuine coordination problem or persists for institutional reasons.').

omega_variable(
    reading_contest_foreclosure,
    'Does the substantial effects reading foreclose the originalist reading, or do both remain live interpretative positions the courts could adopt?',
    'Jurisprudential analysis: the substantial effects reading forecloses originalism if accepting originalism logically requires denying that the constitutional text grants federal power to regulate intrastate activity with effects on interstate commerce. The readings coexist if both can be held as coherent interpretations of the same text by different parties.',
    'If foreclosed, the originalist reading is eliminated by the logic of this reading''s core premise (federal power over intrastate activity with effects is textually granted). If coexisting, both readings remain live for different judicial coalitions and the kernel contest persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Whether this reading''s premise logically eliminates the originalist alternative.').

omega_variable(
    jurisdictional_nexus_enforcement,
    'Is the jurisdictional nexus requirement actually enforced by courts, or is it a formal requirement that courts satisfy while permitting federal regulation even when nexus is tenuous or inferred?',
    'Case audit: count rejections where lack of nexus was the decisive ground vs. cases where tenuous nexus was accepted. A high rejection rate shows enforcement; a low rate shows performativity.',
    'If enforced, the nexus requirement is a real constraint on federal reach. If performative, it is a legitimating cover for federal expansion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jurisdictional_nexus_enforcement, empirical, 'Whether the jurisdictional nexus requirement functions as a binding constraint or a formal requirement easily satisfied.').

omega_variable(
    suppression_structural_vs_institutional,
    'Is the rising suppression requirement (0.32 to 0.58) structural (states cannot defend their regulatory space even when they try) or institutional (federal resistance has hardened but states retain legal and political tools to contest federal jurisdiction)?',
    'Jurisdictional variation: if states attempting to defend non-economic regulatory space (family law, criminal justice) succeed in some contexts and fail in others, suppression is institutional. If federal preemption is near-universal regardless of state effort, suppression is structural.',
    'If structural, the constraint is high-suppression snare (states cannot resist). If institutional, it is tangled rope with rising institutional pressure but remaining contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_institutional, empirical, 'Whether rising suppression reflects structural inability to resist or increased federal institutional power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 1937, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1937, 0.12).
narrative_ontology:measurement_basis(comm_tr_t1937, observed).
narrative_ontology:measurement(comm_tr_t1960, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement_basis(comm_tr_t1960, observed).
narrative_ontology:measurement(comm_tr_t1980, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1980, 0.32).
narrative_ontology:measurement_basis(comm_tr_t1980, observed).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2000, 0.39).
narrative_ontology:measurement_basis(comm_tr_t2000, observed).
narrative_ontology:measurement(comm_tr_t2015, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement_basis(comm_tr_t2015, observed).
narrative_ontology:measurement(comm_tr_t2026, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2026, 0.41).
narrative_ontology:measurement_basis(comm_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement_basis(comm_be_t1937, observed).
narrative_ontology:measurement(comm_be_t1960, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1960, 0.48).
narrative_ontology:measurement_basis(comm_be_t1960, observed).
narrative_ontology:measurement(comm_be_t1980, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement_basis(comm_be_t1980, observed).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement_basis(comm_be_t2000, observed).
narrative_ontology:measurement(comm_be_t2015, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement_basis(comm_be_t2015, observed).
narrative_ontology:measurement(comm_be_t2026, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(comm_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1937, 0.32).
narrative_ontology:measurement_basis(comm_su_t1937, observed).
narrative_ontology:measurement(comm_su_t1960, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1960, 0.42).
narrative_ontology:measurement_basis(comm_su_t1960, observed).
narrative_ontology:measurement(comm_su_t1980, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1980, 0.51).
narrative_ontology:measurement_basis(comm_su_t1980, observed).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2000, 0.56).
narrative_ontology:measurement_basis(comm_su_t2000, observed).
narrative_ontology:measurement(comm_su_t2015, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement_basis(comm_su_t2015, observed).
narrative_ontology:measurement(comm_su_t2026, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(comm_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__substantial_effects_limited_reading, 0.18).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, dormant_commerce_clause_constraint).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, state_police_power_boundary).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, federal_agency_jurisdictional_scope).

% DUAL FORMULATION NOTE:
% The Commerce Clause kernel generates three constraint stories: (1) originalist_narrow_reading — federal power limited to direct interstate commerce; (2) substantial_effects_limited_reading (this story) — federal power over intrastate economic activity with substantial effects, but bounded by jurisdictional nexus and non-pretextual limits; (3) expansive_federal_reading — all economic activity with aggregate effects on national commerce. These are not the same constraint viewed from different angles; they are three structurally distinct readings of the same constitutional text with different ε values, beneficiary structures, and enforcement mechanisms. The substantial_effects_limited_reading influences both siblings by establishing the jurisdictional nexus and non-pretextual limits as the middle ground; expansive_federal_reading pushes against these limits; originalist_narrow_reading argues the entire framework is an unauthorized expansion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_text__substantial_effects_limited_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
