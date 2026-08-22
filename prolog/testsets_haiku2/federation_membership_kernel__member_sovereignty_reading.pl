% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__member_sovereignty_reading, []).

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
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: EU Free Movement Bounded by Member State Welfare Capacity (Sovereignty Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the EU free movement kernel:
 *   the member sovereignty reading. In this framing, free movement rights are
 *   not absolute but conditioned on economic contribution; member states
 *   retain authority to exclude economically inactive migrants to protect
 *   welfare system sustainability and labor market stability. The constraint
 *   emerges from a foundational tension between the EU's commitment to free
 *   movement (Articles 20-21 TFEU) and member states' responsibility for
 *   welfare systems and labor market regulation. The member sovereignty
 *   reading prioritizes member state authority and social solidarity
 *   institutions over expansive personal mobility rights. This reading is
 *   contested by the integration reading (which treats free movement as a
 *   fundamental EU citizen right to be interpreted expansively by the ECJ)
 *   and the welfare coordination reading (which seeks to decouple welfare
 *   from mobility through EU-level coordination of social protection rules
 *   rather than member state gatekeeping). The three readings coexist within
 *   the EU framework, with different institutional actors (member states,
 *   ECJ, Commission) advancing different interpretations of the founding
 *   commitment.
 *
 * KEY AGENTS:
 *   - member_state_governments: institutional agenda-setters, retain formal authority to condition free movement on economic activity
 *   - economically_inactive_migrants: powerless payers, bear the cost of exclusion gates and restricted access
 *   - sending_state_workers: organized, constrained payers, face restricted mobility within EU unless they meet economic criteria
 *   - receiving_state_welfare_systems: institutional beneficiaries, protected from fiscal demand by economic gatekeeping
 *   - European Court of Justice: institutional observer, interprets free movement scope and constrains member state authority through case law
 *   - EU Commission: institutional observer, mediates between ECJ mobility expansionism and member state welfare concerns
 *   - sending_state_governments: excluded, would object to mobility restrictions but have no formal voice in the rule-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.71).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "EU Free Movement Bounded by Member State Welfare Capacity (Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, 'a38b1e9d-fad4-40c2-9e03-6752136e8cc0').
narrative_ontology:cs_kernel_codification('a38b1e9d-fad4-40c2-9e03-6752136e8cc0', formalized).
narrative_ontology:cs_authority_grounding('a38b1e9d-fad4-40c2-9e03-6752136e8cc0', extraction).
narrative_ontology:cs_interpretation_layer_present('a38b1e9d-fad4-40c2-9e03-6752136e8cc0').
narrative_ontology:cs_reading_relation('a38b1e9d-fad4-40c2-9e03-6752136e8cc0', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('a38b1e9d-fad4-40c2-9e03-6752136e8cc0', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('a38b1e9d-fad4-40c2-9e03-6752136e8cc0', foundational, member_state_welfare_autonomy).
narrative_ontology:cs_axiom_status(member_state_welfare_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('a38b1e9d-fad4-40c2-9e03-6752136e8cc0', member_state_welfare_autonomy, conventional).
narrative_ontology:cs_axiom('a38b1e9d-fad4-40c2-9e03-6752136e8cc0', foundational, economic_contribution_basis_for_social_solidarity).
narrative_ontology:cs_axiom_status(economic_contribution_basis_for_social_solidarity, holdable).
narrative_ontology:cs_axiom_grounding('a38b1e9d-fad4-40c2-9e03-6752136e8cc0', economic_contribution_basis_for_social_solidarity, deontological).
narrative_ontology:cs_reference_frame('a38b1e9d-fad4-40c2-9e03-6752136e8cc0', member_state_constitutional_autonomy).
narrative_ontology:cs_drift_state('a38b1e9d-fad4-40c2-9e03-6752136e8cc0', post_2004_enlargement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a38b1e9d-fad4-40c2-9e03-6752136e8cc0', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, member_state_welfare_systems).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, domestic_labor_market_protection).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_state_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, sending_state_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain formal authority to exclude economically inactive migrants and set welfare eligibility rules. They administer the exclusion gates (public funds test, social assistance restrictions) and justify them as protecting social solidarity and fiscal sustainability. Their authority is contested by ECJ interpretations that expand free movement, requiring domestic governments to litigate in the supranational forum to maintain exclusions.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, member_state_governments, agenda_setter,
    institutional, generational, constrained, continental).

% Are excluded from residence rights in member states where they cannot demonstrate economic activity or sufficient resources. They bear the cost of restricted access to EU territory and member state welfare benefits. Their exit options are limited to remaining in sending states or moving outside EU entirely. Identity lock is minimal (they are not citizens of the receiving state); structural barriers are the binding constraint.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants, payer,
    powerless, biographical, trapped, continental).

% Face restricted access to higher-wage labor markets when they do not meet economic activity thresholds in receiving states. They benefit from EU membership and trade access, but lose mobility option within the union. Brain drain still occurs but is dampened by receiving-state welfare restrictions. They experience the constraint as a bounded but still-available exit—conditional on employment.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_workers, payer,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__member_sovereignty_reading, sending_state_workers, beneficiary).

% Are protected from unmanaged demand by exclusion gates: member states can require economic activity or sufficient resources before granting welfare access. This protects fiscal sustainability and the mutual obligation logic that underpins social insurance. The constraint's operation allows welfare systems to maintain contribution-based design without being overwhelmed by migrants unable to contribute.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_systems, beneficiary,
    institutional, generational, mobile, continental).

% Benefits from labor mobility conditioned on economic activity: workers are selected for employment, not for welfare access. This reduces wage pressure at the bottom of the labor market and preserves negotiating power for indigenous workers in low-skill sectors. The constraint operates as a filtering gate that couples mobility to labor demand rather than welfare eligibility.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, domestic_labor_market_protection, beneficiary,
    institutional, biographical, analytical, continental).

% Interprets the scope of free movement rights in cases brought by migrants or member states. ECJ has narrowed member state exclusion authority through case law (restricting what counts as economically inactive, expanding social benefits as rights), creating ongoing tension between court-endorsed mobility and member state welfare protection. The observer seat is positioned above the constraint to adjudicate whether it complies with EU law.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, european_court_of_justice, observer,
    institutional, generational, analytical, continental).

% Monitors member state compliance with free movement principles and can initiate enforcement actions against discriminatory restrictions. It operates as a mediating institution between the ECJ's expansive mobility interpretation and member state welfare concerns, but typically tilts toward market integration and mobility. It sits between the constraint's enforcement (member state gatekeeping) and the overarching legal principle it serves.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, eu_commission, observer,
    institutional, generational, analytical, continental).

% Would object to restrictions on their citizens' mobility, but have no formal voice in the EU's free movement settlement. They lose productive workers through emigration and brain drain, but cannot negotiate the receiving states' welfare or employment criteria. Their exclusion from the rule-setting process means the constraint operates without their input, though they experience its consequences.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_governments, excluded,
    institutional, generational, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__member_sovereignty_reading, member_state_governments).
narrative_ontology:fixing_cost_class(federation_membership_kernel__member_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Couples labor mobility to economic contribution, preventing welfare system overload while maintaining some free movement. Solves the collective-action problem of sustaining mutual social insurance in an open border: if welfare access were decoupled from economic activity, receiving states would face fiscal pressure and incentive to exclude non-contributors; the constraint coordinates by enforcing economic gatekeeping.
% TRANSFER_FUNCTION: Transfers labor market access from economically inactive migrants to member states (in the form of fiscal protection), and from sending state workers (restricted mobility) to receiving state workers and welfare recipients (protected wages and social insurance). The constraint redirects the gains from unrestricted mobility toward welfare system sustainability.
% ABSENT_VOICES: Sending state governments and economically inactive migrants are formally excluded from the rule-making process. Sending state governments would argue for unrestricted mobility to reduce unemployment at home; migrants would argue for welfare access independent of economic activity. Their absence means the constraint is negotiated between receiving state governments and EU supranational institutions, not including the voices most harmed by restrictions.
% DISAPPEARANCE_RATIONALE: If this constraint vanished and free movement were fully unconditioned on economic activity, receiving state welfare systems would face immediate demand pressure; member states would likely introduce alternative gatekeeping (language requirements, residence fees, administrative delays) or withdraw from EU mobility commitments. Labor market wage pressure would increase at the bottom, and sending state brain drain would accelerate. The constraint's disappearance would trigger institutional reorganization around welfare access and labor market sorting.
% FOUNDING_PROBLEM: Early EU expansion to lower-income member states and post-2004 enlargement created tension between free movement commitments and welfare system sustainability: open borders without economic gatekeeping risked fiscally overwhelming receiving-state welfare systems and reducing indigenous wage-bargaining power. Member states needed authority to condition mobility on economic contribution to preserve social solidarity and fiscal sustainability.
% FOUNDING_PROBLEM_CORROBORATION: Member state governments and national labor unions attest the problem is still live, citing welfare demand and wage pressure. The ECJ has issued rulings narrowing member state exclusion authority, which the Commission supports—they attest the founding problem is overstated and that free movement benefits outweigh welfare costs. Independent academic analysis from welfare economists and labor scholars is split: some support member state concerns about fiscal sustainability; others argue welfare demand from EU migrants is minimal and the problem is more symbolic than fiscal.
narrative_ontology:disappearance_verdict(federation_membership_kernel__member_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__member_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__member_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_kernel__member_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__member_sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__member_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__member_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 (t=2024), reflecting the constraint's asymmetry: member state governments extract fiscal protection and labor market control from economically inactive migrants and sending state workers, who bear the cost of restricted mobility. The measurement series shows extractiveness rising from 0.52 (2004, post-enlargement baseline) to 0.68 (2024), driven by ECJ case law narrowing member state exclusion authority, which forces member states to defend exclusions more actively and more explicitly. Suppression rises from 0.54 to 0.71 over the same period, reflecting the increasing administrative apparatus required to enforce gatekeeping: member states have developed sophisticated tests for economic activity, sufficient resources, and public funds, administered through national courts and EU administrative bodies. Theater ratio rises from 0.28 to 0.42, indicating that a growing share of enforcement activity is devoted to justifying and defending restrictions rather than identifying economically active migrants. The constraint was born from genuine coordination need (prevent welfare overload in open borders) but has hardened into active defense of member state prerogative against ECJ expansion of mobility rights. The measurement series reflects this hardening: as ECJ interpretations narrow member state authority, member states invest more heavily in maintaining exclusion gates.
 *
 * PERSPECTIVAL GAP:
 *   From the member state agenda-setter seat, this constraint is genuine coordination: managing welfare demand and labor market pressure in an open border is a real problem, and the solution (condition mobility on economic activity) is rational and necessary. From the economically inactive migrant seat, the same structure operates as pure exclusion: the welfare protection it provides to receiving states is paid for entirely by those who are excluded, with no participation in the rule-setting. From the sending state worker seat, it is constrained mobility: the coordination function exists (welfare systems stay solvent), but the cost is borne disproportionately by workers from lower-income states. The engine computes per-seat classifications from power, exit options, and the structural beneficiary/victim positioning; this perspectival gap should emerge in divergence between the member state computed type and the migrant/sending-state computed type.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state governments occupy the agenda-setter seat with high power and mobile exit (institutional actors that could exit EU or renegotiate terms). Their directionality is near beneficiary—they collect welfare protection and labor market control. Economically inactive migrants occupy the target seat with powerless power and trapped exit; they have no alternative but to comply or leave EU territory entirely. Their directionality is near full target. Sending state workers are intermediate: they have some bargaining power (organized labor) but constrained exit (they can leave sending state for employment elsewhere in EU, or leave EU entirely, but not easily). Their directionality is near target-but-not-fully, reflecting constrained but not trapped exit. Receiving state welfare systems are beneficiary-positioned (they collect protection) but lack agency—they are institutional arrangements, not actors. The constraint's structure is asymmetric: member states set the rule and collect the benefit; migrants and sending state workers bear the cost. The engine computes directionality per seat from beneficiary/victim declarations and exit; the authored claim is tangled_rope (hybrid coordination/extraction) because the constraint performs both functions: it coordinates welfare system protection (genuine collective-action problem) and extracts from those restricted by the exclusion gates (asymmetric transfer).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-2004 enlargement welfare demand pressure) is contested in its persistence (founding_problem_status=contested). Member states argue the problem remains live; EU institutions argue it is overstated. The disappearance verdict is world_rearranges—if the constraint vanished, institutional reorganization would follow. The constraint shows rising theater ratio (0.28→0.42), indicating that enforcement activity is increasingly devoted to justifying and defending restrictions rather than implementing genuine gatekeeping. This pattern is consistent with mandatrophy: a constraint whose original function (prevent welfare overload) may be less pressing but whose extraction (member state labor market control) persists, necessitating theatrical performance to justify continued restriction. The measurement series supports this reading: extractiveness and suppression continue to rise even as welfare demand pressure has stabilized post-2015, suggesting that the mechanism persists beyond its founding problem. The constraint's classification remains tangled_rope (hybrid) rather than sliding to pure snare because the coordination function is real and necessary—welfare systems do depend on economic gatekeeping to avoid overload. But the rising theater ratio signals that extraction is becoming the constraint's primary function, with coordination increasingly subordinate to it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (welfare system overload from open migration post-2004) still materially live, or has it been structurally solved and the constraint now persists primarily as member state rent collection?',
    'Empirical analysis of welfare demand from EU migrants in receiving states, comparing predicted overload scenarios (2004) with actual outcomes (2024). If actual welfare demand is minimal relative to predicted, and member states continue to defend exclusions, the problem has been solved but the constraint persists.',
    'If the founding problem is dead, the classification shifts from tangled_rope (genuine coordination + extraction) toward snare (pure extraction with defensive narrative), triggering mandatrophy evaluation. If the founding problem is live, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the constraint''s founding problem persists or has been solved, leaving only extraction.').

omega_variable(
    member_sovereignty_vs_integration_foreclosure,
    'Do the member sovereignty reading and the integration reading foreclose each other (logically incompatible within one framework), or do they coexist as competing interpretations held by different institutional actors?',
    'Legal analysis: can a member state simultaneously recognize EU citizenship rights to free movement (integration reading core) and maintain authority to exclude economically inactive migrants (member sovereignty core)? The question is whether one framework can hold both principles or whether commitment to one forecloses the other.',
    'If the readings foreclose each other, the constraint is located at a fundamental choice point in EU constitutional architecture; if they coexist, the constraint is a negotiated compromise held together by institutional balance rather than logical unity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(member_sovereignty_vs_integration_foreclosure, conceptual, 'Whether the member sovereignty and integration readings logically foreclose each other or coexist as competing EU institutional readings.').

omega_variable(
    welfare_coordination_alternative,
    'Could the welfare coordination reading (EU-level anti-social-dumping rules + member state welfare design autonomy) operationally replace the member sovereignty reading''s gatekeeping, while preserving welfare system sustainability?',
    'Comparison with existing EU coordination mechanisms (social security coordination, minimum standards): are they sufficient to prevent welfare overload without explicit economic gatekeeping?',
    'If welfare coordination is sufficient, the member sovereignty reading''s extraction (restricted mobility) becomes unnecessary for its coordination function, and the constraint''s justification weakens. If coordination is insufficient, member sovereignty''s gatekeeping remains functionally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_coordination_alternative, empirical, 'Whether EU welfare coordination mechanisms could substitute for member state economic gatekeeping.').

omega_variable(
    brain_drain_cost_distribution,
    'Does the constraint''s restriction on sending state workers'' mobility actually reduce brain drain (by dampening incentives to emigrate), or merely redirect it (workers leave for non-EU destinations instead)?',
    'Time-series analysis of emigration patterns from low-income EU member states: do mobility restrictions in wealthy EU states correlate with reduced emigration to EU destinations, or merely shift emigration to non-EU countries?',
    'If restriction reduces brain drain, the constraint imposes a real cost on sending states'' human capital stock. If restriction merely redirects it, the cost is borne by destinations outside the EU rather than by sending states themselves, shifting the externality location.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brain_drain_cost_distribution, empirical, 'Whether mobility restrictions reduce or redirect brain drain from sending states.').

omega_variable(
    reading_committer_boundary,
    'Is the member sovereignty reading''s core commitment to member state authority over welfare access a genuine normative principle of the EU framework, or a strategic claim advanced by certain member state governments in particular periods?',
    'Historical/genealogical: trace the member sovereignty reading through EU treaty language, ECJ jurisprudence, and member state constitutional traditions. If the principle appears consistently across multiple sources and time periods, it is a genuine reading of the framework; if it appears only in strategic state assertions at particular moments, it is a reading advanced in dispute rather than a framework commitment.',
    'If the reading is genuine, it represents a legitimate interpretation of EU constitutional pluralism; if it is strategic, it is a reading claimed in dispute rather than grounded in framework commitments, and has lower normative authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_committer_boundary, conceptual, 'Whether member sovereignty is a genuine constitutional principle of the EU framework or a strategic claim in disputes over migration and welfare.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 2004, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t2004, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2004, 0.28).
narrative_ontology:measurement(fede_tr_t2008, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2008, 0.32).
narrative_ontology:measurement(fede_tr_t2012, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2012, 0.37).
narrative_ontology:measurement(fede_tr_t2016, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2016, 0.4).
narrative_ontology:measurement(fede_tr_t2020, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(fede_be_t2004, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2004, 0.52).
narrative_ontology:measurement(fede_be_t2008, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2008, 0.59).
narrative_ontology:measurement(fede_be_t2012, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2012, 0.64).
narrative_ontology:measurement(fede_be_t2016, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2016, 0.66).
narrative_ontology:measurement(fede_be_t2020, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(fede_be_t2024, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t2004, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2004, 0.54).
narrative_ontology:measurement(fede_su_t2008, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2008, 0.62).
narrative_ontology:measurement(fede_su_t2012, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2012, 0.68).
narrative_ontology:measurement(fede_su_t2016, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2016, 0.7).
narrative_ontology:measurement(fede_su_t2020, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(fede_su_t2024, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__member_sovereignty_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__member_sovereignty_reading, 0.18).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the federation_membership_kernel. The kernel is the EU's foundational commitment to both free movement and member state welfare system autonomy. The member_sovereignty_reading (this story) emphasizes member state authority to condition mobility on economic contribution; the integration_reading emphasizes expansive free movement as constitutive of EU citizenship; the welfare_coordination_reading seeks to reconcile both through EU-level social coordination. Each reading instantiates a different constraint with different ε, different beneficiary/victim structures, and different terminal classifications. The three readings coexist within EU institutional architecture, with different seats (member states, ECJ, Commission) advancing different readings. ε-invariance requires separate stories because the three readings substantially disagree on who bears costs and who captures benefits: the member sovereignty reading extracts from migrants and sending state workers; the integration reading treats mobility as a right with no cost-bearers; the welfare coordination reading distributes costs through EU-level mechanisms. Linking the three readings through network.affects_constraints enables analysis of how institutional contestation over the kernel affects the constraint's operation at each reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_kernel__member_sovereignty_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
