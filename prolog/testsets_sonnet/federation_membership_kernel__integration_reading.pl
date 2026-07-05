% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__integration_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: federation_membership_kernel__integration_reading
 *   human_readable: EU Free Movement as Constitutive Fundamental Right (Integration Reading)
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This story instantiates the integration reading of the federation
 *   membership kernel: free movement as a fundamental right constitutive of
 *   EU citizenship, with the ECJ as the interpretive authority that has
 *   expanded its scope well beyond the original market-access rationale
 *   (Maastricht citizenship, Baumbast, Zambrano, Dano-line cases and their
 *   aftermath) to cover economically inactive persons, family reunification,
 *   and near-equal welfare access. Two sibling readings of the same
 *   underlying kernel exist as separate constraints:
 *   member_sovereignty_reading (bounding free movement by national welfare
 *   capacity and labor protection) and welfare_coordination_reading
 *   (coordination of national systems without harmonization, preserving
 *   welfare design autonomy while enforcing anti-social-dumping rules). This
 *   story does not describe or average across those readings — it is the
 *   clean instantiation of the expansive-interpretation position, with its
 *   own stable ε reflecting the extraction that specifically follows FROM
 *   that expansive reading (displaced local labor, uncompensated
 *   receiving-state fiscal burden, externalized sending-state brain drain,
 *   and treaty-override of national labor protections). The sibling readings
 *   carry structurally different beneficiary/victim sets and would compute
 *   different ε values; they are separate files linked via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.58).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.61).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "EU Free Movement as Constitutive Fundamental Right (Integration Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, '006947e0-beac-465a-8670-ae7dca01e1c5').
narrative_ontology:cs_kernel_codification('006947e0-beac-465a-8670-ae7dca01e1c5', fixed_text).
narrative_ontology:cs_authority_grounding('006947e0-beac-465a-8670-ae7dca01e1c5', lineage).
narrative_ontology:cs_interpretation_layer_present('006947e0-beac-465a-8670-ae7dca01e1c5').
narrative_ontology:cs_reading_relation('006947e0-beac-465a-8670-ae7dca01e1c5', federation_membership_kernel__member_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('006947e0-beac-465a-8670-ae7dca01e1c5', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('006947e0-beac-465a-8670-ae7dca01e1c5', foundational, free_movement_constitutive_of_citizenship).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('006947e0-beac-465a-8670-ae7dca01e1c5', free_movement_constitutive_of_citizenship, conventional).
narrative_ontology:cs_axiom('006947e0-beac-465a-8670-ae7dca01e1c5', foundational, supranational_interpretive_supremacy_over_national_labor_protection).
narrative_ontology:cs_axiom_status(supranational_interpretive_supremacy_over_national_labor_protection, holdable).
narrative_ontology:cs_axiom_grounding('006947e0-beac-465a-8670-ae7dca01e1c5', supranational_interpretive_supremacy_over_national_labor_protection, conventional).
narrative_ontology:cs_reference_frame('006947e0-beac-465a-8670-ae7dca01e1c5', maastricht_citizenship_settlement).
narrative_ontology:cs_drift_state('006947e0-beac-465a-8670-ae7dca01e1c5', post_dano_welfare_litigation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('006947e0-beac-465a-8670-ae7dca01e1c5', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, cross_border_employers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, single_market_integration_project).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, sending_state_remittance_flows).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_skill_base).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, eu_citizenship_constitutive_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, single_market_completion_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Treaty free movement provisions expansively through case law, extending protections to economically inactive citizens, family members, and cross-border service recipients. Rulings bind national courts and legislatures with direct effect and supremacy, overriding domestic labor market and welfare eligibility rules it finds incompatible with free movement. Sets the scope of the right itself through successive judgments rather than treaty amendment.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, european_court_of_justice, agenda_setter,
    institutional, civilizational, analytical, continental).

% Move across borders to access higher wages, better employment conditions, or family reunification, protected by equal treatment guarantees and non-discrimination on grounds of nationality. Their mobility is precisely what the constraint exists to secure; exit from any one national labor market is a live option because the right guarantees entry to all others.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, mobile_eu_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Draw on a continent-wide labor pool without the friction of work-permit regimes, able to source labor where wages are lowest and relocate operations to exploit wage differentials the free movement right helps sustain. Can arbitrage between national labor markets in ways individual workers and states cannot.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, cross_border_employers, beneficiary,
    organized, generational, arbitrage, continental).

% Compete for jobs, wages, and public housing against an enlarged labor pool they had no vote in admitting; local wage compression and job displacement in low-skill sectors are attributed by receiving-state authorities to structural change rather than the free movement regime itself. Have no meaningful exit — leaving the national labor market is not a remedy available to those without capital or transferable credentials, and no compensatory mechanism exists at EU level.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, displaced_local_labor, payer,
    powerless, biographical, trapped, national).

% Bear the fiscal cost of extending social benefits, healthcare, and education access to mobile citizens under equal-treatment rulings that have progressively narrowed the grounds on which states may restrict access, without any corresponding EU fiscal transfer to offset the cost. Cannot unilaterally restrict eligibility without risking infringement proceedings or ECJ censure; national parliaments retain nominal welfare design authority but operate under a ceiling set by Luxembourg jurisprudence.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_welfare_systems, payer,
    institutional, generational, constrained, national).

% Loses working-age, often skilled or working-age population to outward migration, eroding the tax base and workforce needed to sustain domestic public services and pension systems. This population has no institutional voice in the sending state once departed, and the sending state has no mechanism to recoup training investment or slow the outflow without violating the same free movement right.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_state_skill_base, payer,
    powerless, generational, trapped, national).

% The completion of the single market as a political and economic project is advanced each time free movement scope is broadened; deepened integration is treated as a self-justifying good by the institutions that administer it, independent of any single national constituency's assessment of costs and benefits.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, single_market_integration_project, beneficiary,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(federation_membership_kernel__integration_reading, single_market_integration_project).

% Nominally co-authors of the treaties, but once ECJ interpretation expands scope beyond what was negotiated, individual member states cannot unilaterally reverse it — amendment requires unanimity among 27 states. Their electorates' objections to specific applications (welfare tourism concerns, labor market pressure) are voiced in domestic politics but have no direct channel into the Court's interpretive process.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, national_governments, excluded,
    institutional, biographical, constrained, national).

% Study wage and employment effects of intra-EU migration across receiving and sending regions, producing contested empirical findings on the net distributional effects that inform, but do not bind, both national policy debate and rounds of ECJ litigation.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, comparative_labor_economists, observer,
    analytical, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a genuinely single labor market across member states: firms can hire without national permit friction, workers can move to where their skills are valued, and a shared citizenship status underwrites cross-border equal treatment that would otherwise require bilateral treaties between every pair of states.
% TRANSFER_FUNCTION: Moves labor supply from lower-wage to higher-wage member states, moves fiscal welfare costs from sending states (which invested in the migrant's education and training) to receiving states (which did not), and moves wage and employment pressure from mobile workers and employers onto displaced local labor and the receiving state's welfare and public-service budgets.
% ABSENT_VOICES: Displaced local labor in receiving states and left-behind communities in sending states have no seat in ECJ proceedings, which turn on treaty interpretation and case facts between named litigants; their interests are represented, if at all, indirectly through member state government submissions, which the Court is not bound to accept.
% DISAPPEARANCE_RATIONALE: If the expansive free movement right disappeared overnight, national labor markets and welfare systems would re-fragment along pre-1992 lines: work permits, residence conditions, and welfare eligibility tests would return, cross-border employers would lose frictionless labor sourcing, and millions of mobile EU citizens currently resident under free movement status would face legal uncertainty about continued residence and benefit access.
% FOUNDING_PROBLEM: Post-war Europe needed to dismantle protectionist national labor markets to complete a common market, and to give the emerging supranational polity a citizenship content beyond trade in goods — free movement of persons was the mechanism chosen to make EU membership tangible to individuals, not just states.
% FOUNDING_PROBLEM_CORROBORATION: The Commission, the Court, and mobile-worker advocacy groups attest the founding problem (market fragmentation, incomplete citizenship) remains live and requires continued expansive interpretation. Independent labor economists and several national court references to the ECJ (e.g. on welfare eligibility for economically inactive citizens) document that the current scope now extends substantially beyond the market-completion rationale into general redistribution, a extension the original single-market founding problem does not itself justify — this corroboration comes from national judiciaries and academic economists outside the Commission/Court institutional axis that benefits from continued expansion.
narrative_ontology:disappearance_verdict(federation_membership_kernel__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__integration_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_kernel__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__integration_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-to-substantial (0.58 by 2024) and has risen steadily as ECJ jurisprudence has widened scope generation over generation of case law, each ruling entrenching the prior one via precedent (stare decisis in all but name for EU law). Suppression (0.61) reflects that member states cannot reverse individual rulings without treaty-level unanimity — a near-total lock-in once the Court has spoken, which is why suppression tracks upward alongside extraction rather than remaining flat: each expansive ruling forecloses a previously available national policy lever. Theater ratio is low (0.22) because the coordination function (a genuinely operative single labor market) is real and substantial, not decorative; the extraction rides on top of, not instead of, a functioning coordination mechanism, which is precisely the tangled-rope signature. Accessibility collapse is moderate (0.5): workable bounded alternatives (bilateral labor agreements, quota systems) existed and still exist in adjacent international contexts, so alternatives have not collapsed as completely as under a mountain claim.
 *
 * PERSPECTIVAL GAP:
 *   From the ECJ's own analytical seat, expansive interpretation is simply fidelity to Treaty text and the logic of citizenship — coordination, not extraction. From displaced local labor's trapped seat, the same jurisprudence is an externally imposed cost with no democratic mandate reachable through ordinary national politics. The engine should compute these as different seat-level classifications from the same structural data; the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile EU workers and cross-border employers sit near the beneficiary end: the right was built to serve their mobility and its exercise is the constraint's stated purpose. Displaced local labor and sending-state skill base sit near the full-target end — trapped exit options, no institutional voice, and no compensatory transfer mechanism. Receiving-state welfare systems are institutional payers: they retain formal authority to design benefits but operate under a real ceiling set by ECJ interpretation, so their exit option is 'constrained' rather than 'trapped' — they can still adjust policy at the margin, just not reverse the core entitlement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (market fragmentation, thin EU citizenship) was real in 1993 and remains partially live — genuine labor market integration benefits exist and are not simply captured rent. But the founding-problem-status is contested: the current scope of the right, extending into general welfare redistribution for the economically inactive, exceeds what market-completion alone would justify, and this drift is corroborated by sources (national courts, independent economists) outside the Commission/Court axis that benefits from continued expansion. This is exactly the tangled-rope signature — the coordination function has not disappeared, but extraction has accumulated on top of it, and neither pure Mountain (natural, uncontested) nor pure Snare (no genuine coordination function) fits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_completion_vs_general_redistribution_boundary,
    'Does the current scope of free movement (covering economically inactive citizens'' near-equal welfare access) remain justified by the single-market-completion rationale that founded the right, or has ECJ interpretation extended it into a general redistributive citizenship right the original Treaty framework did not contemplate?',
    'Doctrinal and empirical analysis tracing the line of cases from Maastricht citizenship through Dano and its progeny, cross-referenced against Treaty drafting history and the economic-integration rationale actually articulated in the founding texts, plus fiscal-transfer data showing whether receiving states have in fact been compensated for the welfare costs of inbound mobile citizens.',
    'If the extension is not supported by the founding rationale, the integration reading''s coordination function is narrower than claimed and a larger share of its measured extraction is properly attributed to judicial policy-making rather than market completion — this would push the classification further toward snare at the margin; if supported, the tangled-rope classification with a substantial genuine coordination core is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_completion_vs_general_redistribution_boundary, conceptual, 'Whether current ECJ scope remains within or has exceeded the market-completion founding rationale.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the integration reading''s expansive ECJ jurisprudence FORECLOSE the member_sovereignty_reading entirely, or does it merely narrow the sovereignty reading''s practical scope while leaving its legal argument live?',
    'Track whether member states retain ANY unilateral exclusion authority post-ruling (e.g. emergency brake mechanisms, transition periods for new member states) — if some sovereignty-preserving mechanisms survive even expansive ECJ jurisprudence, the relationship is influences/narrows rather than a clean foreclosure.',
    'If genuinely forecloses, member_sovereignty_reading becomes a purely aspirational or pre-integration position with no live legal purchase; if only narrows, both readings remain simultaneously arguable positions in ongoing litigation and treaty negotiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether the integration and sovereignty readings can coexist as live legal positions or whether one has fully displaced the other.').

omega_variable(
    fiscal_compensation_mechanism_absence,
    'Is the absence of an EU-level fiscal transfer mechanism compensating receiving states for welfare costs and sending states for skill-base loss a deliberate design choice consistent with the integration project''s political feasibility constraints, or a structural gap that undermines the coordination claim?',
    'Compare with federal systems (e.g. US interstate migration) that DO have fiscal equalization mechanisms accompanying free internal mobility, and assess whether the absence of an EU equivalent is politically contingent (blocked by net-contributor states) or structurally necessary to the current EU fiscal architecture.',
    'If the absence is a contingent political choice rather than a structural necessity, it strengthens the case that the current extraction level is not an inherent cost of coordination but an avoidable distributional choice — supporting classification toward the extractive end of tangled_rope rather than toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_compensation_mechanism_absence, empirical, 'Whether the lack of fiscal compensation for free movement''s costs is structurally necessary or a contingent political failure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1993, federation_membership_kernel__integration_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(fede_tr_t1999, federation_membership_kernel__integration_reading, theater_ratio, 1999, 0.12).
narrative_ontology:measurement(fede_tr_t2005, federation_membership_kernel__integration_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(fede_tr_t2011, federation_membership_kernel__integration_reading, theater_ratio, 2011, 0.18).
narrative_ontology:measurement(fede_tr_t2017, federation_membership_kernel__integration_reading, theater_ratio, 2017, 0.2).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_kernel__integration_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(fede_be_t1993, federation_membership_kernel__integration_reading, base_extractiveness, 1993, 0.32).
narrative_ontology:measurement(fede_be_t1999, federation_membership_kernel__integration_reading, base_extractiveness, 1999, 0.38).
narrative_ontology:measurement(fede_be_t2005, federation_membership_kernel__integration_reading, base_extractiveness, 2005, 0.44).
narrative_ontology:measurement(fede_be_t2011, federation_membership_kernel__integration_reading, base_extractiveness, 2011, 0.5).
narrative_ontology:measurement(fede_be_t2017, federation_membership_kernel__integration_reading, base_extractiveness, 2017, 0.55).
narrative_ontology:measurement(fede_be_t2024, federation_membership_kernel__integration_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1993, federation_membership_kernel__integration_reading, suppression_requirement, 1993, 0.35).
narrative_ontology:measurement(fede_su_t1999, federation_membership_kernel__integration_reading, suppression_requirement, 1999, 0.42).
narrative_ontology:measurement(fede_su_t2005, federation_membership_kernel__integration_reading, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(fede_su_t2011, federation_membership_kernel__integration_reading, suppression_requirement, 2011, 0.53).
narrative_ontology:measurement(fede_su_t2017, federation_membership_kernel__integration_reading, suppression_requirement, 2017, 0.58).
narrative_ontology:measurement(fede_su_t2024, federation_membership_kernel__integration_reading, suppression_requirement, 2024, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__integration_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__integration_reading, 0.12).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the federation_membership_kernel. The integration_reading (this file) treats free movement as an expansively-interpreted fundamental right with the ECJ as supranational scope-setter, yielding a beneficiary set (mobile workers, cross-border employers, the integration project) and victim set (displaced local labor, receiving-state welfare systems, sending-state skill base) distinct from its siblings. member_sovereignty_reading inverts the authority structure (member states retain exclusion authority) and would classify differently — likely with a smaller or differently composed victim set centered on excluded would-be migrants rather than displaced local labor. welfare_coordination_reading occupies a middle institutional architecture (coordination without harmonization) and would show lower extraction and lower suppression than this reading, since it preserves more national policy discretion. All three share the same underlying kernel (the constitutional status of free movement within EU membership) but instantiate structurally distinct constraints with different ε values, consistent with the ε-invariance principle — they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
