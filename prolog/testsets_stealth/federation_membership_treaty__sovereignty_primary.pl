% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__sovereignty_primary, []).

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
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Member-State Consent Conditionality on Free Movement (Sovereignty-Primary Reading)
 *   domain: political economy/federalism/migration policy
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_primary reading of the
 *   federation_membership_treaty kernel: free movement operates as a
 *   consent-conditioned grant, with member states retaining authority to
 *   protect national labor markets and welfare systems. The standing
 *   arrangement under contest — and therefore the referent of epsilon — is
 *   the actual conditional-access regime: treaty-level movement rights hedged
 *   by transitional employment bans, residence registration, welfare
 *   eligibility tests, and expulsion powers, exercised and renewed by
 *   receiving states. Per the reading-indexed rule, the epsilon authored here
 *   is this arrangement as the sovereignty-primary reading assesses it: the
 *   reading credits much of the conditionality as legitimate self-government,
 *   and the metric reflects that discount rather than an abolition-of-borders
 *   baseline. The sibling readings (integration_primary,
 *   subsidiarity_balance) are separate constraints in separate files with
 *   their own epsilon, beneficiary/victim structures, and classifications;
 *   they are linked through the network, not folded into this one. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as tangled_rope
 *   on structural grounds while the metrics are authored from the
 *   arrangement's observed operation, and the engine measures any divergence.
 *
 * KEY AGENTS:
 *   - - member_state_governments: Agenda setter (institutional/arbitrage) — holds the consent gate, sets and renews admission conditions, demonstrated treaty-withdrawal exit
 *   - - domestic_labor_market_incumbents: Primary beneficiary (organized/constrained) — collects wage-bargaining and hiring-pipeline protection
 *   - - national_welfare_contributors: Secondary beneficiary (organized/constrained) — protected contributor pool behind eligibility tests
 *   - - mobile_workers: Primary target (moderate/constrained) — bears conditional access, waiting periods, and removal exposure
 *   - - cross_border_employers: Dual-positioned payer/beneficiary (powerful/mobile) — absorbs hiring friction while sharing single-market gains
 *   - - sending_state_governments: Excluded voice (institutional/constrained) — objects from outside the consent forum where decisions are taken
 *   - - eu_court_of_justice: Analytical observer (institutional/analytical) — adjudicates proportionality of national conditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.62).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.66).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Member-State Consent Conditionality on Free Movement (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political economy/federalism/migration policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, 'ab9d72c3-f877-4736-8e71-b3194d61cdf5').
narrative_ontology:cs_kernel_codification('ab9d72c3-f877-4736-8e71-b3194d61cdf5', fixed_text).
narrative_ontology:cs_authority_grounding('ab9d72c3-f877-4736-8e71-b3194d61cdf5', lineage).
narrative_ontology:cs_interpretation_layer_present('ab9d72c3-f877-4736-8e71-b3194d61cdf5').
narrative_ontology:cs_reading_relation('ab9d72c3-f877-4736-8e71-b3194d61cdf5', federation_membership_treaty__integration_primary, forecloses).
narrative_ontology:cs_reading_relation('ab9d72c3-f877-4736-8e71-b3194d61cdf5', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('ab9d72c3-f877-4736-8e71-b3194d61cdf5', foundational, movement_right_is_consent_derived).
narrative_ontology:cs_axiom_status(movement_right_is_consent_derived, holdable).
narrative_ontology:cs_axiom_grounding('ab9d72c3-f877-4736-8e71-b3194d61cdf5', movement_right_is_consent_derived, conventional).
narrative_ontology:cs_axiom('ab9d72c3-f877-4736-8e71-b3194d61cdf5', foundational, national_welfare_boundary_is_sovereign_reserve).
narrative_ontology:cs_axiom_status(national_welfare_boundary_is_sovereign_reserve, holdable).
narrative_ontology:cs_axiom_grounding('ab9d72c3-f877-4736-8e71-b3194d61cdf5', national_welfare_boundary_is_sovereign_reserve, deontological).
narrative_ontology:cs_reference_frame('ab9d72c3-f877-4736-8e71-b3194d61cdf5', sovereign_compact_reserved_powers).
narrative_ontology:cs_drift_state('ab9d72c3-f877-4736-8e71-b3194d61cdf5', contemporary_post_brexit_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ab9d72c3-f877-4736-8e71-b3194d61cdf5', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, member_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, domestic_labor_market_incumbents).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_welfare_contributors).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, mobile_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, cross_border_employers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, cross_border_employers).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, state_consent_conditionality).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, national_welfare_sustainability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and renew the conditions under which other states' citizens may enter their labor markets and welfare systems: transitional hiring bans after enlargements, residence registration, welfare eligibility waiting periods, public-policy expulsion powers, and negotiated opt-outs. Their consent is the gate the arrangement runs on, and they demonstrated at least one full exit path by withdrawing from the movement regime entirely.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, member_state_governments, agenda_setter,
    institutional, generational, arbitrage, continental).

% Workers and firms already established inside a national labor market. The conditions on incoming competitors shield their wage bargaining and hiring pipelines at the margin, particularly in trades and regions with concentrated inflows. The protection is ambient — they cannot decline it individually, and their political voice defends it.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, domestic_labor_market_incumbents, beneficiary,
    organized, biographical, constrained, national).

% Current contributors to contributory insurance and tax-financed benefit systems. Eligibility rules reserve benefits behind contribution histories and lawful-residence tests, keeping the claims pool matched to the contributing population. They carry the fiscal risk the eligibility machinery manages and consistently poll in favor of retention.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_welfare_contributors, beneficiary,
    organized, biographical, constrained, national).

% Citizens of one member state who take up work in another. They encounter registration duties, multi-year bans on formal employment during transitional control periods, waiting periods before benefit eligibility, and exposure to removal if they become economically inactive. Their livelihood sits where the work is, so leaving the host state means abandoning accumulated employment, housing, and family settlement; the realistic alternative is returning home or moving to a third state that consents on easier terms.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, mobile_workers, payer,
    moderate, biographical, constrained, continental).

% Firms that hire across borders. During control periods they face recruitment bans and sponsorship friction that raise labor costs; they lobby against extensions and litigate through industry associations. At the same time they operate inside the same single-market framework and benefit from the predictability the consent architecture gives national regulators, and they can relocate production to whichever state's terms suit them.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, cross_border_employers, payer,
    powerful, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__sovereignty_primary, cross_border_employers, beneficiary).

% Governments of states with high emigration. Their nationals bear the admission conditions, and the states collect remittances and demographic pressure relief from the flows. When a receiving state tightens or extends controls, they object diplomatically and in council formations, but the decision itself is taken in the receiving state's own forum, where they hold no vote.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, sending_state_governments, excluded,
    institutional, generational, constrained, continental).

% Adjudicates whether national conditions respect the treaty framework: strikes down residence tests it finds discriminatory, upholds conditions it finds proportionate, and thereby draws the line between permissible consent conditions and impermissible barriers. It collects nothing from the arrangement and pays nothing into it.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, eu_court_of_justice, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__sovereignty_primary, domestic_labor_market_incumbents).
narrative_ontology:fixing_cost_class(federation_membership_treaty__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Paces and distributes labor mobility so host labor markets and contributory welfare systems absorb newcomers at rates their institutions can process, and maintains mutual trust between differently situated welfare states by letting each bound its own redistributive community.
% TRANSFER_FUNCTION: Moves labor-market access and welfare eligibility from mobile workers to domestic constituencies as conditional grants, and moves authority over admission from the union level to individual member states.
% ABSENT_VOICES: Mobile workers and would-be movers deterred before ever relocating have no seat in the national forums where conditions are set; sending-state communities losing diaspora ties and employers priced out of labor supply object from outside. They are absent because the consent mechanism is exercised unilaterally by the receiving state.
% DISAPPEARANCE_RATIONALE: If the consent gate vanished overnight, labor allocation across the union would reorganize within months, contributory benefit pools would face immediate eligibility claims from newly arrived workers, domestic wage structures in gateway regions would adjust, and the receiving states' registration and removal machinery would lose its object.
% FOUNDING_PROBLEM: Post-war reconstruction and successive enlargements required managing labor flows between economies at very different income levels while preserving domestic social contracts built on contributory insurance; states needed assurance that open borders would not overwhelm welfare budgets or destabilize wages faster than institutions could adapt.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: European Commission impact assessments accompanying each enlargement's transitional arrangements, peer-reviewed labor economics on the 2004 accession wave's wage and fiscal effects, and OECD migration outlooks all attest that absorption capacity and contributory-pool integrity remain binding concerns. Mobile-worker advocacy organizations dispute the magnitude of the strain but concede it is real in concentrated localities.
narrative_ontology:disappearance_verdict(federation_membership_treaty__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_treaty__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__sovereignty_primary, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the transfer from mobile workers is substantial — transitional employment bans ran up to seven years after the 2004 and 2007 enlargements, welfare waiting periods and habitual-residence tests gate benefits, and economically inactive movers face removal — but the sovereignty-primary reading discounts a large share as the legitimate price of bounding a contributory community, so epsilon sits well below snare territory. Suppression 0.66 is a raw structural property, unscaled by power or scope in the engine's computation: the arrangement persists through active machinery (registration regimes, labor-market tests, removal proceedings, reinstated internal border controls), not through voluntary assent alone. Theater ratio 0.46: a growing share of enforcement performs control for domestic audiences — benefit-tourism measures aimed at a fiscally marginal population — while the core gating functions continue to operate; the rising series tracks that rhetorical substitution. Accessibility collapse 0.32: alternatives persist (remain home, move to a consenting state, employer sponsorship, judicial challenge), so understanding the constraint does not close the option set. Resistance 0.55: court litigation has struck down discriminatory tests, sending states object diplomatically, and employer associations lobby against extensions. The three temporal series share one six-point grid (T0-T20, mapped to 2004-2024, the period when consent conditionality was most actively exercised); the suppression_requirement series is authored because this story specifically tracks enforcement-capacity build-up, which the scalar base_properties.suppression cannot show.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute divergent classifications from identical structural data. From the member-state government's position the arrangement is self-authored coordination it built, administers, and can exit by treaty withdrawal — effective extraction at that seat is damped toward subsidy. From the mobile worker's position the same structure operates as enforced conditionality on livelihood. The two institutional seats at nominally equal power diverge sharply: receiving-state governments administer the gate while sending-state governments stand outside it, so identical global standing yields opposite directionalities — differentiated entirely by which side of the consent mechanism an actor occupies. The excluded seat registers no voice at all, which the consensus-provenance check reads as signal rather than silence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: member_state_governments, domestic_labor_market_incumbents, and national_welfare_contributors sit near the beneficiary end (low d, damped or inverted effective extraction), with incumbents and contributors anchored there by constrained exit — the protection is ambient and they cannot decline it. Victim declarations drive the targets: mobile_workers sit near the full-target end, pushed further by constrained exit (livelihood binds them to the host state). Cross_border_employers derive a middling d: victim-listed for hiring friction but pulled toward the beneficiary side by mobile exit and their dual role. Sending_state_governments and eu_court_of_justice hold excluded and observer roles respectively and feed no extraction arithmetic. Scope amplification is the engine's business: the continental scope of the arrangement modestly raises effective extraction at target seats because verification of proportionate use is harder at that scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim keeps both halves of the structure visible and prevents symmetric mislabeling. A pure-extraction reading would erase the genuine coordination function — pacing absorption and bounding contributory pools is a real collective-action problem, independently corroborated by Commission assessments and labor economics rather than only by the arrangement's defenders. A pure-coordination reading would erase the asymmetric transfer: mobile workers pay through the same structure that pays domestic incumbents, and the enforcement machinery exists to hold that asymmetry in place. Mandatrophy is not resolved: the founding problem (income-differential flows against contributory national welfare systems) remains live, so the arrangement is not a vestige performing a dead function — the theater_ratio rise is a symptom layered onto functioning machinery, not evidence the machinery itself is dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is the sovereignty_primary reading of the federation_membership_treaty kernel — what would the sibling readings change structurally?',
    'Comparison against the sibling stories: integration_primary removes mobile_workers from the victim set (they become rights-holders) and flips restrictions to presumptively illegitimate, collapsing epsilon toward the coordination floor; subsidiarity_balance replaces the consent gate with proportionality review, converting member_state_governments from agenda_setter to constrained administrator.',
    'If a sibling reading were adopted as the operative constraint, the beneficiary/victim structure inverts or dissolves and the classification recomputes from scratch; this file''s verdicts are valid only within the sovereignty_primary instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer-frame routing: one kernel, three readings, this file carries exactly one.').

omega_variable(
    source_of_right_locus,
    'Where exactly do the readings disagree — is the free-movement right pre-political and constitutive of the union, or delegated by sovereign states and revocable by the same consent that granted it?',
    'Doctrinal analysis of which premise courts and treaty practice treat as load-bearing: whether national conditions are exceptions requiring justification inside a prior right, or exercises of reserved sovereignty defining the right''s scope ab initio.',
    'Resolving the locus determines which reading''s axiom set survives contact with the treaty text and whether the sovereignty reading''s consent premise or the integration reading''s constitutive premise anchors the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(source_of_right_locus, conceptual, 'The specific structural element on which the sibling readings diverge.').

omega_variable(
    fiscal_impact_dispute,
    'Do intra-EU mobile workers impose net fiscal costs that justify welfare conditionality, or are they net contributors whose gating is solution-seeking?',
    'Administrative-data studies linking tax and contribution records to benefit claims by origin cohort across host states, controlling for age structure and lifecycle stage.',
    'If mobile workers are robustly net contributors, the welfare-conditionality component of epsilon is extraction without coordination warrant and the theater_ratio understates performative enforcement; if costs concentrate in specific cohorts or localities, part of the gating earns its coordination keep.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_impact_dispute, empirical, 'Whether the welfare half of the consent gate tracks a real fiscal externality.').

omega_variable(
    absorption_capacity_locality,
    'Does concentrated local strain (gateway towns absorbing disproportionate inflows into housing, schools, and primary care) justify national-level gating even where aggregate effects are positive?',
    'Sub-national service-capacity and wage data in high-inflow localities versus national aggregates; natural experiments from transitional-control expiry dates.',
    'If strain is real and localized, the coordination function is genuine but mis-scoped (national gating for a local problem), which would support a subsidiarity-style remedy rather than validating the sovereignty reading''s national gate; if strain is negligible everywhere, the coordination half thins toward cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absorption_capacity_locality, empirical, 'Whether the coordination function survives disaggregation below the national level.').

omega_variable(
    consent_authority_durability,
    'Is member-state consent authority a durable feature of the political structure, or a contingent coalition position that decays as generational attitudes toward mobility shift?',
    'Longitudinal attitude surveys crossed with actual renewal votes on transitional controls and eligibility rules across successive political cycles.',
    'If durability is low, the arrangement drifts toward scaffold-like transience despite lacking a sunset clause; if high, the tangled_rope classification is stable across the observable future and enforcement intensification continues.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_authority_durability, empirical, 'Persistence question bearing on lifecycle drift direction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__sovereignty_primary, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t4, federation_membership_treaty__sovereignty_primary, theater_ratio, 4, 0.28).
narrative_ontology:measurement_basis(fede_tr_t4, observed).
narrative_ontology:measurement(fede_tr_t8, federation_membership_treaty__sovereignty_primary, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(fede_tr_t8, observed).
narrative_ontology:measurement(fede_tr_t12, federation_membership_treaty__sovereignty_primary, theater_ratio, 12, 0.39).
narrative_ontology:measurement_basis(fede_tr_t12, observed).
narrative_ontology:measurement(fede_tr_t16, federation_membership_treaty__sovereignty_primary, theater_ratio, 16, 0.43).
narrative_ontology:measurement_basis(fede_tr_t16, observed).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__sovereignty_primary, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(fede_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__sovereignty_primary, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t4, federation_membership_treaty__sovereignty_primary, base_extractiveness, 4, 0.47).
narrative_ontology:measurement_basis(fede_be_t4, observed).
narrative_ontology:measurement(fede_be_t8, federation_membership_treaty__sovereignty_primary, base_extractiveness, 8, 0.51).
narrative_ontology:measurement_basis(fede_be_t8, observed).
narrative_ontology:measurement(fede_be_t12, federation_membership_treaty__sovereignty_primary, base_extractiveness, 12, 0.56).
narrative_ontology:measurement_basis(fede_be_t12, observed).
narrative_ontology:measurement(fede_be_t16, federation_membership_treaty__sovereignty_primary, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(fede_be_t16, observed).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__sovereignty_primary, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(fede_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__sovereignty_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t4, federation_membership_treaty__sovereignty_primary, suppression_requirement, 4, 0.48).
narrative_ontology:measurement_basis(fede_su_t4, observed).
narrative_ontology:measurement(fede_su_t8, federation_membership_treaty__sovereignty_primary, suppression_requirement, 8, 0.52).
narrative_ontology:measurement_basis(fede_su_t8, observed).
narrative_ontology:measurement(fede_su_t12, federation_membership_treaty__sovereignty_primary, suppression_requirement, 12, 0.57).
narrative_ontology:measurement_basis(fede_su_t12, observed).
narrative_ontology:measurement(fede_su_t16, federation_membership_treaty__sovereignty_primary, suppression_requirement, 16, 0.62).
narrative_ontology:measurement_basis(fede_su_t16, observed).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__sovereignty_primary, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(fede_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__sovereignty_primary, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'EU free movement' decomposes into three structurally distinct readings of one treaty kernel, linked per the epsilon-invariance principle. This file carries sovereignty_primary; integration_primary and subsidiarity_balance carry their own epsilon, beneficiary/victim structures, and claimed types. The shared upstream referent is the treaty text itself; each reading cites it as warrant, so this reading's consent doctrine exerts structural pressure on the subsidiarity reading's balancing exercise (every renewed control shifts the line the balancer must strike). Epsilon differs across the family because the beneficiary/victim structure differs: mobile_workers are victims under this reading, rights-holders under integration_primary, and bounded rights-holders under subsidiarity_balance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
