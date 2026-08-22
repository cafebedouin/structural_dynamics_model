% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: federation_membership_kernel__integration_reading
 *   human_readable: EU Free Movement as Constitutive Fundamental Right (Integration Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This story authors the integration reading of the federation membership
 *   kernel: free movement as a constitutive fundamental right of EU
 *   citizenship, interpreted expansively by the ECJ to maximize labor
 *   mobility and equal treatment. Under this reading's own lights, the
 *   standing arrangement (expansive ECJ jurisprudence overriding national
 *   labor-market and welfare-eligibility conditions) is the referent for
 *   extractiveness — not any bounded or coordination-only alternative. The
 *   2004/2007 enlargements are treated as the inflection point where the
 *   coordination function (single market completion) increasingly generated
 *   visible asymmetric costs: displaced local labor in receiving states,
 *   uncompensated welfare exposure, and externalized brain drain in sending
 *   states, while ECJ doctrine (Grzelczyk, Martinez Sala, Ruiz
 *   Zambrano-adjacent citizenship jurisprudence, and the broader
 *   proportionality-review posture toward national restrictions) hardened
 *   around the expansive interpretation, foreclosing member-state attempts to
 *   recalibrate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.58).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.62).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "EU Free Movement as Constitutive Fundamental Right (Integration Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, 'fa56c380-c8cf-44c5-bd85-d0af9ff470e4').
narrative_ontology:cs_kernel_codification('fa56c380-c8cf-44c5-bd85-d0af9ff470e4', fixed_text).
narrative_ontology:cs_authority_grounding('fa56c380-c8cf-44c5-bd85-d0af9ff470e4', lineage).
narrative_ontology:cs_interpretation_layer_present('fa56c380-c8cf-44c5-bd85-d0af9ff470e4').
narrative_ontology:cs_reading_relation('fa56c380-c8cf-44c5-bd85-d0af9ff470e4', federation_membership_kernel__member_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('fa56c380-c8cf-44c5-bd85-d0af9ff470e4', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('fa56c380-c8cf-44c5-bd85-d0af9ff470e4', foundational, eu_citizenship_constitutive_of_membership).
narrative_ontology:cs_axiom_status(eu_citizenship_constitutive_of_membership, holdable).
narrative_ontology:cs_axiom_grounding('fa56c380-c8cf-44c5-bd85-d0af9ff470e4', eu_citizenship_constitutive_of_membership, deontological).
narrative_ontology:cs_axiom('fa56c380-c8cf-44c5-bd85-d0af9ff470e4', foundational, supranational_proportionality_review_supersedes_national_discretion).
narrative_ontology:cs_axiom_status(supranational_proportionality_review_supersedes_national_discretion, holdable).
narrative_ontology:cs_axiom_grounding('fa56c380-c8cf-44c5-bd85-d0af9ff470e4', supranational_proportionality_review_supersedes_national_discretion, conventional).
narrative_ontology:cs_reference_frame('fa56c380-c8cf-44c5-bd85-d0af9ff470e4', treaty_market_integration_founding_text).
narrative_ontology:cs_drift_state('fa56c380-c8cf-44c5-bd85-d0af9ff470e4', post_enlargement_citizenship_jurisprudence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fa56c380-c8cf-44c5-bd85-d0af9ff470e4', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, multinational_employers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, single_market_integration_project).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, ecj_supranational_authority).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_origin_communities).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, national_labor_market_regulators).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, eu_citizenship_as_fundamental_status).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, single_market_completion_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, supranational_legal_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cross national borders within the EU to work, access equal-treatment protections, and claim social benefits under conditions ECJ jurisprudence has progressively equalized with nationals. Their mobility is the constraint's core justification and their gains are direct and visible.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, mobile_eu_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Draw on a continent-wide labor pool without the friction of visa regimes or work-permit quotas, allowing wage arbitrage across member states and flexible staffing. Free movement is treated as an input cost advantage they did not have to negotiate for themselves.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, multinational_employers, beneficiary,
    powerful, generational, arbitrage, continental).

% Interprets Treaty free-movement provisions expansively through case law (Viking, Laval progeny reversed in spirit, Dano notwithstanding, and the broader citizenship jurisprudence since Grzelczyk), extending equal-treatment entitlements and striking down national measures that condition benefits or labor protections on residency or nationality. It administers the doctrine and has full discretion to narrow or expand scope through subsequent rulings, but its institutional legitimacy is bound up with maximizing integration, so it rarely narrows.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, ecj_supranational_authority, agenda_setter,
    institutional, civilizational, analytical, continental).

% Compete for jobs, apprenticeships, and wage levels against an enlarged labor supply they did not vote to admit and cannot exit from — leaving their local labor market means abandoning family, home, and often the only labor market their skills transfer into. Bear downward wage pressure in low-skill and some mid-skill sectors without a seat in the ECJ's interpretive process.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, displaced_local_labor, payer,
    powerless, biographical, trapped, national).

% Fund unemployment benefits, healthcare, housing subsidies, and family benefits extended to mobile EU citizens under equal-treatment rulings, without receiving fiscal transfers from sending states to offset the cost. National governments cannot simply exclude EU migrants from welfare rolls without risking ECJ infringement proceedings, so they absorb the cost or attempt narrow administrative workarounds that get litigated and often struck down.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_welfare_systems, payer,
    institutional, generational, constrained, national).

% Lose working-age, often skilled residents to outward migration (brain drain), eroding local tax bases, healthcare staffing, and demographic stability, while the departed workers' productive years are captured by receiving economies. The origin community has no mechanism within the free-movement framework to be compensated for this loss.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_state_origin_communities, payer,
    moderate, generational, trapped, national).

% Design labor market protections, posted-worker rules, and welfare eligibility criteria intended to shield domestic labor conditions, only to see key provisions overridden or narrowly reinterpreted by ECJ rulings that treat national protective measures as presumptively suspect restrictions on free movement unless they survive strict proportionality review.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, national_labor_market_regulators, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__integration_reading, national_labor_market_regulators, excluded).

% Political scientists, legal scholars, and Eurobarometer survey designers who study whether free movement functions as promised — measuring wage effects, welfare fiscal flows, and public backlash — without being a party to the extraction or the benefit themselves.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, eu_citizens_analytical, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__integration_reading, diffuse).
narrative_ontology:fixing_cost_class(federation_membership_kernel__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Free movement solves a genuine coordination problem: without it, 27 separate national labor markets, work-permit regimes, and social-security systems would fragment the single market and prevent labor from flowing to where it is most productive, undermining the EU's foundational economic integration project.
% TRANSFER_FUNCTION: The arrangement moves labor supply and fiscal welfare burden from sending states (which lose workers and tax base) to receiving states (which gain workers and bear the marginal welfare cost) and from displaced local labor (which bears wage and employment competition) to mobile workers and the employers who hire them at lower net cost — mediated and locked in place by ECJ doctrine that forecloses national attempts to renegotiate the terms.
% ABSENT_VOICES: Displaced local low-wage workers and national labor regulators have no direct standing before the ECJ to argue that a specific ruling's proportionality balance undervalues domestic labor market protection; they are represented, if at all, through national governments litigating as defendants in infringement proceedings, which is a weaker and more diffuse form of voice than the direct rights-holder standing enjoyed by mobile citizens.
% DISAPPEARANCE_RATIONALE: If ECJ's expansive free-movement jurisprudence disappeared overnight and member states regained full discretion over labor market access and welfare eligibility, cross-border labor flows would likely contract sharply, multinational employers would face higher recruiting friction and localized wage-setting, receiving states could reintroduce residency-based welfare conditions, and the single market's labor-mobility pillar would need renegotiation — a substantial institutional and economic rearrangement, not a return to a natural baseline.
% FOUNDING_PROBLEM: Post-war Europe needed to prevent renewed continental fragmentation and enable the free flow of labor as one of the four foundational freedoms (goods, services, capital, labor) that the common market — later single market — was built on, to bind member state economies together and raise aggregate welfare through efficient labor allocation.
% FOUNDING_PROBLEM_CORROBORATION: The Commission and ECJ attest the founding problem remains live, citing continuing labor market fragmentation risk and enlargement dynamics. Independent labor economists studying wage effects in receiving-state low-skill sectors (e.g., post-2004 enlargement literature) and national auditors-general reviewing welfare fiscal exposure attest, from outside the beneficiary set, that the arrangement has shifted from solving cross-border coordination failure toward absorbing costs unilaterally onto specific receiving-state communities and sending-state origin regions without compensating mechanisms — a genealogy corroborated by parties who neither collect the mobility gains nor administer the doctrine.
narrative_ontology:disappearance_verdict(federation_membership_kernel__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__integration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.32 to 0.58 across the interval as enlargement expanded the pool of mobile labor and ECJ case law progressively narrowed the space for national conditionality on welfare access. Suppression rises faster and higher (0.35 to 0.62) because the mechanism of persistence is doctrinal: infringement proceedings and the supremacy of EU law over conflicting national provisions mean member states cannot simply legislate around unwelcome rulings — the enforcement machinery hardens over time as the jurisprudential corpus accretes precedent. Theater ratio stays comparatively low and only slowly rising (0.15 to 0.28) because the coordination function (actual labor mobility, actual single-market integration) remains substantially real throughout the period; this is not a hollowed-out performance, it is a genuine coordination structure carrying increasing extractive weight.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile EU workers and multinational employers sit near the beneficiary end: the former gain equal-treatment rights and labor market access, the latter gain a low-friction continental hiring pool. The ECJ is agenda_setter with analytical/institutional exit, deriving legitimacy from the very expansiveness it administers. Displaced local labor and national labor regulators sit near the target end: trapped or constrained exit, no standing in the interpretive process, bearing wage and policy-autonomy costs through the same doctrinal structure that delivers the coordination benefit. Receiving-state welfare systems and sending-state origin communities are institutional/moderate payers whose costs are diffuse but real and structurally unrecoverable within this reading's own framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war market fragmentation, labor misallocation across national borders) is genuinely partially live — labor mobility gains are real and documented — which is why this reading resists simple snare classification. But the founding-problem-status is authored as contested rather than live: independent labor economists and national auditors attest the arrangement now does more than solve the original coordination failure, it also absorbs costs onto communities with no compensating mechanism. Classifying this as tangled_rope (not snare, not rope) preserves both facts simultaneously: a genuine coordination function persists alongside asymmetric extraction sustained by active ECJ enforcement — exactly the hybrid the tangled_rope category exists to name, and exactly what a pure-extraction or pure-coordination label would mislabel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_or_asymmetric_transfer,
    'Is expansive ECJ free-movement jurisprudence better understood as completing a genuine single-market coordination function that happens to generate transitional friction, or as a mechanism that has shifted into a durable asymmetric transfer from displaced local labor and receiving-state welfare systems to mobile workers and multinational employers?',
    'Long-run panel analysis of wage effects, welfare fiscal exposure by member state, and brain-drain demographic indicators in sending regions, cross-referenced against ECJ docket composition (proportion of rulings narrowing vs. expanding national discretion) over the 1993-2024 interval.',
    'If the coordination function dominates, the classification would move toward rope with declining weight on the tangled component; if the transfer function dominates and coordination gains have plateaued, the classification would move toward snare as the coordination cover becomes vestigial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_or_asymmetric_transfer, empirical, 'Whether integration-reading free movement remains net coordination-positive or has become net extractive.').

omega_variable(
    kernel_framing_membership_versus_market_completion,
    'Is the kernel underlying this reading better modeled as a citizenship/membership commitment (EU citizenship as constitutive status, entitling equal treatment as a matter of political community) or as a market-completion commitment (labor mobility as an input to single-market efficiency, entitling equal treatment as an instrumental means)? The integration_reading''s own case law (Grzelczyk-line jurisprudence) increasingly frames free movement in citizenship terms rather than pure market-functional terms, which is a distinct legitimacy claim layered above the original Treaty market-integration text.',
    'Doctrinal history analysis distinguishing citizenship-grounded rulings from market-functional rulings across the ECJ''s free-movement case law corpus; track which framing predominates in the proportionality-review reasoning over time.',
    'If citizenship framing predominates, the axiom eu_citizenship_constitutive_of_membership is the operative foundational claim and the reading''s authority grounding leans toward a rights-based extraction logic; if market-completion framing predominates, the operative claim is closer to instrumental efficiency, which would weaken the deontological force of the equal-treatment entitlement against national welfare-capacity objections.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_membership_versus_market_completion, conceptual, 'Whether the integration reading''s kernel is citizenship-constitutive or market-completion-instrumental, and how that framing choice was made.').

omega_variable(
    fiscal_compensation_mechanism_absence,
    'Is the absence of a fiscal transfer mechanism between receiving and sending states (to offset welfare cost and brain drain respectively) a structural design gap that could be closed within the integration reading''s own framework, or is it constitutive of the reading — i.e., would adding such a mechanism effectively convert this into the welfare_coordination_reading?',
    'Comparative analysis of EU cohesion funds and structural funds as partial existing compensation mechanisms, assessed against the scale of documented welfare and brain-drain externalities to determine whether existing transfers are proportionate or nominal.',
    'If a compensation mechanism could be added without abandoning the expansive-rights premise, the tangled_rope classification''s extractive weight could be substantially reduced without foreclosing the integration reading; if the absence is constitutive, closing the gap would functionally migrate this story toward the welfare_coordination_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_compensation_mechanism_absence, conceptual, 'Whether uncompensated fiscal externalities are a fixable design gap or structurally definitional to this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1993, federation_membership_kernel__integration_reading, theater_ratio, 1993, 0.15).
narrative_ontology:measurement(fede_tr_t1998, federation_membership_kernel__integration_reading, theater_ratio, 1998, 0.17).
narrative_ontology:measurement(fede_tr_t2004, federation_membership_kernel__integration_reading, theater_ratio, 2004, 0.2).
narrative_ontology:measurement(fede_tr_t2010, federation_membership_kernel__integration_reading, theater_ratio, 2010, 0.23).
narrative_ontology:measurement(fede_tr_t2016, federation_membership_kernel__integration_reading, theater_ratio, 2016, 0.26).
narrative_ontology:measurement(fede_tr_t2020, federation_membership_kernel__integration_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_kernel__integration_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t1993, federation_membership_kernel__integration_reading, base_extractiveness, 1993, 0.32).
narrative_ontology:measurement(fede_be_t1998, federation_membership_kernel__integration_reading, base_extractiveness, 1998, 0.38).
narrative_ontology:measurement(fede_be_t2004, federation_membership_kernel__integration_reading, base_extractiveness, 2004, 0.47).
narrative_ontology:measurement(fede_be_t2010, federation_membership_kernel__integration_reading, base_extractiveness, 2010, 0.51).
narrative_ontology:measurement(fede_be_t2016, federation_membership_kernel__integration_reading, base_extractiveness, 2016, 0.55).
narrative_ontology:measurement(fede_be_t2020, federation_membership_kernel__integration_reading, base_extractiveness, 2020, 0.57).
narrative_ontology:measurement(fede_be_t2024, federation_membership_kernel__integration_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1993, federation_membership_kernel__integration_reading, suppression_requirement, 1993, 0.35).
narrative_ontology:measurement(fede_su_t1998, federation_membership_kernel__integration_reading, suppression_requirement, 1998, 0.4).
narrative_ontology:measurement(fede_su_t2004, federation_membership_kernel__integration_reading, suppression_requirement, 2004, 0.5).
narrative_ontology:measurement(fede_su_t2010, federation_membership_kernel__integration_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(fede_su_t2016, federation_membership_kernel__integration_reading, suppression_requirement, 2016, 0.6).
narrative_ontology:measurement(fede_su_t2020, federation_membership_kernel__integration_reading, suppression_requirement, 2020, 0.61).
narrative_ontology:measurement(fede_su_t2024, federation_membership_kernel__integration_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__integration_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__integration_reading, 0.12).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the federation_membership_kernel per the ε-invariance principle: the natural-language label 'EU free movement' covers structurally distinct claims about who bears authority to bound the right and how costs are allocated. integration_reading (this file) authors the expansive-ECJ-doctrine arrangement as tangled_rope with rising ε (0.32->0.58) driven by uncompensated welfare and brain-drain externalities under active enforcement. member_sovereignty_reading authors the same underlying Treaty text under a bounded-authority framing with a different beneficiary/victim structure (member states and domestic labor as beneficiaries of retained discretion). welfare_coordination_reading authors a middle framing emphasizing anti-social-dumping coordination with preserved member-state welfare autonomy, expected to show lower ε than this reading. Each story carries its own stable ε assessed by its own reading's lights; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
