% ============================================================================
% CONSTRAINT STORY: liability_attribution__developer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__developer_liability, []).

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
 *   constraint_id: liability_attribution__developer_liability
 *   human_readable: Developer-as-Primary-Liable-Party Doctrine
 *   domain: technology governance / legal theory / regulatory design
 *
 * SUMMARY:
 *   This story instantiates one reading within the liability_attribution
 *   kernel: the doctrine that developers — the creators of a general-purpose
 *   AI capability — bear primary legal and regulatory liability for
 *   downstream harms, as opposed to the deployer or a shared-liability
 *   standard. The reading is attractive to regulators for administrability
 *   (developers are identifiable, well-capitalized, and traceable) but
 *   structurally externalizes risk away from the parties who actually control
 *   deployment context, configuration, and end use. This is a genuinely
 *   distinct constraint from deployer_liability and shared_liability, not a
 *   different observation angle on one constraint — each reading produces a
 *   different victim set, a different beneficiary set, and a different
 *   epsilon. They are linked, not merged, per the ε-invariance principle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__developer_liability, 0.66).
domain_priors:suppression_score(liability_attribution__developer_liability, 0.58).
domain_priors:theater_ratio(liability_attribution__developer_liability, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, extractiveness, 0.66).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Developer-as-Primary-Liable-Party Doctrine").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "technology governance / legal theory / regulatory design").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, '1378e28d-182d-4d7b-afb0-28c98c877ca9').
narrative_ontology:cs_kernel_codification('1378e28d-182d-4d7b-afb0-28c98c877ca9', distributed).
narrative_ontology:cs_authority_grounding('1378e28d-182d-4d7b-afb0-28c98c877ca9', distributed).
narrative_ontology:cs_reading_relation('1378e28d-182d-4d7b-afb0-28c98c877ca9', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('1378e28d-182d-4d7b-afb0-28c98c877ca9', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('1378e28d-182d-4d7b-afb0-28c98c877ca9', foundational, creation_of_capability_is_the_proximate_locus_of_responsibility).
narrative_ontology:cs_axiom_status(creation_of_capability_is_the_proximate_locus_of_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('1378e28d-182d-4d7b-afb0-28c98c877ca9', creation_of_capability_is_the_proximate_locus_of_responsibility, conventional).
narrative_ontology:cs_axiom('1378e28d-182d-4d7b-afb0-28c98c877ca9', secondary, identifiable_well_resourced_party_should_bear_liability_for_administrability).
narrative_ontology:cs_axiom_status(identifiable_well_resourced_party_should_bear_liability_for_administrability, holdable).
narrative_ontology:cs_axiom_grounding('1378e28d-182d-4d7b-afb0-28c98c877ca9', identifiable_well_resourced_party_should_bear_liability_for_administrability, instrumental).
narrative_ontology:cs_reference_frame('1378e28d-182d-4d7b-afb0-28c98c877ca9', product_liability_manufacturer_analogy).
narrative_ontology:cs_drift_state('1378e28d-182d-4d7b-afb0-28c98c877ca9', post_foundation_model_proliferation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1378e28d-182d-4d7b-afb0-28c98c877ca9', '').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, model_deploying_firms).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, downstream_integrators).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, end_use_operators).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, model_development_labs).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, open_weight_maintainers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, independent_ai_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, end_use_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trains and releases the underlying capability (a model, a library, a foundation system). Under this reading, bears primary legal and regulatory liability for downstream harms because it created the capability, regardless of how far removed it is from the deployment context that produced the harm. Cannot fully predict every downstream use, cannot inspect every deployment, and cannot contractually disclaim liability in jurisdictions adopting this reading. Exit means either withdrawing capability from a jurisdiction, over-restricting release (closed-sourcing, heavy gating), or absorbing litigation and compliance cost as a fixed tax on building at all.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, model_development_labs, payer,
    powerful, generational, constrained, global).

% Releases openly modifiable capability with no ongoing control over how it is deployed once released. Under developer-liability, this actor carries the heaviest asymmetry: liability attaches to creation, but the maintainer has the least visibility into or control over eventual use of anyone in the chain. Has essentially no exit short of ceasing open releases altogether, which forecloses the coordination benefit the ecosystem depends on.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, open_weight_maintainers, payer,
    moderate, biographical, trapped, global).

% Small teams or solo developers building on top of foundation capability. Lack the legal resources of large labs to manage compliance or litigation risk, yet are swept into the same liability standard as capability creators if they fine-tune or repackage a model. Cannot negotiate liability allocation with upstream providers or downstream deployers; must either avoid building entirely or accept exposure they cannot insure against.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, independent_ai_developers, payer,
    powerless, biographical, trapped, national).

% Integrates the capability into a product or service and controls the deployment context — who uses it, how it's configured, what guardrails are applied. Under this reading, externalizes the bulk of legal and regulatory risk upstream to the developer, despite holding the decision authority over the specific use that caused harm. Can select jurisdictions, structure contracts, and choose which capabilities to integrate based on which upstream provider will absorb liability exposure, giving it effective arbitrage over the liability allocation itself.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, model_deploying_firms, beneficiary,
    institutional, generational, arbitrage, global).

% Builds applications on top of deployed capability, configuring it for specific end-use contexts. Benefits from a liability regime that treats the underlying model as the locus of legal risk, since it shifts scrutiny away from the integrator's own configuration and prompt-engineering choices. Can switch upstream providers relatively easily if liability terms become unfavorable.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, downstream_integrators, beneficiary,
    organized, biographical, mobile, national).

% Operates the capability in a live context (a hospital, a call center, a hiring pipeline) and makes the operational choices that most proximately produce outcomes. Benefits from reduced legal exposure under this reading, since fault is attributed upstream to the capability's creator rather than to the operational decision to deploy it in this way. Bears reputational and operational cost when harms occur but rarely bears the legal cost.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, end_use_operators, beneficiary,
    moderate, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(liability_attribution__developer_liability, end_use_operators, payer).

% The people actually injured by a deployed capability's failure — denied a loan, misdiagnosed, wrongly flagged. Their interest is in whichever liability rule most reliably produces compensation and behavior change, not in which party along the chain is doctrinally 'primary.' They are not parties to the doctrinal debate about developer versus deployer liability and have no seat in the standard-setting process, even though the rule's structure determines whether they can recover at all.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, harmed_end_users, excluded,
    powerless, immediate, trapped, local).

% Sets and enforces the liability standard through statute, agency rule, or common-law doctrine. Chooses developer-liability partly because the capability creator is often the more identifiable, better-resourced, and more traceable party — an administrability preference that does not track causal contribution to any specific harm. Can revise the standard but faces sustained lobbying pressure from deploying firms favoring the status quo.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, regulators_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__developer_liability, model_deploying_firms).
narrative_ontology:fixing_cost_class(liability_attribution__developer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives regulators and courts a single, identifiable, well-capitalized point of legal contact for a diffuse and technically opaque harm-generating chain — rather than litigating fault across every deployment context individually, liability attaches at the point of creation, which is easier to locate and easier to regulate ex ante.
% TRANSFER_FUNCTION: Moves legal, compliance, and litigation risk from the parties who control the specific deployment decision (deployers, integrators, operators) to the party who built the general-purpose capability, even where that party had no visibility into or control over the eventual use.
% ABSENT_VOICES: Harmed end users have no seat in the doctrinal contest between developer-liability, deployer-liability, and shared-liability framings — the debate is conducted among labs, deploying firms, and regulators, with the injured party's interest (reliable compensation, behavior change) treated as a secondary consideration to administrability and industry lobbying outcomes.
% DISAPPEARANCE_RATIONALE: If developer-primary liability were replaced overnight by deployer-primary or shared liability, deploying firms would face direct exposure for configuration and use-context decisions they currently externalize, open-weight release practices would likely expand as creators shed disproportionate risk, and litigation strategy across the industry would shift its target from labs to the firms actually operating the capability in harmful contexts.
% FOUNDING_PROBLEM: As general-purpose AI capabilities proliferated through complex value chains, courts and regulators needed a liability anchor for harms whose proximate cause was hard to trace through layers of fine-tuning, integration, and deployment configuration — attaching liability to the creator offered a stable, identifiable target.
% FOUNDING_PROBLEM_CORROBORATION: Regulators and legal scholars attest the administrability rationale is real and unresolved. Independent legal academics and plaintiff-side litigation groups — outside both the lab and deployer beneficiary sets — argue the doctrine has drifted from a genuine causation-tracing problem into a convenient risk-externalization tool for deploying firms with deployment control, since the party with the most proximate control over harm-producing configuration is systematically the one shielded by this reading.
narrative_ontology:disappearance_verdict(liability_attribution__developer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__developer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__developer_liability, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(liability_attribution__developer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__developer_liability, 0.66, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__developer_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__developer_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__developer_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.42) reflecting early-stage regulatory attention to AI harms and rises to 0.66 as litigation and statutory frameworks concretize around developer-anchored liability, layering compliance cost and litigation exposure onto capability creators independent of their proximity to actual harm. Suppression tracks the hardening of this doctrine into binding law (rising from 0.38 to 0.58) as courts establish precedent and regulators codify the standard, closing off the developer's ability to contractually reallocate risk. Theater ratio stays comparatively low (0.18 to 0.28) because the liability exposure is materially real, not primarily symbolic — labs actually face litigation and compliance cost, not merely reputational performance.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator's seat this reads as sensible administrability: pick the traceable, resourced party. From the model development lab's seat, the same doctrine reads as bearing cost for decisions it never made and could not have foreseen — the engine should compute a materially different type at the payer seats than at the agenda-setter or beneficiary seats, and that divergence is the point of this reading rather than a defect in it.
 *
 * DIRECTIONALITY LOGIC:
 *   Model development labs, open-weight maintainers, and independent developers are declared victims because the doctrine attaches liability at the point of creation regardless of downstream control — this drives their directionality toward the full-target end, amplified for open-weight maintainers and independent developers whose exit options (trapped) are worse than the large labs' (constrained, with some capacity to gate release or lobby). Deploying firms, integrators, and end-use operators are declared beneficiaries because they retain the actual decision authority over deployment context while the liability doctrine routes legal exposure upstream — this pulls their directionality toward the beneficiary end even though they are the proximate cause of many specific harms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an identifiable liability anchor for a technically opaque value chain — remains partially live (opacity and value-chain complexity are real), which is why founding_problem_status is authored as contested rather than dead. But the doctrine's persistence is increasingly attributable to deploying-firm lobbying advantage and administrative convenience rather than to a demonstrated superiority in producing compensation or behavior change, which is why the classification sits at tangled_rope rather than a clean rope: there is a genuine coordination function (administrability) riding alongside an asymmetric extraction (risk externalized from the party with deployment control to the party with creation responsibility).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    developer_liability_kernel_reading,
    'Is developer-primary liability the structurally correct reading of causal responsibility for AI-capability harms, or is it one contested reading among several equally defensible allocations (deployer_liability, shared_liability) that happens to be administratively convenient for regulators and structurally favorable to deploying firms?',
    'Comparative outcome analysis across jurisdictions that have adopted different readings: track compensation rates for harmed end users, rates of harm-reducing behavior change by deployers, and litigation cost distribution under each regime over a multi-year window.',
    'If developer-primary liability does not outperform deployer-primary or shared liability on compensation and behavior-change outcomes, the doctrine''s persistence is better explained by administrability convenience and deploying-firm lobbying than by superior causal attribution — supporting reclassification of this reading toward more extractive framing over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developer_liability_kernel_reading, conceptual, 'Whether developer-primary liability is structurally justified or one contested, administratively convenient reading among several.').

omega_variable(
    sibling_reading_delta,
    'What would change structurally if a deployer_liability or shared_liability reading were adopted instead — specifically, would the victim and beneficiary sets fully invert, or would some parties (e.g., independent developers, harmed end users) remain in a similarly disadvantaged position across all three readings?',
    'Author the sibling constraint stories (deployer_liability, shared_liability) with matched stakeholder sets and compare beneficiary/victim declarations directly; check whether harmed_end_users'' excluded status persists across all three readings.',
    'If harmed end users remain excluded and under-compensated across all three readings, the kernel contest itself may be a distraction from a deeper structural problem (no reading adequately serves the injured party) rather than a genuine debate about optimal allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_delta, conceptual, 'Whether the three kernel readings meaningfully differ in outcomes for the least powerful stakeholder or merely relocate extraction among powerful parties.').

omega_variable(
    opacity_burden_allocation,
    'Is it defensible to place the burden of managing or disclosing model opacity entirely on the developer, given that deployment-context opacity (how the capability is configured and used) is often equally or more responsible for harm than model-level opacity?',
    'Technical audit studies comparing the share of documented AI-harm incidents attributable to model-level failure versus deployment-configuration failure across a representative incident database.',
    'If deployment-configuration failure accounts for a comparable or larger share of harms, placing the full opacity-management burden on developers under this reading is empirically mismatched to the actual causal structure of harm.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(opacity_burden_allocation, empirical, 'Whether opacity-related harm is predominantly a developer-side or deployer-side phenomenon.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__developer_liability, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__developer_liability, theater_ratio, 0, 0.18).
narrative_ontology:measurement(liab_tr_t4, liability_attribution__developer_liability, theater_ratio, 4, 0.2).
narrative_ontology:measurement(liab_tr_t8, liability_attribution__developer_liability, theater_ratio, 8, 0.22).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__developer_liability, theater_ratio, 12, 0.24).
narrative_ontology:measurement(liab_tr_t16, liability_attribution__developer_liability, theater_ratio, 16, 0.25).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__developer_liability, theater_ratio, 20, 0.27).
narrative_ontology:measurement(liab_tr_t24, liability_attribution__developer_liability, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__developer_liability, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(liab_be_t4, liability_attribution__developer_liability, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(liab_be_t8, liability_attribution__developer_liability, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(liab_be_t12, liability_attribution__developer_liability, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(liab_be_t16, liability_attribution__developer_liability, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(liab_be_t20, liability_attribution__developer_liability, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(liab_be_t24, liability_attribution__developer_liability, base_extractiveness, 24, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__developer_liability, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(liab_su_t4, liability_attribution__developer_liability, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(liab_su_t8, liability_attribution__developer_liability, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(liab_su_t12, liability_attribution__developer_liability, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(liab_su_t16, liability_attribution__developer_liability, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(liab_su_t20, liability_attribution__developer_liability, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(liab_su_t24, liability_attribution__developer_liability, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__developer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__shared_liability).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'AI liability attribution' per the ε-invariance principle: developer_liability, deployer_liability, and shared_liability each instantiate a structurally distinct constraint with a different victim/beneficiary allocation, not three observational angles on one constraint. All three should be authored as separate files and cross-linked via affects_constraints; each carries its own epsilon (this story's is 0.66, reflecting substantial and rising extraction from creators).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
