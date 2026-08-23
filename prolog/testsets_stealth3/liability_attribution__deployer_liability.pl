% ============================================================================
% CONSTRAINT STORY: liability_attribution__deployer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__deployer_liability, []).

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
 *   constraint_id: liability_attribution__deployer_liability
 *   human_readable: Deployer-Primary Liability Allocation for AI Systems
 *   domain: technology governance/legal theory/regulatory design
 *
 * SUMMARY:
 *   A liability-allocation regime assigns primary responsibility for
 *   AI-caused harm to the deploying party, on the ground that deployers
 *   control the deployment context and make the final use decisions. The
 *   arrangement solves a real problem — someone must be reachable and
 *   incentivized at the point of harm — while transferring a substantial cost
 *   burden onto deployers, including for risks embedded in model behavior
 *   they cannot inspect. The expected structural delta of this reading is
 *   visible throughout: deployers sit in the victim set, foundation model
 *   providers are shielded from downstream harm, and opacity converts into
 *   the deployer's due-diligence burden. This file instantiates ONE reading
 *   of the liability_attribution kernel; the developer_liability and
 *   shared_liability readings are separate constraints with their own epsilon
 *   values, beneficiary/victim sets, and classifications, linked through
 *   network.affects_constraints. Epsilon's referent is the standing
 *   arrangement under contest — the deployer-primary allocation as enacted
 *   and proposed — assessed by this reading's own analytic lights: endorsing
 *   the allocation does not mean pretending it is costless, and the reading
 *   authors the burden transfer it can see. Claimed type and metrics are
 *   authored independently: the claim states what this reading holds
 *   structurally true; the metrics describe the arrangement's actual
 *   operation.
 *
 * KEY AGENTS:
 *   - ai_deployers_enterprise: primary target (powerful/constrained) — bears primary liability, partially re-absorbs it through negotiated indemnity and captive insurance
 *   - small_deployers_startups: primary target (moderate/trapped) — bears the same liability without contractual relief; nearest the full-target seat
 *   - foundation_model_providers: principal beneficiary (institutional/arbitrage) — shielded from downstream harm, retains licensing revenue
 *   - injured_third_parties: coordination beneficiary with passed-through costs (powerless/constrained)
 *   - liability_insurers: market beneficiary (institutional/mobile) — collects premiums on the created risk class
 *   - compliance_auditors: market beneficiary (organized/mobile) — sells the due-diligence apparatus the burden requires
 *   - ai_legislators_regulators: agenda setter (institutional/mobile) — chooses and can revise the allocation
 *   - courts_judiciary: agenda setter through interpretation (institutional/constrained) — tunes the burden's practical weight
 *   - open_source_model_communities: excluded voice (organized/mobile) — outside the drafting conversation the allocation emerged from
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__deployer_liability, 0.66).
domain_priors:suppression_score(liability_attribution__deployer_liability, 0.6).
domain_priors:theater_ratio(liability_attribution__deployer_liability, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, extractiveness, 0.66).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__deployer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__deployer_liability, "Deployer-Primary Liability Allocation for AI Systems").
narrative_ontology:topic_domain(liability_attribution__deployer_liability, "technology governance/legal theory/regulatory design").

domain_priors:requires_active_enforcement(liability_attribution__deployer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, 'ffddd1e9-3508-4df3-bca0-6131f67878f9').
narrative_ontology:cs_kernel_codification('ffddd1e9-3508-4df3-bca0-6131f67878f9', formalized).
narrative_ontology:cs_authority_grounding('ffddd1e9-3508-4df3-bca0-6131f67878f9', distributed).
narrative_ontology:cs_reading_relation('ffddd1e9-3508-4df3-bca0-6131f67878f9', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('ffddd1e9-3508-4df3-bca0-6131f67878f9', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('ffddd1e9-3508-4df3-bca0-6131f67878f9', foundational, deployment_context_control_warrants_primary_liability).
narrative_ontology:cs_axiom_status(deployment_context_control_warrants_primary_liability, holdable).
narrative_ontology:cs_axiom_grounding('ffddd1e9-3508-4df3-bca0-6131f67878f9', deployment_context_control_warrants_primary_liability, instrumental).
narrative_ontology:cs_axiom('ffddd1e9-3508-4df3-bca0-6131f67878f9', secondary, opaque_capability_due_diligence_duty).
narrative_ontology:cs_axiom_status(opaque_capability_due_diligence_duty, holdable).
narrative_ontology:cs_axiom_grounding('ffddd1e9-3508-4df3-bca0-6131f67878f9', opaque_capability_due_diligence_duty, instrumental).
narrative_ontology:cs_reference_frame('ffddd1e9-3508-4df3-bca0-6131f67878f9', deployment_control_centric_allocation).
narrative_ontology:cs_drift_state('ffddd1e9-3508-4df3-bca0-6131f67878f9', contemporary_legislative_contestation, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ffddd1e9-3508-4df3-bca0-6131f67878f9', '').
narrative_ontology:cs_kernel_id(liability_attribution__deployer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, injured_third_parties).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, liability_insurers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, compliance_auditors).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, ai_deployers_enterprise).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, small_deployers_startups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, injured_third_parties).
narrative_ontology:constraint_vindicates(liability_attribution__deployer_liability, contextual_control_liability_principle).
narrative_ontology:constraint_vindicates(liability_attribution__deployer_liability, solvent_defendant_accessibility_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Integrate foundation models into products and services offered to customers: they select use contexts, set safeguards, and decide whether and where to ship. When a deployment causes harm they answer first, including for model behaviors they cannot inspect inside the licensed weights. They partially offset the exposure through negotiated indemnification clauses, captive insurance, and price pass-through, with leverage proportional to procurement size.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_deployers_enterprise, payer,
    powerful, biographical, constrained, global).

% Deploy the same underlying models without negotiating power: standard API terms disclaim provider warranties, so they absorb the full liability exposure attached to their use cases. Insurance minimums and reserve requirements can exceed runway, and their realistic exits are abandoning the product line, narrowing to uninsurable niches, or leaving the jurisdiction.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, small_deployers_startups, payer,
    moderate, immediate, trapped, national).

% Train and license the base capabilities whose downstream use generates the harm cases. Under this allocation they owe no primary duty for deployment-stage harms; terms of service disclaim warranties and cap exposure. They keep licensing revenue while the risk signal lands on their customers, and their choices during training shape exactly the risks someone else pays for.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, foundation_model_providers, beneficiary,
    institutional, generational, arbitrage, global).

% People harmed by deployed AI systems gain a solvent, identifiable defendant at the point of harm instead of chasing an opaque multi-party development chain. They also carry passed-through costs: deployers raise prices, restrict use cases, or withdraw services to manage exposure, and recovery still depends on proving fault at the deployment stage.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, injured_third_parties, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__deployer_liability, injured_third_parties, payer).

% Underwrite the deployer-side exposure the allocation creates, collecting premiums on a risk class that statute converted into a purchasable product. They price policies off deployment audits and documentation, and can decline lines or jurisdictions that price badly.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, liability_insurers, beneficiary,
    institutional, generational, mobile, global).

% Sell the due-diligence apparatus deployers now need because model opacity became their burden to investigate: model evaluations, deployment reviews, documentation packages. Revenue scales with the strictness of the duty, and the certificates they issue become the exhibits litigants read afterward.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, compliance_auditors, beneficiary,
    organized, biographical, mobile, continental).

% Draft and enact the allocation, choosing which seat bears primary liability and what counts as deployment-context control. They hear lobbying from model providers, deployer trade groups, insurers, and plaintiff interests, and can amend, withdraw, or replace the allocation between sessions.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_legislators_regulators, agenda_setter,
    institutional, biographical, mobile, national).

% Adjudicate the harm cases the allocation routes to them, interpreting what deployment-context control and adequate due diligence mean in concrete disputes. Their doctrine fills the gaps statutes leave and effectively tunes how heavy the burden sits on each deployer seat.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, courts_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Release openly licensed model weights outside commercial deployment chains. The legislative conversation about liability allocation is conducted among model providers, deployers, insurers, and plaintiff representatives; the position that allocation rules written for commercial stacks misfit distributed, non-commercial development is rarely seated. Under this reading they bear nothing, but successor rules built on the same frame could reach them.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, open_source_model_communities, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:fixing_cost_class(liability_attribution__deployer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns a single identifiable liable party at the point of harm so injured parties face a solvent defendant rather than an untraceable development chain, and concentrates deployment-stage safety incentives on the actor who selects the use context and controls the deployment decision.
% TRANSFER_FUNCTION: Moves expected liability costs (damages, defense costs, insurance premiums, due-diligence and audit expenditure) from the general pool of harm victims and from upstream capability creators onto AI system deployers; moves assurance revenue to insurers and auditors; moves avoided-liability surplus to foundation model providers.
% ABSENT_VOICES: Open-source model communities are not seated in drafting processes dominated by commercial labs, deployer trade groups, and insurers; end users bearing passed-through prices and narrowed service availability speak only through consumer channels; would-be entrants deterred by uninsurable exposure have no seat at all.
% DISAPPEARANCE_RATIONALE: If the allocation vanished overnight, no default liable party exists at the point of harm: compensation collapses into ad hoc litigation against whichever entity plaintiffs can reach, insurers withdraw a priced product line, deployer due-diligence spending loses its legal anchor, and providers lose the contractual shield that currently structures their licensing terms. Deployment incentives, insurance markets, and contracting practice all reorganize.
% FOUNDING_PROBLEM: Harm cases involving AI systems arise from causally diffuse stacks — training data, base capability, fine-tuning, integration, deployment context — that traditional product liability cannot attribute through opaque models; victims need a reachable defendant and some actor needs ex ante incentives to prevent deployment-stage harm.
% FOUNDING_PROBLEM_CORROBORATION: Courts confronting actual AI-harm dockets, insurer actuarial desks pricing the exposure, tort-law scholarship outside the benefiting parties, and plaintiff-bar filings all attest that attribution difficulty is real and unresolved. Deployer trade groups corroborate the founding problem while contesting this particular allocation of it — corroboration of the problem is not endorsement of the reading.
narrative_ontology:disappearance_verdict(liability_attribution__deployer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__deployer_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__deployer_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(liability_attribution__deployer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__deployer_liability, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__deployer_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__deployer_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__deployer_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.66 at interval end) because the transferred burden includes model-intrinsic risk the deployer did not create and cannot fully price: the delta 'opacity becomes deployer's due diligence burden' is the extraction mechanism made explicit. Suppression (0.60) reflects that deployers cannot contract around the statutory floor in most designs and cannot exit the markets they serve, though jurisdictional and contractual margins persist — hence accessibility_collapse at 0.50 rather than higher. Resistance (0.62) is real and concentrated in deployer trade groups; provider-side resistance is minimal because providers benefit. Theater (0.33) is rising: as the duty matures, documentation-and-certification activity grows faster than demonstrated harm reduction, the classic Goodhart signature of a diligence standard converting into paperwork. All three temporal series run on ONE shared grid (t=0,2,4,6,8,10,12) so every metric is authored at every examined time point; points through t=4 are observed from early adopter instruments and insurance-market formation, later points are projected maturation. The suppression_requirement series is authored deliberately: this story traces enforcement-capacity build-up (regulatory staffing, case-law hardening, audit regimes), a rising ratchet, not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats compute differently from the same structure. From the foundation-model-provider seat the arrangement is a clean boundary: it licenses capability, the customer chooses the use, and the liability follows the choice. From the small-deployer seat the same boundary is an uncompensated transfer — it absorbs full exposure under warranty-disclaiming standard terms it had no power to negotiate. The two deployer seats diverge from each other despite identical formal victim status: enterprise deployers convert market power into contractual risk-shifting, small deployers cannot. The injured-third-party seat sees the compensation guarantee the arrangement exists to provide; the auditor and insurer seats see revenue. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (providers, injured parties, insurers, auditors) place those seats near the beneficiary end of d; victim declarations (both deployer seats) place them near the target end, amplified by weak exit. Two directionality overrides are declared because the derivation cannot see contractual capacity: powerful agents (the enterprise deployer seat) derive as near-full targets from victim status plus constrained exit, but negotiated indemnification, captive insurance, and price pass-through measurably dampen their realized burden, so d is overridden to 0.82; moderate agents (the small-deployer seat) have no such relief and sit at 0.93, near full-target. No other seats need overrides — the derivation from declared roles and exit options captures them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — attribution through opaque multi-party stacks — is live, corroborated by courts, insurers, and scholarship outside the benefiting parties, so no mandatrophy is declared and none is due. The tangled_rope classification earns its keep by blocking both available mislabels: a pure-snare reading would erase the genuine coordination function (solvent-defendant access and concentrated deployment incentives), which injured parties demonstrably collect; a pure-rope reading would erase the asymmetric shielding that lets capability creators externalize the risks their design choices create. The trajectory to watch is theater_ratio: if diligence certification continues decoupling from harm reduction while extraction keeps climbing, the arrangement drifts toward performance-of-safety with the burden intact — the piton signature — and the mismatch between founding_problem_status=live and any future world_rearranges-to-world_unchanged movement would flag it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which party should bear primary liability for AI-system harm — the deployer (this reading), the developer, or the value chain jointly by causal contribution?',
    'Comparative legislative adoption and appellate doctrine across jurisdictions: track which allocation survives contact with real harm cases, insurance pricing, and appeal, and whether convergence emerges on one reading or a partitioned scheme.',
    'Under developer_liability the victim and beneficiary sets invert — deployers exit the victim set and foundation model providers enter it, flipping every directionality value in this file. Under shared_liability both sets fragment along causal-contribution lines and the single capturer disappears. This story''s classification is valid only for the deployer-primary instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this constraint is one of three rival readings of the liability_attribution kernel; sibling readings restructure the victim/beneficiary sets entirely.').

omega_variable(
    deployer_control_reality,
    'Does deployment-context control translate into real risk-reduction capacity when the underlying model is opaque to the deployer, or is the control premise largely nominal?',
    'Incident data comparing harm rates across deployers with differing diligence practices, plus controlled studies of mitigation efficacy (guardrails, use restrictions, monitoring) available at the deployment stage.',
    'If deployer control is substantially nominal, the reading extracts from a seat without corresponding preventive capacity and the efficiency justification collapses toward shared_liability or developer_liability; if control is real, the extraction tracks genuine incentive placement and the coordination component strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deployer_control_reality, empirical, 'Whether the factual premise of the reading — deployers can actually prevent the harms they are charged for — holds under opacity.').

omega_variable(
    indemnification_absorption,
    'Can contractual indemnification and insurance markets re-absorb the deployer/provider asymmetry after enactment, making the statutory allocation effectively shared, or do warranty disclaimers and non-delegable duty designs keep the asymmetry rigid?',
    'Track indemnification clause prevalence and enforceability in provider contracts, deployer liability insurance penetration and pricing, and litigation over disclaimer enforceability in harm cases.',
    'If contracts absorb the asymmetry, the statutory reading matters less than bargaining power and the extraction concentrates wherever leverage is thinnest (small deployers); if blocked, the asymmetry is rigid and the provider shield is a pure statutory artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indemnification_absorption, empirical, 'Whether private ordering dilutes or preserves the public allocation''s asymmetry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__deployer_liability, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(liab_tr_t0, observed).
narrative_ontology:measurement(liab_tr_t2, liability_attribution__deployer_liability, theater_ratio, 2, 0.18).
narrative_ontology:measurement_basis(liab_tr_t2, observed).
narrative_ontology:measurement(liab_tr_t4, liability_attribution__deployer_liability, theater_ratio, 4, 0.21).
narrative_ontology:measurement_basis(liab_tr_t4, observed).
narrative_ontology:measurement(liab_tr_t6, liability_attribution__deployer_liability, theater_ratio, 6, 0.24).
narrative_ontology:measurement_basis(liab_tr_t6, projected).
narrative_ontology:measurement(liab_tr_t8, liability_attribution__deployer_liability, theater_ratio, 8, 0.27).
narrative_ontology:measurement_basis(liab_tr_t8, projected).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__deployer_liability, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(liab_tr_t10, projected).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__deployer_liability, theater_ratio, 12, 0.33).
narrative_ontology:measurement_basis(liab_tr_t12, projected).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__deployer_liability, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(liab_be_t0, observed).
narrative_ontology:measurement(liab_be_t2, liability_attribution__deployer_liability, base_extractiveness, 2, 0.52).
narrative_ontology:measurement_basis(liab_be_t2, observed).
narrative_ontology:measurement(liab_be_t4, liability_attribution__deployer_liability, base_extractiveness, 4, 0.55).
narrative_ontology:measurement_basis(liab_be_t4, observed).
narrative_ontology:measurement(liab_be_t6, liability_attribution__deployer_liability, base_extractiveness, 6, 0.58).
narrative_ontology:measurement_basis(liab_be_t6, projected).
narrative_ontology:measurement(liab_be_t8, liability_attribution__deployer_liability, base_extractiveness, 8, 0.61).
narrative_ontology:measurement_basis(liab_be_t8, projected).
narrative_ontology:measurement(liab_be_t10, liability_attribution__deployer_liability, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(liab_be_t10, projected).
narrative_ontology:measurement(liab_be_t12, liability_attribution__deployer_liability, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(liab_be_t12, projected).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__deployer_liability, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(liab_su_t0, observed).
narrative_ontology:measurement(liab_su_t2, liability_attribution__deployer_liability, suppression_requirement, 2, 0.44).
narrative_ontology:measurement_basis(liab_su_t2, observed).
narrative_ontology:measurement(liab_su_t4, liability_attribution__deployer_liability, suppression_requirement, 4, 0.48).
narrative_ontology:measurement_basis(liab_su_t4, observed).
narrative_ontology:measurement(liab_su_t6, liability_attribution__deployer_liability, suppression_requirement, 6, 0.52).
narrative_ontology:measurement_basis(liab_su_t6, projected).
narrative_ontology:measurement(liab_su_t8, liability_attribution__deployer_liability, suppression_requirement, 8, 0.55).
narrative_ontology:measurement_basis(liab_su_t8, projected).
narrative_ontology:measurement(liab_su_t10, liability_attribution__deployer_liability, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(liab_su_t10, projected).
narrative_ontology:measurement(liab_su_t12, liability_attribution__deployer_liability, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(liab_su_t12, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__deployer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__shared_liability).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'who is liable for AI harm?' decomposes into three structurally distinct allocations — deployer_liability (this file), developer_liability, and shared_liability — because each reading assigns different victim and beneficiary sets and therefore a different, internally stable epsilon. Per the epsilon-invariance principle they are separate stories, not one story with a measurement parameter. This reading functions as the upstream baseline in the family: shared-liability drafts argue proportionality against the deployer-primary default, and developer-liability advocacy defines itself against the shielding this reading grants providers. Each family member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_attribution__deployer_liability, powerful, 0.82).
constraint_indexing:directionality_override(liability_attribution__deployer_liability, moderate, 0.93).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
