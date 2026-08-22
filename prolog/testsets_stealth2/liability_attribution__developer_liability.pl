% ============================================================================
% CONSTRAINT STORY: liability_attribution__developer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Developer-Primary AI Liability Allocation
 *   domain: technological/legal
 *
 * SUMMARY:
 *   A liability-allocation regime is consolidating around the principle that
 *   the organization which trains and releases an AI capability bears primary
 *   legal responsibility for harms the capability later causes in anyone's
 *   deployment. Provider obligations are codified upstream; deployers
 *   integrate the capability under their own operational control while
 *   contractually steering expected loss upstream; injured parties gain a
 *   solvent, technically informed defendant. This file instantiates ONE
 *   reading of the contested liability_attribution kernel — the
 *   developer_liability reading — as a clean, epsilon-invariant constraint.
 *   The epsilon referent is the standing arrangement under contest: the
 *   developer-primary allocation itself, assessed by this reading's own
 *   lights. Sibling readings (deployer_liability, shared_liability) are
 *   separate constraint stories with different victim sets and therefore
 *   different epsilon values; they are linked through the network, not folded
 *   into this one. The claim/metric relationship is deliberately
 *   unreconciled: the structural claim is tangled_rope (genuine coordination
 *   function plus asymmetric transfer plus active enforcement), and the
 *   metrics are authored independently as descriptive estimates.
 *
 * KEY AGENTS:
 *   - frontier_model_developers: Primary target (powerful/constrained) — bears liability assigned at creation time for harms surfacing in deployments it does not control
 *   - small_ai_startups: Secondary target (moderate/mobile) — exposed through contract chains without incumbent defenses; exits by pivoting or selling
 *   - open_source_developers: Peripheral target (moderate/mobile) — personal-asset exposure reaches unpaid contributors; exit is stopping
 *   - enterprise_deployers: Primary beneficiary with residual exposure (powerful/constrained) — externalizes expected loss upstream, retains negligence tail
 *   - harmed_end_users: Protected beneficiary (powerless/trapped) — gains a solvent, technically informed defendant
 *   - liability_insurers: Intermediary beneficiary (institutional/arbitrage) — collects premiums on a new risk class
 *   - ai_regulators: Agenda setter (institutional/analytical) — fixes the allocation and is still building enforcement capacity
 *   - technology_law_scholars: Analytical observer — sees cross-jurisdictional divergence between codified duties and courtroom practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__developer_liability, 0.66).
domain_priors:suppression_score(liability_attribution__developer_liability, 0.55).
domain_priors:theater_ratio(liability_attribution__developer_liability, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, extractiveness, 0.66).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Developer-Primary AI Liability Allocation").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "technological/legal").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, '96a48a97-5952-4e6c-83c9-b675f45addbd').
narrative_ontology:cs_kernel_codification('96a48a97-5952-4e6c-83c9-b675f45addbd', distributed).
narrative_ontology:cs_authority_grounding('96a48a97-5952-4e6c-83c9-b675f45addbd', distributed).
narrative_ontology:cs_reading_relation('96a48a97-5952-4e6c-83c9-b675f45addbd', liability_attribution__deployer_liability, forecloses).
narrative_ontology:cs_reading_relation('96a48a97-5952-4e6c-83c9-b675f45addbd', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('96a48a97-5952-4e6c-83c9-b675f45addbd', foundational, creation_locus_primary_liability).
narrative_ontology:cs_axiom_status(creation_locus_primary_liability, holdable).
narrative_ontology:cs_axiom_grounding('96a48a97-5952-4e6c-83c9-b675f45addbd', creation_locus_primary_liability, instrumental).
narrative_ontology:cs_axiom('96a48a97-5952-4e6c-83c9-b675f45addbd', secondary, creator_knowledge_disclosure_duty).
narrative_ontology:cs_axiom_status(creator_knowledge_disclosure_duty, holdable).
narrative_ontology:cs_axiom_grounding('96a48a97-5952-4e6c-83c9-b675f45addbd', creator_knowledge_disclosure_duty, deontological).
narrative_ontology:cs_reference_frame('96a48a97-5952-4e6c-83c9-b675f45addbd', creator_accountability_anchor).
narrative_ontology:cs_drift_state('96a48a97-5952-4e6c-83c9-b675f45addbd', contemporary_regulatory_codification, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('96a48a97-5952-4e6c-83c9-b675f45addbd', '').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, enterprise_deployers).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, harmed_end_users).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, liability_insurers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, frontier_model_developers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, small_ai_startups).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, open_source_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, enterprise_deployers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and amend the statutes and agency rules that fix who answers legally when an AI system causes harm, currently weighting primary responsibility toward the organization that trained and released the underlying model. Consultation processes are dominated by incumbent labs and deployer trade associations. Their own enforcement capacity — investigation staff, technical audit tooling, disclosure verification — is still being built out, which shapes how much of the assigned responsibility is practically collectible.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, ai_regulators, agenda_setter,
    institutional, generational, analytical, continental).

% Train and release large general-purpose models sold or licensed to many downstream deployers. Under the prevailing allocation they retain legal exposure for harms that materialize far downstream, in deployment contexts they neither chose nor supervise. They respond with expanded disclosure teams, indemnity negotiations, insurance purchase, and litigation reserves. Their market position depends on continuing to ship, so exit means ceding the frontier to rivals; several also fuse the accepted burden with a public identity as responsible stewards of the technology.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, frontier_model_developers, payer,
    powerful, biographical, constrained, global).

% Build narrow applications on top of foundation models and sell to early customers. The same primary-responsibility allocation reaches them through contract chains and platform terms, but without the legal departments, insurance budgets, or litigation reserves of incumbents; a single claim can end the company. Their realistic exits are pivoting to a non-AI product or selling before exposure crystallizes.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, small_ai_startups, payer,
    moderate, immediate, mobile, national).

% Publish model weights, training code, and evaluation tooling without payment, for reputation and conviction. Liability theories aimed at capability creators reach their personal assets, and several jurisdictions' proposals would make hobbyist contributors defendants. Their exit is simply stopping — and visible departures of maintainers are already documented where litigation began.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, open_source_developers, payer,
    moderate, biographical, mobile, global).

% Integrate purchased or licensed models into products they control end to end — choosing contexts, users, and safeguards. Because primary responsibility sits upstream, their contractual posture is to demand broad indemnities while resisting reciprocal ones, and their expected liability costs fall accordingly. They still carry exposure for their own negligent operation, which they manage with insurance and usage policies.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, enterprise_deployers, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__developer_liability, enterprise_deployers, payer).

% People injured by deployed systems — denied credit, misdiagnosed, defamed by chatbots, struck by autonomous vehicles. The allocation gives them a solvent, identifiable defendant with the deepest technical knowledge instead of a thinly capitalized operator. Individually they have little power, and they have no exit from a society saturated with these systems.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, harmed_end_users, beneficiary,
    powerless, biographical, trapped, global).

% Underwrite the new exposure, price developer-risk policies, and collect premiums on a risk class that barely existed a decade ago. Actuarial models of opaque-system failure are immature, so margins are wide and exclusions extensive. They can withdraw from lines or reprice annually, and they lobby on the drafting of liability standards.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, liability_insurers, beneficiary,
    institutional, biographical, arbitrage, continental).

% Track doctrine across jurisdictions, publish comparative analyses of where responsibility actually lands, and testify in consultations. They see the whole allocation structure and its divergences across borders, including the gap between codified provider duties and courtroom practice.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, technology_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__developer_liability, enterprise_deployers).
narrative_ontology:fixing_cost_class(liability_attribution__developer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a default bearer of last resort for harms from opaque learned systems, giving injured parties a solvent, technically informed defendant and concentrating safety incentives at the point of greatest capability knowledge.
% TRANSFER_FUNCTION: Moves expected loss — defense costs, settlements, insurance premiums, disclosure-compliance expenditure — from deployers and injured parties onto capability developers; moves compensation probability toward injured parties.
% ABSENT_VOICES: Open-source maintainers and startup founders were thinly represented in the consultations that shaped provider-obligation drafts — incumbent labs and deployer associations dominated testimony. Future injured populations are definitionally absent. Deployer-side in-house counsel were heavily present; the developer-periphery seats were not.
% DISAPPEARANCE_RATIONALE: If the allocation vanished overnight, injured parties would face a compensation vacuum with no solvent defendant, deployers would reprice or uninsured-risk their operations, insurer product lines would collapse or restructure, and developer investment in disclosure and safety documentation would be re-optimized against a different liability surface. The AI supply chain's contractual architecture is built around this allocation and would be renegotiated clause by clause.
% FOUNDING_PROBLEM: Opaque AI systems began injuring people through failures no single actor obviously controlled: the operator chose the context, the creator built the capability, and neither plainly 'caused' the harm in classical tort terms. The arrangement was built to close that accountability gap — to guarantee that someone with the capacity to prevent harm at scale answers for it.
% FOUNDING_PROBLEM_CORROBORATION: Consumer-protection agencies and academic tort scholarship corroborate that the compensation-and-accountability problem remains live, citing unresolved casualty classes and jurisdictional gaps. Deployer trade associations — beneficiaries of the current allocation — attest instead that contractual risk management has largely solved it; that dissent from the benefiting seat is itself structural signal. Corroboration comes from outside the beneficiary set.
narrative_ontology:disappearance_verdict(liability_attribution__developer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__developer_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__developer_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(liability_attribution__developer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__developer_liability, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is substantial (0.66 at interval end) because the allocation assigns developers losses that arise substantially from deployment-time decisions they do not make: the transfer is decoupled from their share of causal control over any given incident. Suppression is moderate (0.55) and predominantly structural — legal exposure, mandatory-disclosure machinery, and contract-chain propagation of provider duties — with a smaller internalized component (responsibility norms inside labs that make contesting the burden feel like betraying a stewardship identity; if that identity frame broke, measured resistance would rise sharply). Theater is low-to-moderate (0.30): much compliance artifact (model cards, disclaimers, red-team reports) is increasingly drafted for courtrooms rather than users, but the disclosure and evaluation functions are not yet hollow. Accessibility_collapse is low (0.42) because alternatives persist — contractual indemnity stacking, insurance, jurisdictional selection, offshore development — each carrying friction but none foreclosed. Resistance is elevated (0.62): developer lobbying, litigation strategy, and open-source withdrawal threats are active and documented. The three temporal series share one grid ({0,3,6,9,12,15}); the suppression_requirement series is authored deliberately because this story's traced dynamic is enforcement-capacity buildout — regulators constructing the machinery that makes the assigned responsibility collectible — not merely shifting extraction. Later points are marked projected: the regime's full maturation lies ahead of the observation date.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently by construction. The sharpest divergence is between same-power seats: frontier_model_developers and enterprise_deployers are both powerful, global, and constrained, yet sit at opposite ends of the directionality range — differentiated entirely by role and exit texture, not by power. A second divergence runs within the victim set: small startups and open-source developers share the frontier labs' payer role but hold mobile exit, so the constraint presses on them as a participation tax (exit) rather than a carrying cost (absorb); the engine should compute different per-seat types from the same role declaration. Coalition potential among victims exists on paper (trade associations span the developer class) but is suppressed by heterogeneous exit options: incumbents can absorb, periphery actors exit, so no unified payer bloc forms.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: enterprise_deployers (externalized expected loss, partial offset from their residual negligence tail), harmed_end_users (compensation certainty, no exit), liability_insurers (premium flow, arbitrage-grade exit keeps them nearest the beneficiary end). Victim declarations drive high directionality, amplified by exit position: frontier developers are constrained (cannot stop shipping without ceding the market), pulling them toward the full-target end; startups and open-source developers are mobile, which damps their effective extraction but converts the burden into suppressed participation. No directionality_overrides are authored: overrides key on power atoms, and the 'powerful' atom hosts two seats with opposed structural relationships (frontier developers and enterprise deployers), so any override would mis-specify one of them. The differentiation is carried by role and exit declarations, where it belongs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, so no mandatrophy resolution is declared. The classification guards against both symmetrical errors: reading the deployer windfall as proof of pure extraction ignores the real coordination leg — compensation certainty for injured parties and incentive concentration at the point of capability knowledge, neither of which the deployer-primary or purely proportional alternatives deliver as cleanly; reading developer acquiescence (voluntary safety commitments, published frameworks) as consent ignores the ratchet — voluntary adoption became the baseline that mandatory duties then hardened. The R5 mismatch consumer should find no zombie flag here: founding_problem_status=live with disappearance_verdict=world_rearranges is the coherent cell. The piton test fails on its own terms: a concentrated beneficiary seat (enterprise_deployers) captures the gains, which is snare-family structure riding on a rope, not inertial maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the liability_attribution kernel (reading: developer_liability). Would adopting a sibling reading — deployer_liability or shared_liability — relocate the victim set and dissolve this arrangement''s beneficiary structure?',
    'Legislative enactment or appellate doctrine consolidation choosing among the three allocations; watch for provider-obligation repeal, deployer-duty statutes, or apportionment frameworks replacing primary-bearer defaults.',
    'Under deployer_liability, developers exit the victim set and deployers enter it, inverting this story''s directionality map. Under shared_liability the concentrated victim/beneficiary split disperses into contribution-weighted shares, collapsing the asymmetric-extraction signature that drives this reading''s tangled_rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one of three readings of the liability-attribution kernel; sibling adoption restructures the victim set.').

omega_variable(
    control_knowledge_locus_dispute,
    'Is the inter-reading disagreement located in whether liability should index to deployment-time control (deployer reading) or creation-time knowledge of the capability (developer reading) — and can any single framework rank both considerations?',
    'Doctrinal analysis of which factor courts treat as decisive when the two conflict — misuse cases, fine-tuning cases, dual-control incidents; legislative findings statements declaring the ranking.',
    'If control dominates, this reading loses its primary-locus claim and converges toward the deployer reading; if knowledge dominates, deployer-side externalization intensifies and this arrangement''s extraction deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_knowledge_locus_dispute, conceptual, 'Where the kernel contest is located: the attribution index (control vs knowledge), not the magnitude of liability.').

omega_variable(
    opacity_administrability,
    'Can courts actually attribute causation through opaque model internals, or does developer-primary liability become strict liability in fact — punishing without steering?',
    'Track disposition patterns in decided cases: do courts find developer-side causation provable, or do they default to settlement and strict-liability reasoning because internals are unexplainable?',
    'If administrable, the arrangement retains incentive content and the coordination leg stays strong. If not, it drifts toward pure transfer plus insurance intermediation, pushing effective classification toward the extractive pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_administrability, empirical, 'Whether opacity makes the developer-liability regime administrable or degenerates into unsteerable strict liability.').

omega_variable(
    open_source_chilling_effect,
    'Does developer-primary liability measurably suppress open-weight release and unpaid contribution?',
    'Release-rate and maintainer-retention data before and after liability rulings and disclosure mandates; surveys of departed maintainers citing legal exposure.',
    'Strong chilling would show the arrangement persisting partly by suppressing an alternative production mode — raising effective suppression above the structural measure and darkening the classification. Weak chilling supports the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_chilling_effect, empirical, 'Whether the liability allocation operates as a participation tax on the open ecosystem.').

omega_variable(
    insurance_intermediation_effect,
    'Does liability insurance preserve the arrangement''s incentive function through actuarial pricing, or does pooled coverage sever the developer''s marginal exposure, leaving premium flow as the main residue?',
    'Compare safety-investment trajectories of insured versus self-insured developers; examine policy exclusions that re-expose marginal behavior; study repricing events after adverse judgments.',
    'Moral-hazard dominance would recast the arrangement as premium-transfer infrastructure benefiting insurers and deployers; effective actuarial discipline would support the incentive-coordination framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(insurance_intermediation_effect, empirical, 'Whether insurance intermediation transmits or severs the liability signal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__developer_liability, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__developer_liability, theater_ratio, 0, 0.16).
narrative_ontology:measurement(liab_tr_t3, liability_attribution__developer_liability, theater_ratio, 3, 0.19).
narrative_ontology:measurement(liab_tr_t6, liability_attribution__developer_liability, theater_ratio, 6, 0.22).
narrative_ontology:measurement(liab_tr_t9, liability_attribution__developer_liability, theater_ratio, 9, 0.25).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__developer_liability, theater_ratio, 12, 0.28).
narrative_ontology:measurement(liab_tr_t15, liability_attribution__developer_liability, theater_ratio, 15, 0.3).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__developer_liability, base_extractiveness, 0, 0.46).
narrative_ontology:measurement(liab_be_t3, liability_attribution__developer_liability, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(liab_be_t6, liability_attribution__developer_liability, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(liab_be_t9, liability_attribution__developer_liability, base_extractiveness, 9, 0.6).
narrative_ontology:measurement(liab_be_t12, liability_attribution__developer_liability, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(liab_be_t15, liability_attribution__developer_liability, base_extractiveness, 15, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__developer_liability, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(liab_su_t3, liability_attribution__developer_liability, suppression_requirement, 3, 0.36).
narrative_ontology:measurement(liab_su_t6, liability_attribution__developer_liability, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(liab_su_t9, liability_attribution__developer_liability, suppression_requirement, 9, 0.47).
narrative_ontology:measurement(liab_su_t12, liability_attribution__developer_liability, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(liab_su_t15, liability_attribution__developer_liability, suppression_requirement, 15, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__developer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__shared_liability).

% DUAL FORMULATION NOTE:
% The colloquial label 'AI liability' decomposes into three structurally distinct constraint stories per the epsilon-invariance principle: developer_liability (this file — victim set: capability creators, concentrated), deployer_liability (victim set: operators, inverted beneficiary structure), and shared_liability (diffuse contribution-weighted burden, no concentrated victim). Their epsilon values differ because their victim sets differ; measuring 'AI liability' as one observable would average across incompatible arrangements. The upstream/downstream structure runs from whichever reading achieves codification first: this reading's provider-duty infrastructure (disclosure mandates, conformity assessment) becomes the scaffolding the shared reading inherits and the deployer reading must negotiate around.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
