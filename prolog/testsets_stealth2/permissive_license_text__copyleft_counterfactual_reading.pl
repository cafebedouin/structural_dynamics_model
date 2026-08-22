% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__copyleft_counterfactual_reading, []).

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
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Permissive Licensing Without Reciprocity Requirement (Copyleft Counterfactual Reading)
 *   domain: economic/technological/legal
 *
 * SUMMARY:
 *   The contested kernel is the permissive license text (MIT, BSD,
 *   Apache-2.0): copyright relaxation granting use, modification, and
 *   redistribution, including into proprietary products, with no obligation
 *   to return changes. Three readings of this kernel are live. This file
 *   instantiates the copyleft counterfactual reading: relaxation without a
 *   reciprocity requirement structurally enables appropriation of commons
 *   labor, and viral reciprocity (GPL-family) is the necessary corrective.
 *   Per Rule 1 the story classifies ONLY this reading as a clean,
 *   epsilon-invariant constraint: the referent of epsilon is the standing
 *   permissive arrangement as this reading assesses it, never the GPL
 *   arrangement this reading endorses. Family decomposition per the
 *   epsilon-invariance principle: the commons_coordination_reading assesses
 *   the same arrangement near the coordination floor (its epsilon sits far
 *   lower); the corporate_moat_reading assesses the same flows as deliberate
 *   strategy from the appropriator's seat. Same referent, reading-indexed
 *   epsilon; the files are linked through network.affects_constraints. KEY
 *   AGENTS (by structural relationship): proprietary_derivative_builders:
 *   primary beneficiary and partial agenda-setter (institutional/arbitrage) -
 *   converts commons labor into closed revenue; commercial_downstream_users:
 *   secondary beneficiary (institutional/arbitrage) - consumes without
 *   triggering any obligation; commons_contributors: primary target
 *   (moderate/constrained) - bears uncompensated absorption;
 *   independent_maintainers: acute target (powerless/trapped) - bears the
 *   upkeep burden; copyleft_advocates: organized challenger holding a
 *   partial-benefit seat (organized/identity_locked); license_stewards:
 *   agenda-setter of the normative order (institutional/mobile);
 *   dual_license_vendors: arbitrageurs between the two regimes
 *   (powerful/arbitrage); ip_policy_scholars: analytical observer.
 *
 * KEY AGENTS:
 *   - proprietary_derivative_builders: primary beneficiary and partial agenda-setter (institutional/arbitrage) - absorbs permissive code into closed products, funds the permissive normative order, filters reciprocal dependencies out of procurement
 *   - commercial_downstream_users: secondary beneficiary (institutional/arbitrage) - deploys and modifies commons software as internal services with no obligation triggered
 *   - commons_contributors: primary target (moderate/constrained) - publishes work expecting commons growth; firms fold it into closed products; returns arrive as adoption and reputation, not reciprocity
 *   - independent_maintainers: acute target (powerless/trapped) - keeps critical packages alive unpaid beneath corporate revenue stacks; exit strands dependents
 *   - copyleft_advocates: organized challenger with partial benefit (organized/identity_locked) - stewards the reciprocal family, harvests permissive code through the one-way compatibility valve, gains recruits from each absorption case
 *   - license_stewards: agenda-setter (institutional/mobile) - curate the approved-license list and host the normative frame in which reciprocity is optional
 *   - dual_license_vendors: arbitrageur (powerful/arbitrage) - prices the gap between the reciprocal free tier and the paid proprietary exception
 *   - ip_policy_scholars: analytical observer (analytical/analytical) - documents license-choice dynamics and appropriation patterns without material stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.71).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.32).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive Licensing Without Reciprocity Requirement (Copyleft Counterfactual Reading)").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "economic/technological/legal").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, 'f0d78709-4115-4eb3-a040-2ec9121fc771').
narrative_ontology:cs_kernel_codification('f0d78709-4115-4eb3-a040-2ec9121fc771', formalized).
narrative_ontology:cs_authority_grounding('f0d78709-4115-4eb3-a040-2ec9121fc771', practice).
narrative_ontology:cs_interpretation_layer_present('f0d78709-4115-4eb3-a040-2ec9121fc771').
narrative_ontology:cs_reading_relation('f0d78709-4115-4eb3-a040-2ec9121fc771', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0d78709-4115-4eb3-a040-2ec9121fc771', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_axiom('f0d78709-4115-4eb3-a040-2ec9121fc771', foundational, nonreciprocal_relaxation_structurally_exploitative).
narrative_ontology:cs_axiom_status(nonreciprocal_relaxation_structurally_exploitative, holdable).
narrative_ontology:cs_axiom_grounding('f0d78709-4115-4eb3-a040-2ec9121fc771', nonreciprocal_relaxation_structurally_exploitative, empirically_contingent).
narrative_ontology:cs_axiom('f0d78709-4115-4eb3-a040-2ec9121fc771', foundational, viral_reciprocity_necessary_for_commons_persistence).
narrative_ontology:cs_axiom_status(viral_reciprocity_necessary_for_commons_persistence, holdable).
narrative_ontology:cs_axiom_grounding('f0d78709-4115-4eb3-a040-2ec9121fc771', viral_reciprocity_necessary_for_commons_persistence, instrumental).
narrative_ontology:cs_reference_frame('f0d78709-4115-4eb3-a040-2ec9121fc771', reciprocity_conditioned_relaxation).
narrative_ontology:cs_drift_state('f0d78709-4115-4eb3-a040-2ec9121fc771', contemporary_cloud_and_ml_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f0d78709-4115-4eb3-a040-2ec9121fc771', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, proprietary_derivative_builders).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, commercial_downstream_users).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, commons_contributors).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, independent_maintainers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, dual_license_vendors).
narrative_ontology:constraint_vindicates(permissive_license_text__copyleft_counterfactual_reading, copyright_default_exclusivity_doctrine).
narrative_ontology:constraint_vindicates(permissive_license_text__copyleft_counterfactual_reading, nonreciprocal_grant_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build operating systems, cloud services, developer platforms, and machine-learning products that incorporate permissively licensed code. Publish selected internal projects under permissive texts to grow ecosystems around their platforms, fund foundations and conferences that keep permissive norms ascendant, and run procurement rules that steer engineering away from reciprocal-license dependencies. Revenue flows from products assembled partly from commons labor; contributions back are selective and strategic. Leaving the arrangement would mean rebuilding proprietary stacks from scratch or negotiating thousands of individual licenses.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, proprietary_derivative_builders, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, proprietary_derivative_builders, agenda_setter).

% Run businesses on software they did not write and rarely help maintain: permissive texts let them deploy, modify internally, and offer the results as hosted services without publishing changes or negotiating rights, because internal use triggers no obligation under any common license. They bear almost none of the upkeep cost. Their alternative is purchasing equivalent capability under proprietary terms at market rates.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, commercial_downstream_users, beneficiary,
    institutional, biographical, arbitrage, global).

% Publish code and documentation under MIT, BSD, or Apache texts expecting wide use and reputational return. Companies fold the work into closed products; the visible return is adoption, citations, and job offers rather than compensation or returned patches. Relicensing future work under reciprocal terms is possible but splits their audience, breaks downstream integrations, and draws community conflict, so most continue on the same terms while the absorption continues.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, commons_contributors, payer,
    moderate, biographical, constrained, global).

% Keep widely depended-upon packages alive nights and weekends while corporations build paid products on top of them. Bug reports arrive from enterprise users; sponsorship arrives sporadically, usually after a publicized failure. Stepping away would strand dependents and damage their standing in the community they identify with, so they continue despite burnout, and the responsibility itself closes the exit.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, independent_maintainers, payer,
    powerless, biographical, trapped, global).

% Steward the reciprocal license family and argue that relaxation without a share-alike term hands the commons to whoever can appropriate fastest. Their projects freely incorporate permissive code - compatibility runs one way - and each prominent absorption case validates their position and recruits supporters and donors. Their organizations run on donations and license-defense litigation funds; stepping back from the position would dissolve a professional and ideological identity built around reciprocity.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates, observer).

% Foundations and institutes curate the approved license list, certify new texts, and host the normative conversation in which reciprocity is framed as one option among many rather than a requirement. Funding and board seats come disproportionately from corporate members. Their standing depends on remaining the neutral arbiter of the licensing space, which discourages them from endorsing any particular reciprocity position.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, license_stewards, agenda_setter,
    institutional, generational, mobile, global).

% Sell the same codebase under a reciprocal license to competitors and under a paid commercial exception to everyone else. The gap between the free reciprocal tier and the paid proprietary tier is their margin, and permissive commons components round out their stacks at zero acquisition cost. Their business model depends on keeping both regimes legible, enforceable, and distinct.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, dual_license_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Study license-choice dynamics, contribution attrition, and firm appropriation patterns across the software commons. They publish analyses of how different license texts distribute costs and gains among contributors, maintainers, and firms, with no material stake in the flows themselves.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, ip_policy_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__copyleft_counterfactual_reading, proprietary_derivative_builders).
narrative_ontology:fixing_cost_class(permissive_license_text__copyleft_counterfactual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permissive licensing minimizes legal friction for software reuse: any party may integrate, modify, and redistribute code without negotiating rights, solving the transaction-cost problem that would otherwise stall cumulative development across organizational boundaries.
% TRANSFER_FUNCTION: Moves uncompensated engineering and maintenance labor from commons contributors and independent maintainers to proprietary derivative builders and commercial downstream users, who convert the commons into closed products, hosted services, and subscription revenue without reciprocal contribution.
% ABSENT_VOICES: Contributors whose earlier work was absorbed before the asymmetry was visible to them would object, as would the prospective contributors deterred by watching that absorption; the commons as a continuing public has no seat at all. Unanimity that friction-reduction is valuable arises partly because the paying seats are diffuse and individually weak, while the benefiting seats are concentrated, funded, and vocally present in every standards conversation.
% DISAPPEARANCE_RATIONALE: If permissive grants vanished overnight, vast proprietary surfaces - mobile operating systems, cloud control planes, embedded stacks, machine-learning toolchains - would rest on license terms they do not satisfy, forcing either mass relicensing negotiations, mass rewrite, or mass compliance conversion; supply chains, product roadmaps, and the corporate funding structure of the commons itself would reorganize within quarters.
% FOUNDING_PROBLEM: Publicly funded and community-produced code faced legal friction: every reuse required individually negotiated permission, which stalled cumulative development and locked publicly financed work inside institutions. Permissive texts granted broad rights upfront to maximize uptake and dissemination.
% FOUNDING_PROBLEM_CORROBORATION: Independent maintainers attest from outside the beneficiary set that friction-reduction is real and still needed - they chose permissive terms to secure adoption - while simultaneously attesting that the absorption pattern is real; academic license-choice research and documented maintainer-burnout testimony corroborate both halves. No party disputes that the friction problem is live; the live dispute is over the missing reciprocity term, which the beneficiaries frame as optional and the paying seats frame as the defect.
narrative_ontology:disappearance_verdict(permissive_license_text__copyleft_counterfactual_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__copyleft_counterfactual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__copyleft_counterfactual_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.71: the referent is the standing permissive arrangement assessed by this reading's lights - systematic conversion of commons labor into closed revenue, escalating as cloud delivery and machine-learning training opened surfaces that even reciprocal licenses had not reached. It is not higher because contributors demonstrably receive real returns (adoption, reputation, employment) through the same channel that absorbs their work, and the arrangement's coordination value is genuine. Suppression 0.32: participation is voluntary and the reciprocal alternative is fully available; the regime's coercive component operates as default-setting and ecosystem gravity rather than prohibition. Suppression is authored as a raw structural property and is not scaled by power or scope - only extractiveness is scaled, by directionality and the global scope's verification difficulty. Theater 0.22: the functional core (frictionless reuse) is real; a growing performative layer of openness branding accompanies appropriation. Accessibility_collapse 0.25: understanding the arrangement does not collapse alternatives - reciprocal licensing remains fully available and widely used, which is precisely why this is not a mountain profile. Resistance 0.55: decades of organized copyleft advocacy, the AGPL closure of the service-provider loophole, and recurring license-switch waves constitute sustained active resistance. The measurement series run on one shared seven-point grid so every tracked metric is authored at every examined time point; the suppression_requirement series is authored deliberately because the story traces enforcement-capacity change: the machinery keeping reciprocity out of corporate dependency graphs (procurement filters, contributor-agreement aggregation, foundation funding leverage) was built up progressively over the interval, matching the rising scalar.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the proprietary builder's seat (beneficiary-declared, arbitrage exit, low directionality) the arrangement presents as a functioning gift economy it helps fund - coordination with negligible personal extraction. From the independent maintainer's seat (trapped, near-full-target directionality) the same arrangement presents as uncompensated servitude beneath revenue-bearing stacks. The copyleft advocate's seat is the strangest: a declared beneficiary whose benefit is indirect (one-way code inflow plus rhetorical fuel) and whose committed position is that the arrangement should not exist in its current form. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: proprietary_derivative_builders and commercial_downstream_users sit near the beneficiary end (d near 0.0-0.1); commons_contributors and independent_maintainers sit near the target end. Two overrides correct derivations the structural data alone would get wrong. First, moderate-power agents (commons_contributors) are overridden to d=0.6: the derivation from victim-plus-constrained-exit would place them near full target, but they receive substantive returns - adoption, reputation, hiring signal - through the very channel that absorbs their work, making their position partially symmetric. Second, organized-power agents (copyleft_advocates) are overridden to d=0.2: the derivation from their beneficiary declaration would place them near full beneficiary, but their gain is indirect - permissive code flows freely into reciprocal projects while the reverse flow is blocked, and each appropriation case recruits for their cause - not rent capture from the arrangement's operation. Institutional-power agents are left to derivation: the builders' beneficiary declaration plus arbitrage exit correctly yields low d, and no override is needed to distinguish them from the stewards, who carry no beneficiary or victim declaration and fall to the power-atom fallback.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - legal friction blocking reuse of publicly funded and community code - is live, not dead: it recurs on every new integration surface (hosted services, machine-learning training corpora), so the arrangement persists by continuing function, not inertia, and the piton reading is ruled out. The classification work is keeping the two mislabelings apart. Reading the arrangement as pure extraction (snare) erases the genuine coordination achievement - frictionless reuse built the modern software stack and delivered real returns to contributors. Reading it as pure coordination (rope) erases the enforced asymmetry - the take-without-return flow is not an accident but is actively maintained by procurement filtering of reciprocal dependencies and by funding leverage over the normative institutions. Tangled rope preserves both facts: real coordination function, asymmetric extraction through the same structure, active enforcement holding the asymmetry. The mandate has not outlived its function; what this reading contests is the missing reciprocity term, not the arrangement's continued usefulness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This file instantiates one reading (copyleft_counterfactual_reading) of the permissive_license_text kernel; what would the sibling readings change structurally?',
    'Compile the sibling stories (permissive_license_text__commons_coordination_reading, permissive_license_text__corporate_moat_reading) and compare computed classifications over the identical referent arrangement.',
    'Under the commons_coordination_reading the same arrangement assesses near the coordination floor (rope-shaped, low epsilon); under the corporate_moat_reading the same flows are endorsed strategy from the appropriator''s seat (high epsilon, beneficiary-held). The disagreement is located in whether non-reciprocity is a defect or a design feature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one of three live readings of the permissive-license kernel; sibling readings instantiate different constraints with different epsilon over the same referent.').

omega_variable(
    counterfactual_identifiability,
    'The reading''s core claim is counterfactual-causal: exploitation follows from relaxation lacking a reciprocity requirement. Can that causal claim be identified when the reciprocal and permissive regimes coevolved over the same history?',
    'Natural experiments across niches where reciprocal variants dominate (AGPL service niches, dual-license markets) versus permissive-dominated niches: compare long-run contributor retention, firm contribution rates, and commons replenishment.',
    'Confirmation pushes the arrangement toward the snare boundary (coordination as cover); disconfirmation relaxes epsilon toward the rope range and weakens the necessity-of-virality axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_identifiability, empirical, 'Identifiability of the no-reciprocity counterfactual given regime coevolution.').

omega_variable(
    exploitation_or_exchange_baseline,
    'Is uncompensated absorption of permissive code exploitation, or a completed exchange in which contributors traded labor for adoption, reputation, and career capital?',
    'Panel data on contributor expectations at contribution time versus realized returns; survey evidence on whether contributors would re-choose the same terms knowing the absorption pattern.',
    'An exchange framing lowers epsilon toward the rope range; an exploitation framing sustains high epsilon and supports the structural-exploitation axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exploitation_or_exchange_baseline, preference, 'Baseline-entitlement dependence of the exploitation verdict.').

omega_variable(
    enforcement_machinery_attribution,
    'How much of the asymmetry''s persistence is active corporate filtering (procurement bans on reciprocal dependencies, contributor-agreement aggregation, foundation funding leverage) versus passive default selection?',
    'Audit corporate dependency policies, contributor-license-agreement registries, and disclosed lobbying against reciprocity mandates.',
    'If persistence is mostly passive, requires_active_enforcement overstates the structure and the classification drifts toward rope; if active, the tangled_rope reading holds and the enforcement-hardening trajectory in the measurement series is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_machinery_attribution, empirical, 'Active-versus-passive attribution of the non-reciprocity asymmetry''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(perm_tr_t0, observed).
narrative_ontology:measurement(perm_tr_t5, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement_basis(perm_tr_t5, observed).
narrative_ontology:measurement(perm_tr_t10, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(perm_tr_t10, observed).
narrative_ontology:measurement(perm_tr_t15, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(perm_tr_t15, observed).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(perm_tr_t20, observed).
narrative_ontology:measurement(perm_tr_t25, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 25, 0.21).
narrative_ontology:measurement_basis(perm_tr_t25, observed).
narrative_ontology:measurement(perm_tr_t30, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(perm_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(perm_be_t0, observed).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(perm_be_t5, observed).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(perm_be_t10, observed).
narrative_ontology:measurement(perm_be_t15, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(perm_be_t15, observed).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(perm_be_t20, observed).
narrative_ontology:measurement(perm_be_t25, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement_basis(perm_be_t25, observed).
narrative_ontology:measurement(perm_be_t30, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement_basis(perm_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(perm_su_t0, observed).
narrative_ontology:measurement(perm_su_t5, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 5, 0.21).
narrative_ontology:measurement_basis(perm_su_t5, observed).
narrative_ontology:measurement(perm_su_t10, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 10, 0.24).
narrative_ontology:measurement_basis(perm_su_t10, observed).
narrative_ontology:measurement(perm_su_t15, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 15, 0.27).
narrative_ontology:measurement_basis(perm_su_t15, observed).
narrative_ontology:measurement(perm_su_t20, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 20, 0.29).
narrative_ontology:measurement_basis(perm_su_t20, observed).
narrative_ontology:measurement(perm_su_t25, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 25, 0.31).
narrative_ontology:measurement_basis(perm_su_t25, observed).
narrative_ontology:measurement(perm_su_t30, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 30, 0.32).
narrative_ontology:measurement_basis(perm_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, resource_allocation).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__corporate_moat_reading).

% DUAL FORMULATION NOTE:
% Constraint family for kernel permissive_license_text. The colloquial label 'permissive licensing' conflates three structurally distinct claims and is decomposed per the epsilon-invariance principle: this file (copyleft_counterfactual_reading, epsilon 0.71, tangled_rope claim), permissive_license_text__commons_coordination_reading (same referent assessed as friction-minimizing coordination, epsilon near the coordination floor), and permissive_license_text__corporate_moat_reading (same flows assessed as endorsed strategy from the appropriator's seat). Same referent arrangement, reading-indexed epsilon values; no story hedges across readings. The commons reading is upstream in exposition - its descriptive friction account is the part of the kernel this reading accepts while rejecting its sufficiency - and the corporate-moat reading documents the appropriation mechanics this reading condemns; each file links the others through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(permissive_license_text__copyleft_counterfactual_reading, moderate, 0.6).
constraint_indexing:directionality_override(permissive_license_text__copyleft_counterfactual_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
