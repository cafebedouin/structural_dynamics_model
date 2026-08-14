% ============================================================================
% CONSTRAINT STORY: algorithmic_attribution_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_attribution_flat_control, []).

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
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: algorithmic_attribution_flat_control
 *   human_readable: Doctrine of Corporate Attribution for Algorithmic Systems
 *   domain: law_and_technology/products_liability/first_amendment
 *
 * SUMMARY:
 *   Across products liability, algorithmic pricing antitrust, and platform
 *   speech doctrine, courts and litigants converge on a single shared
 *   commitment: that a firm can be treated in law as the author of what its
 *   algorithmic systems do. This is not four different doctrines that happen
 *   to share vocabulary — it is one persisting attribution principle applied
 *   to structurally different questions (is this a defect? is this concerted
 *   action? is this speech?). Every actor invokes the same principle while
 *   disputing what kind of act it makes the firm the author of. This story
 *   authors that shared commitment FLAT, as a single constraint, without
 *   decomposing into separate per-domain readings — the contestation is
 *   captured through stakeholder seat divergence and omega variables rather
 *   than through reading decomposition.
 *
 * KEY AGENTS:
 *   - injured_products_liability_plaintiffs: powerless/trapped — need broad attribution to establish a defendant and duty of care
 *   - realpage_and_algorithmic_pricing_vendors: organized/arbitrage — needs narrow attribution to avoid antitrust coordination liability, benefits from a favorable reading exactly where plaintiffs need an unfavorable one
 *   - third_circuit: institutional/analytical — reads algorithmic curation as attributable first-party expressive conduct in the speech context
 *   - department_of_justice: institutional/analytical — needs attribution to reach through software to a hub-and-spoke conspiracy
 *   - legal_scholars_and_appellate_judges: analytical/analytical — observe the doctrine's incoherence across contexts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_attribution_flat_control, 0.42).
domain_priors:suppression_score(algorithmic_attribution_flat_control, 0.38).
domain_priors:theater_ratio(algorithmic_attribution_flat_control, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_attribution_flat_control, extractiveness, 0.42).
narrative_ontology:constraint_metric(algorithmic_attribution_flat_control, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(algorithmic_attribution_flat_control, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(algorithmic_attribution_flat_control, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(algorithmic_attribution_flat_control, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_attribution_flat_control, tangled_rope).
narrative_ontology:human_readable(algorithmic_attribution_flat_control, "Doctrine of Corporate Attribution for Algorithmic Systems").
narrative_ontology:topic_domain(algorithmic_attribution_flat_control, "law_and_technology/products_liability/first_amendment").

domain_priors:requires_active_enforcement(algorithmic_attribution_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(algorithmic_attribution_flat_control, algorithmic_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_attribution_flat_control, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_attribution_flat_control, algorithmic_pricing_vendors).
narrative_ontology:constraint_victim(algorithmic_attribution_flat_control, injured_products_liability_plaintiffs).
narrative_ontology:constraint_victim(algorithmic_attribution_flat_control, rental_market_tenants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(algorithmic_attribution_flat_control, realpage_and_algorithmic_pricing_vendors).
narrative_ontology:constraint_beneficiary(algorithmic_attribution_flat_control, algorithm_manufacturers_general).
narrative_ontology:constraint_victim(algorithmic_attribution_flat_control, algorithm_manufacturers_general).
narrative_ontology:constraint_vindicates(algorithmic_attribution_flat_control, corporate_personhood_of_algorithmic_output).
narrative_ontology:constraint_vindicates(algorithmic_attribution_flat_control, firm_as_author_of_automated_conduct).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sue a manufacturer after an algorithmic control system (e.g. in a vehicle or medical device) causes harm, invoking attribution to establish the firm authored the defective behavior and owes a duty of care. They need attribution to succeed for exactly the opposite reason RealPage needs it to fail — they want the algorithm's conduct pinned on the firm as a product defect, and the doctrine's flexibility about what counts as 'authorship' determines whether they can even get to a jury.
narrative_ontology:constraint_stakeholder(algorithmic_attribution_flat_control, injured_products_liability_plaintiffs, payer,
    powerless, biographical, trapped, national).

% Built and licenses revenue-management software that recommends rents to competing landlords using shared nonpublic data. Argues that the software's pricing recommendations are not attributable to RealPage as anticompetitive coordination — the algorithm merely processes independently-submitted data, and any 'coordination' is the landlords' independent adoption of suggestions, not RealPage's authored conduct. Simultaneously benefits from attribution being read narrowly here, exactly where it is read broadly for products-liability defendants.
narrative_ontology:constraint_stakeholder(algorithmic_attribution_flat_control, realpage_and_algorithmic_pricing_vendors, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(algorithmic_attribution_flat_control, realpage_and_algorithmic_pricing_vendors, beneficiary).

% Pay algorithmically-coordinated rents allegedly elevated by shared use of the pricing software across a metro area's landlords. They bear the transfer if attribution succeeds in classifying the software's output as RealPage's own coordinating conduct (a per se antitrust violation) rather than as parallel independent business decisions; if attribution fails, the alleged price-fixing has no legal author and no remedy exists.
narrative_ontology:constraint_stakeholder(algorithmic_attribution_flat_control, rental_market_tenants, payer,
    powerless, biographical, trapped, regional).

% Adjudicates whether an algorithm's automated content-recommendation output constitutes the platform's own 'first-party speech' for First Amendment purposes (Anderson v. TikTok) — ruling that curation and algorithmic amplification IS attributable expressive conduct by the platform, not merely third-party content the platform passively hosts. This ruling reads attribution broadly in a direction that benefits the platform's liability shield in some contexts while exposing it in others, depending on which doctrine (Section 230 or First Amendment) is doing the work.
narrative_ontology:constraint_stakeholder(algorithmic_attribution_flat_control, third_circuit, agenda_setter,
    institutional, generational, analytical, national).

% Prosecutes RealPage and landlord defendants under Sherman Act theories, arguing the algorithmic pricing tool operationalizes an agreement among competitors — attributing the coordinating function to RealPage as the hub of a hub-and-spoke conspiracy. The DOJ's theory requires attribution to reach past the software to both RealPage's design choices and the landlords' knowing participation, a harder attribution claim than either products liability or content moderation doctrine requires.
narrative_ontology:constraint_stakeholder(algorithmic_attribution_flat_control, department_of_justice, agenda_setter,
    institutional, generational, analytical, national).

% The broader class of firms deploying algorithmic decision systems (autonomous vehicles, content platforms, pricing engines, hiring tools) that must operate under a doctrine whose content shifts case by case depending on which body of law and which litigant is invoking it. They benefit when attribution is read narrowly (limiting liability) and are harmed when it is read broadly (expanding it), and cannot predict in advance which reading a given court or agency will apply to their product.
narrative_ontology:constraint_stakeholder(algorithmic_attribution_flat_control, algorithm_manufacturers_general, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(algorithmic_attribution_flat_control, algorithm_manufacturers_general, beneficiary).

% Study the doctrine's incoherence across products liability, antitrust, and First Amendment contexts, noting that 'attribution' functions as a single verbal formula stretched to answer structurally different questions (is this a product defect? is this concerted action? is this speech?) without a unifying theory of what makes automated conduct the firm's own act.
narrative_ontology:constraint_stakeholder(algorithmic_attribution_flat_control, legal_scholars_and_appellate_judges, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(algorithmic_attribution_flat_control, diffuse).
narrative_ontology:fixing_cost_class(algorithmic_attribution_flat_control, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single legal vocabulary — 'the firm authored this algorithmic act' — that lets courts and agencies resolve who bears legal consequences for automated systems without requiring a new doctrinal apparatus for every domain (products liability, antitrust, speech) each time software replaces human decision-making.
% TRANSFER_FUNCTION: Moves liability exposure and litigation risk between firms and the people affected by their algorithmic systems, with the direction of the transfer depending entirely on which doctrinal context invokes attribution — broad attribution transfers risk to firms (products liability, antitrust coordination theories); narrow attribution transfers risk to plaintiffs and consumers (RealPage's defense, platform immunity claims).
% ABSENT_VOICES: The algorithmic systems' actual designers and the third-party data contributors whose independent submissions feed pricing/recommendation engines are rarely named parties; their design and input choices are folded into 'the firm's' attributed conduct without their own liability exposure being litigated, and consumers harmed by unattributed algorithmic coordination (where courts find no single author) have no forum at all.
% DISAPPEARANCE_RATIONALE: If courts stopped treating algorithmic output as imputable to the deploying firm at all, entire liability regimes collapse: products liability claims against automated systems would fail for want of a defendant's authored conduct, antitrust hub-and-spoke theories against algorithmic pricing coordination would have no hub, and platform immunity fights would dissolve because there would be nothing to attribute either to the platform or shield as third-party content. Conversely if attribution became automatic and total, firms could not disclaim any algorithmic output as independent third-party conduct, collapsing distinctions between recommendation, coordination, and passive hosting that current doctrine still treats as different acts.
% FOUNDING_PROBLEM: Legal liability doctrine was built around human agency — intent, authorship, concerted action all presuppose a human actor whose mental state and conduct can be examined. Automated systems that generate recommendations, prices, or content without a specific human decision at the point of output broke that presupposition, and courts needed some principle to decide whether the deploying firm 'did' the thing the algorithm did.
% FOUNDING_PROBLEM_CORROBORATION: Independent legal academics (not litigants or regulators) documenting the doctrinal incoherence across the products-liability, antitrust, and First Amendment lines attest the underlying problem — how to locate legal authorship in automated conduct — remains genuinely unresolved; it is not merely a cover story asserted by firms seeking to escape liability, since plaintiffs' bar scholarship independently corroborates the same gap from the opposite interest.
narrative_ontology:disappearance_verdict(algorithmic_attribution_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(algorithmic_attribution_flat_control, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(algorithmic_attribution_flat_control, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-13',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(algorithmic_attribution_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(algorithmic_attribution_flat_control, 0.42, 'claude-sonnet-5', 'algorithmic_authorless_harm_2026_20260813_215102', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_attribution_flat_control_tests).
:- end_tests(algorithmic_attribution_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rising modestly over the measured interval: the doctrine itself is not primarily an extraction mechanism, but its elasticity is increasingly exploited — firms invoke narrow attribution where liability threatens and implicitly benefit from broad attribution's coordination-solving function where it protects their business model (e.g., licensing a pricing algorithm rather than directly fixing prices). Suppression is moderate (0.38) — no single party is coerced into accepting a fixed reading, but the doctrinal indeterminacy itself functions as a soft suppression of predictable recourse for plaintiffs and tenants, since the same firm can argue opposite attribution theories in different fora. Theater ratio is moderate (0.3): courts perform doctrinal continuity ('this is just ordinary tort/antitrust/First Amendment law applied to new facts') while substantively deciding novel questions about machine agency without acknowledging the novelty.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of RealPage and algorithm manufacturers generally, the doctrine looks like ordinary, functional legal categorization — a coordination mechanism that lets business proceed with reasonable certainty about which acts are theirs. From the seat of an injured products-liability plaintiff or a tenant paying an algorithmically-coordinated rent, the same doctrine looks like a moving target: the firm that benefits from disclaiming authorship in one forum is often the same firm claiming authorship (as protected speech, as legitimate business judgment) in another. The engine's per-seat computation should reflect this: powerful, organized, arbitrage-exit stakeholders see something closer to a workable coordination rule; powerless, trapped stakeholders see something closer to extraction dressed as neutral doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the firms that can selectively invoke whichever attribution reading suits the forum — platform operators favor broad attribution when it lets them claim algorithmic curation as protected first-party speech (shielding against certain claims) and narrow attribution when it lets them disclaim the algorithm's coordinating function (shielding against antitrust liability). Victims are the parties who need a stable, predictable attribution rule to obtain a remedy at all: products-liability plaintiffs need broad attribution to establish authorship of a defect; rental tenants need broad attribution to establish RealPage as the coordinating hub. Both victim groups are structurally powerless and trapped relative to the doctrine's shifting content — they cannot choose which court or which legal theory governs their harm.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to locate legal authorship when a human decision-maker is replaced by an automated system — remains genuinely live; it has not become a dead mandate maintained by inertia. Classifying this as tangled_rope rather than snare acknowledges the doctrine still solves a real coordination problem (courts need SOME principle to adjudicate algorithmic-conduct cases, and 'attribution to the deploying firm' is a coherent, workable starting point that avoids either total impunity for automated harm or absurd strict liability for every algorithmic output). The extraction is that firms with sophisticated counsel can navigate the doctrine's elasticity to select favorable readings context by context, while individual plaintiffs and tenant classes face the doctrine as a fixed, unpredictable wall.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unified_theory_of_algorithmic_authorship,
    'Is there a single coherent theory of what makes automated conduct ''the firm''s own act,'' or is ''attribution'' simply a label courts apply post hoc to reach the result each doctrinal context independently favors?',
    'Systematic doctrinal analysis comparing the actual tests applied in products liability (design defect, foreseeability), antitrust (agreement, concerted action), and First Amendment (editorial judgment, expressive conduct) cases to determine whether a common substantive standard for ''authorship'' can be extracted, or whether the tests are irreducibly domain-specific despite sharing vocabulary.',
    'If a unified theory exists, the current inconsistency is a transitional problem correctable by doctrinal harmonization (supporting a rope or scaffold reading). If no unified theory exists and the doctrine''s elasticity is structurally permanent, sophisticated repeat-player firms will persistently be able to select favorable readings across fora, which supports the tangled_rope or even snare reading for the affected victim classes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unified_theory_of_algorithmic_authorship, conceptual, 'Whether algorithmic attribution doctrine has one coherent underlying standard or is an assemblage of unrelated context-specific tests.').

omega_variable(
    regulatory_capture_via_selective_invocation,
    'Do firms systematically invest in litigation and lobbying strategies that shape which attribution reading prevails in which forum, such that the doctrine''s apparent flexibility is itself a product of asymmetric resource investment rather than neutral judicial reasoning?',
    'Track litigation funding, amicus participation, and outcome patterns across products-liability, antitrust, and First Amendment algorithmic-attribution cases to see whether well-resourced repeat players consistently obtain their preferred reading at a rate exceeding what neutral doctrinal application would predict.',
    'If yes, the doctrine''s flexibility functions as extraction cover — a genuinely tangled_rope-to-snare-leaning structure where the coordination story (''courts need SOME rule'') masks systematic asymmetric capture of the rule''s content. If no, the observed divergence across cases is better explained by genuinely different fact patterns and legal questions, supporting a more benign tangled_rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_via_selective_invocation, empirical, 'Whether the doctrine''s flexibility is exploited through resourced litigation strategy or reflects genuine doctrinal variation.').

omega_variable(
    corporate_personhood_extension_naturalness,
    'Is treating a firm as the ''author'' of its algorithm''s autonomous output a natural extension of existing corporate-agency doctrine (respondeat superior, corporate mens rea), or a novel and contestable expansion that happens to be convenient for both plaintiffs and defendants depending on context?',
    'Historical and comparative analysis of how corporate agency doctrine treated non-human instrumentalities (mechanical devices, statistical models, early automated systems) prior to modern algorithmic systems, to establish whether current attribution doctrine is continuous with settled corporate-law principles or a genuine doctrinal innovation.',
    'If continuous with settled doctrine, the current friction is application-level rather than foundational, and beneficiary declarations here would not indicate false naturalization. If a genuine innovation dressed in the language of settled doctrine, this constraint would be a stronger candidate for false-summit-style scrutiny — a novel constructed rule presented as natural extension of established law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_personhood_extension_naturalness, conceptual, 'Whether the attribution doctrine is a natural extension of prior corporate-agency law or a novel doctrinal construction presented as continuous with it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_attribution_flat_control, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algo_tr_t0, algorithmic_attribution_flat_control, theater_ratio, 0, 0.2).
narrative_ontology:measurement(algo_tr_t4, algorithmic_attribution_flat_control, theater_ratio, 4, 0.22).
narrative_ontology:measurement(algo_tr_t8, algorithmic_attribution_flat_control, theater_ratio, 8, 0.25).
narrative_ontology:measurement(algo_tr_t12, algorithmic_attribution_flat_control, theater_ratio, 12, 0.27).
narrative_ontology:measurement(algo_tr_t16, algorithmic_attribution_flat_control, theater_ratio, 16, 0.29).
narrative_ontology:measurement(algo_tr_t20, algorithmic_attribution_flat_control, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(algo_be_t0, algorithmic_attribution_flat_control, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(algo_be_t4, algorithmic_attribution_flat_control, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(algo_be_t8, algorithmic_attribution_flat_control, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(algo_be_t12, algorithmic_attribution_flat_control, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(algo_be_t16, algorithmic_attribution_flat_control, base_extractiveness, 16, 0.41).
narrative_ontology:measurement(algo_be_t20, algorithmic_attribution_flat_control, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(algo_su_t0, algorithmic_attribution_flat_control, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(algo_su_t4, algorithmic_attribution_flat_control, suppression_requirement, 4, 0.28).
narrative_ontology:measurement(algo_su_t8, algorithmic_attribution_flat_control, suppression_requirement, 8, 0.31).
narrative_ontology:measurement(algo_su_t12, algorithmic_attribution_flat_control, suppression_requirement, 12, 0.34).
narrative_ontology:measurement(algo_su_t16, algorithmic_attribution_flat_control, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(algo_su_t20, algorithmic_attribution_flat_control, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_attribution_flat_control, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(algorithmic_attribution_flat_control, 0.12).
narrative_ontology:affects_constraint(algorithmic_attribution_flat_control, realpage_antitrust_algorithmic_pricing).
narrative_ontology:affects_constraint(algorithmic_attribution_flat_control, platform_algorithmic_curation_first_amendment).
narrative_ontology:affects_constraint(algorithmic_attribution_flat_control, autonomous_system_products_liability).

% DUAL FORMULATION NOTE:
% This story authors the shared attribution PRINCIPLE flat, as a single constraint, rather than decomposing into per-domain readings (products liability / antitrust / First Amendment). The construction perturbation instructs flat authoring here deliberately: the contested question of whether these are genuinely one constraint or three is left to be tested by comparison against a decomposed reading-set version of the same substrate, rather than resolved by this story's own construction choice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
