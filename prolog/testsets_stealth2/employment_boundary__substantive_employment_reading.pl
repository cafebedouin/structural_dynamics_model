% ============================================================================
% CONSTRAINT STORY: employment_boundary__substantive_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__substantive_employment_reading, []).

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
 *   constraint_id: employment_boundary__substantive_employment_reading
 *   human_readable: Economic Dependence and Algorithmic Control Standard for Platform Work
 *   domain: labor economics/platform economy/social policy
 *
 * SUMMARY:
 *   Across ride-hail, delivery, and microtask platforms, the working
 *   relationship is constituted by service agreements labeling workers
 *   independent contractors, while day-to-day work is directed by algorithmic
 *   management: dispatch algorithms allocate tasks, acceptance-rate and
 *   cancellation metrics gate access to work, customer ratings discipline
 *   service quality, and deactivation removes income without hearing or
 *   appeal. Workers supply their own vehicles, fuel, insurance, and unpaid
 *   waiting time; earnings carry no floor; sickness, injury, and old age fall
 *   outside employer-provided provision and land on households and public
 *   assistance. Platforms finance a standing legal and political defense of
 *   the contractor classification — arbitration clauses with class-action
 *   waivers, ballot-measure campaigns, legislative lobbying — because the
 *   classification is the margin on which the business model's labor-cost
 *   advantage rests. This story authors that standing arrangement as the
 *   economic-dependence-and-algorithmic-control account of it: a work
 *   relationship functionally indistinguishable from employment in dependence
 *   and control, priced and governed as if it were independent trade. KEY
 *   AGENTS (by structural relationship): - platform_workers: Primary target
 *   (moderate/constrained) — bear the costs the classification shifts to
 *   them: capital, risk, unpaid time, missing social provision -
 *   platform_operators: Primary beneficiary and agenda setter
 *   (institutional/arbitrage) — collect the labor-cost differential and set
 *   the contractual and algorithmic terms - platform_consumers: Incidental
 *   beneficiary (organized/mobile) — receive lower prices partly financed by
 *   the shifted costs - public_social_insurance_systems: Secondary payer
 *   (institutional/trapped) — absorb uncovered injury, illness, and old-age
 *   costs of misclassified workers - labor_regulators_and_courts:
 *   Adjudicating observer (institutional/analytical) — apply classification
 *   tests whose outcomes set the boundary's reach - gig_worker_collectives:
 *   Excluded voice (organized/trapped) — organize for reclassification but
 *   were shut out of the rulemaking venues where classification regimes were
 *   written
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, 0.66).
domain_priors:suppression_score(employment_boundary__substantive_employment_reading, 0.67).
domain_priors:theater_ratio(employment_boundary__substantive_employment_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__substantive_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__substantive_employment_reading, "Economic Dependence and Algorithmic Control Standard for Platform Work").
narrative_ontology:topic_domain(employment_boundary__substantive_employment_reading, "labor economics/platform economy/social policy").

domain_priors:requires_active_enforcement(employment_boundary__substantive_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__substantive_employment_reading, '29a812ed-9712-4422-9f46-175e589a71bc').
narrative_ontology:cs_kernel_codification('29a812ed-9712-4422-9f46-175e589a71bc', formalized).
narrative_ontology:cs_authority_grounding('29a812ed-9712-4422-9f46-175e589a71bc', distributed).
narrative_ontology:cs_reading_relation('29a812ed-9712-4422-9f46-175e589a71bc', employment_boundary__formalist_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('29a812ed-9712-4422-9f46-175e589a71bc', employment_boundary__hybrid_security_reading, forecloses).
narrative_ontology:cs_axiom('29a812ed-9712-4422-9f46-175e589a71bc', foundational, economic_dependence_defines_employment).
narrative_ontology:cs_axiom_status(economic_dependence_defines_employment, holdable).
narrative_ontology:cs_axiom_grounding('29a812ed-9712-4422-9f46-175e589a71bc', economic_dependence_defines_employment, deontological).
narrative_ontology:cs_axiom('29a812ed-9712-4422-9f46-175e589a71bc', foundational, algorithmic_control_constitutes_employer_control).
narrative_ontology:cs_axiom_status(algorithmic_control_constitutes_employer_control, holdable).
narrative_ontology:cs_axiom_grounding('29a812ed-9712-4422-9f46-175e589a71bc', algorithmic_control_constitutes_employer_control, empirically_contingent).
narrative_ontology:cs_axiom('29a812ed-9712-4422-9f46-175e589a71bc', secondary, contract_labels_subordinate_to_working_substance).
narrative_ontology:cs_axiom_status(contract_labels_subordinate_to_working_substance, holdable).
narrative_ontology:cs_axiom_grounding('29a812ed-9712-4422-9f46-175e589a71bc', contract_labels_subordinate_to_working_substance, conventional).
narrative_ontology:cs_reference_frame('29a812ed-9712-4422-9f46-175e589a71bc', dependence_control_protective_boundary).
narrative_ontology:cs_drift_state('29a812ed-9712-4422-9f46-175e589a71bc', contemporary_platform_economy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('29a812ed-9712-4422-9f46-175e589a71bc', '').
narrative_ontology:cs_kernel_id(employment_boundary__substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_operators).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_consumers).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, public_social_insurance_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft the service agreements that label workers independent contractors, operate the dispatch, rating, and deactivation systems that direct daily work, and fund the legal and political defense of the classification. They collect the difference between what an employment relationship would cost and what contractor engagement costs, and can restructure operations, enter or leave markets, or adjust business models faster than any regulator can respond.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Supply vehicles, fuel, insurance, and unpaid waiting time; accept or decline dispatched work under acceptance-rate and rating metrics; earn without a floor and without sick pay, injury coverage, or pension accrual. Leaving the platform means forfeiting an income stream many households rely on; moving to traditional employment is possible but bounded by local labor-market conditions, and multi-apping spreads rather than removes the dependency.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_workers, payer,
    moderate, biographical, constrained, global).

% Receive fast, cheap, on-demand transport and delivery priced below what employee-based provision would likely cost. They also carry a diffuse share of the arrangement's costs through taxes that fund emergency care, supplemental assistance, and old-age support for workers without employer provision. Switching between rival apps is easy, which keeps their individual leverage high and their attention on price and speed.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_consumers, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__substantive_employment_reading, platform_consumers, payer).

% Absorb the injury, illness, unemployment, and old-age costs of a workforce classified outside mandatory contribution schemes — through public healthcare, workers'-compensation gaps, housing and food assistance, and eventual old-age poverty programs. They cannot decline the function: the safety net operates wherever workers fall through, and contribution bases erode as more work migrates outside covered employment.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, public_social_insurance_systems, payer,
    institutional, generational, trapped, national).

% Apply classification tests — control, integration, economic-reality, ABC — whose outcomes determine whether any given platform workforce falls inside or outside employment law. They hear misclassification claims, weigh testimony from all other seats, and in several jurisdictions have already redrawn the boundary for specific platforms; their doctrinal choices, not platform preferences alone, set the constraint's reach.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, labor_regulators_and_courts, observer,
    institutional, generational, analytical, national).

% Organize drivers and couriers for reclassification, strike, and litigate, but were largely absent from the venues where classification rules were actually written — the platform-backed ballot measure that cemented contractor status in California was drafted and funded by the operators, with worker organizations outside the drafting room. Their members' livelihoods depend on the arrangement they are excluded from redesigning.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, gig_worker_collectives, excluded,
    organized, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__substantive_employment_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__substantive_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Real-time matching of dispersed labor supply to fluctuating demand, with integrated routing, payment handling, and reputation infrastructure — solved once, centrally, instead of through per-worker job search and per-transaction negotiation.
% TRANSFER_FUNCTION: Moves the costs of labor — vehicle capital, insurance, injury risk, unpaid waiting time, retirement provision — from platforms to workers themselves; transfers the resulting savings partly to consumers as lower prices and partly to platform margins; and shifts the contingent liabilities (uncovered injury, illness, old-age poverty) onto public social-insurance systems.
% ABSENT_VOICES: Gig worker collectives and deactivated or injured workers were largely absent from the rulemaking tables where classification regimes were set — the leading platform-backed ballot measure was drafted and funded by the operators with worker organizations excluded from drafting. Social-insurance actuaries, who absorb the shifted liabilities, are likewise rarely consulted in classification debates.
% DISAPPEARANCE_RATIONALE: If the contractor classification vanished overnight and every platform worker held employee status, platform pricing would rise to carry payroll taxes and insurance contributions, fleet and logistics models would restructure around scheduled shifts, some marginal demand would retreat to slower alternatives, payroll contributions would flow into social-insurance funds that currently absorb the shortfall, and the operators' unit economics would be rebuilt from the ground up — the platform labor market would reorganize around the employment relationship.
% FOUNDING_PROBLEM: Employment law's boundary was drawn to distinguish dependent wage-workers — who needed protection from an employer's power over their livelihood — from genuinely independent traders; the supervisory control test presumed a visible boss directing work at a fixed workplace, and the whole protective apparatus (wage floors, working-time limits, social insurance) attached to whichever side of that line a worker fell.
% FOUNDING_PROBLEM_CORROBORATION: Labor-law scholarship (De Stefano, Davidov and the economic-dependence literature), ILO decent-work reporting, and the UK Supreme Court's rider-classification reasoning all attest, from outside the platform industry, that the protective purpose remains live while the bilateral control-test machinery no longer captures algorithmically managed work. Platform industry groups dispute that the founding problem justifies extending employee status, arguing the modern arrangement is a new form of independent trade — the status is genuinely contested rather than settled in either direction.
narrative_ontology:disappearance_verdict(employment_boundary__substantive_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__substantive_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__substantive_employment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(employment_boundary__substantive_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__substantive_employment_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__substantive_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__substantive_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.66 at interval end: the classification decouples control from responsibility, shifting roughly a quarter to a third of employee-equivalent labor cost onto workers (vehicle capital, insurance, unpaid positioning and waiting time, injury and old-age risk) while platforms retain fare and fee control. It is moderate rather than maximal because workers do receive gross fares and real scheduling flexibility. Suppression is 0.67 and is authored as a raw structural property, unscaled by power or scope: the arrangement persists through mandatory arbitration with class-action waivers, algorithmic deactivation as discipline, and heavily funded political defense of the classification (the California ballot measure alone absorbed roughly two hundred million dollars of platform spending). Theater ratio is 0.40 and rising: the 'be your own boss' and flexibility framing consumes a growing share of the arrangement's public activity relative to its matching function, as legal and communications budgets shift toward defending the label rather than operating the service. Accessibility collapse is 0.52 — alternatives partially persist (traditional jobs, multi-apping, exiting gig work entirely), but income necessity, local labor-market conditions, and platform network effects keep exits costly, and platforms demonstrably could adopt employment models in some markets yet maintain contractor form wherever law allows. Resistance is 0.68: courier and driver strikes, successful litigation (the UK apex court's rider decision), the Spanish rider statute, and the EU platform-work directive campaign show sustained organized pushback, including coalition formation among nominally powerless individual workers. The temporal series run on one shared grid (2009, 2012, 2015, 2018, 2021, 2024) with every tracked metric authored at every point; all three trajectories rise monotonically — an enforcement ratchet rather than a cycle, so no cyclical-pattern machinery is invoked. The claimed type (tangled_rope) is authored from structure — a genuine matching-and-payment coordination function wrapped around an asymmetric cost-shift — independently of the metric values, which are authored from the arrangement's observed operation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the operator seat the arrangement is a legitimately contracted commercial relationship it built and defends: costs are allocated by agreement, and the classification is ordinary contract freedom — low experienced burden, high defended benefit. From the worker seat the same structure operates as compulsory risk absorption under algorithmic command: the label says independent, the dispatch queue says managed. From the consumer seat it is cheap, fast service with no visible counterparty harm. From the public-insurer seat it is an unfunded liability arriving through emergency rooms and supplemental assistance rolls. From the adjudicator seat it is doctrinal indeterminacy — tests built for supervised factory labor applied to app-mediated work. The engine computes these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. platform_operators sit near the beneficiary pole: they collect the cost differential, set the terms, and hold arbitrage-grade exit (restructuring, market exit, business-model pivots). platform_workers sit near the target pole: they bear the shifted costs with constrained exit — income necessity and localized labor markets, not immobility as such, bind them. platform_consumers derive low directionality from their beneficiary declaration tempered by their secondary payer position (tax-financed backstops for uninsured workers), landing them mildly on the benefit side. public_social_insurance_systems illustrate that power does not set directionality: institutionally powerful and unable to exit the safety-net function, they are nonetheless declared victims and so derive high directionality — they pay for risks the classification externalized. labor_regulators_and_courts take the analytical seat with negligible directionality; gig_worker_collectives, excluded from the rulemaking table, carry the target-side interest without agenda-setting access.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification machinery was built for a founding problem that has partly died in its original form: the bilateral control test presupposed a visible supervisor directing work at a fixed workplace, and that doctrinal apparatus no longer describes how platform work is actually governed — hence mandatrophy_resolved is declared true for the inherited machinery. The classification prevents two symmetrical mislabelings. Reading the arrangement as pure coordination (a rope of flexible matching) would launder the cost-shift behind the flexibility narrative; reading it as pure extraction (a snare with no coordinating function) would condemn the genuine matching, payment, and reputation infrastructure that workers and consumers demonstrably use. The tangled-rope structure keeps both facts visible: the coordination function is real, and the classification boundary riding on top of it is where the asymmetry lives. The receipt surface sharpens this: gains accrue to a named seat (the operators), and fixing is prohibitively expensive relative to any single fixer's benefit — reclassification requires defeating entrenched contract architecture and a well-funded political defense — which is why the arrangement persists despite broad doctrinal dissatisfaction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story instantiates the substantive_employment_reading of the employment_boundary kernel; how would the sibling readings restructure the constraint if they prevailed?',
    'Jurisdiction-by-jurisdiction adoption tracking: which definitional criterion legislatures and apex courts enact (dependence-and-control tests, contract-and-supervision tests, or third-category statutes) determines which reading binds and therefore which victim set and obligation structure is real.',
    'A formalist victory empties the victim set and dissolves the constraint into ordinary contract law; a hybrid victory splits the victim set into a protected middle tier with partial obligations; this reading''s victory converts platforms into full obligation-bearers carrying social insurance and job-security duties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame omega: which reading of the employment-boundary kernel binds, and what each would change structurally.').

omega_variable(
    definitional_criterion_axis,
    'Where exactly do the readings disagree — is the dispute over the definitional criterion (what fact constitutes employment), over the population in scope, or over the protection package attached to the status?',
    'Doctrinal analysis of enacted tests and appellate reasoning across jurisdictions: locate which element each rule treats as decisive and whether divergent outcomes track criterion, population, or package.',
    'If the dispute is purely over the criterion, this reading and the formalist sibling cannot both be operative in one framework (mutual foreclosure); if over population or package, partial overlaps and hybrid settlements become stable equilibria instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definitional_criterion_axis, conceptual, 'Locates the structural element on which the kernel''s readings diverge.').

omega_variable(
    dependence_prevalence,
    'Do platform workers exhibit the economic dependence the substantive premise requires, or are most supplementary earners with genuine outside options?',
    'Earnings-dependence surveys and administrative data: share of platform workers deriving more than half of household income from platform work, weekly hours committed, and observed transition rates into traditional employment.',
    'High dependence prevalence confirms the victim declaration and raises effective extraction on the worker seat; low prevalence shrinks the victim set toward a dependent subset and pulls this reading toward the hybrid settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependence_prevalence, empirical, 'Whether the dependence premise holds for the worker population as a whole.').

omega_variable(
    suppression_internalization_split,
    'Is worker-side attachment to contractor status — visible in support for platform-backed ballot measures and ''be your own boss'' identification — internalized ideology or a rational preference for flexibility?',
    'Post-reclassification welfare tracking in converted jurisdictions (Spain''s rider law, UK worker-status rulings): persistence of stated contractor preference after protections arrive indicates genuine preference; reported relief, reduced tolerance of unpaid hours, and higher retention indicate the prior attachment was adaptive or internalized.',
    'If internalized, behavioral consent understates coercion and the constraint''s true suppression exceeds the structural measure; if rational, part of the measured suppression is priced-in choice and the case for the reading softens accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized component of worker-side consent to the classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__substantive_employment_reading, 2009, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t2009, employment_boundary__substantive_employment_reading, theater_ratio, 2009, 0.2).
narrative_ontology:measurement(empl_tr_t2012, employment_boundary__substantive_employment_reading, theater_ratio, 2012, 0.24).
narrative_ontology:measurement(empl_tr_t2015, employment_boundary__substantive_employment_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(empl_tr_t2018, employment_boundary__substantive_employment_reading, theater_ratio, 2018, 0.33).
narrative_ontology:measurement(empl_tr_t2021, employment_boundary__substantive_employment_reading, theater_ratio, 2021, 0.37).
narrative_ontology:measurement(empl_tr_t2024, employment_boundary__substantive_employment_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(empl_be_t2009, employment_boundary__substantive_employment_reading, base_extractiveness, 2009, 0.42).
narrative_ontology:measurement(empl_be_t2012, employment_boundary__substantive_employment_reading, base_extractiveness, 2012, 0.48).
narrative_ontology:measurement(empl_be_t2015, employment_boundary__substantive_employment_reading, base_extractiveness, 2015, 0.54).
narrative_ontology:measurement(empl_be_t2018, employment_boundary__substantive_employment_reading, base_extractiveness, 2018, 0.59).
narrative_ontology:measurement(empl_be_t2021, employment_boundary__substantive_employment_reading, base_extractiveness, 2021, 0.63).
narrative_ontology:measurement(empl_be_t2024, employment_boundary__substantive_employment_reading, base_extractiveness, 2024, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t2009, employment_boundary__substantive_employment_reading, suppression_requirement, 2009, 0.35).
narrative_ontology:measurement(empl_su_t2012, employment_boundary__substantive_employment_reading, suppression_requirement, 2012, 0.44).
narrative_ontology:measurement(empl_su_t2015, employment_boundary__substantive_employment_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(empl_su_t2018, employment_boundary__substantive_employment_reading, suppression_requirement, 2018, 0.58).
narrative_ontology:measurement(empl_su_t2021, employment_boundary__substantive_employment_reading, suppression_requirement, 2021, 0.63).
narrative_ontology:measurement(empl_su_t2024, employment_boundary__substantive_employment_reading, suppression_requirement, 2024, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__substantive_employment_reading, resource_allocation).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, employment_boundary__formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, employment_boundary__hybrid_security_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'employment status of platform workers' decomposes into three structurally distinct constraints — readings of one kernel — per the epsilon-invariance principle: the formalist reading (incumbent, historically higher empirical confidence), this substantive reading (contestant, dependence-and-control criterion), and the hybrid security reading (mediator, third-category proposal). Each carries its own epsilon, victim set, and classification; forcing one story to span all three would make epsilon observer-relative. The formalist reading is upstream (its doctrine is cited as settled ground against reclassification); this reading pressures both siblings downstream — its evidentiary base (dependence data, algorithmic-control documentation) is what hybrid proposals cite when building presumptions-of-employment compromises. Family members are linked via affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
