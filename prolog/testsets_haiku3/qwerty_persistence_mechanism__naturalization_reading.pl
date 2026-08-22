% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__naturalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__naturalization_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: qwerty_persistence_mechanism__naturalization_reading
 *   human_readable: QWERTY Keyboard Standard (Naturalization Reading)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The QWERTY keyboard layout persists as the global standard for
 *   Latin-alphabet typing. Under the naturalization reading, this persistence
 *   reflects genuine adequacy through fair competition: QWERTY became
 *   dominant because it was the first layout to reach market scale and to
 *   accumulate trained typists, which made it rational for every subsequent
 *   typist to learn QWERTY (maximizing employment options) and for every
 *   employer to adopt QWERTY (maximizing hiring pool). Switching costs are
 *   real and substantial—they represent genuine skill investment, not
 *   manufactured lock-in. The reading acknowledges that alternative layouts
 *   like Dvorak offer theoretical ergonomic or speed advantages, but claims
 *   those advantages are either empirically unproven, insufficient to justify
 *   coordinated retraining, or absent altogether. Under this reading, the
 *   constraint exhibits minimal extraction, negligible theater, and low
 *   active suppression: QWERTY persists through coordination equilibrium, not
 *   coercion. This reading contrasts with the lock-in reading (coordination
 *   failure despite technical inferiority) and the beneficiary-extraction
 *   reading (manufacturers/incumbents actively maintained QWERTY to protect
 *   market power).
 *
 * KEY AGENTS:
 *   - Typists with QWERTY investment (organized, constrained exit): benefit from the standard's persistence because their human capital remains valuable
 *   - Alternative layout proponents (powerless, mobile exit): bear coordination friction; cannot individually switch without retraining and incompatibility costs
 *   - Keyboard manufacturers (organized, mobile exit): benefit modestly from standardization but do not depend on QWERTY for survival; could retool at reasonable cost
 *   - Office managers and employers (institutional, mobile exit): standardize on QWERTY because it matches the labor supply, not because they prefer it
 *   - Typing educators (institutional, constrained exit): teach QWERTY because it is the standard employers demand; they would teach Dvorak if the labor market coordinated on it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__naturalization_reading, 0.18).
domain_priors:suppression_score(qwerty_persistence_mechanism__naturalization_reading, 0.12).
domain_priors:theater_ratio(qwerty_persistence_mechanism__naturalization_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__naturalization_reading, rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__naturalization_reading, "QWERTY Keyboard Standard (Naturalization Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__naturalization_reading, "economic/technological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__naturalization_reading, 'd7018976-38a1-4db7-bad4-347d1c03a035').
narrative_ontology:cs_kernel_codification('d7018976-38a1-4db7-bad4-347d1c03a035', distributed).
narrative_ontology:cs_authority_grounding('d7018976-38a1-4db7-bad4-347d1c03a035', practice).
narrative_ontology:cs_interpretation_layer_present('d7018976-38a1-4db7-bad4-347d1c03a035').
narrative_ontology:cs_reading_relation('d7018976-38a1-4db7-bad4-347d1c03a035', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7018976-38a1-4db7-bad4-347d1c03a035', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('d7018976-38a1-4db7-bad4-347d1c03a035', foundational, qwerty_technical_adequacy).
narrative_ontology:cs_axiom_status(qwerty_technical_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('d7018976-38a1-4db7-bad4-347d1c03a035', qwerty_technical_adequacy, empirically_contingent).
narrative_ontology:cs_axiom('d7018976-38a1-4db7-bad4-347d1c03a035', foundational, switching_cost_reflects_genuine_skill_investment).
narrative_ontology:cs_axiom_status(switching_cost_reflects_genuine_skill_investment, holdable).
narrative_ontology:cs_axiom_grounding('d7018976-38a1-4db7-bad4-347d1c03a035', switching_cost_reflects_genuine_skill_investment, empirically_contingent).
narrative_ontology:cs_reference_frame('d7018976-38a1-4db7-bad4-347d1c03a035', coordination_equilibrium_on_uniform_layout).
narrative_ontology:cs_drift_state('d7018976-38a1-4db7-bad4-347d1c03a035', digital_input_era_post_2000, gap(stable, minor, false)).
narrative_ontology:cs_created_at('d7018976-38a1-4db7-bad4-347d1c03a035', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, typists_with_qwerty_investment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, keyboard_manufacturers).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__naturalization_reading, alternative_layout_proponents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Millions of typists worldwide have invested years learning QWERTY touch-typing. This skill remains valuable and portable because QWERTY is the global standard; they can use any keyboard, work in any office, and maintain their typing speed without retraining. Switching to Dvorak would require months of retraining to regain speed, which would represent a loss of human capital. The constraint benefits them by making their skill valuable; it does not extract from them because the constraint imposes no fee or obligation, only the coordination equilibrium that makes QWERTY learning the rational choice.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, typists_with_qwerty_investment, beneficiary,
    organized, biographical, constrained, global).

% Manufacturers benefit modestly from the QWERTY standard because it allows them to standardize production tooling, simplify marketing, and achieve economies of scale. A keyboard is a keyboard; the layout is a surface specification. Under the naturalization reading, manufacturers did not actively enforce QWERTY; they simply built keyboards in the layout their customers (typists) were trained to use. They could retool to produce Dvorak or any other layout at reasonable cost if demand shifted; their benefits from QWERTY standardization are real but not dependent on suppressing alternatives.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, keyboard_manufacturers, beneficiary,
    organized, generational, mobile, global).

% Ergonomists, typing researchers, and enthusiasts argue that alternative layouts (Dvorak, Colemak, Workman) offer speed or ergonomic advantages. They bear the cost of choosing a non-standard layout: they cannot use shared keyboards in offices without friction; they must retrain anyone who wants to use their keyboard; they cannot leverage the installed base of QWERTY knowledge. Under the naturalization reading, this friction reflects coordination cost, not suppression. Any typist can learn Dvorak; the loss is that their skill is not portable to the broader labor market, and retraining from QWERTY is expensive.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, alternative_layout_proponents, payer,
    powerless, generational, mobile, global).

% Offices and employers standardize on QWERTY keyboards because they are the default available product and because all hires are trained in QWERTY. Under the naturalization reading, they do not actively prefer QWERTY; they are passively responding to the labor supply. If the labor supply had all been trained in Dvorak, offices would install Dvorak keyboards without concern. The coordination problem is one-directional: maintaining QWERTY requires only inaction (do nothing, use the standard everyone already knows); switching to Dvorak requires coordinated retraining of every office worker.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, office_managers_and_employers, agenda_setter,
    institutional, biographical, mobile, global).

% Schools teach QWERTY typing because it is the standard that all students will encounter in subsequent employment, education, and office contexts. They do not advocate for QWERTY's superiority or defend it against alternatives; they teach what the labor market demands. Under the naturalization reading, educators are neutral implementers of the coordination equilibrium, not enforcers. If the labor market had coordinated on Dvorak, educators would teach Dvorak instead. Their actions perpetuate QWERTY, but not through active preference or enforcement.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, typing_educators_schools_and_universities, agenda_setter,
    institutional, generational, constrained, national).

% Researchers investigate the empirical claims about typing speed, error rates, and ergonomic outcomes across layouts. The body of evidence is mixed and contested: some studies show advantages for Dvorak in specific tasks; others find comparable performance; all agree switching costs are real. Researchers do not maintain QWERTY; they measure its properties and compare it to alternatives. Under the naturalization reading, the empirical record should show that Dvorak advantages (if real) are insufficient to justify coordinated retraining, or that such advantages are unproven or conditional on usage patterns.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, typing_researchers_and_ergonomists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform keyboard layout so that typing skill remains portable across devices and employment contexts. Solves a genuine collective-action problem: each individual typist has incentive to learn the standard layout because it maximizes employment options; each employer has incentive to use the standard layout because it maximizes the hiring pool; each manufacturer has incentive to produce the standard layout because it matches existing demand. The coordination function is real and necessary.
% TRANSFER_FUNCTION: Under the naturalization reading, the constraint transfers coordination cost (the cost of being in the minority if you learn a non-standard layout) to those who prefer alternatives. This is not extraction from a beneficiary; it is the natural friction of choosing a non-standard option in a coordination problem. All layout choices impose equivalent friction relative to their adoption scale—Dvorak would impose the same friction on QWERTY learners if roles were reversed.
% ABSENT_VOICES: Ergonomists and disability-services practitioners exist in the conversation (typing research literature, academic conferences) but are peripheral to the office-manager and educator decisions that implement the standard. Pre-QWERTY typing communities (users of Sholes, Lillie, Crandall, and other early machines) are absent because they are historical. Their testimony would show that layout choice was not inevitable; QWERTY happened to be first to reach scale and network effects. Industrial-design advocates for fully customizable or remappable keyboards are present in some technical communities but absent from mainstream office standardization decisions.
% DISAPPEARANCE_RATIONALE: If QWERTY disappeared as a standard overnight, offices would coordinate on some alternative layout within 2–3 years. A coordinated shift to Dvorak, Colemak, or any other layout is entirely possible; the world would reorganize. What would NOT change: the need for a uniform standard. Under the naturalization reading, QWERTY's particular dominance is not required by nature or by market power—it is required by the coordination equilibrium that happened to freeze around QWERTY's early success.
% FOUNDING_PROBLEM: In the 1880s, early mechanical typewriters (Sholes & Glidden, Lillie, Crandall, etc.) used different keyboard layouts. There was no universal standard; a typist trained on one machine would have to relearn on another. Manufacturers faced a problem: without a standard, typing skill was machine-specific and not portable, reducing the value of typing as a labor skill. Manufacturers and typists had mutual interest in a coordinated standard—one that would make skill portable and machines interchangeable. QWERTY emerged as dominant not through superior design but through first-mover advantage: the Sholes & Glidden machine, using the QWERTY layout, reached market scale first and accumulated trained typists. Once the installed base existed, every subsequent machine maker had incentive to adopt QWERTY (to access trained operators) and every typist had incentive to learn QWERTY (to maximize employment options). The founding problem was: how do we establish a single standard so typing skill remains valuable and portable?
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology (David, 1985; Winner, 1980) and economists studying path dependence (Liebowitz & Margolis, 1990; Arthur, 1989) corroborate that the coordination problem remains live. As of the present day, offices and educators cannot individually opt out of QWERTY standardization without bearing switching cost. The disputed claim is not whether the coordination problem persists (all parties agree it does) but whether QWERTY is the *best* solution or merely the *first* solution. The naturalization reading asserts that QWERTY's adequacy justifies continued standardization; the lock-in reading asserts that continued standardization persists despite inadequacy (path-dependent coordination failure); the extraction reading asserts that beneficiaries actively maintain QWERTY to suppress superior alternatives.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__naturalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__naturalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__naturalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__naturalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__naturalization_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__naturalization_reading_tests).
:- end_tests(qwerty_persistence_mechanism__naturalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint imposes coordination costs that are symmetrical—every layout choice imposes equivalent friction on those who learn it. QWERTY does not extract from Dvorak learners any more than Dvorak would extract from QWERTY learners under a coordinated switch. The beneficiaries (QWERTY-trained typists) benefit only from the *existence* of the standard, not from QWERTY specifically. Suppression is minimal (0.12) because no party actively enforces QWERTY's dominance: typists choose it rationally, manufacturers do not defend it preferentially, and employers do not lobby for it. The slight non-zero suppression reflects the friction of coordinating a switch—if a coalition of Dvorak proponents tried to coordinate mass adoption, they would face passive resistance from the installed base, but this is coordination cost, not active coercion. Theater is negligible (0.05) because the arrangement performs exactly its stated function: coordinating on a standard. Manufacturers occasionally issue superficial claims about QWERTY optimality, but these are minor theatrical elements; the constraint persists without performative cover. The measurement series shows extractiveness and suppression remaining flat across the interval (t=0 to t=140 years), with slight decay early and stabilization by mid-interval—consistent with a coordination equilibrium that neither tightens nor loosens. The constraint exhibits no directional drift toward either greater extraction or greater theatrical maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The alternative-layout proponents' seat and the installed-base typists' seat should compute differently from the research-observer seat. From the proponent position, QWERTY appears as an obstacle to adopting a technically superior layout—they perceive it as extractive relative to their interests. From the installed-base position, QWERTY is a valuable standard that makes their skill portable—they perceive it as purely beneficial. From the analytical seat, the constraint appears as a coordination equilibrium: both sides are rationally locked into their respective positions (Dvorak learners cannot switch without sacrifice; QWERTY learners cannot switch without sacrificing trained skill), but the equilibrium is stable and efficient for the collective, not captured. The engine should compute type divergence across these seats, reflecting the perspectival gap, without the divergence itself signaling extraction—it is a feature of coordination problems that different seats perceive them differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived primarily from coordination-equilibrium structure, not from beneficiary/victim dynamics. QWERTY-trained typists are listed as beneficiaries because they benefit from the standard's existence (their skill remains valuable); but they are not extracting value from alternative-layout proponents—they are simply occupying the dominant equilibrium position. Alternative-layout proponents are listed as payers because they bear coordination friction, but they are not paying an extraction fee to beneficiaries; they are bearing the cost of being in the minority faction of a coordination problem. The beneficiary entry serves to establish that the typist population has organized interest in the standard's persistence, but this should not drive directionality toward a target-reading (snare/tangled-rope) classification. Instead, the structural fact is: nobody is enforcing QWERTY; it persists because individually rational choices aggregate into a stable equilibrium. Directionality should trend all seats toward 0.5 (symmetric) or toward beneficiary-end values for the installed base (0.2–0.4 range, reflecting their slight advantage in skill portability), with some upward drift for the marginalized proponents (0.6–0.7 range, reflecting their higher switching cost relative to potential benefit). The power atoms differentiate: organized typists and manufacturers sit at higher power, giving them some institutional ability to formalize the standard, but this formalization is responsive to the existing equilibrium, not generative of it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem remains live (typists and employers still need coordination on a layout), and the constraint's structural purpose (establish a portable skill standard) remains fully functional. There is no mandatrophy—the founding problem has not outlived its usefulness. The reading explicitly rejects the lock-in reading's claim that QWERTY persists despite technical inferiority; it asserts instead that QWERTY is adequately functional and that Dvorak advantages are either empirically unproven or insufficient to justify a coordinated switch. This means the reading classifies the constraint as a working rope (coordination with low extractive overhead), not as a piton (atrophied function). The mandatrophy question does not apply here because the constraint remains structurally vital.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dvorak_ergonomic_advantage_empirical_status,
    'Do alternative layouts like Dvorak offer measurable ergonomic or speed advantages over QWERTY in controlled typing tasks?',
    'Meta-analysis of peer-reviewed typing-speed and fatigue studies comparing Dvorak, Colemak, and QWERTY under controlled conditions. Empirical measurement of keystroke frequency, finger travel distance, and error rates across cohorts trained on each layout.',
    'If substantial advantages exist and are empirically robust, the naturalization reading weakens: QWERTY''s persistence becomes harder to defend as adequacy-through-fair-competition and more consistent with lock-in or beneficiary-maintenance. If advantages are minimal or contested, the naturalization reading is strengthened. If advantages are conditional on usage patterns (e.g., high-speed typists benefit, casual typists do not), the reading requires refinement to account for heterogeneous coordination equilibria.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dvorak_ergonomic_advantage_empirical_status, empirical, 'Empirical status of Dvorak technical advantage claims').

omega_variable(
    coordination_switch_feasibility_and_cost,
    'How much would a coordinated transition to an alternative layout cost (in retraining, keyboard production, software adaptation)? Is such a transition within the feasible range for a major employer or education system?',
    'Cost-benefit analysis of retraining programs (school districts, government agencies, large firms) that attempted layout switches. Natural experiments from non-Latin-script regions (e.g., East Asia) that chose different input methods rather than QWERTY. Projection models of transition cost as a function of installed base size.',
    'If coordination cost is genuinely prohibitive (years of productivity loss, massive retraining expense), the constraint becomes less a matter of fair competition and more a matter of true lock-in. If coordination cost is modest (weeks to months, manageable retraining expense), the naturalization reading holds: QWERTY persists because switching is not worth it given its adequate functionality, not because alternatives are suppressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_switch_feasibility_and_cost, empirical, 'Feasibility and cost class of coordinated layout switches').

omega_variable(
    manufacturer_active_maintenance_versus_passive_standardization,
    'Did keyboard manufacturers (Underwood, Royal, IBM, and later electronic keyboard makers) actively lobby, market, or enforce QWERTY to maintain market dominance, or did they simply build what the existing labor supply was trained to use?',
    'Historical records: manufacturer patent filings, market research, advertising claims, trade association positions, and pricing strategies. Interviews or archival records from manufacturers and designers. Counterfactual analysis: did any manufacturer attempt to market a layout-agnostic or alternative-layout keyboard?',
    'If manufacturers actively enforced QWERTY (suppressed alternatives, lobbied against competing layouts, bundled exclusionary contracts), the beneficiary-extraction reading is supported and the naturalization reading weakens—extraction would be real. If manufacturers were passive responders to the installed base (built what sold, did not actively suppress alternatives, would have built alternatives if demand existed), the naturalization reading is strengthened—the constraint is coordination equilibrium, not enforced lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufacturer_active_maintenance_versus_passive_standardization, empirical, 'Whether manufacturer behavior toward QWERTY was active enforcement or passive standardization').

omega_variable(
    alternative_layout_adoption_in_niche_communities,
    'Why have Dvorak, Colemak, and other alternative layouts failed to achieve adoption even in communities where coordination cost is minimal (programmers, writers, typing enthusiasts)?',
    'Survey and analysis of adoption rates in high-literacy communities; interviews with programmers and power users about layout choice; measurement of typing-speed and error-rate claims from Dvorak users versus control groups; social-network analysis of alternative-layout communities.',
    'If alternatives fail to gain traction even when switching cost is low (programmers can retrain at minimal productivity loss), this suggests QWERTY''s adequacy is genuinely sufficient to overcome preference drift—the naturalization reading holds. If alternatives gain some traction but plateau below critical mass, this is consistent with a weak lock-in or coordination problem, consistent with both the naturalization reading and the lock-in reading (depending on whether the plateau reflects rational assessment of switching cost or path-dependent coordination failure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_layout_adoption_in_niche_communities, empirical, 'Adoption dynamics of alternative layouts in low-switching-cost communities').

omega_variable(
    reading_committer_kernel_contest,
    'Is the persistence of QWERTY best explained by fair competition among alternatives (naturalization reading), path-dependent coordination failure (lock-in reading), or active beneficiary maintenance (extraction reading)?',
    'No single empirical fact resolves this; the three readings reflect incommensurable framings of the same persistence pattern. The resolution depends on which facts count as primary evidence: speed/ergonomic data (favors naturalization if Dvorak advantage is unproven, favors lock-in if Dvorak advantage is clear), manufacturer behavior (favors extraction if active suppression is shown, favors naturalization if passive), adoption patterns in low-cost communities (favors naturalization if alternatives fail to gain traction despite low cost, favors lock-in if they gain some traction but plateau), and the perceived adequacy of QWERTY''s function (favors naturalization if function is deemed sufficient, favors lock-in if function is deemed barely acceptable but better alternatives exist).',
    'The three readings are incompatible at the level of classification: a constraint classified as rope under the naturalization reading would be classified as tangled-rope or snare under the extraction reading, and as snare under the lock-in reading. The engine computes classification from structural metrics; my authored claim is the naturalization reading''s classification (rope). If the measurement of beneficiary structure, enforcement activity, and suppression level confirms low extraction and low enforcement, the naturalization reading is empirically supported. If measurement shows active suppression or concentrated beneficiary extraction, the reading should be reclassified under one of the sibling readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_committer_kernel_contest, conceptual, 'Kernel contest: which reading best explains QWERTY''s persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__naturalization_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 80, 0.05).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 100, 0.05).
narrative_ontology:measurement(qwer_tr_t120, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 120, 0.05).
narrative_ontology:measurement(qwer_tr_t140, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 140, 0.05).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 40, 0.19).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 60, 0.18).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 80, 0.17).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 100, 0.18).
narrative_ontology:measurement(qwer_be_t120, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 120, 0.18).
narrative_ontology:measurement(qwer_be_t140, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 140, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(qwer_su_t20, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 20, 0.13).
narrative_ontology:measurement(qwer_su_t40, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement(qwer_su_t60, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 60, 0.12).
narrative_ontology:measurement(qwer_su_t80, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 80, 0.11).
narrative_ontology:measurement(qwer_su_t100, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 100, 0.12).
narrative_ontology:measurement(qwer_su_t120, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 120, 0.12).
narrative_ontology:measurement(qwer_su_t140, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 140, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__naturalization_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__naturalization_reading, 0.02).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% The QWERTY keyboard constraint family comprises three readings of a single kernel (the persistent dominance of QWERTY layout). The naturalization_reading asserts that QWERTY persists because it was/became genuinely adequate and alternatives lapsed through fair competition; the lock_in_reading asserts that QWERTY persists through path-dependent coordination failure despite technical inferiority; the beneficiary_extraction_reading asserts that QWERTY persists because manufacturers and incumbents actively maintained it to protect market power. These three readings instantiate different constraints with different epsilon values, beneficiary structures, and classifications. They are linked via network.affects_constraints to enable cross-reading contamination analysis—if empirical evidence shows strong manufacturer suppression of alternatives, the beneficiary_extraction_reading's ε increases and the naturalization_reading's claim becomes harder to sustain. The naturalization_reading influences the other two by establishing the baseline adequacy claim that the lock-in reading contests and the extraction reading explains away.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
