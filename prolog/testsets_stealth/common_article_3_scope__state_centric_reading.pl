% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__state_centric_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: common_article_3_scope__state_centric_reading
 *   human_readable: Common Article 3 Threshold-Gated Scope (State-Centric Reading)
 *   domain: legal/international-humanitarian-law
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions sets a minimum
 *   humanitarian floor for non-international armed conflict. The
 *   state-centric reading instantiates that floor as strictly
 *   threshold-gated: the article binds only once violence crosses intensity
 *   and organization thresholds, and never reaches law-enforcement
 *   operations. Because each government classifies its own conflicts, the
 *   reading hands states the gatekeeping pen: below the line, detention,
 *   interrogation, and prosecution run wholly on domestic rules. This file is
 *   ONE reading of the contested kernel common_article_3_scope; the expansive
 *   and customary readings are separate constraint stories with their own
 *   victim sets and epsilon values, linked through the network block. KEY
 *   AGENTS (by structural relationship): national_governments: agenda-setter
 *   and primary beneficiary (institutional/arbitrage) — control
 *   classification and collect the discretion the line preserves;
 *   regular_state_armed_forces: beneficiary (organized/constrained) — operate
 *   below the line free of convention duties;
 *   irregular_fighters_below_threshold: primary target (organized/trapped) —
 *   denied the floor, prosecutable for participation;
 *   civilians_in_internal_violence_zones: target (powerless/constrained) —
 *   exposed to military force without the humanitarian minimum;
 *   icrc_delegates: analytical observer (institutional/analytical);
 *   human_rights_treaty_bodies: excluded voice (institutional/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.68).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.72).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "Common Article 3 Threshold-Gated Scope (State-Centric Reading)").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "legal/international-humanitarian-law").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, 'f1b74032-b585-461d-af97-95619522b996').
narrative_ontology:cs_kernel_codification('f1b74032-b585-461d-af97-95619522b996', fixed_text).
narrative_ontology:cs_authority_grounding('f1b74032-b585-461d-af97-95619522b996', extraction).
narrative_ontology:cs_interpretation_layer_present('f1b74032-b585-461d-af97-95619522b996').
narrative_ontology:cs_reading_relation('f1b74032-b585-461d-af97-95619522b996', common_article_3_scope__expansive_human_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('f1b74032-b585-461d-af97-95619522b996', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('f1b74032-b585-461d-af97-95619522b996', foundational, ca3_application_requires_intensity_organization_thresholds).
narrative_ontology:cs_axiom_status(ca3_application_requires_intensity_organization_thresholds, holdable).
narrative_ontology:cs_axiom_grounding('f1b74032-b585-461d-af97-95619522b996', ca3_application_requires_intensity_organization_thresholds, conventional).
narrative_ontology:cs_axiom('f1b74032-b585-461d-af97-95619522b996', foundational, ihl_inapplicable_to_law_enforcement_operations).
narrative_ontology:cs_axiom_status(ihl_inapplicable_to_law_enforcement_operations, holdable).
narrative_ontology:cs_axiom_grounding('f1b74032-b585-461d-af97-95619522b996', ihl_inapplicable_to_law_enforcement_operations, conventional).
narrative_ontology:cs_reference_frame('f1b74032-b585-461d-af97-95619522b996', sovereign_conflict_classification_prerogative).
narrative_ontology:cs_drift_state('f1b74032-b585-461d-af97-95619522b996', contemporary_post_tadic_icc_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f1b74032-b585-461d-af97-95619522b996', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, national_governments).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, regular_state_armed_forces).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, irregular_fighters_below_threshold).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, civilians_in_internal_violence_zones).
narrative_ontology:constraint_vindicates(common_article_3_scope__state_centric_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(common_article_3_scope__state_centric_reading, war_crime_distinction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classify their own internal violence for legal purposes: declaring an insurgency an armed conflict triggers Common Article 3 duties, while labeling it banditry, terrorism, or a law-enforcement operation keeps the operation inside domestic law. They draft rules of engagement, set detention regimes, and instruct their courts on which framework applies. International criticism carries diplomatic cost but no automatic legal consequence, and shifting between the war frame and the crime frame remains available whenever convenient.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, national_governments, agenda_setter,
    institutional, biographical, arbitrage, national).

% Conduct the operations. When the state keeps a campaign below the armed-conflict threshold, soldiers face no convention-based duties toward captured opponents: detention, interrogation, and prosecution run entirely on domestic rules the government writes. Above the threshold, the same units owe minimum-treatment guarantees. They act on legal advice from their own government and have no independent route to reclassify a conflict.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, regular_state_armed_forces, beneficiary,
    organized, biographical, constrained, national).

% Members of armed groups in confrontations the state classes as below the threshold. They can be killed, captured, interrogated, and prosecuted for rebellion or terrorism under ordinary domestic law, with no convention-based minimum on treatment and no privilege shielding mere participation. Leaving the group invites prosecution by the state and retaliation by comrades; staying fuses survival with the cause. Group leaders occasionally declare adherence to Common Article 3 unilaterally, which changes little in practice.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, irregular_fighters_below_threshold, payer,
    organized, biographical, trapped, regional).

% Live where internal violence simmers below the declared-conflict line: neighborhoods raided under emergency powers, regions under curfew, towns shelled in operations officially described as policing. They hold whatever protections domestic law and, where incorporated, human-rights treaties provide, enforced if at all by the same institutions conducting the operations. Flight is possible but destroys livelihoods; many stay and absorb the risk.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, civilians_in_internal_violence_zones, payer,
    powerless, biographical, constrained, local).

% Visit places of detention, monitor conduct, and confidentially press governments to apply Common Article 3 minimums broadly. Their leverage is persuasion and presence: they operate only where states admit them, file no litigation, and publish most findings confidentially. They maintain the customary-law record that later tribunals cite.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, icrc_delegates, observer,
    institutional, civilizational, analytical, global).

% Treaty committees and regional courts assert that human-rights floors apply concurrently to all internal violence, below any armed-conflict threshold. They issue views and judgments naming abuses, but they hold no seat in the classification decision, and their remedies against governments operating on their own territory amount to condemnation, reporting, and slow compliance pressure.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, human_rights_treaty_bodies, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__state_centric_reading, national_governments).
narrative_ontology:fixing_cost_class(common_article_3_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the boundary between war and crime: reserving conduct-of-hostilities and convention-based detention rules for organized violence above stated intensity and organization thresholds, and keeping ordinary violence under domestic criminal law and police procedure. Gives states advance notice of when heavier legal obligations attach and prevents battlefield rules from absorbing everyday governance.
% TRANSFER_FUNCTION: Moves legal discretion from persons caught in internal violence to the state: below the threshold, the government alone decides use-of-force, detention, interrogation, and prosecution standards, while opponents receive whatever domestic law, written and applied by that same government, provides.
% ABSENT_VOICES: The fighters and residents below the threshold have no seat anywhere the classification is made: states decide alone, in cabinet rooms and military legal offices. Human-rights bodies object from outside the frame; the ICRC argues from its monitoring seat; affected communities learn the classification from the conduct of the operation itself.
% DISAPPEARANCE_RATIONALE: If the threshold-gated scope vanished overnight, every organized internal confrontation would carry convention-based minimums from the first shot: detention regimes, interrogation practice, and prosecution rules worldwide would rearrange immediately, and the war/crime boundary that organizes domestic security law would need rebuilding.
% FOUNDING_PROBLEM: In 1949 the drafters faced civil wars fought with international-war methods but outside the Conventions' reach: they wrote Common Article 3 as a minimal floor for internal conflicts while leaving the exact line of application open, to avoid forcing states to treat rebels as belligerents or to submit internal policing to battlefield law.
% FOUNDING_PROBLEM_CORROBORATION: The problem's liveness is corroborated outside the benefiting parties: the ICRC's updated commentaries and customary-IHL study document continuous dispute over the line; the ICTY Appeals Chamber's Tadic jurisdiction decision and the Rome Statute's Article 8(2)(c) both presuppose that the threshold question governs real cases; UN commissions of inquiry on Syria and elsewhere turn on it. What no outsider corroborates is the reading's discretionary use: the claim that the line sits wherever each government says it sits rests almost entirely on state assertion.
narrative_ontology:disappearance_verdict(common_article_3_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__state_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_article_3_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__state_centric_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68 at interval end) because the scope line withdraws an entire protection package — humane-treatment minimums, judicial guarantees — precisely in the situations where states most want freedom of action, though the withdrawal is not total: domestic criminal law partially substitutes, and conflicts above the line do receive the floor. Suppression is high (0.72) because the arrangement's persistence depends on actively defending the classification prerogative: resisting expansive readings diplomatically, limiting monitoring access, and litigating characterization. Theater is moderate (0.38): genuine law-enforcement situations exist and the classificatory work is real, but a growing share of 'law-enforcement operation' labels function as cover for military-scale campaigns. Accessibility collapse is moderate (0.55): within a state's own framework, alternative characterizations collapse once the classification prerogative is asserted, yet the sibling readings remain institutionally alive in tribunals and treaty bodies. Resistance is substantial (0.60): sustained pushback from the ICRC, human-rights bodies, academia, and some courts. The temporal series share one grid. Extractiveness dips slightly around 1989-1999 as ad hoc tribunals articulated objective threshold tests that constrained pure discretion, then peaks in the counterterror era as classification battles intensified; suppression_requirement is tracked because the story specifically traces enforcement-capacity change — the machinery for holding the line hardened markedly after 2001 before easing modestly as human-rights-law convergence eroded the line's practical stakes. The claim (tangled_rope) and the metrics are authored independently: the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   From the government seat, the arrangement is prudent legal ordering: predictability about when heavy obligations attach, sovereignty over internal security, and protection of ordinary policing from battlefield law. From the fighter and civilian seats, the same line is protection withdrawn at the moment of maximum vulnerability — the state decides alone whether the floor exists. Identical text, opposite constraints, computed per seat from the structural data rather than adjudicated by the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments sit nearest the beneficiary pole: they declare who benefits from the line by deciding where it falls, and their arbitrage-grade exit (shifting between war and crime frames) damps any cost they bear. Regular armed forces benefit concretely below the line and are bound by their government's framing — low directionality, constrained exit. Irregular fighters sit near the full-target end: they bear the withdrawn protections directly, and their exit is trapped in both directions — surrender invites domestic prosecution for rebellion, departure invites reprisal, and group membership fuses survival with cause (relational identity lock; if that fusion broke, individual exit would widen and the target-side concentration would soften). Civilians in violence zones are targets with constrained exit (displacement is costly), though the criminal-law paradigm offers them partial procedural shelter when the state chooses the policing frame. ICRC delegates and human-rights bodies are analytical seats: neither collects nor pays through the line.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading this as pure rope would erase the asymmetric extraction: the same threshold that keeps policing civilian also strips the floor from people at the state's mercy. Reading it as pure snare would erase the genuine coordination function: the war/crime boundary protects due process for ordinary crime and prevents conduct-of-hostilities rules from swallowing peacetime law — a function no sibling reading denies outright. Tangled rope holds both halves. On obsolescence: the founding problem (internal violence lacking any conventional floor) is live, so no mandatrophy is declared; the contest is over where the line sits, not whether the arrangement has outlived its function. The mismatch consumer will find status=live paired with verdict=world_rearranges — no zombie flag — which matches the historical record: the line is invoked more, not less, as internal violence proliferates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Does the state-centric threshold-gated scope correctly instantiate the Common Article 3 kernel, or do the expansive or customary readings better capture the text''s protective purpose?',
    'Doctrinal convergence: ICJ and ICC jurisprudence, cumulative state practice, or eventual protocol amendment settling the scope line.',
    'Adopting the expansive reading extends the protected set to all participants in organized internal violence and raises effective extraction on governments; adopting the customary reading makes this constraint''s scope empirically path-dependent rather than fixed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the CA3 scope kernel this constraint instantiates, and what the sibling readings would change structurally.').

omega_variable(
    threshold_manipulation_vs_binding_force,
    'Are the intensity and organization thresholds applied as binding legal tests, or manipulated through unilateral state classification to keep chosen campaigns below the line?',
    'Compare official classifications against independent conflict-intensity indicators (fatality rates, territorial control, group organization) across a corpus of internal conflicts.',
    'Systematic manipulation converts the coordination function into cover and pushes the arrangement toward pure extraction with identifiable victims; consistently binding thresholds support the tangled-rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_manipulation_vs_binding_force, empirical, 'Whether the threshold line binds states or serves as a discretionary classification instrument.').

omega_variable(
    concurrent_ihrl_substitution,
    'Does concurrent application of international human rights law below the threshold substitute for the withdrawn Common Article 3 floor, or do states derogate and evade it in internal operations?',
    'Outcome comparison of detention and use-of-force practice in below-threshold operations across jurisdictions with strong versus weak human-rights incorporation.',
    'Full substitution shrinks this constraint''s practical footprint toward its coordination floor; systematic derogation restores the full measure of withdrawn protection to the extraction account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(concurrent_ihrl_substitution, empirical, 'Whether human-rights law fills the gap the scope line opens, determining the line''s real-world cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__state_centric_reading, theater_ratio, 1949, 0.15).
narrative_ontology:measurement(comm_tr_t1959, common_article_3_scope__state_centric_reading, theater_ratio, 1959, 0.2).
narrative_ontology:measurement(comm_tr_t1969, common_article_3_scope__state_centric_reading, theater_ratio, 1969, 0.28).
narrative_ontology:measurement(comm_tr_t1979, common_article_3_scope__state_centric_reading, theater_ratio, 1979, 0.33).
narrative_ontology:measurement(comm_tr_t1989, common_article_3_scope__state_centric_reading, theater_ratio, 1989, 0.31).
narrative_ontology:measurement(comm_tr_t1999, common_article_3_scope__state_centric_reading, theater_ratio, 1999, 0.35).
narrative_ontology:measurement(comm_tr_t2009, common_article_3_scope__state_centric_reading, theater_ratio, 2009, 0.44).
narrative_ontology:measurement(comm_tr_t2019, common_article_3_scope__state_centric_reading, theater_ratio, 2019, 0.4).
narrative_ontology:measurement(comm_tr_t2025, common_article_3_scope__state_centric_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__state_centric_reading, base_extractiveness, 1949, 0.42).
narrative_ontology:measurement(comm_be_t1959, common_article_3_scope__state_centric_reading, base_extractiveness, 1959, 0.5).
narrative_ontology:measurement(comm_be_t1969, common_article_3_scope__state_centric_reading, base_extractiveness, 1969, 0.57).
narrative_ontology:measurement(comm_be_t1979, common_article_3_scope__state_centric_reading, base_extractiveness, 1979, 0.61).
narrative_ontology:measurement(comm_be_t1989, common_article_3_scope__state_centric_reading, base_extractiveness, 1989, 0.59).
narrative_ontology:measurement(comm_be_t1999, common_article_3_scope__state_centric_reading, base_extractiveness, 1999, 0.64).
narrative_ontology:measurement(comm_be_t2009, common_article_3_scope__state_centric_reading, base_extractiveness, 2009, 0.71).
narrative_ontology:measurement(comm_be_t2019, common_article_3_scope__state_centric_reading, base_extractiveness, 2019, 0.69).
narrative_ontology:measurement(comm_be_t2025, common_article_3_scope__state_centric_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__state_centric_reading, suppression_requirement, 1949, 0.5).
narrative_ontology:measurement(comm_su_t1959, common_article_3_scope__state_centric_reading, suppression_requirement, 1959, 0.55).
narrative_ontology:measurement(comm_su_t1969, common_article_3_scope__state_centric_reading, suppression_requirement, 1969, 0.6).
narrative_ontology:measurement(comm_su_t1979, common_article_3_scope__state_centric_reading, suppression_requirement, 1979, 0.63).
narrative_ontology:measurement(comm_su_t1989, common_article_3_scope__state_centric_reading, suppression_requirement, 1989, 0.61).
narrative_ontology:measurement(comm_su_t1999, common_article_3_scope__state_centric_reading, suppression_requirement, 1999, 0.66).
narrative_ontology:measurement(comm_su_t2009, common_article_3_scope__state_centric_reading, suppression_requirement, 2009, 0.76).
narrative_ontology:measurement(comm_su_t2019, common_article_3_scope__state_centric_reading, suppression_requirement, 2019, 0.74).
narrative_ontology:measurement(comm_su_t2025, common_article_3_scope__state_centric_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, icrc_customary_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Common Article 3's scope' conflates three structurally distinct claims about one text. This story authors the state-centric instantiation only: a threshold-gated scope line whose epsilon reflects the standing arrangement (protection withheld below the line) as this reading assesses it. The expansive reading yields a different victim set — all participants in organized internal violence — and higher effective extraction on states; the customary reading makes scope empirically path-dependent. Linked via affects_constraints per the epsilon-invariance decomposition rule; each member of the family carries its own stable epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
