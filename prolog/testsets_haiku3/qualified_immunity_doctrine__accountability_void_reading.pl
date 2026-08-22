% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__accountability_void_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__accountability_void_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__accountability_void_reading
 *   human_readable: Qualified Immunity Doctrine: Accountability Void Reading
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   Qualified immunity is a judge-made doctrine that shields law enforcement
 *   officers from civil liability in § 1983 suits unless the plaintiff can
 *   show the officer violated a 'clearly established' constitutional right.
 *   Under the accountability void reading, the doctrine operates as a
 *   systematic extraction mechanism: officers who commit constitutional
 *   violations escape liability and damages; victims are left without remedy;
 *   the doctrine's enforcement depends on courts actively dismissing cases
 *   early and narrowing the 'clearly established' bar to near-impossibility.
 *   The beneficiary set is officers who would otherwise face civil
 *   consequences; the victim set is persons whose constitutional rights were
 *   violated and who lack redress. Structurally, qualified immunity creates a
 *   second-order constraint on the first-order constraint (the constitutional
 *   right itself)—it renders the constitutional remedy unavailable while
 *   leaving the rights nominally intact, which is the hallmark of pure
 *   extraction: the right exists on paper, the remedy is gone.
 *
 * KEY AGENTS:
 *   - law_enforcement_officers: Primary beneficiary (shielded from liability); institutional power; trapped exit (career depends on law enforcement, cannot exit role without losing livelihood)
 *   - constitutional_violation_survivors: Primary victim (bear the constraint's full extraction—violation without remedy); powerless or moderate power depending on resources; identity-locked (the violation becomes part of their lived experience, inescapable)
 *   - federal_courts: Agenda-setter and interpreter (courts administer the doctrine, define 'clearly established', dismiss cases on immunity grounds); institutional power; analytical time horizon
 *   - civil_rights_advocacy_organizations: Secondary actors attempting to organize victims into litigation coalitions; powerful institutional voice but facing high suppression from doctrine itself
 *   - state_legislatures: Analytical observer (some state legislatures have waived immunity for certain violations, creating a real-world experiment in immunity-free regimes)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, 0.91).
domain_priors:suppression_score(qualified_immunity_doctrine__accountability_void_reading, 0.88).
domain_priors:theater_ratio(qualified_immunity_doctrine__accountability_void_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, extractiveness, 0.91).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__accountability_void_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__accountability_void_reading, "Qualified Immunity Doctrine: Accountability Void Reading").
narrative_ontology:topic_domain(qualified_immunity_doctrine__accountability_void_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__accountability_void_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__accountability_void_reading, 'ea5452c7-a3a2-4683-81dc-94ae674b6620').
narrative_ontology:cs_kernel_codification('ea5452c7-a3a2-4683-81dc-94ae674b6620', formalized).
narrative_ontology:cs_authority_grounding('ea5452c7-a3a2-4683-81dc-94ae674b6620', extraction).
narrative_ontology:cs_interpretation_layer_present('ea5452c7-a3a2-4683-81dc-94ae674b6620').
narrative_ontology:cs_reading_relation('ea5452c7-a3a2-4683-81dc-94ae674b6620', qualified_immunity_doctrine__protective_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea5452c7-a3a2-4683-81dc-94ae674b6620', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('ea5452c7-a3a2-4683-81dc-94ae674b6620', foundational, victim_remedy_access_foreclosed_by_doctrine).
narrative_ontology:cs_axiom_status(victim_remedy_access_foreclosed_by_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('ea5452c7-a3a2-4683-81dc-94ae674b6620', victim_remedy_access_foreclosed_by_doctrine, empirically_contingent).
narrative_ontology:cs_axiom('ea5452c7-a3a2-4683-81dc-94ae674b6620', secondary, extraction_persists_independent_of_founding_problem).
narrative_ontology:cs_axiom_status(extraction_persists_independent_of_founding_problem, holdable).
narrative_ontology:cs_axiom_grounding('ea5452c7-a3a2-4683-81dc-94ae674b6620', extraction_persists_independent_of_founding_problem, empirically_contingent).
narrative_ontology:cs_reference_frame('ea5452c7-a3a2-4683-81dc-94ae674b6620', constitutional_remedy_availability_era).
narrative_ontology:cs_drift_state('ea5452c7-a3a2-4683-81dc-94ae674b6620', contemporary_post_2010_judicial_doctrine_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ea5452c7-a3a2-4683-81dc-94ae674b6620', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, constitutional_violation_survivors).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, civil_rights_plaintiffs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, civil_rights_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Officers are shielded from civil liability by qualified immunity. They benefit directly: even when they violate a plaintiff's constitutional rights, they face no damages and often no trial (immunity is typically granted on summary judgment). Officers cite the doctrine as essential to vigorous policing—if they faced constant litigation fear, they would act more cautiously, which they argue harms public safety. Their exit is trapped: law enforcement is their career, and immunity is a national rule backed by courts, so they cannot opt out. As organized interests (police unions, law enforcement associations), they actively defend the doctrine in courts and legislatures.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers, beneficiary,
    organized, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers, agenda_setter).

% Individuals whose constitutional rights were violated by police (unlawful arrest, excessive force, wrongful detention, civil rights deprivations). They are the primary victims of this constraint. They sought to sue the officer under § 1983 for damages and are barred by qualified immunity. Their exit is identity-locked: the violation happened to them, they cannot un-violate themselves, and the constraint bars the legal remedy. They bear the cost without choice and without recourse. Most lack resources for extended litigation; some are killed and thus permanently unable to pursue claims.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, constitutional_violation_survivors, payer,
    powerless, biographical, identity_locked, national).

% Organizations like the ACLU, NAACP, Center for Constitutional Rights, and victim advocacy groups attempt to organize constitutional violation survivors, fund litigation, bring test cases to overturn qualified immunity, and lobby legislatures. They bear the cost of mounting resistance (litigation resources, political capital) and are partly excluded from the beneficiary decision-making (courts decide the doctrine, and they have no seat at that table, only advocacy access). Their exit is constrained: they are committed to the civil rights cause and cannot simply leave the arena.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_organizations, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__accountability_void_reading, civil_rights_organizations, excluded).

% Federal courts (including the Supreme Court) created and maintain qualified immunity doctrine through case law. Courts set the standard for 'clearly established' rights, dismiss cases on immunity grounds, and shape the doctrine's scope. They claim to balance officer protection against victim remedy, but the measurement data shows the balance has drifted heavily toward officer protection. Courts administer the doctrine as if it is neutral procedure, but the doctrine's persistence depends on their active maintenance. Courts could overturn it (as they created it) but have not.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Some state legislatures have waived qualified immunity for certain violations (New Mexico for police, Colorado, Vermont). They create an experiment in immunity-free regimes within their borders. As observers, they see the outcomes of immunity waiver but cannot overturn federal doctrine—they can only waive it at the state level. Their vantage point is analytical: they measure whether officer conduct or litigation burden changes meaningfully when immunity is removed.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, state_legislatures, observer,
    institutional, generational, analytical, regional).

% State and federal prosecutors could charge officers with crimes for constitutional violations (deprivation of rights under 18 U.S.C. § 242), but routinely decline. They are excluded from the beneficiary decision-making (the constraint affects their ability to prosecute the officers they work alongside) and are constrained by organizational loyalty and professional interdependence with police. If victims had robust civil remedies via § 1983, criminal prosecution barriers would be less consequential—but the absence of civil remedy leaves criminal prosecution as the only check, and prosecutors rarely use it.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, prosecutorial_system, excluded,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__accountability_void_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Qualified immunity coordinates the law enforcement system by shielding officers from distraction and liability costs, enabling them to focus on enforcement without constant lawsuit fear. It solves a real administrative problem: processing countless § 1983 suits drains officer time and agency resources. It also provides a procedural rule for courts: immunity doctrine gives courts a mechanism to dismiss cases early rather than trying every factual dispute about constitutional violation.
% TRANSFER_FUNCTION: The constraint transfers liability costs from officers to victims. When an officer violates a constitutional right, the doctrine redirects the harm: the victim bears the injury (physical, psychological, financial) AND bears the loss of civil remedy (no damages, no judgment against the officer). The transfer is from constitutional violation survivors to shielded officers and the law enforcement system. The cost is asymmetric: officers gain immunity (a concrete benefit), victims lose remedy (an abstract but material benefit).
% ABSENT_VOICES: Frivolous litigants (parties who sued officers for conduct that did not violate their rights) are absent from the debate, yet the doctrine is justified by protecting against them. Their absence is necessary to the doctrine's framing: if they were present, they would say 'we filed baseless suits and the doctrine rightly dismissed them,' which would support the protective reading. But we do not know how many such suits exist or how costly they are. Criminal defendants would also be present if they could speak: the doctrine prevents them from suing officers for constitutional violations committed in obtaining their conviction, yet they are largely excluded from the § 1983 debate (it applies to civil rights, not criminal procedure).
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, the § 1983 civil remedy would function as written: officers would be liable for constitutional violations. Civil rights litigation would increase substantially (lower barrier to entry, higher stakes). Officer behavior would likely shift—some commentators argue it would become more cautious (good for rights protection), others argue it would become more aggressive (bad for rights protection—'sue me' posture). Police departments would likely increase liability insurance and reduce frivolous-looking enforcement actions. Victim compensation for constitutional violations would become available through damages. The law enforcement system would reorganize around a different set of incentives. The removal of the constraint is therefore not neutral—multiple institutional arrangements would rearrange.
% FOUNDING_PROBLEM: In the 1970s–1980s, law enforcement officers faced a growing number of § 1983 suits, many of which were perceived as baseless or harassing. Officers worried about lawsuit costs, damages awards, and distraction from enforcement. The founding problem was real: if every interaction a police officer had could spawn a lawsuit, litigation burden would be substantial and potentially deterring. Courts adopted qualified immunity to filter out cases that did not allege clearly established rights, reducing the burden on officers and courts both.
% FOUNDING_PROBLEM_CORROBORATION: Police unions, law enforcement associations, and the Department of Justice argue the founding problem is still live: officers face constant lawsuit threat and must be protected to enforce effectively. Civil rights organizations, academics, and victim advocates argue the founding problem is dead: contemporary data shows litigation burden on officers is not exceptional relative to other professions; officer litigation fear is not documented by systematic survey; and the doctrine has become so protective that it now shields even egregious violations. State legislatures that waived immunity (New Mexico, Colorado) report that officer conduct has not deteriorated and litigation burden has not exploded, suggesting the founding problem is overstated. The mismatch between founding problem (frivolous suits burden) and measured outcome (victims cannot sue for real violations) suggests the founding problem has been solved while the constraint persists for other reasons (institutional inertia, officer interest in immunity, judicial path-dependence).
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__accountability_void_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__accountability_void_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__accountability_void_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__accountability_void_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__accountability_void_reading, 0.91, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.91 at endpoint) because the doctrine creates a near-absolute bar to liability: even when an officer violates a constitutional right, if the right was not 'clearly established' at the time (a retroactive standard), the officer walks free and the victim gets no damages. The gap between violation and remedy is closed by the doctrine's operation, not by the victim's failure to prove harm—the harm is proven, but the remedy is foreclosed. Suppression is equally high (0.88) because the doctrine's persistence depends on active judicial enforcement (dismissal on qualified immunity grounds, narrowing of 'clearly established' doctrine, summary judgment before trial). Theater ratio is moderate (0.42): courts claim to balance officer protection against victim remedy, but the balance has drifted heavily toward officer protection over decades of doctrine refinement; the judicial reasoning theater is real, but the outcome theater (the claim that the doctrine leaves victims a meaningful remedy path) is mostly performance. Accessibility of alternatives is high (0.79): once a plaintiff understands qualified immunity, the fact that they cannot sue the officer becomes clear—alternatives (criminal prosecution by the state, state tort law) are visible but structurally inadequate (prosecutors are part of law enforcement, state sovereigns often claim immunity). Resistance is high (0.72): civil rights organizations, academics, victims' families mount consistent resistance through litigation, legislative advocacy, and public pressure, but resistance has not moved the Supreme Court to overturn the doctrine. The measurement series shows extraction and suppression rising over the interval (t=0 to t=25, corresponding roughly to the period 1975–2020): the doctrine has become increasingly protective of officers as courts have narrowed 'clearly established' and expanded qualified immunity to more contexts (policing, pretrial detention, special education).
 *
 * PERSPECTIVAL GAP:
 *   The officer seat and the victim seat should compute very differently. Officers compute the constraint as protective (it shields them from baseless suits, enables vigorous policing, is justified by past litigation burden—this is the protective_scaffold_reading frame). Victims compute it as extractive (their constitutional right is violated, they have no remedy, the violation goes unpunished—this is the accountability_void_reading frame, the reading this story instantiates). Courts compute themselves as neutral (they administer a procedural rule that balances legitimate interests—this is the institutional default, but the measurement data shows the balance has drifted). The engine computes a per-seat classification: from the officer seat, the constraint may compute as rope or tangled_rope (coordination + extraction, but the officer benefits). From the victim seat, it computes as snare (pure extraction, no coordination benefit). This divergence is the entire point—the same doctrine looks protective from one structural position and extractive from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Law enforcement officers are the structural beneficiary (d near 1.0 → full target, reversed): they collect the benefit of immunity, which is an exemption from liability. Their directionality d is near 0.0 (beneficiary end) because the constraint subsidizes them—immunity removes a cost (lawsuit, damages) they would otherwise bear. Constitutional violation survivors are the structural target (d near 1.0 → full target): they bear the extraction directly—they are harmed by the officer's violation AND denied remedy by the doctrine. Their exit options are highly constrained (no civil remedy, state remedies inadequate, criminal prosecution unlikely), so they sit at the target end. Federal courts are the agenda-setter (institutional power, analytical exit) but not a straightforward beneficiary or victim—they administer the doctrine and claim neutrality, but the doctrine's persistence depends on their narrowing of 'clearly established'. Courts sit near d=0.5 (symmetric) from the perspective of the constitutional rights system: they coordinate officer immunity (a real administrative function for the law enforcement system) but extract victims' remedy access (asymmetric payoff).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (frivolous litigation overwhelming officers) was arguably live in the 1970s–1980s when the doctrine was adopted, but has become contested or dead. Courts continue to cite the founding problem as justification, but discovery docket data and officer survey data show litigation burden on officers is not exceptional—many professions face higher litigation exposure. Meanwhile, the victim harm (denial of remedy for actual constitutional violations) has become substantially higher and more visible. The constraint shows signs of mandatrophy: it was built to solve a real problem, but the problem has attrophied while the constraint persists and grows more protective. The measurement series shows rising extractiveness and theater ratio, consistent with a constraint whose original function has been superseded by institutional inertia and judicial path-dependence. However, mandatrophy is not the final classification: the constraint is actively enforced (courts work to maintain it), has identifiable beneficiaries (officers who profit from immunity), and produces real asymmetric extraction (victims bear the cost). It is not a piton (atrophied and mostly theatrical) because the enforcement is too vigorous. It is a snare (pure extraction disguised as necessary procedure).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_alternative_framing,
    'Is qualified immunity a systematic extraction mechanism guaranteeing impunity (accountability void reading), or a necessary procedural scaffold protecting vigorous law enforcement (protective reading), or a judicially fabricated doctrine lacking constitutional warrant (constitutional fidelity reading)?',
    'This constraint instantiates the accountability_void_reading. Sibling readings are authored as separate constraint stories (constraint_qualified_immunity_protective_scaffold, constraint_qualified_immunity_constitutional_fidelity) with different ε values, beneficiary structures, and victim sets. The three readings coexist as live positions in the ongoing lawsuit ecosystem; resolution would require Supreme Court overruling or congressional amendment, not empirical fact-finding.',
    'Under this reading, ε_base ≈ 0.91 (near-absolute bar to liability creates massive extraction). Under the protective scaffold reading, ε would be substantially lower (0.35–0.45) and the type would be rope or tangled_rope. Under the constitutional fidelity reading, ε would be equally high but the beneficiary set would shift (the judiciary that authorized it, not officers) and the constraint would be classified as tangled_rope with the court as the hidden payer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_alternative_framing, conceptual, 'Kernel reading ambiguity: accountability void vs. protective scaffold vs. constitutional fidelity').

omega_variable(
    suppression_mechanism_internalization,
    'Is measured suppression (0.88) structural—external legal barriers and threat of retaliation—or internalized—officers'' belief that constitutional protections do not apply to them, or that violating them carries no consequence?',
    'Post-doctrine change empirical study: if officers'' conduct patterns remain suppressive after immunity is curtailed (e.g., in jurisdictions that waive immunity for certain classes of violation, or after partial legislative repeal), suppression is partially internalized. If conduct liberalizes when immunity is removed, suppression was primarily structural.',
    'If internalized, the measured suppression underestimates the constraint''s true coercive force—victims carry internalized officer impunity beliefs into the post-doctrine era. If structural, suppression is contained by the legal rule and would decline with rule removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized in officer and victim populations').

omega_variable(
    plaintiff_coalition_power,
    'Are constitutional violation survivors and civil rights plaintiffs sufficiently organized to mount a credible repeal campaign, or does the constraint''s diffuse victim set lack the power to exit via political pressure?',
    'Track civil rights coalition funding, litigation docket volume, legislative testimony frequency, and state-level immunity-waiver adoption. If organized pressure produces measurable policy shifts, coalition power is real; if high-profile cases produce no legislative movement despite sympathetic facts, power is low.',
    'If victims have real coalition power, the measured resistance (0.72) understates the barrier they face—a powerful organized group meeting an 0.88 suppression wall is a different story than a diffuse group meeting it. Conversely, low victim coalition power explains why high resistance has not translated to remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plaintiff_coalition_power, empirical, 'Whether victim group coalition power is sufficient to generate credible exit threat').

omega_variable(
    founding_problem_status_mismatch,
    'The doctrine was adopted (mid-1960s–early-1980s) to protect officers from baseless harassment litigation and enable vigorous law enforcement. Is that founding problem still live, has it been solved, or is the founding rationale obsolete?',
    'Compare litigation frequency and cost-to-defend metrics pre- and post-immunity adoption (historical data); assess current officer survey data on litigation fear and deterrent effect. If litigation burden is low and officers report no material fear, the founding problem is dead and the doctrine persists as rent-seeking. If burden is substantial and officer reports confirm fear-based restraint, the problem is still live.',
    'Status = live → the constraint may be justified (though this reading disputes that). Status = dead → the constraint is captured zombie enforcement, a Piton candidate. Status = contested → tie-breaker depends on which empirical sources are considered authoritative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_status_mismatch, empirical, 'Whether the founding problem (frivolous litigation against officers) is still live or has atrophied').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__accountability_void_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t0, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(qual_tr_t5, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(qual_tr_t10, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(qual_tr_t15, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement(qual_tr_t20, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(qual_tr_t25, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(qual_be_t0, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(qual_be_t5, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 5, 0.77).
narrative_ontology:measurement(qual_be_t10, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 10, 0.83).
narrative_ontology:measurement(qual_be_t15, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 15, 0.88).
narrative_ontology:measurement(qual_be_t20, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 20, 0.9).
narrative_ontology:measurement(qual_be_t25, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 25, 0.91).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t0, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0, 0.71).
narrative_ontology:measurement(qual_su_t5, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(qual_su_t10, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(qual_su_t15, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 15, 0.84).
narrative_ontology:measurement(qual_su_t20, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 20, 0.87).
narrative_ontology:measurement(qual_su_t25, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 25, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__accountability_void_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(qualified_immunity_doctrine__accountability_void_reading, 0.12).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine__constitutional_fidelity_reading).

% DUAL FORMULATION NOTE:
% qualified_immunity_doctrine kernel decomposes into three structurally distinct readings: accountability_void_reading (this story, ε ≈ 0.91, snare type, officers beneficiary, victims are constitutional violation survivors); protective_scaffold_reading (ε ≈ 0.35–0.45, rope or tangled_rope type, officers+law enforcement system beneficiary, victims would be frivolous-suit targets); constitutional_fidelity_reading (ε ≈ 0.88–0.92, tangled_rope type, federal courts beneficiary through institutional capture, victims are constitutional rights). Each reading grounds a different constraint story because the ε-referent changes: accountability_void measures the standing arrangement (immunity in place) from the victim's perspective; protective_scaffold measures the counterfactual (immunity absent, frivolous suits present) and argues the standing arrangement is better; constitutional_fidelity measures the standing arrangement from the perspective of constitutional authority. The ε-invariance principle requires three stories, not one. They are linked via network.affects_constraints because they share a kernel and the public debate moves between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
