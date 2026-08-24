% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__protective_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__protective_scaffold_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: qualified_immunity_doctrine__protective_scaffold_reading
 *   human_readable: Qualified Immunity — Protective Scaffold Reading
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   This constraint story models the 'protective scaffold' reading of
 *   qualified immunity: the doctrine is framed as a transitional safeguard
 *   enacted to shield officers performing dangerous duties from vexatious
 *   litigation, enabling decisive action in uncertain circumstances. The
 *   reading asserts the doctrine coordinates a genuine collective-action
 *   problem — officers would otherwise hesitate in high-stakes encounters —
 *   while acknowledging it extracts from constitutional violation survivors
 *   denied remedy when rights aren't 'clearly established.' The claim/metric
 *   gap is deliberate: the reading CLAIMS scaffold (transitional,
 *   coordination-justified) while the authored metrics describe a constraint
 *   with moderate extraction, active suppression, rising theater, and no
 *   functional sunset — the engine measures that divergence.
 *
 * KEY AGENTS:
 *   - law_enforcement_officers: Primary beneficiary (organized/constrained) — receive personal liability shield
 *   - constitutional_violation_survivors: Primary payer (powerless/trapped) — denied damages when rights not 'clearly established'
 *   - police_departments: Agenda setter (institutional/arbitrage) — administer policy, capture reduced liability costs
 *   - federal_courts: Agenda setter (institutional/analytical) — define 'clearly established' standard through precedent
 *   - municipal_governments: Payer (organized/constrained) — bear settlement costs when immunity denied, benefit when granted
 *   - civil_rights_attorneys: Excluded (moderate/constrained) — represent victims but structurally barred by immunity standard
 *   - legal_scholars: Observer (analytical/analytical) — analyze doctrine's evolution and empirical effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, 0.52).
domain_priors:suppression_score(qualified_immunity_doctrine__protective_scaffold_reading, 0.58).
domain_priors:theater_ratio(qualified_immunity_doctrine__protective_scaffold_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__protective_scaffold_reading, scaffold).
narrative_ontology:human_readable(qualified_immunity_doctrine__protective_scaffold_reading, "Qualified Immunity — Protective Scaffold Reading").
narrative_ontology:topic_domain(qualified_immunity_doctrine__protective_scaffold_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:has_sunset_clause(qualified_immunity_doctrine__protective_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__protective_scaffold_reading, '5fc8d826-ae95-4da4-8f7f-28d7b96920a1').
narrative_ontology:cs_kernel_codification('5fc8d826-ae95-4da4-8f7f-28d7b96920a1', fixed_text).
narrative_ontology:cs_authority_grounding('5fc8d826-ae95-4da4-8f7f-28d7b96920a1', lineage).
narrative_ontology:cs_interpretation_layer_present('5fc8d826-ae95-4da4-8f7f-28d7b96920a1').
narrative_ontology:cs_reading_relation('5fc8d826-ae95-4da4-8f7f-28d7b96920a1', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_reading_relation('5fc8d826-ae95-4da4-8f7f-28d7b96920a1', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('5fc8d826-ae95-4da4-8f7f-28d7b96920a1', foundational, qualified_immunity_enables_vigorous_enforcement).
narrative_ontology:cs_axiom_status(qualified_immunity_enables_vigorous_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('5fc8d826-ae95-4da4-8f7f-28d7b96920a1', qualified_immunity_enables_vigorous_enforcement, instrumental).
narrative_ontology:cs_axiom('5fc8d826-ae95-4da4-8f7f-28d7b96920a1', foundational, clearly_established_standard_balances_accountability_and_protection).
narrative_ontology:cs_axiom_status(clearly_established_standard_balances_accountability_and_protection, holdable).
narrative_ontology:cs_axiom_grounding('5fc8d826-ae95-4da4-8f7f-28d7b96920a1', clearly_established_standard_balances_accountability_and_protection, conventional).
narrative_ontology:cs_reference_frame('5fc8d826-ae95-4da4-8f7f-28d7b96920a1', harlow_fitzgerald_framework).
narrative_ontology:cs_drift_state('5fc8d826-ae95-4da4-8f7f-28d7b96920a1', contemporary_rights_litigation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5fc8d826-ae95-4da4-8f7f-28d7b96920a1', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, police_departments).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, municipal_governments).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, municipal_governments).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__protective_scaffold_reading, vigorous_law_enforcement_requires_litigation_protection).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__protective_scaffold_reading, clearly_established_standard_balances_accountability_and_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive personal immunity from damages suits for constitutional violations unless the right was 'clearly established' in fact-specific precedent. The shield enables decisive action but also protects against accountability for excessive force, false arrest, and other violations. Officers cannot individually opt out of the doctrine; it applies automatically. Exit would require legislative reform or Supreme Court reversal — institutionally constrained.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers, beneficiary,
    organized, biographical, constrained, national).

% Individuals whose constitutional rights were violated (excessive force, unlawful search, false arrest, etc.) but who cannot recover damages because no prior case held the exact same conduct unconstitutional in the same jurisdiction. They bear the full cost of the violation — physical, financial, psychological — with no remedy. Exit is trapped: the 'clearly established' standard is a legal barrier they cannot overcome individually, and class certification is rarely available for damages.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors, payer,
    powerless, immediate, trapped, national).

% Set training, supervision, and use-of-force policies that shape the frequency and nature of constitutional violations. Benefit from qualified immunity by avoiding §1983 liability costs and indemnification obligations. Can arbitrage across jurisdictions (state vs federal court, different circuits) and influence doctrine through amicus briefs and policy advocacy. Their institutional position lets them shape the constraint's application.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, police_departments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__protective_scaffold_reading, police_departments, beneficiary).

% Define and apply the 'clearly established' standard through precedent. The Supreme Court sets the framework; circuit courts apply it fact-specifically, creating the patchwork of rights that determines immunity outcomes. Courts are not personally affected by the constraint's extractive consequences but their institutional legitimacy is tied to the doctrine's perceived fairness. Exit is analytical: they can revise the standard (as the Supreme Court did in Pearson v. Calloway) but face institutional inertia and stare decisis.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Bear financial liability for officer misconduct under Monell when qualified immunity fails (policy/custom claims) and for settlements. Benefit when immunity grants summary judgment, avoiding litigation costs and payouts. Exit is constrained: they cannot unilaterally change the doctrine, but can adopt reforms (body cameras, civilian oversight, use-of-force policies) that reduce violations and thus immunity invocations. Fiscal pressure creates incentive to reform, but political resistance from police unions constrains action.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, municipal_governments, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__protective_scaffold_reading, municipal_governments, beneficiary).

% Represent constitutional violation survivors but are structurally excluded from meaningful recovery by the 'clearly established' standard. Must invest resources in cases likely to be dismissed on immunity grounds. Their professional viability depends on navigating or challenging the doctrine. Exit is constrained: they can shift practice areas, pursue legislative reform, or litigate in state courts under state constitutions, but the federal §1983 pathway is the primary vehicle for police accountability.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_attorneys, excluded,
    moderate, biographical, constrained, national).

% Analyze the doctrine's historical evolution, empirical effects, and theoretical coherence. Produce the evidence base (e.g., Schwartz's empirical studies, Baude's historical critique) that informs judicial and legislative debates. Neither collect nor pay the constraint's extraction; their position is analytical. Exit is analytical: they can change research focus but the doctrine remains a central object of constitutional law scholarship.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects officers from the burden and chilling effect of defending against meritless lawsuits, enabling decisive action in uncertain, high-stakes encounters where hesitation could cost lives.
% TRANSFER_FUNCTION: Moves the cost of constitutional violations from officers and their employers to victims who are denied damages remedies when the violated right was not 'clearly established' in fact-specific precedent at the time of the violation.
% ABSENT_VOICES: Constitutional violation survivors who cannot meet the 'clearly established' bar — their absence is structural, not incidental. The doctrine's standard excludes them by design: a right is only 'clearly established' if prior precedent put the violation 'beyond debate,' which requires a prior victim to have litigated and won on nearly identical facts. The first victim of a novel violation is structurally silenced.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, officers would face personal liability exposure, prompting departments to rapidly adopt stricter use-of-force policies, enhanced training, and early intervention systems. Municipalities would face increased litigation costs initially, then policy-driven reductions in violations. Victims would gain access to damages remedies for violations currently immunized. The law enforcement accountability ecosystem would fundamentally reorganize.
% FOUNDING_PROBLEM: Post-Civil War Reconstruction era: federal officers enforcing civil rights laws in hostile Southern courts faced vexatious, bad-faith prosecutions designed to obstruct federal authority. The immunity doctrine originated to protect federal officers performing congressionally mandated duties from state-court harassment.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (William Baude, 'Is Qualified Immunity Unlawful?'; Joanna Schwartz, 'How Qualified Immunity Fails') document the doctrine's drift from Reconstruction-era federal officer protection to broad immunity for state/local officers. No non-beneficiary source corroborates the current scope as necessary for the original founding problem. The Supreme Court in Harlow v. Fitzgerald (1982) explicitly replaced the subjective good-faith test with an objective standard, acknowledging the doctrine's policy justification had shifted.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__protective_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__protective_scaffold_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__protective_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__protective_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__protective_scaffold_reading, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate: the doctrine externalizes litigation costs to victims but does not enrich officers directly — it shields them from personal financial ruin. Suppression (0.58) is moderate-high: the 'clearly established' standard actively bars remedies for violations where precedent lacks fact-specific matching, requiring continuous judicial enforcement. Theater ratio (0.38) is significant: the 'vigorous enforcement' justification persists while the doctrine's application has expanded far beyond its Reconstruction-era founding context, making the protective rationale increasingly performative. Accessibility collapse (0.42) is moderate: alternatives (legislative reform, Monell liability, state constitutions) exist but are institutionally difficult. Resistance (0.48) is moderate: sustained academic, judicial, and legislative challenges exist but have not overturned the core doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (officers, departments) experience the constraint as coordination — a necessary shield against litigation risk that enables public safety. The payer seats (victims, municipalities) experience it as extraction — a barrier to accountability that externalizes constitutional violation costs. The engine computes this divergence from the structural data; the protective scaffold reading's claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Officers and departments are structural beneficiaries (d ~0.15-0.25): they collect the immunity shield, control policy implementation, and have institutional exit options (qualified immunity is one layer among many protections). Constitutional violation survivors are structural targets (d ~0.85): they bear the full cost of rights violations without remedy, have no exit from the legal standard, and are identity-locked into the victim role by the violation itself. Municipal governments sit near symmetric (d ~0.5): they pay settlements when immunity fails but avoid them when it succeeds; their exit is constrained by fiscal and political reality. Federal courts are analytical agenda-setters (d ~0.5): they administer the standard but are not personally affected by its extractive consequences.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting Reconstruction-era officers from vexatious suits in hostile courts) is dead — the historical context vanished over a century ago. Yet the arrangement persists and has expanded in scope. The protective reading prevents mislabeling by acknowledging the coordination function (officers do face litigation risk) while the metrics reveal the function has atrophied into a broad immunity regime that extracts from victims. The mandatrophy is unresolved: the doctrine's mandate has outlived its function but persists through judicial inertia and institutional capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading of the qualified_immunity_doctrine kernel. How does the protective scaffold reading''s structural framing differ from the accountability_void_reading and constitutional_fidelity_reading in ways that produce different ε values?',
    'Comparative constraint story generation for each sibling reading, then cross-reading ε comparison. The kernel''s ε-invariance principle requires each reading to author its own ε for the same standing arrangement.',
    'If sibling readings author substantially different ε values for the same referent, the kernel contains structurally distinct constraints masquerading under one label — confirming the ε-invariance decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Kernel-reading decomposition: protective_scaffold vs accountability_void vs constitutional_fidelity').

omega_variable(
    transitional_claim_vs_permanent_operation,
    'The reading claims qualified immunity is a transitional scaffold (has_sunset_clause: true), but the doctrine has operated for 42+ years since Harlow with no sunset mechanism. Is the scaffold claim a genuine transitional justification or a cover story for permanent extraction?',
    'Historical analysis of whether any judicial or legislative actor has ever proposed or implemented a sunset review for qualified immunity; comparison to genuine scaffolds (e.g., post-disaster emergency powers with statutory expiration).',
    'If the scaffold claim is cover, the constraint reclassifies toward tangled_rope or snare; if genuine, the missing sunset mechanism is an implementation failure, not a structural mischaracterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_claim_vs_permanent_operation, empirical, 'Whether the scaffold framing matches operational reality or masks permanent extraction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by constitutional violation survivors structural (the ''clearly established'' standard as legal barrier) or internalized (attorneys declining cases, victims not filing because they know the standard is insurmountable)?',
    'Empirical study of civil rights filing rates pre/post major immunity expansions; attorney survey data on case selection criteria; comparison to jurisdictions with modified immunity standards.',
    'If substantially internalized, effective suppression exceeds the structural measure — the constraint operates partly through deterrence of claims, not just denial of remedies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism for victims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__protective_scaffold_reading, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qi_protective_scaffold_tr_t0, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(qi_protective_scaffold_tr_t0, observed).
narrative_ontology:measurement(qi_protective_scaffold_tr_t7, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 7, 0.18).
narrative_ontology:measurement_basis(qi_protective_scaffold_tr_t7, observed).
narrative_ontology:measurement(qi_protective_scaffold_tr_t14, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 14, 0.24).
narrative_ontology:measurement_basis(qi_protective_scaffold_tr_t14, observed).
narrative_ontology:measurement(qi_protective_scaffold_tr_t21, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 21, 0.3).
narrative_ontology:measurement_basis(qi_protective_scaffold_tr_t21, observed).
narrative_ontology:measurement(qi_protective_scaffold_tr_t28, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 28, 0.34).
narrative_ontology:measurement_basis(qi_protective_scaffold_tr_t28, observed).
narrative_ontology:measurement(qi_protective_scaffold_tr_t35, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 35, 0.37).
narrative_ontology:measurement_basis(qi_protective_scaffold_tr_t35, observed).
narrative_ontology:measurement(qi_protective_scaffold_tr_t42, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 42, 0.38).
narrative_ontology:measurement_basis(qi_protective_scaffold_tr_t42, observed).

% Extraction over time
narrative_ontology:measurement(qi_protective_scaffold_be_t0, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(qi_protective_scaffold_be_t0, observed).
narrative_ontology:measurement(qi_protective_scaffold_be_t7, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 7, 0.38).
narrative_ontology:measurement_basis(qi_protective_scaffold_be_t7, observed).
narrative_ontology:measurement(qi_protective_scaffold_be_t14, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 14, 0.42).
narrative_ontology:measurement_basis(qi_protective_scaffold_be_t14, observed).
narrative_ontology:measurement(qi_protective_scaffold_be_t21, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 21, 0.46).
narrative_ontology:measurement_basis(qi_protective_scaffold_be_t21, observed).
narrative_ontology:measurement(qi_protective_scaffold_be_t28, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 28, 0.49).
narrative_ontology:measurement_basis(qi_protective_scaffold_be_t28, observed).
narrative_ontology:measurement(qi_protective_scaffold_be_t35, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 35, 0.51).
narrative_ontology:measurement_basis(qi_protective_scaffold_be_t35, observed).
narrative_ontology:measurement(qi_protective_scaffold_be_t42, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 42, 0.52).
narrative_ontology:measurement_basis(qi_protective_scaffold_be_t42, observed).

% Suppression requirement over time
narrative_ontology:measurement(qi_protective_scaffold_su_t0, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(qi_protective_scaffold_su_t0, observed).
narrative_ontology:measurement(qi_protective_scaffold_su_t7, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 7, 0.42).
narrative_ontology:measurement_basis(qi_protective_scaffold_su_t7, observed).
narrative_ontology:measurement(qi_protective_scaffold_su_t14, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 14, 0.48).
narrative_ontology:measurement_basis(qi_protective_scaffold_su_t14, observed).
narrative_ontology:measurement(qi_protective_scaffold_su_t21, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 21, 0.53).
narrative_ontology:measurement_basis(qi_protective_scaffold_su_t21, observed).
narrative_ontology:measurement(qi_protective_scaffold_su_t28, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 28, 0.57).
narrative_ontology:measurement_basis(qi_protective_scaffold_su_t28, observed).
narrative_ontology:measurement(qi_protective_scaffold_su_t35, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 35, 0.61).
narrative_ontology:measurement_basis(qi_protective_scaffold_su_t35, observed).
narrative_ontology:measurement(qi_protective_scaffold_su_t42, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 42, 0.58).
narrative_ontology:measurement_basis(qi_protective_scaffold_su_t42, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__protective_scaffold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(qualified_immunity_doctrine__protective_scaffold_reading, 0.12).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine__accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine__constitutional_fidelity_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, monell_liability_doctrine).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, section_1983_litigation).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, police_union_collective_bargaining).

% DUAL FORMULATION NOTE:
% This constraint family (qualified_immunity_doctrine kernel) decomposes the single doctrinal label into three structurally distinct readings with different ε values, beneficiary/victim sets, and claimed types. The protective_scaffold_reading claims scaffold with moderate ε (0.52); accountability_void_reading claims snare with high ε; constitutional_fidelity_reading claims mountain (illegitimate) with ε assessed from the reading's own lights. All three share the standing arrangement (current qualified immunity doctrine) as referent but author different structural relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__protective_scaffold_reading, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
