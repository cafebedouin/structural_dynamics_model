% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__retributive_reading, []).

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
 *   constraint_id: state_killing_legitimacy__retributive_reading
 *   human_readable: Retributive Legitimacy of State Killing (Lex Talionis)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   The retributive reading of state killing legitimacy grounds capital
 *   punishment in the principle of lex talionis: the murderer, by taking a
 *   life, forfeits their own right to life in strict proportion. This is not
 *   a consequentialist claim (deterrence) but a deontological one — justice
 *   requires the offender's death as the only punishment that matches the
 *   crime in kind. The constraint operates through statutes, sentencing
 *   guidelines, appellate review, and execution protocols. Its beneficiaries
 *   are collective (society's moral order, victims' families seeking
 *   vindication); its victims are the condemned (including the wrongfully
 *   convicted). The reading presents itself as pure coordination — the law's
 *   answer to 'what does justice demand?' — but structurally it authorizes
 *   the state to kill a powerless class, requires elaborate active
 *   enforcement to maintain, and extracts the totality of the offender's
 *   future. The claimed type (tangled_rope) reflects this dual structure:
 *   genuine coordination of collective condemnation fused with asymmetric
 *   extraction of life.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, 0.88).
domain_priors:suppression_score(state_killing_legitimacy__retributive_reading, 0.92).
domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "Retributive Legitimacy of State Killing (Lex Talionis)").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, '27f13fd3-d47d-45f5-bee9-1b4b2fd9966e').
narrative_ontology:cs_kernel_codification('27f13fd3-d47d-45f5-bee9-1b4b2fd9966e', formalized).
narrative_ontology:cs_authority_grounding('27f13fd3-d47d-45f5-bee9-1b4b2fd9966e', lineage).
narrative_ontology:cs_interpretation_layer_present('27f13fd3-d47d-45f5-bee9-1b4b2fd9966e').
narrative_ontology:cs_reading_relation('27f13fd3-d47d-45f5-bee9-1b4b2fd9966e', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('27f13fd3-d47d-45f5-bee9-1b4b2fd9966e', state_killing_legitimacy__abolition_reading, forecloses).
narrative_ontology:cs_axiom('27f13fd3-d47d-45f5-bee9-1b4b2fd9966e', foundational, proportional_desert_justifies_lethal_punishment).
narrative_ontology:cs_axiom_status(proportional_desert_justifies_lethal_punishment, holdable).
narrative_ontology:cs_axiom_grounding('27f13fd3-d47d-45f5-bee9-1b4b2fd9966e', proportional_desert_justifies_lethal_punishment, deontological).
narrative_ontology:cs_axiom('27f13fd3-d47d-45f5-bee9-1b4b2fd9966e', foundational, lex_talionis_as_moral_law).
narrative_ontology:cs_axiom_status(lex_talionis_as_moral_law, holdable).
narrative_ontology:cs_axiom_grounding('27f13fd3-d47d-45f5-bee9-1b4b2fd9966e', lex_talionis_as_moral_law, deontological).
narrative_ontology:cs_reference_frame('27f13fd3-d47d-45f5-bee9-1b4b2fd9966e', classical_retributive_justice).
narrative_ontology:cs_drift_state('27f13fd3-d47d-45f5-bee9-1b4b2fd9966e', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('27f13fd3-d47d-45f5-bee9-1b4b2fd9966e', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, society_moral_order).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, victims_families_retributive).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, convicted_murderers).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, wrongfully_convicted).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, victims_families_retributive).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, proportional_desert_justifies_lethal_punishment).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, lex_talionis_as_moral_law).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, state_as_agent_of_retributive_justice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sentenced to death under retributive statutes. They bear the full cost of the constraint — their lives. The retributive reading frames this as 'forfeited right' through desert, but structurally they have no exit, no bargaining power, and no ability to refuse the transfer. Appeals processes exist but are procedural, not substantive exits from the death sentence once affirmed.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, convicted_murderers, payer,
    powerless, immediate, trapped, national).

% A non-zero subset of those sentenced to death are factually innocent. They bear the same terminal cost as the guilty but without the retributive reading's moral predicate. The constraint's operation does not distinguish them at the point of execution; the error rate is a structural feature of the enforcement machinery, not a bug the constraint corrects.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, wrongfully_convicted, payer,
    powerless, immediate, trapped, national).

% The retributive reading claims society benefits through vindication of moral law — 'justice is done' when the murderer's life is taken in proportion to the crime. This benefit is collective, diffuse, and non-material: moral satisfaction, social cohesion around shared normative commitments, affirmation that the legal order takes murder with ultimate seriousness. No individual citizen collects a tangible transfer.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, society_moral_order, beneficiary,
    organized, generational, analytical, national).

% Families of murder victims who endorse retributive justice may experience closure or vindication through execution — a psychic benefit the reading counts as coordination. But they also pay costs: years of appeal proceedings, media exposure, the moral weight of participating in a killing, and the risk that execution does not deliver the promised closure. Their position is dual: they are the constraint's most cited beneficiaries but also bear distinctive burdens.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, victims_families_retributive, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__retributive_reading, victims_families_retributive, payer).

% The correctional, judicial, and executive branches that administer death sentences: prosecutors who seek them, juries that impose them, judges who affirm them, governors who can commute them, prison staff who carry them out. They set the procedural agenda, control the timeline, and hold the monopoly on legitimate violence. Their institutional survival does not depend on any single execution, but the death penalty as an institution sustains budgets, jurisdictions, and professional roles.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Organizations and individuals who argue state killing is categorically illegitimate. They are structurally excluded from the constraint's operation — their objections do not stop executions, and the retributive reading's internal logic treats abolitionist premises as irrelevant (category error: they deny the desert premise the reading takes as axiomatic). They operate externally: litigation, legislation, public campaigns, international pressure.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, abolitionist_advocates, excluded,
    organized, biographical, mobile, national).

% Academics, jurists, and philosophers who analyze the constraint from outside its operation. They do not collect from it, pay into it, or administer it. Their role is to map the structure: trace the desert argument, measure error rates, compare deterrence claims, document international divergence. Their exit is trivial — they can change research focus — but their analytical products feed back into the constraint's legitimacy contests.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social response to murder by establishing a single, authoritative, proportional penalty that expresses the community's moral condemnation and prevents private vengeance. The lex talionis principle (life for life) provides a determinate answer to 'what does the murderer owe?' that claims to close the cycle of retaliation.
% TRANSFER_FUNCTION: Transfers the convicted murderer's life to the state as the agent of collective moral order. The transfer is framed as the offender's debt paid — not a taking but a forfeiture. No material resource flows to beneficiaries; the 'gain' is the satisfaction of proportional desert, the maintenance of the moral law's authority, and the foreclosure of private vengeance.
% ABSENT_VOICES: The executed (silenced by the constraint itself); future potential offenders whose deterrence is claimed but who cannot consent to the threat; international human rights bodies that declare capital punishment a violation of the right to life (excluded by sovereignty claims); the wrongfully convicted whose voices are extinguished before exoneration can occur.
% DISAPPEARANCE_RATIONALE: If the retributive license to kill vanished overnight, jurisdictions retaining capital punishment would face immediate legal vacuums: death sentences would become unenforceable, prosecutors would lose their ultimate charging lever, prison systems would absorb death-row populations, and the moral vocabulary of 'just deserts' for murder would lose its statutory anchor. The rearrangement would be legal, institutional, and symbolic — not merely attitudinal.
% FOUNDING_PROBLEM: How to punish the gravest crime (murder) in a way that is proportionate, public, and final — satisfying the moral intuition that the punishment must 'fit' the crime in kind (life for life) rather than merely in degree, thereby preventing both private vengeance and state leniency.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal codes (Code of Hammurabi, Mosaic law, Roman twelve tables) attest to lex talionis as the founding principle of proportional punishment for homicide. Modern criminologists (e.g., Zimring, Hood) and human rights bodies (UN Human Rights Committee, Council of Europe) attest that the founding problem is now differently framed: incarceration achieves incapacitation, proportionality is measured in years not kind, and the error rate of capital systems undermines the desert claim. The corroboration is split — the principle's antiquity is documented, its contemporary necessity is disputed by parties outside the beneficiary set.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__retributive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_legitimacy__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__retributive_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the transfer is total and irreversible — the offender's entire future lifespan. Suppression is extreme (0.92) because the constraint's persistence depends on actively preventing exit (appeals exhaustion, clemency denial, physical restraint) and suppressing alternatives (life without parole, restorative justice). Theater ratio is moderate (0.45) and rising: the coordination function (proportional justice) is real but a growing share of the apparatus (protracted appeals, ceremonial protocols, witness rooms, drug protocols) performs legitimacy rather than delivering it. Accessibility collapse is high (0.78) — once the desert framework is accepted, alternatives appear as 'injustice' or 'leniency' — but not total, as abolitionist frameworks persist. Resistance is substantial (0.65) from abolitionists, international law, and the constraint's own error rate.
 *
 * PERSPECTIVAL GAP:
 *   From the state_execution_apparatus seat, the constraint appears as a rope: a genuine coordination mechanism that solves the problem of proportional punishment with minimal coercive overhead (due process is the coordination cost). From the convicted_murderers seat, it appears as a snare: pure extraction with no coordination benefit to them, maintained by overwhelming force. From society_moral_order, it appears as a mountain: the desert principle feels like a moral fact, not a choice. The engine computes these divergences from the structural data — the single authored claim (tangled_rope) does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Convicted murderers and wrongfully convicted are full targets (d ≈ 1.0): they bear the total extraction with trapped exit. Society_moral_order is a diffuse beneficiary (d ≈ 0.1): it collects moral satisfaction but no material gain, and its 'exit' is analytical (it can change its mind). Victims_families_retributive sit near symmetric (d ≈ 0.5): they receive psychic vindication but pay procedural and moral costs. State_execution_apparatus is an agenda_setter with institutional power and analytical exit — it administers the constraint but does not personally extract. Abolitionist_advocates are excluded (not in the constraint's directional calculus). Legal_scholars are observers (analytical seat).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proportional punishment for murder) is contested: modern incarceration achieves incapacitation, and the error rate of capital systems undermines the desert claim's reliability. Yet the constraint persists without a sunset clause. The mandatrophy analysis: the original coordination function (preventing private vengeance, providing determinate proportionality) has been partially displaced by life-without-parole and developed prison systems, but the extraction function (state killing as moral theater) has intensified. The constraint is not a piton (no inertial drift — it is actively defended) but a tangled_rope where the coordination story is real but the extraction is structurally necessary to the form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    desert_as_coordination_or_cover,
    'Is the lex talionis desert principle a genuine coordination mechanism (solving the vengeance problem) or a rationalization for state killing that serves other functions (political theater, racial control, institutional maintenance)?',
    'Comparative analysis: jurisdictions that abolished capital punishment but retained strong victim-rights frameworks — did private vengeance increase? Did moral order collapse? If not, the coordination function is substitutable and the desert claim is non-essential.',
    'If desert is substitutable coordination, the constraint is a snare (extraction with a cover story). If desert is the ONLY mechanism that prevents vengeance/moral collapse, it is a genuine tangled_rope with irreducible coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desert_as_coordination_or_cover, conceptual, 'Whether the retributive principle is structurally necessary for social coordination or a contingent justification.').

omega_variable(
    error_rate_as_structural_feature,
    'Is the wrongful conviction rate a bug the constraint could fix (better forensics, higher standards) or a structural feature of any system that kills based on human judgment?',
    'Historical analysis of error rates across epochs and jurisdictions with varying procedural protections. If error rate asymptotes above zero regardless of reforms, it is structural.',
    'If structural, the wrongfully_convicted victim class is inevitable, making the constraint''s extractiveness non-contingent and its coordination claim (justice for the guilty) contaminated by injustice to the innocent. This pushes classification toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(error_rate_as_structural_feature, empirical, 'Whether the constraint''s victim set necessarily includes the innocent.').

omega_variable(
    proportionality_measurement_problem,
    'Can ''proportional desert'' be measured or operationalized without smuggling in the very retributive intuition it claims to ground?',
    'Philosophical analysis: does any metric of proportionality (years of life lost, suffering inflicted, moral gravity) avoid circularity? Empirical: do jurors, judges, and publics converge on ''proportional'' sentences across cases?',
    'If proportionality is inherently subjective/unordered, the coordination function collapses — there is no determinate answer the constraint enforces, only a power decision masked as principle. This would reclassify toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_measurement_problem, conceptual, 'Whether the constraint''s central coordinating concept (proportionality) has determinate content.').

omega_variable(
    kernel_framing_alternatives,
    'Does the state_killing_legitimacy kernel admit only these three readings (retributive, deterrence, abolition), or are there structurally distinct framings (e.g., restorative, communicative, expressive) that would produce different constraint stories?',
    'Survey of legal-philosophical literature for legitimacy theories that do not reduce to the triplet. Test whether each produces a distinct ε, beneficiary/victim structure, and type.',
    'If additional readings exist, the kernel decomposition is incomplete. The current three-story family may miss constraints that are live in practice (e.g., restorative justice frameworks that reject both desert and deterrence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternatives, conceptual, 'Completeness of the kernel''s reading decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(skl_retributive_tr_t1900, state_killing_legitimacy__retributive_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(skl_retributive_tr_t1930, state_killing_legitimacy__retributive_reading, theater_ratio, 1930, 0.25).
narrative_ontology:measurement(skl_retributive_tr_t1960, state_killing_legitimacy__retributive_reading, theater_ratio, 1960, 0.35).
narrative_ontology:measurement(skl_retributive_tr_t1972, state_killing_legitimacy__retributive_reading, theater_ratio, 1972, 0.4).
narrative_ontology:measurement(skl_retributive_tr_t1976, state_killing_legitimacy__retributive_reading, theater_ratio, 1976, 0.42).
narrative_ontology:measurement(skl_retributive_tr_t2000, state_killing_legitimacy__retributive_reading, theater_ratio, 2000, 0.44).
narrative_ontology:measurement(skl_retributive_tr_t2025, state_killing_legitimacy__retributive_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(skl_retributive_be_t1900, state_killing_legitimacy__retributive_reading, base_extractiveness, 1900, 0.75).
narrative_ontology:measurement(skl_retributive_be_t1930, state_killing_legitimacy__retributive_reading, base_extractiveness, 1930, 0.78).
narrative_ontology:measurement(skl_retributive_be_t1960, state_killing_legitimacy__retributive_reading, base_extractiveness, 1960, 0.82).
narrative_ontology:measurement(skl_retributive_be_t1972, state_killing_legitimacy__retributive_reading, base_extractiveness, 1972, 0.7).
narrative_ontology:measurement(skl_retributive_be_t1976, state_killing_legitimacy__retributive_reading, base_extractiveness, 1976, 0.85).
narrative_ontology:measurement(skl_retributive_be_t2000, state_killing_legitimacy__retributive_reading, base_extractiveness, 2000, 0.87).
narrative_ontology:measurement(skl_retributive_be_t2025, state_killing_legitimacy__retributive_reading, base_extractiveness, 2025, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(skl_retributive_su_t1900, state_killing_legitimacy__retributive_reading, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(skl_retributive_su_t1930, state_killing_legitimacy__retributive_reading, suppression_requirement, 1930, 0.88).
narrative_ontology:measurement(skl_retributive_su_t1960, state_killing_legitimacy__retributive_reading, suppression_requirement, 1960, 0.9).
narrative_ontology:measurement(skl_retributive_su_t1972, state_killing_legitimacy__retributive_reading, suppression_requirement, 1972, 0.8).
narrative_ontology:measurement(skl_retributive_su_t1976, state_killing_legitimacy__retributive_reading, suppression_requirement, 1976, 0.92).
narrative_ontology:measurement(skl_retributive_su_t2000, state_killing_legitimacy__retributive_reading, suppression_requirement, 2000, 0.91).
narrative_ontology:measurement(skl_retributive_su_t2025, state_killing_legitimacy__retributive_reading, suppression_requirement, 2025, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__retributive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_legitimacy__retributive_reading, 0.12).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__deterrence_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% Part of the state_killing_legitimacy constraint family (kernel_id: state_killing_legitimacy). This retributive_reading grounds legitimacy in deontological desert (lex talionis). The deterrence_reading grounds it in consequentialist prevention. The abolition_reading denies legitimacy entirely. The three readings share the same operational referent (state execution of murderers) but instantiate different constraints with different ε, beneficiary/victim structures, and types. The retributive reading forecloses the abolition_reading (mutually exclusive core premises) and coexists_with the deterrence_reading (different justifications, same outcome, different coalitions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_legitimacy__retributive_reading, powerless, 0.98).
constraint_indexing:directionality_override(state_killing_legitimacy__retributive_reading, organized, 0.15).
constraint_indexing:directionality_override(state_killing_legitimacy__retributive_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
