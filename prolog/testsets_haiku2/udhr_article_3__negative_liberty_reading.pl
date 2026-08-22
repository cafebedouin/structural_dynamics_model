% ============================================================================
% CONSTRAINT STORY: udhr_article_3__negative_liberty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__negative_liberty_reading, []).

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
 *   constraint_id: udhr_article_3__negative_liberty_reading
 *   human_readable: UDHR Article 3 — Negative Liberty Reading (Freedom from State Violence)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   Article 3 of the Universal Declaration of Human Rights prohibits
 *   arbitrary deprivation of life and liberty. This constraint story
 *   instantiates ONE READING: the negative liberty reading, which interprets
 *   Article 3 as a prohibition on state violence except where justified
 *   through transparent, adversarial due process. Under this reading, the
 *   state cannot execute capital punishment (substantive abolition), cannot
 *   imprison without adjudication and appeal (procedural habeas corpus), and
 *   cannot use lethal force even in response to security threats except where
 *   narrow proportionality and imminence tests are met. The negative liberty
 *   reading privileges individual protection over collective security and
 *   frames 'security' as freedom from state arbitrariness rather than freedom
 *   from criminal harm. This is a contested reading among a kernel of three:
 *   the positive entitlement reading interprets Article 3 as obligating the
 *   state to provide material conditions (welfare, healthcare) necessary for
 *   life; the procedural hybrid reading grounds legitimacy in due process
 *   protections without resolving whether security is negative liberty or
 *   positive entitlement. The three readings coexist across different
 *   parties, legal traditions, and ideological camps. This JSON instantiates
 *   ONLY the negative liberty reading as a clean, ε-invariant constraint —
 *   not a measurement of all three readings or an averaged interpretation.
 *
 * KEY AGENTS:
 *   - individual_persons (powerless, trapped): benefit from the right not to be arbitrarily killed or imprisoned; bear subjective security risk if collective security measures are restricted
 *   - state_security_apparatus (institutional, constrained): must operate under procedural constraints and substantive prohibitions; loses discretionary speed and scope
 *   - civil_liberties_advocates (organized, mobile): benefit from the constraint's enforcement, organize litigation to entrench and expand procedural protections
 *   - victims_of_crimes (organized, constrained, excluded from core contract): bear the procedural cost: the state cannot punish their offenders beyond the constraint's scope
 *   - security_maximizers (powerful, constrained): argue the reading unnecessarily restricts state capacity for collective security; contest the assumption that procedure and security coexist
 *   - procedural_authorities (institutional, analytical): administer the constraint through courts and due process; have delegated authority and institutional interest in its procedural elaboration
 *   - state_legislature (institutional, constrained): enacts enabling legislation; faces pressure from both sides to narrow or expand the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, 0.68).
domain_priors:suppression_score(udhr_article_3__negative_liberty_reading, 0.45).
domain_priors:theater_ratio(udhr_article_3__negative_liberty_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__negative_liberty_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__negative_liberty_reading, "UDHR Article 3 — Negative Liberty Reading (Freedom from State Violence)").
narrative_ontology:topic_domain(udhr_article_3__negative_liberty_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__negative_liberty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__negative_liberty_reading, '3c5211d8-a53e-47bb-a357-18bf5d7fd973').
narrative_ontology:cs_kernel_codification('3c5211d8-a53e-47bb-a357-18bf5d7fd973', fixed_text).
narrative_ontology:cs_authority_grounding('3c5211d8-a53e-47bb-a357-18bf5d7fd973', lineage).
narrative_ontology:cs_interpretation_layer_present('3c5211d8-a53e-47bb-a357-18bf5d7fd973').
narrative_ontology:cs_reading_relation('3c5211d8-a53e-47bb-a357-18bf5d7fd973', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_reading_relation('3c5211d8-a53e-47bb-a357-18bf5d7fd973', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('3c5211d8-a53e-47bb-a357-18bf5d7fd973', foundational, individual_dignity_inviolable_by_state).
narrative_ontology:cs_axiom_status(individual_dignity_inviolable_by_state, holdable).
narrative_ontology:cs_axiom_grounding('3c5211d8-a53e-47bb-a357-18bf5d7fd973', individual_dignity_inviolable_by_state, deontological).
narrative_ontology:cs_axiom('3c5211d8-a53e-47bb-a357-18bf5d7fd973', foundational, capital_punishment_incompatible_with_dignity).
narrative_ontology:cs_axiom_status(capital_punishment_incompatible_with_dignity, holdable).
narrative_ontology:cs_axiom_grounding('3c5211d8-a53e-47bb-a357-18bf5d7fd973', capital_punishment_incompatible_with_dignity, deontological).
narrative_ontology:cs_reference_frame('3c5211d8-a53e-47bb-a357-18bf5d7fd973', individual_dignity_vs_state_power).
narrative_ontology:cs_drift_state('3c5211d8-a53e-47bb-a357-18bf5d7fd973', contemporary_securitization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3c5211d8-a53e-47bb-a357-18bf5d7fd973', '').
narrative_ontology:cs_kernel_id(udhr_article_3__negative_liberty_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, individual_persons).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, civil_liberties_advocates).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, state_security_apparatus).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, collective_security_operations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, victims_of_crimes).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, security_maximizers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the right not to be arbitrarily killed or imprisoned by state action. Under this reading, the state cannot execute capital punishment even for heinous crimes, cannot imprison without due process regardless of collective security need, and cannot use violence in self-defense where any procedural alternative exists. The benefit is existential security: the state's violence is constrained to transparent, rule-bounded mechanisms. The cost is individual subjective security risk if collective security measures (rapid detention, preemptive force) are restricted.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, individual_persons, beneficiary,
    powerless, biographical, trapped, universal).

% Must operate under strict procedural constraints and substantive prohibitions on lethal force. Cannot execute criminals (abolition of capital punishment), cannot detain without due process (habeas corpus mandate), cannot use lethal force in response to security threats except in narrow circumstances of imminent proportional defense. These restrictions elevate operational cost (longer trials, more oversight, constrained response options) and reduce effectiveness in rapid threat response. The state agency that administers punishment cannot unilaterally determine that a person should die or be imprisoned; the constraint transfers that determination to a court with adversarial procedure.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, state_security_apparatus, payer,
    institutional, generational, constrained, universal).

% Advocate for the legal and institutional entrenchment of Article 3 under this reading. They frame the constraint as defense against state abuse, vindicate the dignity principle (that the state must not treat persons as disposable), and organize litigation and legislative pressure to maintain or expand the procedural and substantive protections. They benefit from the constraint's enforcement and interpret its meaning through advocacy litigation.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, civil_liberties_advocates, beneficiary,
    organized, generational, mobile, universal).

% Suffer harm from criminal acts. Under the negative liberty reading, they bear the procedural cost: the state's response to their harm is bounded by due process, cannot include vigilante justice or collective punishment, and may exclude capital punishment even for persons they hold responsible for serious crimes. They are excluded from the core contract (individual vs. state) but implicated in its consequences: they cannot demand that the state exceed the constraints to punish their offenders. Victim advocacy groups contest the reading's legitimacy by arguing the constraint extracts from those who have already been harmed.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, victims_of_crimes, payer,
    organized, biographical, constrained, universal).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__negative_liberty_reading, victims_of_crimes, excluded).

% Hold security-consequentialist positions: that the state should be able to use preemptive detention, rapid capital punishment for terrorism, collective security measures (curfews, surveillance, detention of suspect populations) to prevent mass harm. They argue the negative liberty reading unnecessarily restricts state capacity to protect the majority from catastrophic security threats. They contest the reading's assumption that procedural constraints on state violence can coexist with security adequacy. This group includes counterterrorism strategists, some law enforcement, and security scholars.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, security_maximizers, payer,
    powerful, biographical, constrained, universal).

% Administer the procedural mechanisms (courts, habeas corpus petitions, due process hearings) that the negative liberty reading instantiates. They interpret Article 3 through adversarial procedure: the state's burden is to prove in open court, subject to cross-examination and appellate review, that a particular deprivation of liberty or life is justified. They have delegated authority to apply the constraint and strong institutional interest in its procedural elaboration. They benefit from the constraint's enforceable status (it creates their jurisdiction) and face conflict when security pressure demands faster, less procedurally elaborate punishment.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, procedural_authorities, agenda_setter,
    institutional, generational, analytical, universal).

% Enacts the enabling legislation that operationalizes Article 3 under this reading: abolition statutes, habeas corpus procedures, rules of evidence, sentencing guidelines that preclude capital punishment. They are constrained by the international human rights treaty and the constitutional reading, but retain the legislative power to define the scope of due process and the procedural mechanisms for administering it. They face pressure from security maximizers to narrow the reading's application (e.g., via derogations in emergencies) and from civil liberties advocates to expand it.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, state_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Study the conceptual and normative structure of Article 3 across its readings. They document the philosophical commitments of the negative liberty reading (that persons have intrinsic dignity that the state cannot violate, that procedure is the only legitimate way to determine punishment, that security cannot justify torture or arbitrary death) and assess its consistency with the procedural and positive readings. They occupy the analytical seat and are not organized as political actors.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, observer_philosophers, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__negative_liberty_reading, diffuse).
narrative_ontology:fixing_cost_class(udhr_article_3__negative_liberty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes transparent, rule-bound procedures for the state's legitimate exercise of coercive power: no person can be deprived of life or liberty except through adversarial legal process with opportunity to contest the deprivation. Solves the coordination problem of how to constrain state violence while preserving state capacity to enforce law — the answer is procedural accountability rather than unrestricted discretion.
% TRANSFER_FUNCTION: Moves the power to determine punishment from state security bureaucracy (that can act unilaterally, rapidly, in secret) to courts operating under public procedure. Transfers authority and legitimacy from efficiency to due process. Persons who would otherwise be subject to summary execution or detention without trial are transferred to a protected class requiring individual adjudication. The state security apparatus loses speed and discretionary scope.
% ABSENT_VOICES: Victims of crimes and their families are structurally excluded from the core contract (though implicated in its consequences). Security maximizers and counterterrorism strategists argue the reading's procedural constraints compromise collective safety but are not granted authority to override the constraint. State security agencies are constrained rather than consulted. Populations in countries that rely on rapid state violence as a deterrent (or substitute for weaker rule-of-law institutions) are not seated in the UDHR interpretation.
% DISAPPEARANCE_RATIONALE: If Article 3 under this reading disappeared — if states could arbitrarily execute and imprison without due process — the political order would reorganize. Constitutional constraints on executive power would erode globally. The balance between individual protection and state authority would shift dramatically toward state discretion. Civil liberties advocates would lose their primary legal anchor, and procedural protections would depend on state benevolence rather than enforceable right. The world would not be unchanged; it would become more autocratic on average.
% FOUNDING_PROBLEM: Absolute monarchies and totalitarian states killed and imprisoned persons at will, without procedure, justification, or possibility of redress. Persons had no legal standing to demand reasons for their own execution or detention. The founding problem is state arbitrariness — the structural power imbalance between person and state, with no mechanism for the person to contest the state's violence.
% FOUNDING_PROBLEM_CORROBORATION: Historical human rights advocates (Enlightenment philosophers, anti-slavery activists, victims of authoritarian regimes) attest the founding problem was acute and motivated the negative liberty reading. Contemporary security maximizers contest whether the problem persists in stable liberal democracies with separated powers; they argue the founding problem is solved by institutional checks rather than substantive prohibitions on state violence. International human rights organizations and civil liberties advocates corroborate that state arbitrariness remains endemic in many jurisdictions and motivates continued Article 3 enforcement.
narrative_ontology:disappearance_verdict(udhr_article_3__negative_liberty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__negative_liberty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__negative_liberty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_article_3__negative_liberty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__negative_liberty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__negative_liberty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__negative_liberty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 at interval end, rising from 0.42 at t0. This rising trajectory reflects the constraint's operation as a Tangled Rope: it coordinates a procedural mechanism (due process solves the collective-action problem of how to constrain state violence) while extracting from the state security apparatus and, indirectly, from victims who cannot demand extraconstitutional punishment of their offenders. The extraction is NOT extraction of material goods but of discretionary power: the state must relinquish unilateral determination of punishment and death. Theater ratio is low (0.22) and stable, indicating that procedural activity (trials, habeas petitions, appellate review) represents genuine constraint enforcement rather than theatrical compliance. Suppression requirement is moderate (0.45) and rises slightly over the interval, reflecting increased institutional pressure to contain security-maximizer challenges to the reading's scope (via emergency derogations, national security exceptions, terrorism-specific carveouts). The measuring interval spans the UDHR's post-1948 adoption through contemporary (roughly 1948–2023), capturing the constraint's entrenchment in liberal democracies and its contestation in security-first regimes.
 *
 * PERSPECTIVAL GAP:
 *   The negative liberty reading produces seat-specific divergence. From the individual/beneficiary seat, the constraint is constitutive protection: the state's violence becomes predictable, contestable, and bounded. From the state security apparatus seat, the constraint is a restriction: operational capacity for rapid response, deterrent punishment, and preemptive detention is reduced. From the security maximizer seat, the constraint is a dangerous compromise: it privileges individual protection in ways that degrade collective safety. From the victim seat, the constraint is a barrier: the state cannot inflict the punishment the victim feels is proportional to their harm. The engine computes these divergences from the structural data (power asymmetries, exit options, beneficiary/victim declarations, directionality overrides). The claimed type is Tangled Rope because the constraint both coordinates (establishes legitimate procedure for state violence) and extracts (from state discretion and collective security options), requires active enforcement (courts must apply the constraint against state pressure), and involves at least one beneficiary (individuals) and one victim/payer (state security apparatus). Different seats would compute different types from this same structural data — that is the measurement the system is designed to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals benefit from the constraint: it prohibits their arbitrary death or imprisonment. Their directionality is low (near 0.0 — beneficiary end). The state security apparatus pays the cost of constraint: it loses discretionary speed and scope. Its directionality is high (near 1.0 — target/payer end). Civil liberties advocates benefit from enforcement and have institutional interest in its continuation; their directionality is low. Victims of crimes bear a diffuse cost (the state cannot punish beyond the constraint's scope) but are not primary targets; their directionality is moderate-to-high (0.55–0.65). Security maximizers are not a stakeholder with organized power to opt out; they are excluded/observer-positioned and their directionality is not computed. The state legislature is agenda-setter but constrained by the international commitment; its directionality depends on whether it views the constraint as binding (near 0.5 — symmetric) or as a negotiable limit it would prefer to exceed (higher). No directionality overrides are needed: the structural derivation from beneficiary/victim + power + exit_options produces the correct d values without exceptional correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy if it continues to solve its founding problem (state arbitrariness) and that problem remains live. The risk of mandatrophy is that the negative liberty reading persists theatrically in stable liberal democracies (due process is real, courts function, capital punishment is abolished) while the actual threat of state arbitrariness has been resolved by institutional separation of powers, judicial independence, and rule-of-law norms. In jurisdictions where state arbitrariness remains acute (authoritarian regimes, countries with weak rule of law), the constraint is either unenforced or enforced against strong state pressure. The theater ratio rising from 0.08 to 0.22 suggests modest increase in procedural activity relative to actual constraint enforcement — courts go through habeas motions, legislatures debate capital punishment abolition — but the ratio remains below 0.5, indicating the function is still genuine. Mandatrophy would manifest as theater_ratio approaching 0.7–0.8 with suppression_requirement stable or declining, indicating the machinery persists without real binding force. The current trajectory does not yet show mandatrophy, but the rising gap between disappearance_verdict (world_rearranges) and observable security outcomes in liberal democracies (world appears stable despite the constraint) creates risk. Resurrection of capital punishment in some jurisdictions or systematic emergency derogations in others would signal mandatrophy (the constraint persists as vestigial procedure while the core extraction — state discretion reduction — is eroded).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    negative_vs_positive_liberty_binary,
    'Is ''security of person'' in Article 3 fundamentally a negative liberty (freedom from state violence) or can it coherently include positive entitlements (provision of material conditions)?',
    'Conceptual analysis of whether a single normative framework can bind negative and positive liberty obligations, or whether they are structurally incompatible commitments that different readings instantiate separately.',
    'If negative and positive are structurally incompatible (forecloses relation), then this reading and the positive entitlement reading cannot coexist in a single legal system — one must be authoritatively chosen. If they are compatible (coexists_with), then both readings remain live and the constraint family represents a kernel under genuine contestation. If compatible but institutionally different, then the procedural hybrid reading is the practical synthesis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(negative_vs_positive_liberty_binary, conceptual, 'The fundamental philosophical divide between negative and positive liberty readings of Article 3.').

omega_variable(
    procedure_vs_substance_collapse,
    'Can procedural protections (due process, habeas corpus, transparent trial) coexist with substantive prohibitions (capital punishment abolition, restrictions on self-defense doctrine) that the negative liberty reading instantiates, or do the substantive prohibitions have to be grounded independently?',
    'Doctrinal analysis of whether courts can coherently apply Article 3 by saying ''procedure is sufficient'' while simultaneously enforcing substantive prohibitions that exist outside any procedure (e.g., capital punishment is prohibited full stop, not ''prohibited unless procedurally authorized'').',
    'If procedure can stand alone, the procedural hybrid reading is a true synthesis. If substantive and procedural are required together, the negative liberty reading cannot be reduced to due process — it requires additional normative commitment to human dignity that transcends procedure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedure_vs_substance_collapse, conceptual, 'The relationship between procedural legitimacy and substantive rights prohibitions.').

omega_variable(
    empirical_security_tradeoff,
    'Empirically, does enforcement of the negative liberty reading''s procedural and substantive constraints reduce the state''s capacity to prevent mass-casualty security threats, or can security be maintained within the constraint''s bounds?',
    'Comparative analysis of security outcomes (terrorism rates, violent crime, mass casualty incidents, state capacity for threat response) in jurisdictions with strong Article 3 enforcement versus those permitting capital punishment, preemptive detention, and relaxed due process. Randomized trials are infeasible; natural experiments (jurisdictions that abolished capital punishment, implemented habeas corpus) provide evidence.',
    'If the constraint measurably reduces effective security (higher attack rates, mass casualties, preventable threats), the security maximizers'' claim that the reading is incompatible with collective safety is empirically grounded — a trade-off is real. If security outcomes are equivalent or improve with the constraint, the reading''s claim that procedure and security coexist is vindicated. If the relationship is null or uncertain, the mandate is contested on empirical grounds and may flip with new evidence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_security_tradeoff, empirical, 'Whether the negative liberty reading''s constraints reduce effective security capacity.').

omega_variable(
    state_capacity_vs_discretion_distinction,
    'When the constraint ''extracts'' from the state security apparatus, is it reducing state capacity (ability to prevent security threats) or state discretion (ability to punish without justification)?',
    'Institutional analysis of whether the constraint prevents the state from detecting, prosecuting, and imprisoning criminals, or merely requires the state to go through process before doing so. If prevention capacity is unaffected and only unilateral discretion is reduced, the extraction is a different kind than if capacity itself is impaired.',
    'If only discretion is extracted (process required but capacity intact), the constraint is procedurally justified and security-compatible — it is an institutional improvement. If capacity is impaired (fewer convictions, longer timelines, more acquittals), the constraint is extracting something the security system needs, and the trade-off is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_vs_discretion_distinction, empirical, 'The distinction between state coercive discretion and state security capacity.').

omega_variable(
    individualism_vs_collectivism_kernel_irresolvable,
    'Is the disagreement between this reading and the security-maximizer reading a difference in reading the same kernel (Article 3 text can bear both interpretations) or a difference in foundational political philosophy (individualism vs. collectivism) that no textual reading can bridge?',
    'Genealogical analysis: if both readings claim to honor Article 3 while reaching opposite conclusions about capital punishment and security detention, the disagreement cannot be settled by textual argument alone. The kernel permits both readings — which means the kernel itself is under-determined by the text and the disagreement is philosophical, not interpretive.',
    'If irresolvable, the ''coexists_with'' relation between this reading and the security maximizer position is permanent — no reading can foreclose the other because they rest on different visions of legitimate political order. The constraint''s persistence depends on institutional power (who gets to set policy) rather than textual/philosophical victory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individualism_vs_collectivism_kernel_irresolvable, preference, 'Whether the disagreement is textual/interpretive or foundational/philosophical.').

omega_variable(
    capital_punishment_abolition_universalizable,
    'Does the negative liberty reading require capital punishment to be absolutely prohibited everywhere (universal abolition), or can it permit jurisdictions to authorize capital punishment if they do so via the constraint''s procedural mechanisms?',
    'Doctrinal reading: some interpretations hold that Article 3 read as negative liberty is compatible with capital punishment if authorized by democratic process and administered through transparent trial. Others hold that some fundamental prohibitions (like capital punishment) are non-negotiable even with procedure. Survey of international jurisprudence and human rights treaty negotiation history will show whether abolition is mandatory or permissive.',
    'If abolition is mandatory, the constraint is more restrictive (higher extraction from security apparatus) and the negative liberty reading forecloses any legal framework permitting capital punishment — a stronger claim. If abolition is permissive (capital punishment is OK if procedurally authorized), the constraint is weaker (lower extraction) and coexists more easily with security-maximizer readings that might accept procedure-bounded capital punishment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_punishment_abolition_universalizable, conceptual, 'Whether capital punishment abolition is a mandatory or permissive element of the negative liberty reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__negative_liberty_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__negative_liberty_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(udhr_tr_t12, udhr_article_3__negative_liberty_reading, theater_ratio, 12, 0.11).
narrative_ontology:measurement(udhr_tr_t25, udhr_article_3__negative_liberty_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(udhr_tr_t37, udhr_article_3__negative_liberty_reading, theater_ratio, 37, 0.18).
narrative_ontology:measurement(udhr_tr_t50, udhr_article_3__negative_liberty_reading, theater_ratio, 50, 0.21).
narrative_ontology:measurement(udhr_tr_t62, udhr_article_3__negative_liberty_reading, theater_ratio, 62, 0.22).
narrative_ontology:measurement(udhr_tr_t75, udhr_article_3__negative_liberty_reading, theater_ratio, 75, 0.22).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__negative_liberty_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(udhr_be_t12, udhr_article_3__negative_liberty_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(udhr_be_t25, udhr_article_3__negative_liberty_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement(udhr_be_t37, udhr_article_3__negative_liberty_reading, base_extractiveness, 37, 0.62).
narrative_ontology:measurement(udhr_be_t50, udhr_article_3__negative_liberty_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(udhr_be_t62, udhr_article_3__negative_liberty_reading, base_extractiveness, 62, 0.67).
narrative_ontology:measurement(udhr_be_t75, udhr_article_3__negative_liberty_reading, base_extractiveness, 75, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__negative_liberty_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(udhr_su_t12, udhr_article_3__negative_liberty_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(udhr_su_t25, udhr_article_3__negative_liberty_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement(udhr_su_t37, udhr_article_3__negative_liberty_reading, suppression_requirement, 37, 0.41).
narrative_ontology:measurement(udhr_su_t50, udhr_article_3__negative_liberty_reading, suppression_requirement, 50, 0.44).
narrative_ontology:measurement(udhr_su_t62, udhr_article_3__negative_liberty_reading, suppression_requirement, 62, 0.45).
narrative_ontology:measurement(udhr_su_t75, udhr_article_3__negative_liberty_reading, suppression_requirement, 75, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__negative_liberty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_article_3__negative_liberty_reading, 0.12).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% Article 3 of the UDHR is a contested kernel instantiated by three structurally distinct constraint stories: (1) negative_liberty_reading (this file) — interprets security as freedom from state arbitrariness, requires capital punishment abolition and expansive due process, extractiveness is high because it reduces state coercive discretion; (2) positive_entitlement_reading — interprets security as state provision of material conditions, extractiveness is high because it obligates resource transfer; (3) procedural_hybrid_reading — grounds legitimacy in due process protections without resolving the negative/positive dispute, extractiveness is lower because it permits multiple substantive readings. The three readings coexist across different legal traditions and ideological camps. They are not alternative measurements of a single constraint — they instantiate different constraints from the same kernel text. The ε-invariance principle requires that when the same text yields multiple ε values under different readings, separate constraint stories are authored. Links via affects_constraints record the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__negative_liberty_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
