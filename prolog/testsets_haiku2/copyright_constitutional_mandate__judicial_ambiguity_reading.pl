% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__judicial_ambiguity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__judicial_ambiguity_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: copyright_constitutional_mandate__judicial_ambiguity_reading
 *   human_readable: Copyright Term Length as Zone of Legislative Discretion
 *   domain: intellectual_property/constitutional_law
 *
 * SUMMARY:
 *   Under this reading, copyright term length is a zone of legislative
 *   discretion, not a judicially enforceable constitutional limitation. The
 *   Constitution says copyright must be granted for 'limited times,' but the
 *   judiciary defers to Congress via rational basis review: as long as
 *   Congress articulates a rational basis for an extension (incentivizing
 *   creation, serving copyright holders' legitimate expectations), the
 *   extension survives constitutional scrutiny. This reading does not claim
 *   the extension actually incentivizes creation—it claims courts do not
 *   verify that empirical claim. The constraint is CLAIMED as scaffold
 *   (transitional, justified by the founding coordination problem) while the
 *   authored metrics describe moderate extraction that has accumulated over
 *   time without judicial contestation—the engine will measure that tension.
 *
 * KEY AGENTS:
 *   - Congressional authority: sets term length and justifies extensions through the legislative process; sole agenda-setter on the policy dimension
 *   - Federal judiciary: applies rational basis review, deferring to legislative judgment; benefits from avoiding empirical adjudication
 *   - Copyright holders: primary beneficiaries of extensions; lobby Congress and defend extensions in litigation
 *   - Public-domain creators: structurally excluded; bear delayed commons access and higher licensing costs
 *   - Constitutional doctrine ('limited times'): eroded as a binding constraint without formal overruling
 *   - Constitutional scholars and technology innovators: excluded from legislative table; provide external critique but lack control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.42).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.28).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Copyright Term Length as Zone of Legislative Discretion").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "intellectual_property/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, 'cf7777d8-7d04-48d2-9af5-13cc61a61645').
narrative_ontology:cs_kernel_codification('cf7777d8-7d04-48d2-9af5-13cc61a61645', fixed_text).
narrative_ontology:cs_authority_grounding('cf7777d8-7d04-48d2-9af5-13cc61a61645', lineage).
narrative_ontology:cs_interpretation_layer_present('cf7777d8-7d04-48d2-9af5-13cc61a61645').
narrative_ontology:cs_reading_relation('cf7777d8-7d04-48d2-9af5-13cc61a61645', copyright_constitutional_mandate__corporate_enclosure_reading, influences).
narrative_ontology:cs_reading_relation('cf7777d8-7d04-48d2-9af5-13cc61a61645', copyright_constitutional_mandate__public_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('cf7777d8-7d04-48d2-9af5-13cc61a61645', foundational, rational_basis_deference_sufficient).
narrative_ontology:cs_axiom_status(rational_basis_deference_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('cf7777d8-7d04-48d2-9af5-13cc61a61645', rational_basis_deference_sufficient, deontological).
narrative_ontology:cs_axiom('cf7777d8-7d04-48d2-9af5-13cc61a61645', foundational, limited_times_legislatively_delegated).
narrative_ontology:cs_axiom_status(limited_times_legislatively_delegated, holdable).
narrative_ontology:cs_axiom_grounding('cf7777d8-7d04-48d2-9af5-13cc61a61645', limited_times_legislatively_delegated, deontological).
narrative_ontology:cs_reference_frame('cf7777d8-7d04-48d2-9af5-13cc61a61645', copyright_utility_theory_balance).
narrative_ontology:cs_drift_state('cf7777d8-7d04-48d2-9af5-13cc61a61645', contemporary_extension_saturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cf7777d8-7d04-48d2-9af5-13cc61a61645', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holders).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_limitation_doctrine).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_creators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, international_trade_bodies).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, rational_basis_deference_doctrine).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, legislative_primacy_in_copyright_scope).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets copyright term length through legislation (Sonny Bono Copyright Term Extension Act 1998, prior extensions). Justifies extension as serving copyright holders' legitimate investment interests and incentivizing creation. Judicial deference via rational basis review means Congress can extend terms repeatedly without explicit constitutional constraint, so long as the extension is facially plausible as incentive-aligned.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Applies rational basis review to copyright extensions, finding them facially rational as incentive mechanisms. Deference frees the judiciary from adjudicating the empirical claim that extension actually incentivizes creation. The judicial role is reduced to checking whether the legislature stated a rational basis; the court is not required to verify it. This reading benefits the judiciary by avoiding a politically exposed empirical inquiry.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_judiciary, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_judiciary, observer).

% Benefit from every extension, which postpones the entry of works into the public domain and extends their monopoly revenue streams. They lobby Congress for extensions and benefit from judicial deference, which makes such extensions nearly always survive constitutional challenge. Their exit is mobile—they can petition Congress for further extensions or adapt their business models.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holders, beneficiary,
    powerful, biographical, mobile, national).

% Bear the cost of extended monopoly: restricted access to foundational cultural works, higher licensing fees for derivative works, delayed enrichment of the commons. They have no seat at the legislative table and are excluded from the constitutional argument. Their exit is trapped—they cannot opt out of copyright law's scope.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_creators, payer,
    powerless, generational, trapped, national).

% The doctrine that 'limited times' is a judicially enforceable constitutional constraint on copyright duration. Judicial deference erodes this doctrine's force by making extensions nearly always rational-basis compliant, effectively neutering 'limited times' as a binding ceiling. The doctrine loses interpretive authority without being formally overruled.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_limitation_doctrine, payer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_limitation_doctrine).

% Argue that rational basis review is too deferential for a constitutional phrase with specific textual meaning ('limited times'), and that courts should scrutinize whether extensions actually serve the constitutional purpose. They are excluded from the legislative process and marginalized in the judiciary's doctrine; their scholarship is cited but not controlling.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_scholars, excluded,
    organized, generational, constrained, national).

% International trade agreements (TRIPS, WIPO agreements) embed minimum copyright standards and disfavor public-domain-first policies. Judicial deference to legislative extensions aligns U.S. practice with international obligations and harmonizes copyright duration across trading partners, reducing arbitrage opportunities.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, international_trade_bodies, beneficiary,
    institutional, generational, arbitrage, global).

% Would benefit from earlier public-domain entry (for training AI, archival systems, interoperability) but are excluded from the constitutional and legislative discourse on term length. They lobby indirectly and face hostile copyright holder coalitions; their exit is constrained—they cannot opt out of copyright scope without massive engineering workarounds.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, technology_sector_innovators, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holders).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__judicial_ambiguity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform national copyright duration via legislative rule, reducing patchwork state-level variations and clarifying the boundary between monopoly and commons for creators and users alike.
% TRANSFER_FUNCTION: Moves the effective control of copyright duration from constitutional text ('limited times') to congressional political economy: every extension transfers public-domain access rights to copyright holders' hands by postponing entry into the commons. The extension also transfers the adjudicatory burden from courts (who would have to define 'limited') to Congress (which can extend as long as it articulates a rational basis).
% ABSENT_VOICES: Public-domain creators, downstream technologists, and international communities favoring open culture are structurally excluded from U.S. legislative copyright negotiations. Constitutional scholars questioning the rational basis test's fit are cited but not controlling. This reading treats their absence as a constitutive feature of the arrangement—their exclusion is what makes judicial deference stable.
% DISAPPEARANCE_RATIONALE: If rational basis deference disappeared and courts enforced 'limited times' as a judicially cognizable ceiling, congressional extensions would face stricter scrutiny and some would fail. The effective copyright term would shorten, public-domain entry would accelerate, and the balance between monopoly and commons would shift. Copyright holders would lobby for constitutional amendment; international negotiations would realign.
% FOUNDING_PROBLEM: Copyright incentive design requires predictable duration to justify authors' investments. The constitutional phrase 'limited times' was ambiguous about what duration counts as 'limited'; without judicial interpretation, copyright terms drifted and became contested at every legislative renewal.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders and Congress assert the founding problem is live: duration unpredictability harms incentives. Constitutional scholars and public-domain advocates assert the problem was solved by prior legislation (1976 Life+50 term) and extensions are rent-extension, not incentive-design. International trade bodies and technology firms offer no direct corroboration—their positions are structural (benefiting from long terms / short terms respectively) rather than empirical.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__judicial_ambiguity_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__judicial_ambiguity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__judicial_ambiguity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).
:- end_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42 at interval end, up from 0.18 in 1976) measures how much the constraint transfers public-domain rights from the commons to copyright holders. The transfer is real and quantifiable: works that would have entered the public domain under Life+50 are still monopolized under Life+70 (Sonny Bono extension) and any future extension. Theater ratio (0.31) reflects that the justification for each extension has shifted: 1976 was genuine incentive design (Life+50 as a coordination function), but each subsequent extension (1988 Berne-TRIPS alignment, 1998 Sonny Bono) rides on rationality that becomes increasingly divorced from empirical incentive effects. The rising theater ratio tracks the growing gap between the stated rationale (incentive) and the apparent function (rent extension). Suppression requirement (0.28) is low because extraction is sustained through deference, not through coercive legal machinery—the suppression is judicial passivity, not active enforcement. Accessibility collapse (0.38) is moderate: alternatives exist in theory (constitutional challenge, congressional reform) but are practically collapsed by the combination of copyright-holder lobbying power and rational basis deference.
 *
 * PERSPECTIVAL GAP:
 *   From Congress's seat, the arrangement is legislative primacy: we set the term, justify it rationally, and courts defer—this is proper separation of powers under rational basis review. From a public-domain creator's seat, the arrangement is coercive: I am excluded from deciding the term, I cannot exit copyright's scope, and courts will not enforce the constitutional 'limited times' ceiling. From the judiciary's seat, the arrangement reduces exposure: we apply rational basis and avoid the politically toxic empirical question of whether extensions actually incentivize. The engine should compute divergent types per seat: congressional and judicial seats likely compute as ropes or scaffolds (coordination with low extraction); public-domain and technology seats compute as snares or tangled-ropes (high extraction with suppression via deference and exclusion).
 *
 * DIRECTIONALITY LOGIC:
 *   Congress is a full beneficiary (d ≈ 0.0): it sets the agenda, controls the justification, and rational basis deference is a rule Congress authored through political economy (prior Justices appointed by both parties have embraced this doctrine). Copyright holders are beneficiaries (d ≈ 0.15): they collects rents, but their exit is mobile—they can petition Congress for further extensions or adapt. Public-domain creators are full targets (d ≈ 0.95): they are trapped (cannot exit copyright scope), excluded (not in the legislative process), and their only recourse is long-term political change or constitutional amendment. The judiciary sits near symmetric (d ≈ 0.5): it benefits from deference doctrine (avoids empirical adjudication) but is also bound by rational basis review—it is not a pure beneficiary because it has constrained itself doctrinally.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does not declare mandatrophy, but the measurement series hints at it: the theater_ratio rises over time (rising gap between stated rationale and apparent function) while extractiveness plateaus (suggesting the mechanism is mature and no longer requires justification). Mandatrophy would consist of a scenario where Congress extended copyright term to a point where 'limited times' ceased to be a plausible reading—where the term approached perpetuity and rational basis review could no longer honestly apply. Under this reading, that mandatrophy would be masked (not judicially recognized) because rational basis deference allows Congress to redefine 'limited' upward arbitrarily. The reading enables scaffolds (transitional coordination justifications) to quietly become enclosures (permanent rent extraction) without judicial intervention, because the judiciary never examines whether the stated justification remains plausible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rational_basis_empirical_verification,
    'Does rational basis review of copyright extensions require courts to verify that extensions actually incentivize creation, or is stating a rational basis sufficient?',
    'Legislative history and judicial opinions explicitly addressing the evidentiary burden for rational basis in copyright context. Post-Eldred doctrine shifts in judicial language would indicate movement on this axis.',
    'If verification is required, extensions that lack empirical support for the incentive claim would fail scrutiny, shortening effective copyright term. If stating a basis is sufficient, extensions can be rationalized indefinitely regardless of empirical effect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rational_basis_empirical_verification, conceptual, 'Whether rational basis includes empirical verification of the stated rationale.').

omega_variable(
    limited_times_ceiling_location,
    'Does ''limited times'' have a specific judicially-cognizable ceiling, or is the phrase entirely delegated to Congress?',
    'Constitutional litigation forcing courts to define a ceiling (e.g., perpetuity bar, ratio to human lifespan, percentage of production cycle). A court opinion explicitly holding a term unconstitutional would resolve.',
    'A judicially-cognizable ceiling would invalidate extensions past that point; unlimited delegation would leave ''limited times'' as purely hortatory, eroding the constraint entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(limited_times_ceiling_location, conceptual, 'Whether ''limited times'' has enforced constitutional limits or is purely legislative.').

omega_variable(
    scaffold_to_enclosure_transition,
    'Is the successive extension pattern (1988, 1998, 2004, pending) evidence of a scaffold that fulfilled its founding purpose and should sunset, or evidence of sustainable legislative balancing?',
    'Comparative analysis of copyright productivity/incentive metrics before and after successive extensions; legislative debate records documenting whether extensions are justified as transitional or permanent.',
    'If transitional scaffolding, extensions should be rolled back when the founding problem is solved (pre-2025). If permanent balancing, the constraint is mislabeled and is actually a tangled_rope or rope, not a scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaffold_to_enclosure_transition, preference, 'Whether the judicial-deference arrangement is a transient coordination mechanism or a stable institutional equilibrium.').

omega_variable(
    deference_doctrine_framing,
    'Does rational basis deference in copyright reflect genuine constitutional ambiguity, or does it reflect judicial choice to defer on a matter courts could regulate?',
    'Historical analysis of rational basis doctrine across domains; comparison to cases where courts rejected rational basis deference in constitutional clauses with similar textual ambiguity.',
    'If constitutional ambiguity is real, this reading is accurate and alternatives (corporate_enclosure, public_scaffold) are readings of the same ambiguous text. If judicial choice, the reading documents one institutional equilibrium among others that courts could adopt.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deference_doctrine_framing, conceptual, 'Whether judicial deference is a response to genuine textual ambiguity or a choice among available doctrines.').

omega_variable(
    excluded_voices_counterfactual,
    'If public-domain creators and technology innovators had a seat at the legislative table, would copyright extensions be structured differently?',
    'Comparative analysis of copyright policy in jurisdictions where non-holder constituencies have political weight; thought experiment: negotiation under Rawlsian veil of ignorance.',
    'Strong structural asymmetry in the legislative process would indicate that exclusion is a constitutive feature of extraction; symmetrical input would suggest the extensions reflect genuine political consensus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_voices_counterfactual, preference, 'Whether the pattern of extensions reflects democratic will or structural asymmetry in lobbying power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 1976, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1976, 0.08).
narrative_ontology:measurement_basis(copy_tr_t1976, observed).
narrative_ontology:measurement(copy_tr_t1988, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1988, 0.12).
narrative_ontology:measurement_basis(copy_tr_t1988, observed).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1998, 0.18).
narrative_ontology:measurement_basis(copy_tr_t1998, observed).
narrative_ontology:measurement(copy_tr_t2010, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2010, 0.26).
narrative_ontology:measurement_basis(copy_tr_t2010, observed).
narrative_ontology:measurement(copy_tr_t2020, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2020, 0.31).
narrative_ontology:measurement_basis(copy_tr_t2020, observed).
narrative_ontology:measurement(copy_tr_t2025, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2025, 0.31).
narrative_ontology:measurement_basis(copy_tr_t2025, projected).

% Extraction over time
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1976, 0.18).
narrative_ontology:measurement_basis(copy_be_t1976, observed).
narrative_ontology:measurement(copy_be_t1988, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1988, 0.28).
narrative_ontology:measurement_basis(copy_be_t1988, observed).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1998, 0.38).
narrative_ontology:measurement_basis(copy_be_t1998, observed).
narrative_ontology:measurement(copy_be_t2010, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement_basis(copy_be_t2010, observed).
narrative_ontology:measurement(copy_be_t2020, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement_basis(copy_be_t2020, observed).
narrative_ontology:measurement(copy_be_t2025, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(copy_be_t2025, projected).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1976, 0.12).
narrative_ontology:measurement_basis(copy_su_t1976, observed).
narrative_ontology:measurement(copy_su_t1988, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1988, 0.16).
narrative_ontology:measurement_basis(copy_su_t1988, observed).
narrative_ontology:measurement(copy_su_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1998, 0.22).
narrative_ontology:measurement_basis(copy_su_t1998, observed).
narrative_ontology:measurement(copy_su_t2010, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2010, 0.26).
narrative_ontology:measurement_basis(copy_su_t2010, observed).
narrative_ontology:measurement(copy_su_t2020, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2020, 0.28).
narrative_ontology:measurement_basis(copy_su_t2020, observed).
narrative_ontology:measurement(copy_su_t2025, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2025, 0.28).
narrative_ontology:measurement_basis(copy_su_t2025, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.18).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_term_extension_pattern__sonny_bono_era).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_entry_delay__commons_enclosure).

% DUAL FORMULATION NOTE:
% The copyright_constitutional_mandate kernel admits three structurally distinct readings: (1) judicial_ambiguity_reading (this story) — 'limited times' is ambiguous; rational basis deference allows congressional extensions. (2) corporate_enclosure_reading — 'limited times' means maximal protection; Congress has constitutional discretion to extend indefinitely. (3) public_scaffold_reading — 'limited times' is a binding ceiling; copyright exists for public enrichment; strict scrutiny applies. Each reading instantiates a different constraint with different epsilon values, beneficiary/victim structures, and mandatrophy risks. Judicial deference (this reading) enables the transition from scaffold to enclosure without judicial invalidation, making it a structural bridge between the public_scaffold and corporate_enclosure endpoints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_constitutional_mandate__judicial_ambiguity_reading, powerless, 0.92).
constraint_indexing:directionality_override(copyright_constitutional_mandate__judicial_ambiguity_reading, organized, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
