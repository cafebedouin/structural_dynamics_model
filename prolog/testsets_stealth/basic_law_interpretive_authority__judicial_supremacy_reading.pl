% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Final Constitutional Interpretive Authority
 *   domain: political/legal/institutional
 *
 * SUMMARY:
 *   Constitutional courts claim and exercise final interpretive authority
 *   over the constitution's meaning, justified by specialized legal expertise
 *   and insulation from political pressure. The judiciary collects
 *   institutional authority, prestige, and docket control from the
 *   arrangement; legislatures and electoral majorities bear the costs when
 *   judicial review blocks or unwinds legislation, and the legislative
 *   process absorbs gridlock costs around constitutionally sensitive policy.
 *   A genuine coordination function - terminal dispute resolution among
 *   co-equal branches - coexists with an asymmetric transfer of decision
 *   authority away from electorates, which is why this story claims
 *   tangled_rope. This file instantiates ONE reading of the
 *   basic_law_interpretive_authority kernel: per the epsilon-invariance
 *   principle, the sibling readings (parliamentary sovereignty, popular
 *   constitutionalism) are separate constraints with their own epsilon
 *   values, seats, and classifications, linked via
 *   network.affects_constraints - nothing here averages across readings. The
 *   claim and the metrics are independent authored facts: the metrics
 *   describe the arrangement's actual operation; the engine computes per-seat
 *   types from the structural data.
 *
 * KEY AGENTS:
 *   - - constitutional_courts: Agenda-setter and primary beneficiary (institutional / doctrinal arbitrage) - administers the interpretive monopoly and collects authority, prestige, and docket control
 *   - - legislature: Primary payer (powerful / constrained) - co-equal on paper, subordinate on constitutional questions; bears struck-legislation and gridlock costs
 *   - - electoral_majorities: Payer (organized / trapped) - enacted preferences reversible by a bench majority it cannot remove
 *   - - constitutional_litigants: Beneficiary (moderate / constrained) - gains access to the terminal forum
 *   - - legal_profession: Beneficiary (organized / identity_locked) - the expertise requirement channels status, work, and identity
 *   - - minority_groups: Conditional beneficiary with secondary payer position (powerless / trapped)
 *   - - popular_constitutional_movements: Excluded voice (organized / trapped) - would relocate interpretive authority into public contestation
 *   - - comparative_constitutional_scholars: Analytical observer - sees the full cross-system structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.58).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.58).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy Reading of Final Constitutional Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "political/legal/institutional").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, '42c3715c-249d-43c7-b3ff-616fb1b07c1b').
narrative_ontology:cs_kernel_codification('42c3715c-249d-43c7-b3ff-616fb1b07c1b', fixed_text).
narrative_ontology:cs_authority_grounding('42c3715c-249d-43c7-b3ff-616fb1b07c1b', expertise).
narrative_ontology:cs_interpretation_layer_present('42c3715c-249d-43c7-b3ff-616fb1b07c1b').
narrative_ontology:cs_reading_relation('42c3715c-249d-43c7-b3ff-616fb1b07c1b', basic_law_interpretive_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('42c3715c-249d-43c7-b3ff-616fb1b07c1b', basic_law_interpretive_authority__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('42c3715c-249d-43c7-b3ff-616fb1b07c1b', foundational, legal_expertise_yields_determinate_answers).
narrative_ontology:cs_axiom_status(legal_expertise_yields_determinate_answers, holdable).
narrative_ontology:cs_axiom_grounding('42c3715c-249d-43c7-b3ff-616fb1b07c1b', legal_expertise_yields_determinate_answers, empirically_contingent).
narrative_ontology:cs_axiom('42c3715c-249d-43c7-b3ff-616fb1b07c1b', foundational, insulation_from_political_pressure_improves_rights_protection).
narrative_ontology:cs_axiom_status(insulation_from_political_pressure_improves_rights_protection, holdable).
narrative_ontology:cs_axiom_grounding('42c3715c-249d-43c7-b3ff-616fb1b07c1b', insulation_from_political_pressure_improves_rights_protection, instrumental).
narrative_ontology:cs_reference_frame('42c3715c-249d-43c7-b3ff-616fb1b07c1b', insulated_expert_final_interpretation).
narrative_ontology:cs_drift_state('42c3715c-249d-43c7-b3ff-616fb1b07c1b', contemporary_polarization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('42c3715c-249d-43c7-b3ff-616fb1b07c1b', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_courts).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_litigants).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, minority_groups).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, minority_groups).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, stare_decisis_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, marbury_finality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide which constitutional interpretations bind the other branches, control their own docket, and write precedents that legislatures and lower courts must follow. Institutional prestige, budgets, and staffing grow with the scope of questions only they may finally settle. Justices serve long fixed or life terms and answer to no electorate; their main exposures are appointment politics at the margin and episodic proposals to curb their jurisdiction.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_courts, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_courts, beneficiary).

% Drafts and passes statutes that take effect subject to later judicial invalidation. When courts strike its work, the labor of winning coalitions is voided and the policy question returns to a body that cannot relitigate the constitutional ruling itself. Overriding a decision requires supermajorities or constitutional amendment; waiting for turnover on the bench takes years. Members campaign on promises the bench can unwind.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legislature, payer,
    powerful, biographical, constrained, national).

% Vote for platforms and candidates expecting enactment. When courts invalidate flagship legislation, the majority's preference is reversed by officials it did not elect and cannot remove before their terms end. Its levers - electing presidents and senators who eventually appoint judges - operate on decade-long delays, and it cannot opt out of the constitutional order that adjudicates its choices.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    organized, biographical, trapped, national).

% Bring the cases that ask courts to invalidate or defend legislation. Access depends on standing doctrine and on resources for years of litigation; winners obtain rulings that bind everyone, not only themselves. Without the terminal forum their claims would have no authoritative resolution at all.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_litigants, beneficiary,
    moderate, immediate, constrained, national).

% Supplies the judges, clerks, advocates, and scholars through whom constitutional questions move. Professional standing, law school curricula, and career ladders are organized around mastery of the interpretive craft the system requires; abandoning that framework would forfeit the credentials, livelihoods, and identities built on it.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession, beneficiary,
    organized, generational, identity_locked, national).

% Rely on courts for protection against hostile majorities - and have at times watched courts validate the hostility instead, or strike down protections won legislatively. Their access runs through the same costly litigation channel as everyone else's; they cannot exit the constitutional order that adjudicates their status.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, minority_groups, beneficiary,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__judicial_supremacy_reading, minority_groups, payer).

% Organize to change constitutional understanding through elections, amendments, and mass argument rather than adjudication. They hold no formal seat: their interpretations carry weight only when courts choose to notice them, and their main instruments - amendment and jurisdiction-stripping - sit behind the highest thresholds in the system.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, popular_constitutional_movements, excluded,
    organized, generational, trapped, national).

% Study how different democracies allocate final interpretive authority - strong-review, parliamentary-supremacy, and hybrid systems - and publish comparisons that none of the domestic participants controls. Their analyses inform reform debates but bind no one.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_courts).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single terminal forum for resolving inter-branch disputes over constitutional meaning, stabilizing legal expectations across the polity and giving embedded rights a venue insulated from electoral cycles.
% TRANSFER_FUNCTION: Moves final decision authority over contested constitutional questions from elected legislatures and the electoral majorities they answer to, to unelected judges; moves agenda-setting over constitutional change to litigants with the standing and resources to sue.
% ABSENT_VOICES: Citizens without standing or litigation resources - the poor and unorganized issue publics - never reach the forum that decides their constitutional claims; legislatures appear only as defendants. Popular constitutional movements would relocate interpretive authority into public contestation but enter the conversation only as amici or protesters outside the courthouse.
% DISAPPEARANCE_RATIONALE: If final judicial authority vanished overnight, every branch would self-certify constitutionality, inter-branch deadlock over constitutional questions would return, precedent would lose hierarchical force, and rights enforcement would become fully political - the legal order would reorganize around departmentalism, a new arbitral institution, or legislative self-supervision.
% FOUNDING_PROBLEM: A written supreme law had to be applied by multiple co-equal branches with no built-in mechanism for resolving disagreements between their interpretations - and transient majorities could rewrite fundamental rights through ordinary legislation.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholarship and the historical record of pre-review governance corroborate that the coordination problem is real. Attestation that courts specifically must solve it comes overwhelmingly from the judiciary and the bar it credentials; external scholars divide between parliamentary and popular solutions, and several stable democracies function without strong judicial review - evidence against necessity from outside the benefiting parties.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.58: the arrangement transfers final decision authority with no electoral mandate and no sunset, but the transfer is bounded - most statutes survive review, and courts ultimately depend on compliance by the other branches. Suppression is 0.58 and is authored as a raw structural property (unscaled by power or scope): rival interpretive authorities are foreclosed through stare decisis, jurisdictional doctrine, and contempt-backed finality, not through participant preference. Theater_ratio is 0.28: opinion-writing is mostly functional legal reasoning, with a growing performative component (neutrality rhetoric, unanimity management, legitimacy maintenance) as polarization raises the stakes of each ruling. Accessibility_collapse is 0.55: once judicial supremacy is established, legislative self-interpretation collapses as a domestic alternative, but parliamentary-supremacy systems abroad and academic alternatives persist, so collapse is partial. Resistance is 0.60: recurring court-curbing proposals, packing threats, jurisdiction-stripping bills, and occasional open defiance. The temporal series run on ONE shared grid (t = 0, 40, 80, 120, 160, 200, 220) with all three metrics authored at every point. The suppression_requirement series is authored deliberately: the story tracks enforcement-capacity maturation - from a weak, deferential bench at t=0 through consolidation of precedent doctrine to a mature, hardened enforcement apparatus by interval end - which is exactly the dynamic the scalar base_properties.suppression cannot carry alone. Identity-lock note: the legal profession's exit option is identity_locked because professional identity is constituted through the expertise claim itself - careers, curricula, and status presuppose that interpretive craft is what resolves constitutional questions; if that frame broke, the profession's stake in the arrangement would convert from identity-fused to merely economic, and its defense of the arrangement would weaken accordingly.
 *
 * PERSPECTIVAL GAP:
 *   Same-level actor dynamics drive the sharpest divergence: the legislature and the courts are nominally co-equal institutional actors with comparable global standing, yet they compute opposite seats. The differentiation is constraint-specific, not power-based - jurisdiction over constitutional meaning, tenure length, and the other branches' compliance dependence give the court a structural position the legislature's nominal parity cannot offset. From the agenda-setter seat the arrangement appears as stewardship of a necessary coordination function; from the payer seats the same structure operates as an enforced transfer of decision authority to unaccountable officials. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations map directly onto directionality. Constitutional_courts sit nearest the beneficiary pole (d near 0.0): they administer the arrangement and collect its principal return, with doctrinal arbitrage as their exit profile. Legislature and electoral_majorities sit nearest the target pole (d near 1.0): they bear the transfer, and their exit options (constrained, trapped) amplify effective extraction - a trapped target cannot arbitrage away its position. Constitutional_litigants and legal_profession derive low-to-moderate d as beneficiaries, with the profession's identity_lock deepening its investment. Minority_groups are pulled off the pure-beneficiary pole by their secondary payer position: they receive protection episodically and pay when courts validate hostility or strike protective legislation. Popular_constitutional_movements carry high d without standing - they bear the closure of alternatives while excluded from the conversation. Scope is national for the domestic seats; the observer seat is global, reflecting comparative scholarship's vantage point.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling in both directions. Calling this a snare ignores the real coordination function: inter-branch interpretive deadlock is a genuine collective-action problem, and some terminal mechanism is plausibly necessary for any written constitution - the arrangement solves a real problem, not cover for one. Calling it a rope ignores that the same structure actively transfers final authority from electorates to insulated officials, requires continuous enforcement (precedent hierarchy, jurisdiction control), and suppresses rival interpretive authorities rather than leaving alternatives open. On the R5 genealogy: the founding problem (inter-branch interpretive conflict plus majority threat to embedded rights) is contested rather than dead - the coordination half remains live, while the claim that courts specifically must solve it is disputed by functioning parliamentary systems. Because the founding problem is not dead and theater_ratio (0.28) sits far below piton range, there is no mandatrophy: the mandate has not outlived its function, though its justification is increasingly contested and the drift_state records that strain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the basic_law_interpretive_authority kernel - how would the parliamentary_sovereignty or popular_constitutionalism readings restructure the beneficiary and victim sets?',
    'Generate the sibling stories and compare computed seat classifications; the disagreement is located in whether final interpretive authority is an allocable institutional good at all.',
    'Under the parliamentary reading the judiciary drops from beneficiary to executing agent and the legislature exits the victim set; under popular constitutionalism no seat holds terminal authority and the victim set dissolves into diffuse coordination costs - both siblings would classify very differently from this file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story instantiates the judicial_supremacy_reading; sibling readings redistribute every seat.').

omega_variable(
    countermajoritarian_price,
    'Is the burden borne by legislatures and electoral majorities a net social cost, or the price of rights protection and inter-branch stability that majoritarian processes systematically undersupply?',
    'Cross-national comparison of rights outcomes and constitutional stability under strong-judicial-review versus parliamentary-supremacy systems, controlling for wealth and democratic age.',
    'If insulated courts systematically outperform on protected-rights outcomes, a large share of the measured burden reclassifies as coordination cost and the constraint moves toward rope; if not, the transfer stands as rent and the classification hardens toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(countermajoritarian_price, empirical, 'Whether the countermajoritarian burden buys countermajoritarian goods.').

omega_variable(
    expertise_determinacy_premise,
    'Does specialized legal expertise actually yield determinate constitutional answers, or do judges'' policy preferences drive outcomes that the expertise framing conceals?',
    'Attitudinal-model testing against legal-model predictions on ideologically salient dockets; inter-rater reliability studies of trained jurists on hard cases.',
    'If preferences dominate, the expertise justification fails on its own terms and the arrangement''s legitimacy erodes toward a raw allocation of power - strengthening extraction-side readings; if law constrains, the coordination claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expertise_determinacy_premise, empirical, 'Empirical status of the expertise premise underlying final judicial authority.').

omega_variable(
    terminal_forum_necessity,
    'Is a terminal interpretive forum a structural necessity of written constitutionalism, or one contingent solution among several (departmentalism, council review, popular entrenchment)?',
    'Comparative institutional analysis of stable democracies operating without final-court supremacy; formal modeling of inter-branch interpretive deadlock rates under alternative allocations.',
    'If necessity holds, part of the arrangement''s persistence is structural rather than maintained interest - mountain-adjacent; if contingent, the arrangement is fully maintained by its beneficiaries and its resistance profile is explained by that maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(terminal_forum_necessity, conceptual, 'Necessity versus contingency of final interpretive authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 0, 220).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(basi_tr_t0, observed).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement_basis(basi_tr_t40, observed).
narrative_ontology:measurement(basi_tr_t80, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 80, 0.16).
narrative_ontology:measurement_basis(basi_tr_t80, observed).
narrative_ontology:measurement(basi_tr_t120, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 120, 0.2).
narrative_ontology:measurement_basis(basi_tr_t120, observed).
narrative_ontology:measurement(basi_tr_t160, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 160, 0.24).
narrative_ontology:measurement_basis(basi_tr_t160, observed).
narrative_ontology:measurement(basi_tr_t200, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 200, 0.27).
narrative_ontology:measurement_basis(basi_tr_t200, observed).
narrative_ontology:measurement(basi_tr_t220, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 220, 0.28).
narrative_ontology:measurement_basis(basi_tr_t220, observed).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.26).
narrative_ontology:measurement_basis(basi_be_t0, observed).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 40, 0.34).
narrative_ontology:measurement_basis(basi_be_t40, observed).
narrative_ontology:measurement(basi_be_t80, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 80, 0.43).
narrative_ontology:measurement_basis(basi_be_t80, observed).
narrative_ontology:measurement(basi_be_t120, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 120, 0.5).
narrative_ontology:measurement_basis(basi_be_t120, observed).
narrative_ontology:measurement(basi_be_t160, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 160, 0.54).
narrative_ontology:measurement_basis(basi_be_t160, observed).
narrative_ontology:measurement(basi_be_t200, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 200, 0.57).
narrative_ontology:measurement_basis(basi_be_t200, observed).
narrative_ontology:measurement(basi_be_t220, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 220, 0.58).
narrative_ontology:measurement_basis(basi_be_t220, observed).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(basi_su_t0, observed).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement_basis(basi_su_t40, observed).
narrative_ontology:measurement(basi_su_t80, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 80, 0.47).
narrative_ontology:measurement_basis(basi_su_t80, observed).
narrative_ontology:measurement(basi_su_t120, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 120, 0.52).
narrative_ontology:measurement_basis(basi_su_t120, observed).
narrative_ontology:measurement(basi_su_t160, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 160, 0.55).
narrative_ontology:measurement_basis(basi_su_t160, observed).
narrative_ontology:measurement(basi_su_t200, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 200, 0.57).
narrative_ontology:measurement_basis(basi_su_t200, observed).
narrative_ontology:measurement(basi_su_t220, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 220, 0.58).
narrative_ontology:measurement_basis(basi_su_t220, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'constitutional interpretation' into three structurally distinct constraints sharing one kernel (basic_law_interpretive_authority). The label conflates rival allocations of finality; per the epsilon-invariance principle each reading gets its own epsilon, beneficiary/victim structure, and classification rather than one story with a measurement parameter. This story (judicial supremacy) is the incumbent arrangement and links to both siblings: the parliamentary reading typically cites judicial failure as evidence for relocating finality, and the popular reading cites both incumbents as evidence that no terminal allocator is legitimate. Edges run from this story to the siblings via affects_constraints; contamination propagation across the family tracks whether the expertise premise degrades.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
