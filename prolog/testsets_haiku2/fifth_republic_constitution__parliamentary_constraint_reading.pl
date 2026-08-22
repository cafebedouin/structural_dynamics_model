% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__parliamentary_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__parliamentary_constraint_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: fifth_republic_constitution__parliamentary_constraint_reading
 *   human_readable: Presidential Authority Requiring Legislative Authorization (Fifth Republic Parliamentary Reading)
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   Under the Fifth Republic's parliamentary constraint reading, the
 *   president is a coordinated executive whose major policy authority depends
 *   on legislative authorization. The president heads the government
 *   nominally but operationally depends on the confidence of the National
 *   Assembly — the legislative majority can withdraw confidence through
 *   Article 49, withhold appropriations, block legislation, and force the
 *   prime minister's resignation. This reading instantiates the constraint as
 *   low-extractiveness coordination: the president and legislature must
 *   negotiate and align; the president cannot unilaterally impose policy; the
 *   legislative majority benefits from the constraint by retaining
 *   substantial veto power. The executive is partly victimized when the
 *   legislature withdraws confidence or blocks budgets. This is distinguished
 *   from the hyper-presidential reading (which treats the president as direct
 *   sovereign, minimally constrained) and the cohabitation reading (which
 *   emphasizes negotiated dual authority). The claim is Rope — genuine
 *   coordination solving the problem of tying executive power to elected
 *   representation. The metrics are low extractiveness (0.28), very low
 *   suppression (0.15, authorization is mostly procedural, not coercive), and
 *   low theater (0.12, the constraint functions as described rather than as
 *   theatrical compliance). The measurement series shows slight upward drift
 *   in extractiveness during periods of unified government (time 0-52, where
 *   executive and legislative majority aligned) and reversion at the end,
 *   reflecting that extraction rises when the constraint is least active
 *   (unified control) and falls when cohabitation or opposition control
 *   reasserts the authorization requirement.
 *
 * KEY AGENTS:
 *   - President: head of state and nominal head of government; constrained by legislative confidence and budgetary authorization; partly a victim when legislature withholds consent
 *   - National Assembly: primary locus of democratic legitimacy and legislative authority; benefits from the constraint by retaining veto over executive policy; can remove the prime minister
 *   - Legislative Majority: the dominant party/coalition; benefits from the constraint by making the president dependent on their support; can dissolve the Assembly and call new elections if the president resists
 *   - Prime Minister: leads the government and is accountable to the Assembly, not the president alone; is the legislative majority's agent and pivot point
 *   - Opposition Parties: constrained by majority control but have electoral exit; can become majority through elections
 *   - Constitutional Courts: observe and police the boundary between authorized and unauthorized executive authority; do not supply authorization themselves
 *   - Hyper-Presidential Advocates: excluded from this reading; would dispute that the president is adequately constrained and would seek to strengthen executive authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.28).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.15).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "Presidential Authority Requiring Legislative Authorization (Fifth Republic Parliamentary Reading)").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "constitutional_law/political_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, 'b0fc0444-ee4a-4ff9-8142-a9a8a00753af').
narrative_ontology:cs_kernel_codification('b0fc0444-ee4a-4ff9-8142-a9a8a00753af', fixed_text).
narrative_ontology:cs_authority_grounding('b0fc0444-ee4a-4ff9-8142-a9a8a00753af', lineage).
narrative_ontology:cs_interpretation_layer_present('b0fc0444-ee4a-4ff9-8142-a9a8a00753af').
narrative_ontology:cs_reading_relation('b0fc0444-ee4a-4ff9-8142-a9a8a00753af', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0fc0444-ee4a-4ff9-8142-a9a8a00753af', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('b0fc0444-ee4a-4ff9-8142-a9a8a00753af', foundational, legislative_authorization_required_for_major_policy).
narrative_ontology:cs_axiom_status(legislative_authorization_required_for_major_policy, holdable).
narrative_ontology:cs_axiom_grounding('b0fc0444-ee4a-4ff9-8142-a9a8a00753af', legislative_authorization_required_for_major_policy, deontological).
narrative_ontology:cs_axiom('b0fc0444-ee4a-4ff9-8142-a9a8a00753af', foundational, executive_accountability_to_elected_representatives).
narrative_ontology:cs_axiom_status(executive_accountability_to_elected_representatives, holdable).
narrative_ontology:cs_axiom_grounding('b0fc0444-ee4a-4ff9-8142-a9a8a00753af', executive_accountability_to_elected_representatives, deontological).
narrative_ontology:cs_reference_frame('b0fc0444-ee4a-4ff9-8142-a9a8a00753af', separation_of_powers_with_legislative_supremacy).
narrative_ontology:cs_drift_state('b0fc0444-ee4a-4ff9-8142-a9a8a00753af', contemporary_fifth_republic, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b0fc0444-ee4a-4ff9-8142-a9a8a00753af', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_restraint_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, electorate).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, president).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, opposition_parties).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__parliamentary_constraint_reading, separation_of_powers_principle).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__parliamentary_constraint_reading, legislative_supremacy_in_appropriations).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__parliamentary_constraint_reading, confidence_vote_executive_accountability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Head of state and executive. Under this reading, the president's effective executive authority is constrained by the need for legislative confidence and appropriations. The president can set executive direction through regulation and administrative action, but major policy requires legislative authorization. Faces removal through confidence votes; budgets can be withheld or conditioned. Must negotiate with the legislative majority to govern effectively. Exits are impossible without ceasing to be president; the constitutional office itself is the trap.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, president, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, president, payer).

% Controls the National Assembly and can withdraw confidence from the president, block legislation, condition appropriations, and impeach for high crimes. Benefits from the constraint by retaining substantial veto power over executive action and the ability to force executive compliance with legislative preferences. Can dissolve the Assembly and call new elections if the president refuses to negotiate. Can replace the prime minister while retaining legislative control. The constraint distributes power toward legislative majorities.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority, agenda_setter).

% Leads the government and must maintain the confidence of the Assembly. Under this reading, the prime minister is the legislative majority's agent — chosen by and accountable to the Assembly, not the president alone. Executes laws passed by the legislature. Can be removed by the Assembly independently of the president's wishes. Benefits from the constraint as it makes the prime minister the legislature's pivot point for executive accountability rather than leaving the president unilaterally in command.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister, payer).

% The people's representative body. Under this reading, the Assembly is the primary locus of democratic legitimacy and the constraint ensures executive action ultimately flows from legislative authorization. The Assembly's power to withhold confidence, condition appropriations, and pass legislation makes it the real governance pivot. Cannot exit; it is the institutional anchor of the constraint.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly, beneficiary,
    institutional, generational, analytical, national).

% Parties not in the legislative majority. Under this reading, opposition parties are constrained by the majority's control of the legislative process; they cannot block legislation or force executive compliance unless they can build a coalition majority. Their leverage is electoral — they can run for office and seek to become the majority. They bear the cost of not holding power and having their preferences overridden by the majority, but they have a clear exit: winning elections.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, opposition_parties, payer,
    organized, biographical, mobile, national).

% Citizens eligible to vote. Under this reading, the constraint benefits the electorate by ensuring executive power flows from democratic authorization through the legislature. Elections determine the legislative majority; the majority then determines executive compliance. Citizens are constrained by the need to organize politically and by the geographic/demographic distribution of districts, but they have regular opportunities (elections) to change the majority and executive authority with it.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, electorate, beneficiary,
    organized, biographical, constrained, national).

% Review the constitutionality of laws and executive acts. Under this reading, courts serve to enforce the constraint by striking down legislative acts that violate the constitutional authorization requirement or executive acts that exceed delegated authority. They do not supply the authorization themselves but police the boundary between authorized and unauthorized power.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Political actors who believe the president should embody national will more directly and be less constrained by the legislature. Under this reading, they are excluded from the conversation about the constraint's legitimacy — their preferred reading (hyper-presidential) is not the one being instantiated here. They can advocate for constitutional amendment, but under the standing arrangement they have no institutional seat. Their exclusion is built into the constraint itself: they must work within or against the legislative authorization requirement.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, hyper_presidential_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__parliamentary_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of directing executive power through elected representatives: coordinates the president's vast administrative machinery with the people's will, expressed through legislative authorization and the confidence mechanism. Prevents executive unilateralism and ties executive action to democratic legitimacy.
% TRANSFER_FUNCTION: Transfers policy-setting authority from the president's unilateral discretion to the legislative majority. The president retains administrative implementation power and regulatory authority, but major policy commitments (budgets, laws, international treaties) require legislative consent. In this reading, the flow is FROM president TO legislature — the president must defer to legislative preferences or risk removal.
% ABSENT_VOICES: The hyper-presidential reading's advocates and those who prefer direct executive sovereignty are structurally excluded from this reading's instantiation. They would argue the president is weakened below effective governance and popular will is distorted by legislative faction. Constitutional courts with strong review powers might also be excluded from some decisions if this reading emphasizes legislative prerogative. Outright presidential dictatorship advocates have no seat at all.
% DISAPPEARANCE_RATIONALE: If the legislative authorization requirement and confidence mechanism vanished, the president would be free to govern without legislative consent — budgets could be imposed, legislation bypassed through decree, confidence votes abolished. The legislative majority would lose its veto power and the mechanism binding executive to representative legitimacy would dissolve. Democratic governance would restructure toward unchecked executive power or require new institutional mediations.
% FOUNDING_PROBLEM: Executive power unconstrained by elected representatives: the problem the Fifth Republic solved by creating the prime minister as a legislative agent, requiring legislative confidence, and preserving legislative budgetary and legislative authority. The founding problem was to prevent a return to the Fourth Republic's executive paralysis AND to prevent the rise of a Bonapartist executive that rules by decree and plebiscite rather than by law.
% FOUNDING_PROBLEM_CORROBORATION: The parliamentary constraint reading is corroborated by the constitution's text (Articles 8, 15, 49), by the practice of the legislative majority's ability to force prime ministers' resignation and to withhold confidence, and by constitutional law scholarship and case law from the Constitutional Council affirming legislative supremacy in appropriations and consent to major executive acts. The hyper-presidential reading disputes whether the founding problem persists or whether presidential authority has de facto expanded; this contestation is documented in academic literature and by advocates for presidentialism. An independent observer (e.g., comparative constitutional analysis by external scholars) would attest that the legislative authorization requirement exists and has been enforced, though its strength varies with political majorities.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__parliamentary_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__parliamentary_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fifth_republic_constitution__parliamentary_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).
:- end_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the constraint solves a genuine coordination problem — tying executive power to elected representation — without requiring one party to extract surplus from another. The president and legislature both benefit from predictability and legitimacy; the constraint makes both more stable. Suppression is very low (0.15) because the constraint operates through transparent constitutional procedure (confidence votes, budgetary authority, legislative passage) rather than through coercion or secrecy. Theater is low (0.12) because the constraint functions as described: legislatures actually withhold confidence, block budgets, and force prime ministers' resignation; authorization is not purely performative. The measurement series shows slight upward drift in extractiveness when unified government occurs (executive and legislative majority are same coalition, so the authorization requirement is less active) and reversion when opposition controls the Assembly or cohabitation forces negotiation — this reflects that extraction rises when the constraint's enforcement is relaxed (unified control) and falls when it is tightened (divided government). Resistance is moderate (0.42) because the hyper-presidential reading and presidential advocates mount real opposition to the constraint, pushing for executive power expansion; the constraint is not universally accepted as legitimate. Accessibility collapse is high (0.78) because once the Fifth Republic constitution is understood, alternatives to legislative authorization are ruled out by constitutional law and enforced by courts — the president cannot simply bypass the Assembly without constitutional amendment, which is difficult.
 *
 * PERSPECTIVAL GAP:
 *   The president and legislative majority experience this constraint fundamentally differently. From the president's seat, the constraint is a limitation on executive authority — the president would prefer unilateral power and experiences the legislative authorization requirement as a cost. From the legislative majority's seat, the constraint is a protection and benefit — it ensures the executive defers to the people's elected representatives. From the electorate's seat, the constraint benefits citizens by ensuring executive power flows from democratic authorization. Courts experience the constraint as a boundary-policing function — they enforce it but do not supply the authorization themselves. The engine should compute these divergences from the structural data: the president as partly victim (power constrained, alternatives blocked), the legislative majority as beneficiary (veto power granted), the electorate as beneficiary (democratic control secured). The divergence reflects that the same constraint is Rope from the legislative majority's and electorate's perspective and Rope/Tangled-Rope from the president's perspective (coordination mixed with reduced authority).
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative majority and electorate sit near the beneficiary end of directionality (d near 0.0-0.2) because they benefit from the constraint without paying its primary cost — they gain veto power and democratic legitimacy. The president sits in the middle (d near 0.4-0.5) because the constraint both coordinates executive power with representation AND constrains unilateral authority — both benefit and cost. The opposition parties sit near the target end (d near 0.6-0.7) because they bear the cost of exclusion from executive power but have electoral exit to address it. Prime minister's directionality is near beneficiary (d near 0.15-0.25) because the prime minister is the legislative majority's agent and benefits from the constraint by being their chosen instrument; the president does not unilaterally remove the PM. This derivation flows from beneficiary/victim declarations + exit options + power atoms without overrides required.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing executive unilateralism while ensuring effective governance — remains live under this reading. The constraint has not suffered mandate dissolution; the legislative authorization requirement and confidence mechanism are actively used. When unified government occurs (executive and legislative majority aligned), the constraint is less activated (legislative majority defers to 'their' president), but it does not become theater — legislative confidence and budgetary authority remain available if the executive crosses the majority. When divided government or cohabitation occurs, the constraint is heavily activated and clearly constrains the president. The Rope classification prevents misreading this as either pure coordination (Rope) or pure extraction (Snare). The theatrical element (0.12) reflects not mandatrophy but simply that in unified government, the constraint's mechanisms are less visibly exercised — they remain functional, dormant. This is the correct Rope picture: coordination that remains stable across different political configurations because both the president and the legislature depend on it for legitimacy and predictability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_vs_formal_constraint_strength,
    'Does the legislative authorization requirement operate as a real constraint on executive power, or is it mostly a formal mechanism that presidents routinely circumvent through executive decree and regulatory authority?',
    'Historical analysis of major policy initiatives (budgets, wars, constitutional amendments) and whether presidents or legislative majorities prevailed when they conflicted; empirical counting of: confidence votes called and succeeded, budgets blocked or conditioned, prime ministers forced to resign, decrees struck down by Constitutional Council.',
    'If the mechanism is routinely circumvented, the constraint is better classified as Piton (degraded Rope where the authorization function has atrophied). If activation is rare but enforcement is consistent when invoked, the constraint is Rope as classified. If the constraint has strengthened over time, the classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actual_vs_formal_constraint_strength, empirical, 'Whether the legislative authorization requirement is functionally active or formally theater.').

omega_variable(
    unified_vs_cohabitation_structural_difference,
    'Is the constraint fundamentally different in its operation during periods of unified government (president and legislative majority same coalition) versus cohabitation (opposite parties control presidency and Assembly)?',
    'Comparative case study of decision-making under unified and divided government; empirical measure of: executive deference to legislature, rate of legislative blocking of executive initiatives, confidence vote frequency, budget disputes.',
    'If the constraint''s operation is substantially different under unified government (more executive discretion) versus cohabitation (more legislative constraint), the constraint may be better understood as two distinct constraints, or as a single constraint with substantial drift over political cycles. The measurement series would need to reflect this cycle rather than treating it as stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unified_vs_cohabitation_structural_difference, empirical, 'Whether the constraint''s structure changes with political alignment.').

omega_variable(
    democratic_benefit_vs_legislative_faction,
    'Does the constraint benefit democracy (by ensuring executive power flows from elected representation) or does it benefit whichever coalition controls the legislature (by giving them disproportionate veto power over executive and other legislative minorities)?',
    'Normative analysis: does the constraint serve democratic legitimacy regardless of which coalition holds the legislative majority, or does it serve the majority''s factional interests? Empirical test: do opposition parties and electoral minorities regard the constraint as legitimate protection of their interests, or as a mechanism for legislative supermajority-dominated governance?',
    'If the constraint genuinely benefits all democratic participants (including minorities, by restraining executive unilateralism), it is true Rope. If it primarily benefits whichever coalition holds the legislative majority, it is Tangled Rope (coordination for some + extraction from others). This affects whether the beneficiary set is broad (legislative majority + electorate + opposition) or narrow (legislative majority only).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_benefit_vs_legislative_faction, preference, 'Whether the constraint is democratic constraint or legislative-majority advantage.').

omega_variable(
    kernel_reading_contest,
    'Which reading of the Fifth Republic constitution is structurally true: the parliamentary constraint reading (president constrained by legislature), the hyper-presidential reading (president as direct sovereign, minimally constrained), or the cohabitation equilibrium reading (dual executive requiring negotiation)?',
    'The contest is not empirically resolvable — it is a constitutional interpretation conflict. The three readings are equally valid as readings of the ambiguous constitutional text. Resolution occurs through political struggle: which reading the dominant legal and political coalitions endorse becomes instantiated in practice. A presidential victory in pushing powers beyond legislative authorization would suggest the hyper-presidential reading is ascending; a legislative coalition''s successful forcing of prime minister resignations or budget rejections would suggest the parliamentary reading remains dominant.',
    'Each reading yields a different constraint type, different beneficiary/victim sets, and different directionality distributions. The parliamentary reading yields Rope (low extraction, genuine coordination); the hyper-presidential reading yields Rope or Piton (executive advantage, legislative restraint); the cohabitation reading yields Tangled Rope (coordination mixed with executive-legislative faction competition). The engine''s job is to compute each reading''s metrics independently and detect when readings collide or converge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which of three live constitutional readings of the Fifth Republic''s separation of powers is instantiated in this constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 0, 65).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t0, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(fift_tr_t0, observed).
narrative_ontology:measurement(fift_tr_t13, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 13, 0.1).
narrative_ontology:measurement_basis(fift_tr_t13, observed).
narrative_ontology:measurement(fift_tr_t26, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 26, 0.11).
narrative_ontology:measurement_basis(fift_tr_t26, observed).
narrative_ontology:measurement(fift_tr_t39, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 39, 0.13).
narrative_ontology:measurement_basis(fift_tr_t39, observed).
narrative_ontology:measurement(fift_tr_t52, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 52, 0.12).
narrative_ontology:measurement_basis(fift_tr_t52, observed).
narrative_ontology:measurement(fift_tr_t65, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 65, 0.12).
narrative_ontology:measurement_basis(fift_tr_t65, observed).

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(fift_be_t0, observed).
narrative_ontology:measurement(fift_be_t13, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 13, 0.27).
narrative_ontology:measurement_basis(fift_be_t13, observed).
narrative_ontology:measurement(fift_be_t26, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 26, 0.29).
narrative_ontology:measurement_basis(fift_be_t26, observed).
narrative_ontology:measurement(fift_be_t39, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 39, 0.31).
narrative_ontology:measurement_basis(fift_be_t39, observed).
narrative_ontology:measurement(fift_be_t52, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 52, 0.28).
narrative_ontology:measurement_basis(fift_be_t52, observed).
narrative_ontology:measurement(fift_be_t65, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 65, 0.28).
narrative_ontology:measurement_basis(fift_be_t65, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(fifth_republic_constitution__parliamentary_constraint_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__parliamentary_constraint_reading, 0.12).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Fifth Republic constitution's separation of powers. The parliamentary_constraint_reading emphasizes legislative authorization and democratic constraint on executive power. The hyper_presidential_reading emphasizes presidential sovereignty and minimal legislative constraint. The cohabitation_equilibrium_reading emphasizes dual executive authority and negotiated power-sharing. All three readings share the same constitutional kernel (Articles 5, 8, 15, 49, 50) but instantiate different structural constraints with different beneficiary/victim sets and classifications. The three constraints form a constraint family linked by network.affects_constraints; their metrics and classifications are independent, but their existence as alternatives is documented in each other's omega variables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__parliamentary_constraint_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
