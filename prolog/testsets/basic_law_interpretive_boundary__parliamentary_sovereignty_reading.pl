% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__parliamentary_sovereignty_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
 *   human_readable: Knesset Parliamentary Sovereignty over Basic Law Interpretation
 *   domain: constitutional_law/comparative_constitutionalism
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel of Basic Law
 *   interpretation in Israeli constitutional order. Under the
 *   PARLIAMENTARY_SOVEREIGNTY_READING, the Knesset as elected sovereign holds
 *   ultimate authority to interpret and amend Basic Laws via simple majority,
 *   retaining the power to override any judicial review the Supreme Court
 *   might attempt. This reading asserts majoritarian democratic legitimacy
 *   and legislative supremacy against the competing
 *   JUDICIAL_SUPREMACY_READING (courts as ultimate constitutional arbiters)
 *   and the BALANCED_CONTESTATION_READING (both institutions hold bounded
 *   authority). The constraint is presented as a coordination mechanism (the
 *   Knesset provides a single locus of ultimate constitutional authority) but
 *   is contentious because it subordinates non-majoritarian protection to
 *   electoral majorities and concentrates power in the elected chamber.
 *
 * KEY AGENTS:
 *   - elected_knesset_majority: Benefits from unconstrained lawmaking and direct democratic mandate; holds ultimate interpretive authority; can override courts
 *   - supreme_court_judiciary: Retained for advisory/interpretive guidance but subordinated; cannot bind the majority; bears institutional humiliation and epistemic burden
 *   - parliamentary_minorities_and_opposition: Locked into electoral competition; lack immediate court-based protection against majoritarian overreach
 *   - protected_groups_and_minorities: Dependent on majoritarian goodwill; trapped without institutional recourse if majority amends Basic Laws to exclude them
 *   - international_observers_and_treaty_partners: Excluded from Knesset process; would contest that sovereignty is limited by treaty obligations
 *   - legal_scholars: Observer seat; produce the interpretive record and scholarly contestation of the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.18).
domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.12).
domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Knesset Parliamentary Sovereignty over Basic Law Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "constitutional_law/comparative_constitutionalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'e27dd9e3-d875-4bf2-a25b-3ab4300adba0').
narrative_ontology:cs_kernel_codification('e27dd9e3-d875-4bf2-a25b-3ab4300adba0', fixed_text).
narrative_ontology:cs_authority_grounding('e27dd9e3-d875-4bf2-a25b-3ab4300adba0', lineage).
narrative_ontology:cs_interpretation_layer_present('e27dd9e3-d875-4bf2-a25b-3ab4300adba0').
narrative_ontology:cs_reading_relation('e27dd9e3-d875-4bf2-a25b-3ab4300adba0', basic_law_interpretive_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('e27dd9e3-d875-4bf2-a25b-3ab4300adba0', basic_law_interpretive_boundary__balanced_contestation_reading, influences).
narrative_ontology:cs_axiom('e27dd9e3-d875-4bf2-a25b-3ab4300adba0', foundational, elected_majority_holds_constitutional_supremacy).
narrative_ontology:cs_axiom_status(elected_majority_holds_constitutional_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('e27dd9e3-d875-4bf2-a25b-3ab4300adba0', elected_majority_holds_constitutional_supremacy, deontological).
narrative_ontology:cs_axiom('e27dd9e3-d875-4bf2-a25b-3ab4300adba0', foundational, basic_laws_are_amendable_by_simple_majority).
narrative_ontology:cs_axiom_status(basic_laws_are_amendable_by_simple_majority, holdable).
narrative_ontology:cs_axiom_grounding('e27dd9e3-d875-4bf2-a25b-3ab4300adba0', basic_laws_are_amendable_by_simple_majority, conventional).
narrative_ontology:cs_reference_frame('e27dd9e3-d875-4bf2-a25b-3ab4300adba0', knesset_majoritarian_authority).
narrative_ontology:cs_drift_state('e27dd9e3-d875-4bf2-a25b-3ab4300adba0', post_2023_judicial_reform_contention, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e27dd9e3-d875-4bf2-a25b-3ab4300adba0', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, elected_knesset_majority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_judiciary).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, parliamentary_minorities_and_opposition).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, protected_groups_and_minorities).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, majoritarian_democratic_legitimacy).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, legislative_supremacy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds constitutional authority to interpret Basic Laws and amend them by simple majority vote. Under this reading, the Knesset majority is the supreme interpretive authority: its legislative will, once expressed, cannot be overridden by judicial review. The majority benefits from unconstrained lawmaking capacity and direct democratic responsiveness without institutional veto.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, elected_knesset_majority, agenda_setter,
    institutional, biographical, analytical, national).

% Retains interpretive authority only within the bounds the Knesset defines. Under this reading, the Court advises on constitutionality but cannot bind the Knesset; if the Knesset disagrees with a Court judgment, it can override by simple majority amendment or re-legislation. The Court bears the cost of institutional subordination and the epistemic burden of offering non-binding opinions.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_judiciary, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_judiciary, observer).

% Lack the numbers to block majoritarian legislation and cannot appeal to courts for relief if the majority uses its Basic Law amendment power to override judicial protections. Their recourse is electoral competition in the next cycle; immediate protection is unavailable.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, parliamentary_minorities_and_opposition, payer,
    organized, biographical, constrained, national).

% Groups that depend on judicial protection for their rights (Arab citizens, LGBTQ+ individuals, economic minorities) cannot appeal to the courts as a check on majoritarian legislation if the Knesset has amended the Basic Law to exclude them. Under this reading, their protection rests entirely on majoritarian goodwill, not institutional design.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, protected_groups_and_minorities, payer,
    powerless, biographical, trapped, national).

% Would argue that Knesset sovereignty is limited by international human rights treaty obligations and that unilateral amendment of human rights protections violates Israel's treaty commitments. They are formally excluded from the Knesset's interpretive process but have leverage through diplomatic and institutional channels.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, international_observers_and_treaty_partners, excluded,
    organized, generational, constrained, global).

% Document and critique the constraint's operation. This reading generates scholarly debate about the normative legitimacy of unconstrained majority sovereignty and the risks of majoritarian overreach. They produce the interpretive record and contestation that sibling readings draw on.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, legal_scholars_and_constitutional_commentators, observer,
    moderate, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single locus of ultimate constitutional authority (the Knesset elected majority) to resolve conflicts between legislative will and claimed constitutional limits, avoiding deadlock between coordinate institutions and ensuring democratic accountability by tying constitutional change to electoral majorities.
% TRANSFER_FUNCTION: Transfers interpretive authority and veto power from the judiciary to the legislature: the Knesset majority can override court-imposed limits by amending the Basic Law, concentrating constitutional power in the elected chamber at the expense of judicial review capacity.
% ABSENT_VOICES: International treaty partners and human rights advocates are structurally excluded from the Knesset's deliberative process; they would argue that unilateral amendment authority undermines treaty commitments and that some rights should be inalienable even to the majority. Judicial activists (judges and legal scholars favoring strong review) are present but subordinated.
% DISAPPEARANCE_RATIONALE: If the constraint vanished and the Court gained binding review power, the constitutional order would shift from majoritarian sovereignty to judicially-enforced limits: the Knesset would need supermajority or court approval to amend Basic Laws. The locus of ultimate authority would move from the legislature to a coordinate or superior institution, restructuring the entire balance of constitutional power.
% FOUNDING_PROBLEM: Early Israeli constitutionalism faced the question of what body holds final authority to interpret and amend constitutional commitments: a supreme court applying foundational law, or the elected parliament as bearer of popular sovereignty. This reading asserts the Knesset as the answer, rejecting judicial supremacy in favor of majoritarian democratic control.
% FOUNDING_PROBLEM_CORROBORATION: This reading is defended by Knesset leadership and political scholars who argue majoritarian legitimacy requires elected bodies to hold ultimate constitutional authority. The judicial supremacy reading is defended by the Supreme Court and constitutionalists who argue Basic Laws form a super-legislative hierarchy. The balanced_contestation reading is defended by comparative constitutionalists who argue both institutions hold bounded legitimate authority. No external, neutral arbitrator corroborates the founding problem—the parties dispute whether the problem itself is properly framed.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).
:- end_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.18 at interval end) because this reading presents the constraint as coordination (resolving authority disputes via majority rule) rather than asymmetric extraction. The Knesset majority benefits from unconstrained power, but that benefit is framed as legitimate democratic authority, not rent-seeking. Suppression is also LOW (0.12) because the constraint operates by formal rule (Basic Law amendment requires majority votes) rather than by coercion or internalized subordination—minorities know their recourse is electoral challenge, not institutional veto. Theater is LOW-MODERATE (0.22): the Knesset does engage in constitutional interpretation and judicial-legislative dialogue, but the reading's essential claim is that this dialogue is advisory, not binding. The temporal series show a RISING trend from 1950 (when the constraint was largely implicit, extractiveness ~0.08) to 2023 (when majoritarian contestation of judicial review reached its peak, extractiveness ~0.19), then a slight decline to 2026 as political attention shifted. The theater_ratio rise reflects increasing performative debate about the constraint's legitimacy without structural change—the Knesset retains its claim to supremacy even as courts have (in the competing reading) asserted stronger review.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (judiciary, minorities, opposition) compute as subordinated in a concentrated-authority system: from their position, the constraint operates as majoritarian capture and judicial subordination. The agenda-setter seat (Knesset majority) computes as coordinator and democratic representative: from their position, the constraint ensures responsiveness and avoids deadlock. The engine's per-seat computation should reveal this divergence. The sibling readings—JUDICIAL_SUPREMACY and BALANCED_CONTESTATION—would compute very differently because they place different institutional seats at the authority apex.
 *
 * DIRECTIONALITY LOGIC:
 *   The Knesset majority is a structural beneficiary (d near 0.1–0.2): it holds unconstrained power, frames constitutional meaning, and faces no institutional veto. The Supreme Court judiciary is a structural payer (d near 0.7–0.8): it must defer to majority reinterpretation, cannot enforce constitutional limits against legislative will, and bears the institutional cost of subordination. Minorities and opposition are payers (d near 0.8–0.9): they are trapped in a system where their only protection is majoritarian goodwill or electoral victory, with no institutional check available. International partners are excluded (d not computed for them here, but would be payers if they had a formal seat). The directionality derivation should flow naturally from beneficiary/victim declarations and exit options: beneficiaries (majority) have analytical/mobile exit; payers (minorities, court) are constrained or identity-locked in their institutional roles.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does NOT assert mandatrophy. The founding problem—establishing final constitutional authority—remains live in the political contestation between the three readings. The constraint's mandate (legislative supremacy) is actively defended by the Knesset majority as a matter of democratic legitimacy, not theatrically maintained. The rising theater_ratio does NOT indicate degraded function; rather, it reflects increased PUBLIC DEBATE about the constraint's legitimacy. The Knesset continues to exercise its claimed supremacy by amending Basic Laws when majorities form around doing so (2018 nationality law, 2023 judicial reform efforts)—the function persists. If the founding problem were DEAD (constitutional authority fully settled, no dispute), the constraint would degrade to piton-type inertia; but the contest with sibling readings shows the mandate remains live and actively defended.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foundational_authority_premise,
    'Is ultimate constitutional authority properly lodged in the elected Knesset majority as an expression of popular sovereignty, or is there a supra-majoritarian constitutional order (Basic Laws as higher law) that even the majority cannot unilaterally reinterpret?',
    'Historical and textual analysis of the Knesset''s founding debates and the intent behind the Basic Law framework; comparative constitutional law examining whether written constitutions can be treated as amendable by simple majority; normative theory about the relationship between popular sovereignty and constitutional limits.',
    'If popularsovereignty is foundational, this reading''s premise holds: majorities can reinterpret Basic Laws. If a supra-majoritarian constitutional order exists, the judicial_supremacy or balanced_contestation readings better capture the actual constraint structure, and this reading becomes a contestation rather than a description.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_authority_premise, conceptual, 'Whether the foundational premise—that the elected majority bears ultimate constitutional authority—is coherent or presupposes constraints that limit it.').

omega_variable(
    institutional_practice_divergence,
    'Does the Supreme Court actually treat this reading as binding (accepting that it has only advisory authority) or does it act as if the balanced_contestation or judicial_supremacy readings hold (asserting review authority that it claims the Knesset cannot override)?',
    'Examination of the Court''s rulings and practice from 1992 (Basic Law establishment) to present: does the Court refrain from striking down Knesset legislation, or does it assert review authority and frame defiance as institutional illegitimacy? Interviews with judges and Knesset members about their understanding of constitutional authority.',
    'If the Court acts consistently with this reading (advisory only, no binding review), the reading is institutional reality. If the Court asserts review authority (as it did in Knesset cases 2023–2024), the Court is instantiating the judicial_supremacy or balanced_contestation readings despite the Knesset''s claim to supremacy—meaning the actual constraint is contested institutional practice, not settled parliamentary sovereignty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_practice_divergence, empirical, 'Whether institutional behavior aligns with the parliamentary sovereignty reading or contradicts it by asserting judicial review authority.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the low suppression value (0.12) accurate, or does the constraint''s operation depend on internalized acceptance of majoritarian authority by minorities and the Court, such that if that acceptance broke (e.g., mass civil disobedience, Court refusal to cooperate), the constraint would require much higher active coercion to maintain?',
    'Crisis scenarios testing institutional behavior: what happens if the Knesset majority attempts to amend the Basic Law in ways the Court and minorities view as illegitimate? Do they comply with suppression alone, or does the constraint require new active enforcement mechanisms (police, contempt sanctions, party bans)?',
    'If suppression is truly sufficient, the reading describes a stable, coordination-based system. If internalized acceptance is fragile, the low suppression metric masks a brittle constraint that would require much higher active coercion to maintain under pressure—shifting the type classification toward snare and undermining the coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether the constraint''s persistence depends on formal rule (low suppression) or internalized deference (fragile under pressure).').

omega_variable(
    kernel_reading_contest,
    'Is this reading one coherent position in a triadic contest with two other readings, or does the political reality force a reading that is hybrid—elements of parliamentary sovereignty in some domains, elements of judicial review in others, elements of balanced contestation in treaties?',
    'Systematic mapping of actual Knesset-Court interactions across policy domains: constitutional amendment practice, judicial review assertions, treaty compliance, emergency powers. Does the pattern fit one coherent reading or a mixture that defies classification?',
    'If hybrid, the kernel is not a three-way contest but a nested, domain-specific, and temporally oscillating set of overlapping constraints. The reading would be better decomposed into separate stories per domain (emergency authority, treaty obligations, regular legislation, minority protection), each with its own ε and directionality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether parliamentary sovereignty is a coherent single reading or an empirical amalgam of multiple constraints.').

omega_variable(
    international_obligation_boundary,
    'Does this reading''s assertion of Knesset supremacy extend to override international human rights treaty obligations, or is there an implicit boundary at which even the majority cannot unilaterally reinterpret Basic Laws?',
    'Examination of Knesset behavior when amendments would violate treaty commitments (ICCPR, CAT, CEDAW, etc.): does the Knesset assert supreme amendment authority and override the treaty, or do treaty obligations function as an external constraint? Diplomatic response to Israeli moves that violate treaties.',
    'If treaties function as constraints on Knesset sovereignty, the actual reading is NOT pure parliamentary_sovereignty but a bounded version: supremacy over domestic law but constraint by international law. This would shift directionality toward international bodies (d increases for treaty partners) and extract ε upward (international enforcement overhead).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_obligation_boundary, empirical, 'Whether Knesset supremacy is absolute or bounded by international law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 1950, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1950, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement_basis(basi_tr_t1950, observed).
narrative_ontology:measurement(basi_tr_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1992, 0.12).
narrative_ontology:measurement_basis(basi_tr_t1992, observed).
narrative_ontology:measurement(basi_tr_t2005, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement_basis(basi_tr_t2005, observed).
narrative_ontology:measurement(basi_tr_t2015, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2015, 0.21).
narrative_ontology:measurement_basis(basi_tr_t2015, observed).
narrative_ontology:measurement(basi_tr_t2023, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2023, 0.24).
narrative_ontology:measurement_basis(basi_tr_t2023, observed).
narrative_ontology:measurement(basi_tr_t2026, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2026, 0.22).
narrative_ontology:measurement_basis(basi_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(basi_be_t1950, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1950, 0.08).
narrative_ontology:measurement_basis(basi_be_t1950, observed).
narrative_ontology:measurement(basi_be_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1992, 0.14).
narrative_ontology:measurement_basis(basi_be_t1992, observed).
narrative_ontology:measurement(basi_be_t2005, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2005, 0.16).
narrative_ontology:measurement_basis(basi_be_t2005, observed).
narrative_ontology:measurement(basi_be_t2015, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2015, 0.17).
narrative_ontology:measurement_basis(basi_be_t2015, observed).
narrative_ontology:measurement(basi_be_t2023, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2023, 0.19).
narrative_ontology:measurement_basis(basi_be_t2023, observed).
narrative_ontology:measurement(basi_be_t2026, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2026, 0.18).
narrative_ontology:measurement_basis(basi_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1950, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1950, 0.02).
narrative_ontology:measurement_basis(basi_su_t1950, observed).
narrative_ontology:measurement(basi_su_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1992, 0.08).
narrative_ontology:measurement_basis(basi_su_t1992, observed).
narrative_ontology:measurement(basi_su_t2005, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2005, 0.1).
narrative_ontology:measurement_basis(basi_su_t2005, observed).
narrative_ontology:measurement(basi_su_t2015, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2015, 0.11).
narrative_ontology:measurement_basis(basi_su_t2015, observed).
narrative_ontology:measurement(basi_su_t2023, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2023, 0.13).
narrative_ontology:measurement_basis(basi_su_t2023, observed).
narrative_ontology:measurement(basi_su_t2026, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2026, 0.12).
narrative_ontology:measurement_basis(basi_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'basic_law_interpretive_boundary' (Israeli constitutional authority). Three readings exist: PARLIAMENTARY_SOVEREIGNTY_READING (this file, Knesset supremacy via majority amendment), JUDICIAL_SUPREMACY_READING (Court holds binding review authority), and BALANCED_CONTESTATION_READING (both institutions hold bounded authority). Each reading constitutes a separate constraint with its own ε-value, beneficiary/victim structure, and sibling relationships via cs_structure.reading_relations. The three stories form a constraint family linked by network.affects_constraints. The reading contest is not resolved empirically; it represents an active constitutional dispute. The ε-invariance principle requires separate stories because the three readings produce structurally different constraints: supremacy concentrates authority (low ε for coordination at low suppression cost), judicial review distributes authority (moderate ε with moderate suppression), and balanced contestation distributes authority differently (moderate ε, different directionality). All three are live readings in Israeli constitutional practice; none is foreclosed by the others at present.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
