% ============================================================================
% CONSTRAINT STORY: family_law_authority__secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__secular_contractual_reading, []).

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
 *   constraint_id: family_law_authority__secular_contractual_reading
 *   human_readable: Civil Marriage as State-Law Contract Between Autonomous Individuals
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   Under the secular contractual arrangement, a marriage comes into legal
 *   existence through registration with the state by two autonomous adults,
 *   and the state supplies a standardized default package of property,
 *   support, inheritance, custody, and decision-authority terms that the
 *   parties may modify within limits. No religious act is required for
 *   validity; religious ceremonies, where performed, are legally inert until
 *   civil registration completes. Entry is voluntary and open to consenting
 *   adults across lines of belief; exit runs through court-supervised
 *   dissolution. The claim and the metrics are independently authored facts
 *   here: the claimed type states what this reading holds to be structurally
 *   true of the arrangement, and the metric values describe its observed
 *   operation, including residual costs this reading itself acknowledges.
 *   Epsilon's referent is the standing registration-based arrangement as this
 *   reading assesses it — not some deregulated alternative. KEY AGENTS (by
 *   structural relationship): - married_couples: primary
 *   participant-beneficiary (moderate/constrained) — receives the default
 *   package, bears fees and exit friction - minor_children_of_marriage:
 *   protected third party (powerless/trapped) — covered by defaults they
 *   cannot consent to - civil_registration_authorities: agenda-setter
 *   (institutional/identity_locked) — maintains the validity ledger -
 *   family_law_judiciary: agenda-setter (institutional/identity_locked) —
 *   enforces the defaults on petition - unregistered_cohabiting_partners:
 *   excluded seat (moderate/mobile) — outside the ledger, buys substitutes
 *   privately - religious_officiants_without_civil_authority: excluded seat
 *   (organized/identity_locked) — ceremonies carry no civil effect alone -
 *   family_law_practitioners: secondary collector (organized/arbitrage) —
 *   revenue rides procedural complexity - third_party_relying_institutions:
 *   reliance beneficiary (organized/arbitrage) — transacts on registered
 *   status - comparative_law_scholars: analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, 0.28).
domain_priors:suppression_score(family_law_authority__secular_contractual_reading, 0.29).
domain_priors:theater_ratio(family_law_authority__secular_contractual_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, suppression_requirement, 0.29).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__secular_contractual_reading, rope).
narrative_ontology:human_readable(family_law_authority__secular_contractual_reading, "Civil Marriage as State-Law Contract Between Autonomous Individuals").
narrative_ontology:topic_domain(family_law_authority__secular_contractual_reading, "comparative_law/political_theory/religious_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__secular_contractual_reading, '30983949-4038-416b-a9c8-da34a33391c5').
narrative_ontology:cs_kernel_codification('30983949-4038-416b-a9c8-da34a33391c5', formalized).
narrative_ontology:cs_authority_grounding('30983949-4038-416b-a9c8-da34a33391c5', self_enforcing).
narrative_ontology:cs_reading_relation('30983949-4038-416b-a9c8-da34a33391c5', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('30983949-4038-416b-a9c8-da34a33391c5', family_law_authority__muslim_shariat_reading, influences).
narrative_ontology:cs_reading_relation('30983949-4038-416b-a9c8-da34a33391c5', family_law_authority__christian_canonical_reading, influences).
narrative_ontology:cs_reading_relation('30983949-4038-416b-a9c8-da34a33391c5', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('30983949-4038-416b-a9c8-da34a33391c5', foundational, civil_registration_sole_validity).
narrative_ontology:cs_axiom_status(civil_registration_sole_validity, holdable).
narrative_ontology:cs_axiom_grounding('30983949-4038-416b-a9c8-da34a33391c5', civil_registration_sole_validity, conventional).
narrative_ontology:cs_axiom('30983949-4038-416b-a9c8-da34a33391c5', foundational, spousal_legal_equality).
narrative_ontology:cs_axiom_status(spousal_legal_equality, holdable).
narrative_ontology:cs_axiom_grounding('30983949-4038-416b-a9c8-da34a33391c5', spousal_legal_equality, deontological).
narrative_ontology:cs_axiom('30983949-4038-416b-a9c8-da34a33391c5', secondary, religious_ceremony_legally_inert).
narrative_ontology:cs_axiom_status(religious_ceremony_legally_inert, holdable).
narrative_ontology:cs_axiom_grounding('30983949-4038-416b-a9c8-da34a33391c5', religious_ceremony_legally_inert, conventional).
narrative_ontology:cs_reference_frame('30983949-4038-416b-a9c8-da34a33391c5', autonomous_individual_consent_baseline).
narrative_ontology:cs_drift_state('30983949-4038-416b-a9c8-da34a33391c5', contemporary_multi_status_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('30983949-4038-416b-a9c8-da34a33391c5', '').
narrative_ontology:cs_kernel_id(family_law_authority__secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, married_couples).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, minor_children_of_marriage).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, third_party_relying_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, family_law_practitioners).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, married_couples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Two adults register a union and receive the state's default package: property presumptions, mutual support obligations, inheritance rights, next-of-kin standing, and presumptive parental responsibility for children born within the union. They pay registration fees and comply with recording formalities. Leaving requires a court-supervised dissolution with waiting periods and possible support or property transfers; staying in an unhappy union because dissolution is costly is a familiar outcome.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, married_couples, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__secular_contractual_reading, married_couples, payer).

% Children born within registered unions are covered by custody presumptions, child-support enforcement, and legitimation rules they never chose. Their care, residence, and financial security ride on defaults set by strangers at the moment their parents registered. They cannot consent to the arrangement or exit it.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, minor_children_of_marriage, beneficiary,
    powerless, biographical, trapped, national).

% Banks, hospitals, insurers, employers, and immigration authorities verify marital status before extending credit, disclosing records, recognizing next-of-kin, or processing visas. Clear registered status lets them transact without bespoke investigation; ambiguous status pushes them back onto individually drafted contracts and affidavits, which they can produce at their own expense.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, third_party_relying_institutions, beneficiary,
    organized, generational, arbitrage, global).

% Registry offices and vital-statistics agencies record marriages, issue certificates, set documentary requirements, and maintain the ledger every other institution reads. They charge fees sized to administration and cannot stop performing the function without collapsing the civil-status infrastructure that property, inheritance, and custody law all cite.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, civil_registration_authorities, agenda_setter,
    institutional, generational, identity_locked, national).

% Family courts adjudicate dissolution, custody, support, and property division under statutory defaults. Their dockets, doctrines, and precedents shape how the default terms actually bind; they enforce obligations on petition and cannot decline the docket.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, family_law_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Committed partners who never register live outside the ledger: no automatic property presumptions, no presumptive next-of-kin standing, no support enforcement. They can approximate parts of the package with wills, powers of attorney, and contracts at their own expense, or accept the exposure.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, unregistered_cohabiting_partners, excluded,
    moderate, biographical, mobile, national).

% Clergy and religious bodies continue solemnizing weddings, but their ceremonies confer no civil status by themselves; couples must complete civil registration separately for legal effect. Many officiants perform both steps in sequence and treat the split as routine; some refuse civil registration on principle and leave their congregants without the default protections.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_officiants_without_civil_authority, excluded,
    organized, generational, identity_locked, global).

% Attorneys specializing in family matters draft marital agreements, navigate the defaults, and represent parties in dissolution. Their revenue scales with the volume and complexity the procedure generates; a materially simplified process would thin their docket, and their skills port to adjacent practice areas if needed.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, family_law_practitioners, beneficiary,
    organized, generational, arbitrage, national).

% Academic and law-reform analysts compare registration regimes across jurisdictions, trace which defaults correlate with lower dispute rates, and publish findings that neither marrying parties nor registries commissioned. They see the full structure without standing inside it.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, comparative_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__secular_contractual_reading, diffuse).
narrative_ontology:fixing_cost_class(family_law_authority__secular_contractual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, centrally legible act — registration — that converts a private union into a status every dependent system (property registries, inheritance law, custody courts, hospitals, credit, immigration) can verify and rely on, replacing per-pair private contracting of each incident with one standardized default package.
% TRANSFER_FUNCTION: Moves enforceable entitlements and obligations between spouses under statutory defaults — support, property shares, inheritance, decision authority — and transfers registration fees and record-keeping compliance from registrants to state offices; on dissolution it moves adjudicated transfers between former spouses.
% ABSENT_VOICES: Unregistered partners have no seat where validity criteria are set; children bound by custody defaults cannot appear; historically, spouses subordinated by earlier default terms entered bargains they had no hand in drafting. Their objections surface only through litigation and reform campaigns after the fact.
% DISAPPEARANCE_RATIONALE: Property transmission, legitimation, custody presumptions, next-of-kin standing, and credit and immigration verification all key off registered status; deleting the registration framework overnight would strand existing unions in legal limbo and force improvised reconstruction of every dependent system within months.
% FOUNDING_PROBLEM: Modernizing states needed uniform, verifiable civil-status records to replace fragmented religious jurisdiction: to secure property transmission, determine the legitimacy and support of children, and enforce spousal obligations under one sovereign law. Later reform waves extended the founding project — removing sectarian tests and gender hierarchy from the contract form so any two consenting adults could register on equal terms.
% FOUNDING_PROBLEM_CORROBORATION: Registration-history scholarship and law-reform commission reports — sources outside the benefiting parties — corroborate the original record-legibility problem as real and substantially solved; state registries self-report continuing need for uniform records; cohabitation-rights advocates, also outside the beneficiary set, dispute that the registered dyad remains the necessary solution and attest the residual problem as overstated.
narrative_ontology:disappearance_verdict(family_law_authority__secular_contractual_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__secular_contractual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__secular_contractual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__secular_contractual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__secular_contractual_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__secular_contractual_reading_tests).
:- end_tests(family_law_authority__secular_contractual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.28 at interval end because the arrangement's residual costs — registration formalities, court-supervised exit friction, exclusion of non-registrants, and default terms parties did not individually negotiate — are real but bounded, and the arrangement is entered voluntarily by people who overwhelmingly judge it net-positive. Suppression is 0.29: unregistered cohabitation, private contracting, and ceremony-only unions are all lawful, but each is degraded relative to registration, so alternatives persist without being crushed. Theater is low (0.18) because the performed activity — recording, adjudicating, certificate issuance — is the function. Accessibility_collapse is 0.45: understanding the arrangement does not force everyone into it, because workable substitutes survive, but each substitute costs effort the default would have absorbed. Resistance is 0.30: principled refusal of civil registration, licensure objections, and family-form pluralist critique exist and recur, without threatening uptake. The temporal series run on one shared grid (every tracked metric authored at every time point 0-60); all three trajectories are monotonic rather than cyclical — extractiveness falls as gender-symmetric defaults, no-fault dissolution, and removal of sectarian entry bars landed; the suppression series tracks the deliberate winding-down of moral-policing enforcement capacity, which is why a suppression_requirement series is authored at all. Suppression is a raw structural property and is not scaled by power or scope in the authored values; only extraction is scaled downstream. Receipt surface: gain_flow is authored as 'diffuse' as an affirmative claim after checking every named seat — couples receive service value, not captured rents; registry fees track administration cost; practitioner income is payment for adjudication and drafting labor; no seat collects the arrangement's surplus. fixing_cost is 'prohibitive': each marginal reform of the framework carries culture-war mobilization costs far exceeding its diffuse per-seat benefit, and wholesale removal carries catastrophic transition costs — though this cell signature is NOT a piton signal here, because unlike a piton this arrangement is actively maintained by visible, concentrated beneficiaries (registrants, courts, registries) rather than drifting on inertia.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. A newly registering couple meets the arrangement as a service it opted into: coordination dominant, extraction near the floor. A spouse mid-dissolution meets the same structure as costly machinery controlling the terms of their exit: extraction-flavored. An unregistered partner meets a boundary, not a bill — they pay the arrangement nothing and are refused its benefits, a structurally different position from either. The registry and the judiciary meet administration and docket load. The engine computes per-seat types from power, exit, and directional data; the authored claim does not adjudicate among these experiences, and the divergence between the couple seat and the dissolving-spouse seat is exactly the signal the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries anchor the low-d end: married_couples (net recipients of the default package, partially offset by fees and exit costs via their secondary payer position), minor_children_of_marriage (full subsidy side, no consent), and third_party_relying_institutions (reliance value, easy substitution keeps them near the beneficiary end). No victim group is declared because no seat bears asymmetric extraction into a capturer's hands — the arrangement's costs are entry-priced, service-priced, or exit-priced back into the parties' own dispute. The excluded and observer seats fall to neutral or analytical handling by design: unregistered partners and officiants participate in no transfer loop, so forcing a beneficiary/target polarity on them would misstate their position. No directionality_overrides are authored: the derivation chain from beneficiary declarations and exit options produces the correct qualitative placement for every seat, and the two institutional agenda-setters are genuinely near-symmetric (service provision against fee revenue; enforcement labor against docket authority), so a per-power-atom override would flatten a distinction the structural data already encodes correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — uniform, verifiable civil status replacing fragmented religious jurisdiction — remains substantially live, and the R5 interview returns contested rather than dead, so no zombie-mandate mismatch fires against the world_rearranges verdict. Classification guards run in both directions here. Against mislabeling as extraction: the diffuse receipt finding (no seat captures the surplus) and voluntary entry distinguish this from a snare, and the absence of a sunset clause marks the coordination as steady-state rather than transitional, ruling out scaffold. Against mislabeling as pure frictionless coordination: the residual extraction series is nonzero and the excluded seats are real, so a naive rope reading that ignored unregistered partners and dissolution friction would undercount the arrangement's costs. The declining extractiveness trajectory is the opposite of the accumulation pattern T17 watches for: this constraint shed extraction as it matured rather than accreting it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the family_law_authority kernel; which structural features would change if a sibling reading (dharmashastra, shariat, canonical, Zoroastrian) instantiated the kernel instead?',
    'Comparative adoption analysis: observe what changes in validity criteria, victim sets, beneficiary structures, and enforcement machinery when jurisdictions shift the governing reading of the same kernel.',
    'A sibling instantiation changes the referent of epsilon entirely — victims, beneficiaries, coordination function, and enforcement are re-derived — so classifications across readings are not directly comparable; only within-reading trajectories are.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one of five readings of the family_law_authority kernel; sibling readings emit different constraints.').

omega_variable(
    residual_exit_friction_location,
    'Is the remaining extraction concentrated in court-supervised dissolution friction, and is that friction inherent to enforcing long-term commitments or removable by design?',
    'Natural experiments from jurisdictions adopting streamlined or mediation-first dissolution: if dispute costs fall materially without weakening third-party reliance or support enforcement, the friction was removable overhead rather than load-bearing.',
    'Removable friction would push epsilon down toward the coordination floor and certify a purer rope; load-bearing friction raises the legitimate coordination-cost component and fixes a floor under the residual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_exit_friction_location, empirical, 'Whether dissolution friction is extractive overhead or inherent coordination cost.').

omega_variable(
    exclusion_suppression_boundary,
    'Unregistered partners forfeit the arrangement''s benefits but pay nothing into it — is their position a suppressed alternative or merely an unbundled option?',
    'Compare jurisdictions offering registered-partnership or de facto equivalents: where equivalents exist, remaining non-registration is revealed preference; where equivalents are legally barred, the boundary is actively enforced.',
    'An enforced boundary raises the suppression measure and accessibility_collapse and weakens rope certification; a revealed-preference boundary leaves the authored values standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_suppression_boundary, conceptual, 'Whether the excluded seat reflects enforcement or option structure.').

omega_variable(
    default_term_fidelity,
    'Do statutory defaults approximate the terms informed parties would have negotiated themselves, or do the defaults quietly transfer value between spouses?',
    'Compare litigated division outcomes against prenuptial baselines and observed bargaining in jurisdictions with strong marital contract freedom; systematic divergence between default outcomes and negotiated baselines exposes embedded transfer.',
    'Divergent defaults raise epsilon and push the classification toward tangled_rope territory; convergent defaults confirm the coordination characterization and the diffuse receipt finding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(default_term_fidelity, empirical, 'Whether imposed defaults track or distort what the parties would choose.').

omega_variable(
    plural_dyad_axiom_tension,
    'Restricting registration to two-person unions sits in tension with this reading''s own autonomous-consent axiom — is the dyadic limit a legitimate scoping choice or an internal inconsistency?',
    'Constitutional litigation over plural-recognition bans and legislative experiments with expanded registration forms reveal whether the limit survives scrutiny under the reading''s own premises.',
    'If the limit is inconsistent with the foundational axiom, residual suppression rises and rope certification weakens; if it is a coherent scoping choice, the dyadic form is part of the coordination design and the authored values stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plural_dyad_axiom_tension, conceptual, 'Internal-consistency tension between the consent axiom and the dyadic registration limit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__secular_contractual_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__secular_contractual_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__secular_contractual_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__secular_contractual_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__secular_contractual_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__secular_contractual_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(fami_tr_t50, family_law_authority__secular_contractual_reading, theater_ratio, 50, 0.17).
narrative_ontology:measurement(fami_tr_t60, family_law_authority__secular_contractual_reading, theater_ratio, 60, 0.18).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__secular_contractual_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fami_be_t10, family_law_authority__secular_contractual_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(fami_be_t20, family_law_authority__secular_contractual_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(fami_be_t30, family_law_authority__secular_contractual_reading, base_extractiveness, 30, 0.34).
narrative_ontology:measurement(fami_be_t40, family_law_authority__secular_contractual_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement(fami_be_t50, family_law_authority__secular_contractual_reading, base_extractiveness, 50, 0.3).
narrative_ontology:measurement(fami_be_t60, family_law_authority__secular_contractual_reading, base_extractiveness, 60, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__secular_contractual_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fami_su_t10, family_law_authority__secular_contractual_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(fami_su_t20, family_law_authority__secular_contractual_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(fami_su_t30, family_law_authority__secular_contractual_reading, suppression_requirement, 30, 0.37).
narrative_ontology:measurement(fami_su_t40, family_law_authority__secular_contractual_reading, suppression_requirement, 40, 0.34).
narrative_ontology:measurement(fami_su_t50, family_law_authority__secular_contractual_reading, suppression_requirement, 50, 0.31).
narrative_ontology:measurement(fami_su_t60, family_law_authority__secular_contractual_reading, suppression_requirement, 60, 0.29).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__secular_contractual_reading, resource_allocation).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__parsi_zoroastrian_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'marriage governance' decomposes into five structurally distinct constraints — one per reading of the family_law_authority kernel — because validity criteria, victim sets, and beneficiary structures differ irreducibly across readings; measuring one reading with another's observables would change epsilon, which marks them as separate constraints under the epsilon-invariance principle. Edges here link the siblings for contamination-propagation analysis. Within the family, the secular contractual reading functions as the upstream reform benchmark that the religious readings are structurally pressed against (equality-constitutional review, registration-only recognition in secular states), while the religious readings persist as live parallel frameworks in dual-track systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
