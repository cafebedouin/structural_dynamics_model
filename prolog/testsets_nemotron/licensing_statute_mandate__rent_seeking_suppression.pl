% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__rent_seeking_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__rent_seeking_suppression, []).

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
 *   constraint_id: licensing_statute_mandate__rent_seeking_suppression
 *   human_readable: Occupational Licensing Statutes as Rent-Seeking Suppression
 *   domain: labor_economic_regulatory
 *
 * SUMMARY:
 *   Statutory occupational licensing regimes are officially justified as
 *   public safety coordination mechanisms — ensuring minimum competence for
 *   services where consumer error is costly. This reading
 *   (rent_seeking_suppression) identifies the same statutes as a
 *   rent-extraction apparatus: incumbent practitioners and their associations
 *   capture the regulatory process to restrict supply, inflate wages, and
 *   block competition. The constraint persists through active enforcement
 *   (unauthorized practice statutes, scope-of-practice litigation, board
 *   disciplinary power) and suppresses alternatives (apprenticeship pathways,
 *   private certification, reputation markets). Beneficiaries are
 *   identifiable and organized; victims are diffuse, disadvantaged, and
 *   structurally excluded from the rulemaking venue.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, 0.72).
domain_priors:suppression_score(licensing_statute_mandate__rent_seeking_suppression, 0.85).
domain_priors:theater_ratio(licensing_statute_mandate__rent_seeking_suppression, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, extractiveness, 0.72).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__rent_seeking_suppression, snare).
narrative_ontology:human_readable(licensing_statute_mandate__rent_seeking_suppression, "Occupational Licensing Statutes as Rent-Seeking Suppression").
narrative_ontology:topic_domain(licensing_statute_mandate__rent_seeking_suppression, "labor_economic_regulatory").

domain_priors:requires_active_enforcement(licensing_statute_mandate__rent_seeking_suppression).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__rent_seeking_suppression, '9b8f5e4b-2ad2-4ce9-b320-2b57a413045a').
narrative_ontology:cs_kernel_codification('9b8f5e4b-2ad2-4ce9-b320-2b57a413045a', formalized).
narrative_ontology:cs_authority_grounding('9b8f5e4b-2ad2-4ce9-b320-2b57a413045a', extraction).
narrative_ontology:cs_interpretation_layer_present('9b8f5e4b-2ad2-4ce9-b320-2b57a413045a').
narrative_ontology:cs_reading_relation('9b8f5e4b-2ad2-4ce9-b320-2b57a413045a', licensing_statute_mandate__public_safety_coordination, forecloses).
narrative_ontology:cs_reading_relation('9b8f5e4b-2ad2-4ce9-b320-2b57a413045a', licensing_statute_mandate__graduated_access_filter, coexists_with).
narrative_ontology:cs_axiom('9b8f5e4b-2ad2-4ce9-b320-2b57a413045a', foundational, licensure_primarily_creates_artificial_scarcity).
narrative_ontology:cs_axiom_status(licensure_primarily_creates_artificial_scarcity, holdable).
narrative_ontology:cs_axiom_grounding('9b8f5e4b-2ad2-4ce9-b320-2b57a413045a', licensure_primarily_creates_artificial_scarcity, empirically_contingent).
narrative_ontology:cs_axiom('9b8f5e4b-2ad2-4ce9-b320-2b57a413045a', foundational, public_safety_justification_is_pretextual).
narrative_ontology:cs_axiom_status(public_safety_justification_is_pretextual, holdable).
narrative_ontology:cs_axiom_grounding('9b8f5e4b-2ad2-4ce9-b320-2b57a413045a', public_safety_justification_is_pretextual, empirically_contingent).
narrative_ontology:cs_reference_frame('9b8f5e4b-2ad2-4ce9-b320-2b57a413045a', statutory_competence_coordination).
narrative_ontology:cs_drift_state('9b8f5e4b-2ad2-4ce9-b320-2b57a413045a', contemporary_deregulation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9b8f5e4b-2ad2-4ce9-b320-2b57a413045a', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, professional_associations).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, licensing_boards).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, prospective_entrants).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, consumers).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, low_income_workers).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__rent_seeking_suppression, barrier_creation_as_income_support).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold valid licenses and practice in the protected occupation. Benefit from reduced competition, higher fees, and occupational closure. Can relocate across jurisdictions with reciprocal licensing agreements; their primary cost is maintaining continuing education and renewal fees.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners, beneficiary,
    organized, biographical, mobile, regional).

% Lobby for stricter licensing statutes, define scope-of-practice boundaries, and administer continuing education requirements. Capture the regulatory apparatus through board appointments and legislative influence. Collect membership dues and certification revenue. Can shift advocacy strategy across jurisdictions and policy windows.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, professional_associations, agenda_setter,
    institutional, generational, arbitrage, national).

% Administer examinations, process applications, enforce scope-of-practice rules, and discipline violators. Staffed predominantly by incumbent practitioners. Funded by license fees; budget scales with licensee population. Their institutional survival depends on the licensing regime's persistence.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, licensing_boards, agenda_setter,
    institutional, generational, analytical, regional).

% Face statutory barriers to entry: mandated education programs (often expensive and time-limited), examination fees, supervised practice hours, and moral character reviews. Excluded from legal practice until all requirements are met. Can attempt alternative occupations, relocate to less restrictive jurisdictions, or operate informally (risking enforcement). Disproportionately drawn from low-income and minority populations.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, prospective_entrants, payer,
    powerless, biographical, constrained, regional).

% Pay higher prices for services due to artificially restricted supply. Face longer wait times and reduced geographic access, especially in underserved areas. Cannot easily evaluate practitioner quality independently of the license signal. Exit options limited to foregoing care, traveling, or using unlicensed providers (with legal and safety risks).
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, consumers, payer,
    moderate, immediate, constrained, regional).

% Blocked from middle-income occupations by credential requirements that demand resources (time, money, social capital) they lack. Forced into lower-wage, unlicensed work with no pathway to licensed status. The licensing regime functions as a class barrier; exit from the constraint requires acquiring the very resources the constraint denies them.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, low_income_workers, payer,
    powerless, biographical, trapped, local).

% Consumer groups, antitrust regulators, and policy researchers who argue licensing exceeds public safety needs. Produce evidence of anticompetitive effects but face institutional barriers to legislative access. Their testimony is heard but rarely determines outcomes; the regulatory venue is structurally tilted toward incumbent interests.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, reform_advocates, excluded,
    moderate, generational, constrained, national).

% Monitor licensing regimes for anticompetitive effects under federal competition law. Can bring enforcement actions against licensing boards that exceed state authorization (per Supreme Court precedent). Their intervention is episodic and jurisdiction-specific; they do not set licensing policy.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, antitrust_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates minimum competence verification and consumer protection signaling through a standardized credential. Provides a trust infrastructure that reduces information asymmetry between service providers and consumers.
% TRANSFER_FUNCTION: Moves economic rents from prospective entrants (foregone income, credentialing costs) and consumers (higher prices, reduced access) to incumbent practitioners (higher fees, reduced competition) and professional associations (dues, certification revenue). The licensing board extracts administrative fees that scale with the licensee population.
% ABSENT_VOICES: Workers who never attempt licensure because the barriers are prohibitive — they are not in the room because the constraint filtered them out before they could organize. Unlicensed practitioners operating informally in marginalized communities, who face criminalization rather than representation. Future consumers in underserved areas who will face access gaps created by today's supply restrictions.
% DISAPPEARANCE_RATIONALE: If licensing statutes vanished overnight, incumbent practitioners would face immediate competition from previously excluded workers; prices would fall; new training and certification models would emerge (private, employer-based, reputation-based); professional associations would lose their regulatory capture revenue; licensing boards would dissolve. The labor market would reorganize around demonstrated competence rather than statutory permission.
% FOUNDING_PROBLEM: Late 19th/early 20th century: unregulated medical and trade practice led to consumer harm from incompetent practitioners; no reliable signal of minimum competence existed; fraudulent credentials were common.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship (e.g., Friedman & Kuznets on medical licensure, Kleiner on occupational licensing) documents that early licensing campaigns were led by incumbent practitioners seeking income protection, with public safety as the stated justification. State legislative records show professional associations drafting and lobbying for the statutes. Consumer advocacy groups and antitrust authorities (FTC, DOJ) corroborate that many current requirements exceed demonstrable safety needs. The founding problem (total absence of competence signaling) is substantially solved by modern information systems; the residual problem (quality variation) does not justify current barrier levels.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__rent_seeking_suppression, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__rent_seeking_suppression, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__rent_seeking_suppression, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(licensing_statute_mandate__rent_seeking_suppression, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__rent_seeking_suppression, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.72) reflects the substantial wedge between licensed and unlicensed earnings (15-30% license premium in meta-analyses) and the deadweight loss from restricted supply. Suppression (0.85) is high because the constraint's survival depends on criminalizing unlicensed practice and blocking legislative reform — not on voluntary compliance. Theater ratio (0.38) captures the real but diminishing coordination function: competence verification exists but is increasingly decoupled from the statutory barrier (e.g., continuing education requirements with no competency assessment). Accessibility collapse (0.42) is moderate: alternatives (informal practice, cross-border mobility, private certification) persist but carry legal risk or limited recognition. Resistance (0.58) is significant: FTC challenges, legislative sunset reviews, and judicial scrutiny (e.g., NCTA v. FTC) create genuine pushback, but the regime holds.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (associations, boards) experience the constraint as a legitimate coordination infrastructure they built and maintain. The payer seats (entrants, consumers, low-income workers) experience it as an enforced barrier that extracts from them. The engine computes this divergence from the structural data: identical constraint, opposite classifications across seats. The claimed_type (snare) reflects the payer-seat reality; the coordination function is real but subsidiary to extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbents and associations are structural beneficiaries (d ~ 0.15): they collect rents, control the agenda, and face mobile exit. Licensing boards are agenda-setters with analytical exit (they administer the system). Prospective entrants are full targets (d ~ 0.9): trapped by statutory barriers, identity-locked by career investment, constrained by jurisdiction. Consumers are payers with constrained exit (d ~ 0.6): they bear price/access costs but cannot individually opt out. Low-income workers are trapped (d ~ 0.95): the constraint functions as a class barrier they cannot surmount. Reform advocates are excluded; antitrust authorities are observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no competence signal) is substantially solved by modern information systems (reviews, ratings, private certification, employer verification). The arrangement persists because the beneficiaries (incumbents, associations) capture the regulatory venue and block sunset review. Mandatrophy is unresolved: the mandate has outlived its coordination function but the extraction function is vigorous. The constraint is a snare, not a piton — active enforcement and concentrated beneficiaries maintain it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_safety_pretext_vs_function,
    'Is the public safety justification a genuine coordination function that incidentally enables extraction, or a deliberate pretext constructed to legitimate rent-seeking?',
    'Counterfactual analysis: if competence verification were separated from supply restriction (e.g., mandatory certification without practice exclusivity), would incumbents support it? Historical analysis of legislative intent: did proponents oppose voluntary certification alternatives?',
    'If pretext, the constraint is pure snare with a coordination facade; if genuine coordination with captured implementation, it is tangled_rope. The classification hinges on whether the coordination function could exist independently of the extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_safety_pretext_vs_function, conceptual, 'Whether the public safety framing is pretext or genuine but captured coordination.').

omega_variable(
    consumer_harm_counterfactual,
    'Would removing licensure actually increase consumer harm, or would alternative quality signals (reputation, private certification, employer liability) adequately substitute?',
    'Natural experiments from deregulation episodes (e.g., nurse practitioner scope expansion, dental therapist authorization, occupational licensing reform in UK/US states). Compare outcomes in licensed vs. unlicensed jurisdictions for the same occupation.',
    'If consumer harm does not increase post-deregulation, the coordination function is falsified and the constraint is snare. If harm increases substantially, the coordination function is real and the constraint is at minimum tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_harm_counterfactual, empirical, 'Whether the coordination function is empirically necessary for consumer protection.').

omega_variable(
    reading_relations_kernel_structure,
    'What is the structural relationship between this rent_seeking_suppression reading and its sibling readings of the licensing_statute_mandate kernel?',
    'Committer-frame analysis: this reading''s core premise (statutes function primarily as rent extraction) logically contradicts the public_safety_coordination reading''s core premise (statutes function primarily as competence coordination) within any single institutional framework. The graduated_access_filter reading coexists as a mechanistic description of how the extraction operates.',
    'This reading forecloses public_safety_coordination within a single framework (they cannot both be the primary function). It coexists_with graduated_access_filter (different analytical lenses on the same mechanism). It influences graduated_access_filter by identifying the class-sorting mechanism as extraction infrastructure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relations_kernel_structure, conceptual, 'Structural relations among the three declared readings of the licensing_statute_mandate kernel.').

omega_variable(
    low_income_worker_identity_lock,
    'Is the trapping of low-income workers purely structural (financial/time barriers) or does it involve internalized identity exclusion (the belief that licensed occupations are ''not for people like me'')?',
    'Longitudinal studies of workers exposed to licensing information interventions: if aspirations shift without barrier removal, internalized exclusion is operative. Compare licensing awareness and occupational self-selection across demographic groups.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression after formal exit. This would amplify the snare classification for the low_income_worker seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(low_income_worker_identity_lock, empirical, 'Structural vs. internalized suppression mechanism for the most vulnerable victim seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__rent_seeking_suppression, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t1900, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(lice_tr_t1930, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 1930, 0.25).
narrative_ontology:measurement(lice_tr_t1960, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 1960, 0.3).
narrative_ontology:measurement(lice_tr_t1980, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 1980, 0.33).
narrative_ontology:measurement(lice_tr_t2000, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 2000, 0.36).
narrative_ontology:measurement(lice_tr_t2025, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(lice_be_t1900, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 1900, 0.35).
narrative_ontology:measurement(lice_be_t1930, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 1930, 0.45).
narrative_ontology:measurement(lice_be_t1960, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(lice_be_t1980, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 1980, 0.62).
narrative_ontology:measurement(lice_be_t2000, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(lice_be_t2025, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t1900, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(lice_su_t1930, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 1930, 0.68).
narrative_ontology:measurement(lice_su_t1960, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 1960, 0.75).
narrative_ontology:measurement(lice_su_t1980, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(lice_su_t2000, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 2000, 0.83).
narrative_ontology:measurement(lice_su_t2025, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__rent_seeking_suppression, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(licensing_statute_mandate__rent_seeking_suppression, 0.1).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__graduated_access_filter).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the licensing_statute_mandate kernel. This reading (rent_seeking_suppression) authors ε=0.72, snare classification, incumbents as beneficiaries, entrants/consumers as victims. The public_safety_coordination reading would author lower ε, rope/tangled_rope classification, consumers as beneficiaries. The graduated_access_filter reading would author moderate ε, tangled_rope classification, class-sorted access as primary mechanism. All three share the statutory text as kernel but instantiate different constraints with different ε, different stakeholder structures, different types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(licensing_statute_mandate__rent_seeking_suppression, organized, 0.15).
constraint_indexing:directionality_override(licensing_statute_mandate__rent_seeking_suppression, powerless, 0.9).
constraint_indexing:directionality_override(licensing_statute_mandate__rent_seeking_suppression, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
