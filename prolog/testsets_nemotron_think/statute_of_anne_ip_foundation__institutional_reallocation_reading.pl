% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__institutional_reallocation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__institutional_reallocation_reading, []).

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
 *   constraint_id: statute_of_anne_ip_foundation__institutional_reallocation_reading
 *   human_readable: Statute of Anne Institutional Reallocation (1710)
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is the founding statute of modern copyright.
 *   This reading — the institutional_reallocation_reading — frames the
 *   statute as a transfer of institutional occupancy: the Stationers'
 *   Company's perpetual monopoly over the book trade was dismantled, and the
 *   legal capacity to control literary reproduction was vested in authors,
 *   who then assigned it to publishers. The constraint is the statutory
 *   copyright system itself. The reading claims the statute's primary
 *   structural operation was reallocation of an existing institutional
 *   position (the right to control printing) from one holder (Stationers) to
 *   a new class (authors/publishers), not the creation of a wholly new
 *   conceptual category.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.48).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.55).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne Institutional Reallocation (1710)").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'b86b048c-e5e4-43eb-8121-ddb6259a6c5e').
narrative_ontology:cs_kernel_codification('b86b048c-e5e4-43eb-8121-ddb6259a6c5e', formalized).
narrative_ontology:cs_authority_grounding('b86b048c-e5e4-43eb-8121-ddb6259a6c5e', lineage).
narrative_ontology:cs_interpretation_layer_present('b86b048c-e5e4-43eb-8121-ddb6259a6c5e').
narrative_ontology:cs_reading_relation('b86b048c-e5e4-43eb-8121-ddb6259a6c5e', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('b86b048c-e5e4-43eb-8121-ddb6259a6c5e', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('b86b048c-e5e4-43eb-8121-ddb6259a6c5e', foundational, copyright_as_institutional_reallocation).
narrative_ontology:cs_axiom_status(copyright_as_institutional_reallocation, holdable).
narrative_ontology:cs_axiom_grounding('b86b048c-e5e4-43eb-8121-ddb6259a6c5e', copyright_as_institutional_reallocation, empirically_contingent).
narrative_ontology:cs_axiom('b86b048c-e5e4-43eb-8121-ddb6259a6c5e', secondary, author_rights_as_transitional_to_publisher_control).
narrative_ontology:cs_axiom_status(author_rights_as_transitional_to_publisher_control, holdable).
narrative_ontology:cs_axiom_grounding('b86b048c-e5e4-43eb-8121-ddb6259a6c5e', author_rights_as_transitional_to_publisher_control, empirically_contingent).
narrative_ontology:cs_reference_frame('b86b048c-e5e4-43eb-8121-ddb6259a6c5e', statutory_copyright_origin).
narrative_ontology:cs_drift_state('b86b048c-e5e4-43eb-8121-ddb6259a6c5e', contemporary_ip_regime, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b86b048c-e5e4-43eb-8121-ddb6259a6c5e', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, booksellers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__institutional_reallocation_reading, statutory_copyright_as_limited_monopoly).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__institutional_reallocation_reading, parliamentary_supremacy_over_common_law_monopoly).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lost its perpetual common-law monopoly over printing and book trade regulation. The Company petitioned Parliament against the bill but was overruled. It adapted by shifting to retail bookselling and using the new statutory copyright system, but its institutional role as the gatekeeper of literary property was fundamentally reallocated.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company, payer,
    institutional, generational, constrained, national).

% Gained statutory copyright via assignment from authors, becoming the primary commercial beneficiaries of the new system. The 14+14 year term with renewal created a tradeable property right that publishers could acquire, license, and enforce. They lobbied for term extensions throughout the century (culminating in the 1842 Act).
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers, beneficiary,
    organized, generational, mobile, national).

% Granted statutory copyright for the first time — a formal legal right in their works. However, the economics of publication meant most authors assigned copyright to publishers for a flat fee or royalty, retaining little ongoing control. The statute's 'encouragement of learning' rationale centered authors, but the commercial structure channeled value to publishers.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors, payer).

% Bore the cost of monopoly pricing during the copyright term. The statute promised eventual public domain entry (after 14+14 years) and price regulation clauses, but enforcement was weak. Scottish and Irish reprints provided cheaper alternatives until the 1774 Donaldson v Beckett decision extended English copyright jurisdiction.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public, payer,
    powerless, biographical, trapped, national).

% Enacted the statute (8 Anne c.19) to 'encourage learned men to compose and write useful books' by breaking the Stationers' perpetual monopoly. The preamble frames copyright as a limited regulatory tool for public learning, not perpetual property. Parliament retained the power to amend terms, which it exercised repeatedly.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, parliament, agenda_setter,
    institutional, generational, analytical, national).

% Interpreted and enforced the statute, developing copyright doctrine through cases like Millar v Taylor (1769) and Donaldson v Beckett (1774). The courts' recognition of statutory copyright over common-law copyright cemented the institutional reallocation. Judicial interpretation became the primary mechanism for adapting the statute to new technologies and commercial practices.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__institutional_reallocation_reading, courts, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaced the Stationers' perpetual monopoly with a limited-term statutory right, creating a structured system for literary property that balanced creator incentives with public access through eventual public domain entry and registration requirements.
% TRANSFER_FUNCTION: Moves exclusive printing rights from the Stationers' Company monopoly to authors (who typically assign to publishers), creating a new chain of title from author→publisher→public. The statute transfers the legal capacity to control reproduction from a trade guild to individual creators, who then transfer it commercially to publishers.
% ABSENT_VOICES: The reading public and aspiring authors outside the London book trade had no voice in the statute's drafting; Scottish and Irish publishers were excluded from the English monopoly structure and later contested its territorial scope; women writers, though growing in number, were structurally marginalized in the assignment economy.
% DISAPPEARANCE_RATIONALE: Without the Statute of Anne, the Stationers' perpetual common-law monopoly would have continued, or a different statutory framework would have emerged. The specific terms (14+14 years, registration, public domain dedication, parliamentary supremacy over common law) shaped all subsequent Anglo-American copyright and the global IP system that descended from it.
% FOUNDING_PROBLEM: The Stationers' Company's perpetual monopoly over printing suppressed competition, limited public access to books, and failed to incentivize new learning. Parliament sought to encourage learned men to compose and write useful books by granting them a limited exclusive right, breaking the Stationers' control while creating a regulated market.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary records and the statute's preamble ('for the Encouragement of Learning') corroborate the learning-incentive purpose. Contemporary pamphlets (e.g., 'The Case of the Booksellers') and the Stationers' own petitions corroborate the monopoly-breaking purpose. However, publishers' subsequent lobbying for term extension (1735, 1774, 1814, 1842) and the 1774 Donaldson v Beckett decision show the founding problem was redefined from 'breaking monopoly' to 'securing property' — a shift attested by legal historians (Rose, Patterson, Deazley) outside the benefiting publisher class.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__institutional_reallocation_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__institutional_reallocation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects that the system extracts from the reading public via monopoly pricing while delivering coordination (registration, limited term, public domain). Suppression (0.55) is moderate: enforcement targeted unlicensed printers and Scottish/Irish reprints, but alternatives persisted (piracy, foreign reprints, manuscript circulation). Theater ratio (0.22) is low initially — the statute's learning rationale was genuine — but rises as publisher lobbying reframes copyright as natural property. Accessibility collapse (0.45) is partial: the public domain and fair abridgment doctrines preserved some alternatives. Resistance (0.52) is significant: the Stationers resisted, Scottish booksellers litigated, and public petitions opposed term extensions.
 *
 * PERSPECTIVAL GAP:
 *   From Parliament's seat, the constraint is a Rope: genuine coordination solving a collective-action problem (monopoly suppression + learning incentive). From the Stationers' seat, it is a Snare: their extraction mechanism destroyed by state action. From publishers' seat, it becomes a Tangled Rope over time: coordination (functional copyright system) + extraction (rent capture via assignment and term extension). From authors' seat, it is a Scaffold that never sunsets: transitional support (statutory right) that becomes permanent structure they don't control. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Stationers' Company: former monopolist, now payer (loses perpetual control, must adapt — d near target end). Publishers: primary beneficiaries (acquire statutory rights via assignment, lobby for expansion — d near beneficiary end). Authors: dual-positioned — formal beneficiaries (granted rights) but functional payers (assign rights for flat fees, constrained exit — d near symmetric). Reading public: payers (monopoly pricing, trapped during term — d near target end). Parliament: agenda-setter (enacts, amends, oversees — d near analytical). Courts: observer/agenda-setter (interpret, develop doctrine — d analytical). The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The statute's founding mandate — 'encouragement of learning' via limited monopoly — was genuine coordination. But the mandate atrophied as publishers captured the system: term extensions (1735, 1814, 1842), erosion of registration/public domain mechanisms, and judicial expansion of scope shifted the constraint toward extraction. The mandatrophy is not resolved; the coordination function (learning incentive) persists but is overlaid with extractive layers. The constraint is a Tangled Rope because both functions operate simultaneously: the public domain and term limits still coordinate, while publisher rents extract.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the Statute of Anne primarily an institutional reallocation (moving existing rights to new holders), a conceptual emergence (creating copyright as a new regulatory category), or an entangled event (both simultaneously and inseparably)?',
    'Historical analysis of parliamentary debates, the statute''s text vs. the Stationers'' prior claims, and the self-understanding of contemporary actors (authors, publishers, Stationers). If the statute''s language and legislative history show deliberate transfer of the Stationers'' claimed perpetual right to authors, institutional reallocation is supported. If it shows a novel ''limited monopoly for learning'' concept without reference to prior rights, conceptual emergence is supported.',
    'If institutional reallocation: the constraint''s ε reflects transfer of extractive capacity from one institutional actor to another (publishers). If conceptual emergence: ε reflects the cost of a new coordination mechanism. If entangled: ε is irreducible to either dimension alone. This reading authors ε for the reallocation frame only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Commitment-frame framing ambiguity: which structural description of the statute is primary.').

omega_variable(
    author_rights_transitional,
    'Were authors'' statutory rights a meaningful shift in bargaining power, or merely a transitional form that publishers immediately captured via assignment?',
    'Economic history of author-publisher contracts 1710-1800: prevalence of copyright assignment vs. license, royalty rates, author retention of rights. If most authors assigned copyright outright for flat fees, the ''author right'' was a conduit to publisher capture. If significant numbers retained rights or negotiated royalties, authors gained independent leverage.',
    'If transitional conduit: the constraint''s beneficiary is publishers (via assignment), authors are functionally payers, and extraction is higher. If meaningful author power: authors are genuine beneficiaries, extraction is lower, and the coordination function is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(author_rights_transitional, empirical, 'Whether the statute''s vesting in authors was substantive or formal.').

omega_variable(
    stationers_monopoly_as_extractive_baseline,
    'Should the Stationers'' Company''s pre-1710 perpetual monopoly be treated as the extractive baseline against which the statute''s extractiveness is measured?',
    'Comparative analysis of book prices, output, and access under the Stationers'' regime (1662-1710) vs. the statutory regime (1710-1800). If the statute reduced prices/increased access, its net extraction is lower than the baseline. If it maintained similar prices but shifted rents to publishers, extraction is reallocated not reduced.',
    'If the baseline was highly extractive, the statute''s ε (0.48) represents a reduction. If the baseline was a coordination mechanism (quality control, apprentice training), the statute''s ε represents new extraction. This reading treats the Stationers'' monopoly as extractive but does not quantify it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stationers_monopoly_as_extractive_baseline, empirical, 'Baseline selection for ε measurement: what is the counterfactual?').

omega_variable(
    suppression_mechanism_institutional,
    'Is the statute''s suppression structural (legal penalties, customs enforcement, judicial doctrine) or does it include internalized acceptance of copyright as legitimate property?',
    'Historical analysis of piracy rates, public attitudes toward ''literary property'' vs. ''piracy'', and the evolution of moral rhetoric around authors'' rights. If suppression persists after legal enforcement weakens (e.g., social stigma against piracy), internalized component is significant.',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than legal penalties alone suggest — the public enforces the constraint on itself. This reading measures structural suppression (0.55) but flags the internalized dimension as open.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_institutional, conceptual, 'Structural vs. internalized suppression in an institutional constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t20, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(stat_tr_t40, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(stat_tr_t60, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(stat_tr_t80, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement(stat_tr_t100, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 100, 0.32).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stat_be_t20, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(stat_be_t40, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(stat_be_t60, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(stat_be_t80, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(stat_be_t100, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(stat_su_t20, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(stat_su_t40, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(stat_su_t60, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(stat_su_t80, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 80, 0.62).
narrative_ontology:measurement(stat_su_t100, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__institutional_reallocation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.12).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the Statute of Anne kernel into three readings: institutional_reallocation (this story), conceptual_emergence, and entangled_event. The ε values differ: reallocation reading shows moderate extraction (publisher capture via assignment); emergence reading shows lower extraction (coordination for learning); entangled reading shows irreducible hybrid ε. The upstream conceptual_emergence_reading (lower ε, Mountain-adjacent) influences the downstream institutional_reallocation_reading (higher ε, Tangled Rope) because the 'learning' rationale is cited to justify the reallocation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statute_of_anne_ip_foundation__institutional_reallocation_reading, institutional, 0.15).
constraint_indexing:directionality_override(statute_of_anne_ip_foundation__institutional_reallocation_reading, organized, 0.1).
constraint_indexing:directionality_override(statute_of_anne_ip_foundation__institutional_reallocation_reading, moderate, 0.55).
constraint_indexing:directionality_override(statute_of_anne_ip_foundation__institutional_reallocation_reading, powerless, 0.85).
constraint_indexing:directionality_override(statute_of_anne_ip_foundation__institutional_reallocation_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
