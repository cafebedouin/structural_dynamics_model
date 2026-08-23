% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__conceptual_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__conceptual_emergence_reading, []).

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
 *   constraint_id: statute_of_anne_ip_foundation__conceptual_emergence_reading
 *   human_readable: Statute of Anne: Copyright as Limited Regulatory Tool for Learning (Conceptual Emergence Reading)
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) created the first modern copyright statute,
 *   establishing copyright as a limited regulatory tool 'for the
 *   encouragement of learning' rather than perpetual property. This reading
 *   (conceptual_emergence_reading) treats the statute as the moment IP
 *   'became thinkable' as a distinct legal category — a new conceptual space
 *   where copyright is a state-granted, time-limited regulatory instrument
 *   serving public learning, not a natural property right of authors or
 *   publishers. The beneficiary is public learning (the reading public,
 *   education, future creators); the victim is the perpetual monopoly (the
 *   Stationers' Company's guild control and the booksellers' claim to
 *   perpetual copyright). This constraint is transitional by design: the
 *   14+14 year term with mandatory public domain reversion is a sunset clause
 *   built into the statute's architecture.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.12).
domain_priors:suppression_score(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.15).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__conceptual_emergence_reading, scaffold).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__conceptual_emergence_reading, "Statute of Anne: Copyright as Limited Regulatory Tool for Learning (Conceptual Emergence Reading)").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__conceptual_emergence_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:has_sunset_clause(statute_of_anne_ip_foundation__conceptual_emergence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'f24841b3-3299-424b-84a1-1ee34b6987f8').
narrative_ontology:cs_kernel_codification('f24841b3-3299-424b-84a1-1ee34b6987f8', formalized).
narrative_ontology:cs_authority_grounding('f24841b3-3299-424b-84a1-1ee34b6987f8', lineage).
narrative_ontology:cs_interpretation_layer_present('f24841b3-3299-424b-84a1-1ee34b6987f8').
narrative_ontology:cs_reading_relation('f24841b3-3299-424b-84a1-1ee34b6987f8', statute_of_anne_ip_foundation__institutional_reallocation_reading, forecloses).
narrative_ontology:cs_reading_relation('f24841b3-3299-424b-84a1-1ee34b6987f8', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('f24841b3-3299-424b-84a1-1ee34b6987f8', foundational, copyright_as_limited_regulatory_tool_for_learning).
narrative_ontology:cs_axiom_status(copyright_as_limited_regulatory_tool_for_learning, holdable).
narrative_ontology:cs_axiom_grounding('f24841b3-3299-424b-84a1-1ee34b6987f8', copyright_as_limited_regulatory_tool_for_learning, conventional).
narrative_ontology:cs_axiom('f24841b3-3299-424b-84a1-1ee34b6987f8', foundational, public_domain_as_constitutive_not_residual).
narrative_ontology:cs_axiom_status(public_domain_as_constitutive_not_residual, holdable).
narrative_ontology:cs_axiom_grounding('f24841b3-3299-424b-84a1-1ee34b6987f8', public_domain_as_constitutive_not_residual, conventional).
narrative_ontology:cs_reference_frame('f24841b3-3299-424b-84a1-1ee34b6987f8', statute_of_anne_1710_limited_term_framework).
narrative_ontology:cs_drift_state('f24841b3-3299-424b-84a1-1ee34b6987f8', donaldson_v_becket_1774, gap(authority_erosion, minor, true)).
narrative_ontology:cs_created_at('f24841b3-3299-424b-84a1-1ee34b6987f8', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors_as_creators).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, perpetual_monopoly_holders).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company_monopoly).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, copyright_as_limited_regulatory_tool).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, learning_not_property_paradigm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the Statute of Anne (1710) establishing copyright as a limited-term regulatory grant to authors for the encouragement of learning, with a 14+14 year term and mandatory registration. Created the statutory framework that replaced the Stationers' perpetual monopoly.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, parliament_1710, agenda_setter,
    institutional, generational, analytical, national).

% The intended beneficiary of the 'encouragement of learning' purpose — the reading public, educational institutions, and future creators who gain access to works entering the public domain after the limited term. Has no organized voice in the legislative process and no exit from the regime.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning, beneficiary,
    powerless, generational, trapped, national).

% Gained statutory recognition of their rights as creators rather than as employees of the Stationers' Company. Received a limited-term property right they could assign or license. Their exit was constrained by the lack of alternative distribution channels.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors_as_creators, beneficiary,
    moderate, biographical, constrained, national).

% Lost its perpetual monopoly over printing and publishing enforced through royal charter and internal regulation. The Stationers' Company had controlled the trade through entry restrictions and perpetual copyright claims; the statute transferred the statutory anchor to authors with a time limit.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company_monopoly, payer,
    organized, generational, constrained, national).

% Booksellers and publishers who had benefited from the Stationers' perpetual copyright system. They lost the ability to hold works indefinitely without statutory limit. Their exit options were constrained by the new statutory framework but they retained significant market power and lobbying capacity.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, perpetual_monopoly_holders, payer,
    powerful, biographical, constrained, national).

% Contemporary jurists (e.g., Blackstone, later Lord Mansfield) who interpreted the statute's purpose and scope. Their readings shaped the early case law (Millar v. Taylor, Donaldson v. Beckett) that determined whether copyright was a common law perpetual right or purely statutory and limited.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, legal_scholars_18th_century, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Created a legally recognized, limited-duration property right in literary works that replaced the Stationers' Company's perpetual monopoly with a statutory grant to authors, establishing a public domain terminus and enabling a regulated market for printed works.
% TRANSFER_FUNCTION: Transferred the statutory anchor of printing rights from the Stationers' Company (perpetual, corporate monopoly) to authors (limited-term, individual statutory grant) with a mandatory public domain reversion after 28 years, moving control from a trade guild to creators and ultimately the public.
% ABSENT_VOICES: The reading public and educational institutions — the nominal 'learning' beneficiaries — had no organized representation in Parliament. Scottish publishers and universities (outside English jurisdiction) were excluded from the legislative process but directly affected by the subsequent litigation over common law copyright.
% DISAPPEARANCE_RATIONALE: If the Statute of Anne's limited-term framework vanished overnight, the Stationers' perpetual monopoly would have persisted by default (common law copyright claims), authors would lack statutory standing independent of the Company, and the public domain terminus that enabled the 18th-century expansion of affordable reprints would not exist. The entire trajectory of Anglo-American copyright — including the constitutional 'limited times' clause — reorganizes around this statutory anchor.
% FOUNDING_PROBLEM: The Stationers' Company's perpetual copyright monopoly (enforced through royal charter and internal regulation since 1662) blocked competition, kept prices high, prevented works from entering the public domain, and gave authors no independent rights — the 'encouragement of learning' was stifled by a trade guild's perpetual control over the book trade.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary parliamentary debates (1709-1710) record the explicit purpose: 'for the encouragement of learning' by vesting copies in authors for limited times. The Stationers' Company's own petitions against the bill confirm the monopoly's existence. Independent corroboration: the 1774 Donaldson v. Beckett decision (House of Lords) confirmed copyright was purely statutory and limited — the founding problem (perpetual monopoly blocking learning) was solved by the statute's limited-term design, though later extensions recreated the problem in new form.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__conceptual_emergence_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__conceptual_emergence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).
:- end_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12 at interval end) because the statute's primary operation is creating a new coordination space (author's statutory right + public domain terminus) rather than extracting from a trapped population. The Stationers' monopoly was the extractive baseline; the statute reduced extraction by imposing a hard term limit. Suppression is low (0.15) because the constraint's persistence does not depend on crushing alternatives — it creates a new legal category that did not exist. Theater ratio is minimal (0.05) because the 'encouragement of learning' purpose was genuinely operational in the early decades (affordable reprints flourished post-1774). Accessibility collapse is moderate-high (0.75) because once the limited-term statutory framework is established, the perpetual monopoly alternative becomes legally incoherent — the conceptual space has a point. Resistance is moderate (0.30) from booksellers litigating for common law perpetual copyright (Millar v. Taylor 1769), overcome in Donaldson v. Beckett (1774).
 *
 * PERSPECTIVAL GAP:
 *   From the Stationers' Company seat, the statute is a Snare (confiscation of perpetual property). From the public learning seat, it is a Scaffold (transitional coordination with a sunset). From the author seat, it is a Rope (genuine coordination gain with limited extraction). From the parliamentary seat, it is a Mountain (constitutional settlement of a new category). The engine computes these divergences from the structural data; the claimed_type 'scaffold' reflects the statute's own self-declared transitional architecture.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament (agenda_setter) sits at the analytical/beneficiary boundary: it created the framework and gained no direct revenue. Public learning (beneficiary, powerless, trapped) is the nominal purpose — d near 0.0. Authors as creators (beneficiary, moderate, constrained) gained statutory standing but limited leverage — d ~0.2. Stationers' monopoly (payer, organized, constrained) lost its perpetual guild control — d ~0.8. Perpetual monopoly holders (payer, powerful, constrained) lost indefinite holdings but retained market power — d ~0.7. Legal scholars (observer, analytical) interpret from outside.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (perpetual monopoly blocking learning) was SOLVED by the statute's limited-term design — confirmed by Donaldson v. Beckett (1774) and the ensuing explosion of affordable reprints. Yet the arrangement persisted and expanded (term extensions, scope expansions) long after the founding problem was dead. This is classic mandatrophy: the coordination function (limited term → public domain) was the justification; the persistent constraint (ever-expanding copyright) serves new beneficiaries (corporate rights holders) while the original beneficiary (public learning) is progressively excluded. The scaffold's sunset clause (28-year term) was the mandate's expiration; its non-observance is the mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_institutional_primacy,
    'Does the Statute of Anne primarily create a new conceptual category (copyright as limited regulatory tool) or primarily reallocate an existing institutional position (from Stationers to authors)?',
    'Analyze the statutory text''s novelty: does it deploy concepts (limited term, public domain, author as proprietor, ''encouragement of learning'') that have no precedent in the Stationers'' regime, or does it merely reassign the same perpetual right to a different holder?',
    'If conceptual primacy, this reading''s claimed_type (scaffold with genuine novelty) stands; if institutional primacy, the institutional_reallocation_reading''s claim (no new category, just new occupant) gains force and this reading''s extraction/suppression metrics may be misattributed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_vs_institutional_primacy, conceptual, 'Whether the statute''s primary structural move is conceptual innovation or institutional reassignment.').

omega_variable(
    learning_beneficiary_operationalization,
    'Was ''encouragement of learning'' an operational constraint on the statute''s administration (e.g., term limits actually enforced, public domain actually functional) or a rhetorical cover for author/publisher interests?',
    'Examine early enforcement: were 28-year terms actually respected? Did works enter the public domain? Did affordable reprints appear post-1774? Compare with later extensions (1842, 1911, 1956, 1988) where ''learning'' rhetoric persisted while terms lengthened.',
    'If operational, the scaffold''s sunset clause is genuine and the low extractiveness is structural; if rhetorical, the statute is a snare from inception and the claimed_type is false.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(learning_beneficiary_operationalization, empirical, 'Whether the statute''s declared purpose was functionally binding or merely decorative.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the conceptual_emergence_reading logically foreclose the institutional_reallocation_reading within a single commitment framework, or do they coexist as descriptions of different dimensions of the same event?',
    'Test whether a single theoretical framework can hold both: ''the statute created a new conceptual category AND merely reallocated institutional positions.'' If the conceptual category IS the reallocation (the new concept just IS the new institutional form), they coexist. If the new concept makes the old institutional form unintelligible, foreclosure obtains.',
    'Foreclosure would mean the kernel has mutually exclusive readings (commitment system fracture); coexistence means the kernel supports multiple stable readings (pluralistic commitment system). This determines the cs_structure.reading_relations assignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Logical relationship between the conceptual emergence and institutional reallocation readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__conceptual_emergence_reading, 1710, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1710, 0.02).
narrative_ontology:measurement(stat_tr_t1725, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1725, 0.03).
narrative_ontology:measurement(stat_tr_t1740, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1740, 0.04).
narrative_ontology:measurement(stat_tr_t1755, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1755, 0.05).
narrative_ontology:measurement(stat_tr_t1774, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1774, 0.05).

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1710, 0.05).
narrative_ontology:measurement(stat_be_t1725, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1725, 0.08).
narrative_ontology:measurement(stat_be_t1740, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1740, 0.1).
narrative_ontology:measurement(stat_be_t1755, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1755, 0.11).
narrative_ontology:measurement(stat_be_t1774, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1774, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1710, 0.1).
narrative_ontology:measurement(stat_su_t1725, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1725, 0.12).
narrative_ontology:measurement(stat_su_t1740, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1740, 0.13).
narrative_ontology:measurement(stat_su_t1755, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1755, 0.14).
narrative_ontology:measurement(stat_su_t1774, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1774, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__conceptual_emergence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.02).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__institutional_reallocation_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__entangled_event_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, us_constitutional_copyright_clause).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, donaldson_v_becket_precedent).

% DUAL FORMULATION NOTE:
% This reading (conceptual_emergence) and institutional_reallocation_reading decompose the Statute of Anne kernel into two constraints with different ε values: this reading ε≈0.12 (genuine coordination, limited extraction); institutional_reallocation ε≈0.35 (reallocation with residual extraction from public domain). The entangled_event_reading sits upstream as the undifferentiated event. Network edges flow from entangled_event → conceptual_emergence and entangled_event → institutional_reallocation; conceptual_emergence influences later copyright development (US clause, Donaldson precedent).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statute_of_anne_ip_foundation__conceptual_emergence_reading, organized, 0.75).
constraint_indexing:directionality_override(statute_of_anne_ip_foundation__conceptual_emergence_reading, powerful, 0.7).
constraint_indexing:directionality_override(statute_of_anne_ip_foundation__conceptual_emergence_reading, powerless, 0.05).
constraint_indexing:directionality_override(statute_of_anne_ip_foundation__conceptual_emergence_reading, moderate, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
