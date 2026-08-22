% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__institutional_reallocation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Statute of Anne (1710) — Institutional Reallocation Reading
 *   domain: legal/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is conventionally described as 'the first
 *   copyright act' creating a new authorial property right. This reading —
 *   the institutional reallocation reading — holds that the statute did not
 *   create a new conceptual category of property but reallocated an existing
 *   monopoly franchise from the Stationers' Company (a London livery company
 *   with a royal charter granting perpetual control over printing) to a new
 *   coalition: Parliament (as grantor), authors (as nominal initial owners),
 *   and publishers (as practical assignees and enforcers). The coordination
 *   function is real — the old monopoly was collapsing — but the extraction
 *   is asymmetric: publishers captured the commercial value of the new
 *   statutory right while authors received a theoretical title they could not
 *   exercise without publisher intermediation. The Stationers' Company,
 *   stripped of its perpetual franchise, became the victim of its own
 *   members' legislative maneuver.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.78).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.82).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne (1710) — Institutional Reallocation Reading").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'c03ed76e-d4a5-4475-9945-dd3fedd5c40f').
narrative_ontology:cs_kernel_codification('c03ed76e-d4a5-4475-9945-dd3fedd5c40f', formalized).
narrative_ontology:cs_authority_grounding('c03ed76e-d4a5-4475-9945-dd3fedd5c40f', lineage).
narrative_ontology:cs_interpretation_layer_present('c03ed76e-d4a5-4475-9945-dd3fedd5c40f').
narrative_ontology:cs_reading_relation('c03ed76e-d4a5-4475-9945-dd3fedd5c40f', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('c03ed76e-d4a5-4475-9945-dd3fedd5c40f', statute_of_anne_ip_foundation__entangled_event_reading, influences).
narrative_ontology:cs_axiom('c03ed76e-d4a5-4475-9945-dd3fedd5c40f', foundational, copyright_is_legislative_reallocation_not_creation).
narrative_ontology:cs_axiom_status(copyright_is_legislative_reallocation_not_creation, holdable).
narrative_ontology:cs_axiom_grounding('c03ed76e-d4a5-4475-9945-dd3fedd5c40f', copyright_is_legislative_reallocation_not_creation, conventional).
narrative_ontology:cs_axiom('c03ed76e-d4a5-4475-9945-dd3fedd5c40f', foundational, author_is_legislative_fiction_for_publisher_assignment).
narrative_ontology:cs_axiom_status(author_is_legislative_fiction_for_publisher_assignment, holdable).
narrative_ontology:cs_axiom_grounding('c03ed76e-d4a5-4475-9945-dd3fedd5c40f', author_is_legislative_fiction_for_publisher_assignment, empirically_contingent).
narrative_ontology:cs_reference_frame('c03ed76e-d4a5-4475-9945-dd3fedd5c40f', stationers_perpetual_monopoly_franchise).
narrative_ontology:cs_drift_state('c03ed76e-d4a5-4475-9945-dd3fedd5c40f', donaldson_v_beckett_1774, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c03ed76e-d4a5-4475-9945-dd3fedd5c40f', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, london_publishers_cartel).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, parliamentary_booksellers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_monopoly).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, author_assignees_pre_anne).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, scottish_and_provincial_booksellers).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__institutional_reallocation_reading, copyright_as_transferable_commercial_asset).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__institutional_reallocation_reading, parliamentary_authority_over_monopoly_grants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the statute to dismantle the Stationers' perpetual monopoly and create a regulated copyright term (14+14 years) vesting initially in authors. The statute was drafted and passed by a coalition of Whig MPs and independent booksellers seeking to break the Stationers' control over the book trade.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, parliament_1710, agenda_setter,
    institutional, generational, analytical, national).

% The coalition of wholesale booksellers and publishers who lobbied for the statute. They gained the ability to acquire copyright from authors by assignment and enforce it against the Stationers' Company and pirate printers. The statute's limited term and registration requirements created a tradeable commercial asset they could exploit more efficiently than the old perpetual monopoly.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, london_publishers_cartel, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__institutional_reallocation_reading, london_publishers_cartel, agenda_setter).

% Lost its perpetual common-law copyright monopoly and its control over the Register of Copies. The statute stripped the Company of its enforcement power over unregistered works and limited copyright to a statutory term. The Company resisted through litigation (Millar v. Taylor, Donaldson v. Beckett) but ultimately lost its institutional franchise.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_monopoly, payer,
    organized, biographical, constrained, national).

% Authors and their assignees who had operated under the Stationers' system. The statute nominally vested copyright in authors, but the commercial reality forced assignment to publishers on publisher-dictated terms. Authors gained a theoretical property right they could not practically exercise without publisher intermediation.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, author_assignees_pre_anne, payer,
    moderate, biographical, constrained, national).

% Gained legal standing to reprint works whose copyright term had expired or which were never registered, breaking the London-centric monopoly. The statute's formalities (registration, deposit at Stationers' Hall, delivery to universities) created compliance costs they could meet more easily than the old perpetual monopoly's opaque enforcement.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, scottish_and_provincial_booksellers, beneficiary,
    moderate, biographical, mobile, national).

% Printers in Scotland, Ireland, and the provinces who operated outside the Stationers' monopoly before 1710 and continued after. The statute gave them a statutory defense for works outside the 14+14 term or non-compliant with formalities, but they faced continued litigation from London publishers until Donaldson v. Beckett (1774) settled the term question.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, pirate_printers_1710_1774, excluded,
    moderate, immediate, trapped, national).

% Modern scholars who read the statute as either (a) creating a new authorial property right (conceptual emergence), (b) merely reallocating existing monopoly rights from Stationers to publishers (institutional reallocation), or (c) an entangled event doing both simultaneously. This reading is the institutional reallocation position.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, legal_historians_20th_c, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaced an unenforceable perpetual monopoly (Stationers' common-law copyright) with a limited statutory term (14+14 years) requiring registration and deposit, creating a definable, tradeable, and enforceable commercial asset for the book trade.
% TRANSFER_FUNCTION: Moves the legal title to the exclusive right of printing from the Stationers' Company (as collective monopoly holder) to individual authors as initial statutory owners, who in practice assign it to publishers for commercial exploitation. The Stationers lose their perpetual franchise; publishers gain a finite, renewable, assignment-based right.
% ABSENT_VOICES: Authors themselves — the statute's nominal beneficiaries — had no organized representation in Parliament. The 'author' in the statute is a legal fiction enabling publisher assignment; actual writers (Swift, Pope, Defoe) negotiated individually with publishers and did not shape the legislation. Their absence is structural: the statute creates a right authors cannot exercise without the very publishers who wrote the bill.
% DISAPPEARANCE_RATIONALE: If the statute vanished overnight in 1710, the Stationers' perpetual common-law monopoly would persist, the book trade would remain organized around the Register of Copies, and the limited-term, registration-based copyright system that enabled the 18th-century expansion of British publishing would not exist. The modern copyright system descends from this statutory framework, not from the common-law monopoly it displaced.
% FOUNDING_PROBLEM: The Stationers' Company's perpetual monopoly had become unenforceable against Scottish and Irish reprints, provincial piracy, and internal defections. The Company's own members (the wholesale booksellers) wanted a limited, enforceable statutory right they could trade and litigate more reliably than the collapsing common-law claim.
% FOUNDING_PROBLEM_CORROBORATION: The Stationers' Company's own minutes (1707-1709) record the decision to petition Parliament for a statutory replacement because the common-law monopoly was failing. The Whig booksellers' correspondence (John Baskett, Jacob Tonson the younger) confirms they drafted the bill's commercial terms. No corroboration from author-organizations exists because none existed — the 'author' was a legislative device, not a constituency.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__institutional_reallocation_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__institutional_reallocation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.78) is high because the statute's commercial operation transferred the economic surplus from a collective monopoly (Stationers' Company) to a concentrated publisher cartel that controlled assignment terms. Suppression (0.82) is high because the constraint's persistence required active enforcement: registration formalities, deposit requirements, litigation against Scottish/Irish reprints, and the 64-year legal battle culminating in Donaldson v. Beckett (1774). Theater ratio (0.18) is low because the statute's coordination function (limited term, registration, definable asset) was genuinely operational — the extraction rode on a real mechanism, not a pure performance. The metric trajectory shows extraction and suppression rising together as publishers consolidated control over the statutory framework, while theater remained low because the coordination function never disappeared.
 *
 * PERSPECTIVAL GAP:
 *   From the publisher seat, the statute is a genuine coordination mechanism (rope-like): it solved the enforcement collapse of the old monopoly and created a tradeable asset. From the Stationers' seat, it is extraction (snare-like): their franchise was taken by legislative fiat. From the author seat, it is a tangled rope: they gained a legal title (coordination) but could only realize its value through publisher assignment on publisher terms (extraction). The engine computes this divergence from the structural data — the authored claim (tangled_rope) reflects the dominant structural reading across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament (agenda_setter, institutional power, analytical exit) sits near the beneficiary end — it gained regulatory authority over the book trade. London publishers (beneficiary/agenda_setter, powerful, mobile) are the primary extractors — they wrote the commercial terms and captured the assignment value. The Stationers' Company (payer, organized, constrained) is the primary target — its perpetual franchise was legislatively extinguished. Authors (payer, moderate, constrained) are secondary targets — they hold nominal title but lack practical exit from publisher assignment. Scottish/provincial booksellers (beneficiary, moderate, mobile) gained competitive space from the statute's formalities. Pirate printers (excluded, moderate, trapped) were the enforcement object — their exclusion maintained the publishers' cartel. Legal historians (observer, analytical) see the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Stationers' monopoly collapse) was dead by 1774 — the statutory system had fully replaced it. Yet the constraint persisted and expanded (term extensions, international treaties, corporate authorship). The mandate (limited term for learning) was overridden by commercial interests who captured the statutory mechanism. The 'author' fiction persists as a legitimating cover for publisher extraction. This is classic mandatrophy: the arrangement outlived its founding problem and was captured by its beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the institutional_reallocation_reading a distinct constraint from the conceptual_emergence_reading and entangled_event_reading, or are they observables of the same constraint?',
    'Apply the ε-invariance test: if measuring the statute''s operation via ''authorial property creation'' yields low extraction but measuring via ''publisher cartel enrichment'' yields high extraction, they are different constraints. This reading authors high extraction (0.78) — the sibling readings would author different ε values for the same statutory text.',
    'If the readings are distinct constraints, each gets its own classification and the kernel is a family linked by network.affects_constraints. If they are one constraint, the framework must model observable-dependent classification (which it rejects by design).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel decomposes into multiple ε-invariant constraints or one constraint with measurement-dependent classification.').

omega_variable(
    author_fiction_vs_reality,
    'Does the statute''s vesting of copyright in ''authors'' create a genuine economic position for writers, or is ''author'' a legislative fiction that only publishers can monetize?',
    'Examine 1710-1774 author-publisher contracts: did any authors retain copyright, negotiate terms, or exploit the statutory term independently? The historical record (Sherbo, Rose, Feather) shows near-universal assignment on publisher terms.',
    'If ''author'' is a pure fiction, the statute''s coordination function is entirely publisher-facing — the beneficiary declaration is structurally accurate. If authors occasionally exercised independent rights, the constraint has a genuine author-beneficiary seat that this reading understates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(author_fiction_vs_reality, empirical, 'Whether the statute''s nominal beneficiary (authors) is a real economic seat or a legislative cover for publisher extraction.').

omega_variable(
    stationers_continuity,
    'Did the Stationers'' Company actually lose its monopoly, or did it transform into the publishers'' cartel via overlapping membership?',
    'Trace Stationers'' Court membership (1710-1774) against the London publishers'' cartel membership. The Company continued as a regulatory body (registering copyrights, collecting fees) while its wholesale bookseller members became the statutory copyright holders.',
    'If the Stationers transformed into the cartel, the victim/beneficiary distinction blurs — the same agents lost a perpetual monopoly and gained a statutory cartel. The extraction would be intra-group redistribution, not inter-group transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stationers_continuity, empirical, 'Whether the Stationers'' Company and the publisher cartel are distinct agent sets or overlapping populations with a role shift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 1710, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(statute_anne_inst_realloc_tr_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1710, 0.08).
narrative_ontology:measurement(statute_anne_inst_realloc_tr_t1725, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1725, 0.12).
narrative_ontology:measurement(statute_anne_inst_realloc_tr_t1740, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1740, 0.15).
narrative_ontology:measurement(statute_anne_inst_realloc_tr_t1755, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1755, 0.17).
narrative_ontology:measurement(statute_anne_inst_realloc_tr_t1774, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1774, 0.18).

% Extraction over time
narrative_ontology:measurement(statute_anne_inst_realloc_be_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1710, 0.35).
narrative_ontology:measurement(statute_anne_inst_realloc_be_t1725, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1725, 0.52).
narrative_ontology:measurement(statute_anne_inst_realloc_be_t1740, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1740, 0.65).
narrative_ontology:measurement(statute_anne_inst_realloc_be_t1755, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1755, 0.72).
narrative_ontology:measurement(statute_anne_inst_realloc_be_t1774, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1774, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(statute_anne_inst_realloc_su_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1710, 0.45).
narrative_ontology:measurement(statute_anne_inst_realloc_su_t1725, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1725, 0.58).
narrative_ontology:measurement(statute_anne_inst_realloc_su_t1740, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1740, 0.68).
narrative_ontology:measurement(statute_anne_inst_realloc_su_t1755, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1755, 0.76).
narrative_ontology:measurement(statute_anne_inst_realloc_su_t1774, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1774, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__institutional_reallocation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the statute_of_anne_ip_foundation kernel. The conceptual_emergence_reading authors low extraction (genuine coordination innovation); the entangled_event_reading authors medium extraction (inseparable conceptual+institutional change). This reading authors high extraction (institutional reallocation benefiting publishers). The three readings form a constraint family linked by network.affects_constraints. The ε-invariance principle requires separate stories because the statutory text's structural operation differs depending on which dimension (conceptual vs institutional) is the referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
