% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__first_holding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__first_holding_reading, []).

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
 *   constraint_id: ip_category_emergence__first_holding_reading
 *   human_readable: Statute of Anne's First Holding of Authorial Rights
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This constraint story, 'ip_category_emergence__first_holding_reading',
 *   interprets the Statute of Anne (1710) as the moment when the author, as a
 *   distinct rights-holder, entered the legitimate claimant set for
 *   intellectual property. It marks a shift from a system dominated by
 *   printers' monopolies to one where authors held statutory, albeit
 *   time-limited, rights. This reading emphasizes the change in the 'occupied
 *   set' of rights-holders and the corresponding shift in who benefited from
 *   and enforced these rights. The constraint is claimed as a Rope by its
 *   proponents (a coordination mechanism for authors and publishers) but
 *   operates with significant extraction and suppression, making it a Tangled
 *   Rope in practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.65).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.7).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "Statute of Anne's First Holding of Authorial Rights").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, '20ac4005-d9b8-482d-81ad-eef1441b52d4').
narrative_ontology:cs_kernel_codification('20ac4005-d9b8-482d-81ad-eef1441b52d4', formalized).
narrative_ontology:cs_authority_grounding('20ac4005-d9b8-482d-81ad-eef1441b52d4', lineage).
narrative_ontology:cs_interpretation_layer_present('20ac4005-d9b8-482d-81ad-eef1441b52d4').
narrative_ontology:cs_reading_relation('20ac4005-d9b8-482d-81ad-eef1441b52d4', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('20ac4005-d9b8-482d-81ad-eef1441b52d4', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('20ac4005-d9b8-482d-81ad-eef1441b52d4', foundational, statutory_grant_defines_right).
narrative_ontology:cs_axiom_status(statutory_grant_defines_right, holdable).
narrative_ontology:cs_axiom_grounding('20ac4005-d9b8-482d-81ad-eef1441b52d4', statutory_grant_defines_right, conventional).
narrative_ontology:cs_axiom('20ac4005-d9b8-482d-81ad-eef1441b52d4', foundational, author_as_primary_claimant).
narrative_ontology:cs_axiom_status(author_as_primary_claimant, holdable).
narrative_ontology:cs_axiom_grounding('20ac4005-d9b8-482d-81ad-eef1441b52d4', author_as_primary_claimant, deontological).
narrative_ontology:cs_reference_frame('20ac4005-d9b8-482d-81ad-eef1441b52d4', statutory_authorial_right_framework).
narrative_ontology:cs_drift_state('20ac4005-d9b8-482d-81ad-eef1441b52d4', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('20ac4005-d9b8-482d-81ad-eef1441b52d4', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, authors_as_rights_holders).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, publishers_under_statute).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, public_domain_users).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, competing_printers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% For the first time, authors gained a statutory right to control the reproduction of their works for a fixed term, shifting from a system where rights primarily resided with printers. This provided a new, albeit limited, form of economic leverage.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, authors_as_rights_holders, beneficiary,
    moderate, biographical, constrained, national).

% While losing their perpetual common law monopoly, publishers adapted to the new statutory framework, often acquiring rights from authors. They became the primary enforcers of the new, time-limited rights, benefiting from the clarity and enforceability of the statute over the previous ambiguous common law.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, publishers_under_statute, agenda_setter,
    institutional, generational, constrained, national).

% Lost the ability to freely copy works after the statutory term expired, as the concept of a time-limited right meant works eventually entered a public domain, but only after a period of exclusive control. Before the statute, many works were effectively in a perpetual 'public domain' for anyone not part of the Stationers' Company.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, public_domain_users, payer,
    powerless, generational, constrained, national).

% Were now legally barred from reprinting works during the statutory term without permission, facing penalties. This curtailed their previous practice of reprinting popular works without formal authorization from the Stationers' Company, which had effectively operated as a closed guild.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, competing_printers, payer,
    moderate, biographical, constrained, local).

% Lost its long-standing common law monopoly over printing, which had effectively granted perpetual rights to its members. The Statute of Anne directly challenged and ultimately superseded this traditional power structure, shifting the locus of control.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, stationers_company, excluded,
    institutional, generational, trapped, national).

% Analyze the historical shift in legal philosophy and economic impact of the Statute of Anne, debating its true intent and long-term consequences for intellectual property law.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a clear, statutory framework for copyright, replacing a fragmented common law system and the Stationers' Company's monopoly. It coordinated the rights of authors and publishers for a fixed term.
% TRANSFER_FUNCTION: Transferred exclusive rights to copy and print from a perpetual common law monopoly held by printers (Stationers' Company) to authors for a fixed term, which authors could then license to publishers. This created a new revenue stream for authors and a more defined, albeit time-limited, asset for publishers.
% ABSENT_VOICES: The broader public, who might have argued for immediate and free access to knowledge, were not directly represented in the legislative process that balanced author/publisher interests. Their 'voice' was implicitly represented by the eventual entry of works into the public domain, but not in the initial setting of the term.
% DISAPPEARANCE_RATIONALE: If the Statute of Anne and its principles vanished, the entire modern framework of copyright law would collapse. Authors would lose statutory protection, publishers would revert to a chaotic common law or guild-based system, and the concept of a time-limited public domain would cease to exist, fundamentally reorganizing the creative economy.
% FOUNDING_PROBLEM: The existing common law and Stationers' Company monopoly created uncertainty for authors, limited public access to works, and led to disputes over copying rights. There was a need for a clear, statutory basis for copyright that balanced authorial incentive with public benefit.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and contemporary legislative debates corroborate that the problems of authorial incentive and clarity of rights were central. While the specific context of the Stationers' Company monopoly is dead, the underlying tension between creator rights and public access remains a live problem in IP law, attested by ongoing legislative reforms and international treaties.
narrative_ontology:disappearance_verdict(ip_category_emergence__first_holding_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__first_holding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ip_category_emergence__first_holding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__first_holding_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__first_holding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__first_holding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is due to the creation of exclusive rights that restrict public access for a period, transferring value from the public domain to authors/publishers. Suppression (0.70) is high because the statutory framework actively enforced these new exclusive rights, curtailing previous practices of free reprinting. Theater ratio is low (0.10) as the statute was genuinely functional in establishing a new legal order, not merely performative. The claimed type is 'rope' from the perspective of its proponents (coordinating authorial incentive), but the metrics reflect its actual operation as a 'tangled_rope' due to the asymmetric extraction and active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of authors and publishers, the Statute of Anne was a necessary coordination mechanism, a 'rope' that clarified rights and incentivized creation. From the perspective of the public and competing printers, it was an extractive 'snare' that restricted access and created new monopolies. The engine's classification as a 'tangled_rope' captures this hybrid nature, where a genuine coordination function is intertwined with asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Authors and publishers are beneficiaries, gaining new or clarified rights and revenue streams. The public domain users and competing printers are victims, losing previous freedoms to copy and facing new legal restrictions. The Stationers' Company, a former beneficiary, became excluded as its monopoly was dismantled. This shift in beneficiary and victim sets is central to the 'first holding' reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    author_agency_vs_publisher_control,
    'To what extent did the Statute of Anne truly empower authors, versus merely shifting the locus of control from one set of publishers (Stationers'' Company) to another (those who acquired authorial rights)?',
    'Detailed historical analysis of author contracts and economic conditions post-1710, comparing author earnings and bargaining power before and after the statute.',
    'If author agency remained low, the ''beneficiary'' status of authors would be overstated, increasing the effective extraction for the ''publishers_under_statute'' seat and pushing the constraint closer to a pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(author_agency_vs_publisher_control, empirical, 'Ambiguity regarding the true beneficiary of authorial rights post-Statute of Anne.').

omega_variable(
    conceptual_vs_enactment_priority,
    'Is the ''first holding'' (enactment of statutory rights) the primary marker of IP category emergence, or is the ''thinkability'' (conceptual coherence of ownable expression) a more fundamental prior condition?',
    'Philosophical analysis of legal concepts and historical evidence of pre-1710 debates on intellectual property, independent of the Statute''s passage.',
    'If ''thinkability'' is primary, this ''first holding'' reading might be reclassified as a ''scaffold'' built upon a more fundamental ''mountain'' of conceptual coherence, or its extractiveness might be re-evaluated as a consequence of a prior, less extractive, conceptual constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_vs_enactment_priority, conceptual, 'Debate over whether the legal enactment or the conceptual coherence of IP is the more fundamental ''emergence'' event.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 1710, 1730).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__first_holding_reading, theater_ratio, 1710, 0.12).
narrative_ontology:measurement(ip_c_tr_t1715, ip_category_emergence__first_holding_reading, theater_ratio, 1715, 0.11).
narrative_ontology:measurement(ip_c_tr_t1720, ip_category_emergence__first_holding_reading, theater_ratio, 1720, 0.1).
narrative_ontology:measurement(ip_c_tr_t1725, ip_category_emergence__first_holding_reading, theater_ratio, 1725, 0.1).
narrative_ontology:measurement(ip_c_tr_t1730, ip_category_emergence__first_holding_reading, theater_ratio, 1730, 0.1).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__first_holding_reading, base_extractiveness, 1710, 0.6).
narrative_ontology:measurement(ip_c_be_t1715, ip_category_emergence__first_holding_reading, base_extractiveness, 1715, 0.62).
narrative_ontology:measurement(ip_c_be_t1720, ip_category_emergence__first_holding_reading, base_extractiveness, 1720, 0.63).
narrative_ontology:measurement(ip_c_be_t1725, ip_category_emergence__first_holding_reading, base_extractiveness, 1725, 0.64).
narrative_ontology:measurement(ip_c_be_t1730, ip_category_emergence__first_holding_reading, base_extractiveness, 1730, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__first_holding_reading, suppression_requirement, 1710, 0.65).
narrative_ontology:measurement(ip_c_su_t1715, ip_category_emergence__first_holding_reading, suppression_requirement, 1715, 0.67).
narrative_ontology:measurement(ip_c_su_t1720, ip_category_emergence__first_holding_reading, suppression_requirement, 1720, 0.68).
narrative_ontology:measurement(ip_c_su_t1725, ip_category_emergence__first_holding_reading, suppression_requirement, 1725, 0.69).
narrative_ontology:measurement(ip_c_su_t1730, ip_category_emergence__first_holding_reading, suppression_requirement, 1730, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__first_holding_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ip_category_emergence' kernel, focusing on the Statute of Anne (1710) as the 'first holding' of authorial rights. It is linked to sibling readings that explore the conceptual 'thinkability' of IP and the synchronic/diachronic relationship between these emergence points.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
