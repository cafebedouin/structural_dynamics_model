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
 *   human_readable: Emergence of Author as IP Rights Holder (First Holding Reading)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the emergence of the author as a statutory
 *   rights holder in intellectual property, specifically focusing on the
 *   shift in legal occupancy marked by the Statute of Anne in 1710. Prior to
 *   this, 'rights' were largely perpetual common law monopolies held by
 *   printers (e.g., the Stationers' Company). The Statute introduced a
 *   time-limited, statutory right, initially vested in the author,
 *   fundamentally changing who could legitimately claim ownership and benefit
 *   from literary works. This reading emphasizes the practical, legal shift
 *   in who 'held' the right, rather than the conceptual 'thinkability' of IP
 *   itself.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.6).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.7).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "Emergence of Author as IP Rights Holder (First Holding Reading)").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, '2de010fb-963f-4c1e-935f-eb6c09d027bf').
narrative_ontology:cs_kernel_codification('2de010fb-963f-4c1e-935f-eb6c09d027bf', formalized).
narrative_ontology:cs_authority_grounding('2de010fb-963f-4c1e-935f-eb6c09d027bf', lineage).
narrative_ontology:cs_interpretation_layer_present('2de010fb-963f-4c1e-935f-eb6c09d027bf').
narrative_ontology:cs_reading_relation('2de010fb-963f-4c1e-935f-eb6c09d027bf', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('2de010fb-963f-4c1e-935f-eb6c09d027bf', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('2de010fb-963f-4c1e-935f-eb6c09d027bf', foundational, author_as_primary_rights_holder).
narrative_ontology:cs_axiom_status(author_as_primary_rights_holder, holdable).
narrative_ontology:cs_axiom_grounding('2de010fb-963f-4c1e-935f-eb6c09d027bf', author_as_primary_rights_holder, conventional).
narrative_ontology:cs_axiom('2de010fb-963f-4c1e-935f-eb6c09d027bf', foundational, statutory_grant_as_origin_of_right).
narrative_ontology:cs_axiom_status(statutory_grant_as_origin_of_right, holdable).
narrative_ontology:cs_axiom_grounding('2de010fb-963f-4c1e-935f-eb6c09d027bf', statutory_grant_as_origin_of_right, conventional).
narrative_ontology:cs_reference_frame('2de010fb-963f-4c1e-935f-eb6c09d027bf', statutory_author_protection_framework).
narrative_ontology:cs_drift_state('2de010fb-963f-4c1e-935f-eb6c09d027bf', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2de010fb-963f-4c1e-935f-eb6c09d027bf', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, authors_as_rights_holders).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, publishers_under_statute).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, public_domain_users).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, unlicensed_printers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained statutory protection for their works, allowing them to control reproduction and receive royalties. This shifted their status from mere creators to legal claimants, but their power remained mediated by publishers.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, authors_as_rights_holders, beneficiary,
    moderate, biographical, constrained, national).

% Transitioned from a perpetual monopoly under common law to a time-limited statutory right, often acquired from authors. They became the primary enforcers of the new IP regime, benefiting from the clarity of statutory protection while adapting to its limits.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, publishers_under_statute, agenda_setter,
    institutional, generational, mobile, national).

% Lost the ability to freely copy and distribute works once they entered the statutory protection regime. Their access to cultural works became subject to the new rights holders' control and pricing.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, public_domain_users, payer,
    powerless, generational, trapped, national).

% Were directly targeted by enforcement actions for reproducing works without permission, losing their previous freedom to print and sell copies of popular works. Their business model was directly undermined by the new statutory rights.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, unlicensed_printers, payer,
    moderate, immediate, constrained, local).

% The former monopolist, whose perpetual common law rights were curtailed by the new statute. While still influential, their previous absolute control over printing was replaced by a more limited, statutory framework. They would have preferred the continuation of their monopoly.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, stationers_company, excluded,
    institutional, generational, constrained, national).

% Analyze the historical shift in legal frameworks and the philosophical underpinnings of intellectual property. They observe the structural changes in rights and enforcement without direct participation in the economic flows.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a clear, time-limited framework for authors and publishers to control the reproduction of literary works, incentivizing creation and investment in publishing by defining who legitimately 'held' the right.
% TRANSFER_FUNCTION: Transferred the primary right to control reproduction from a perpetual common law right (often held by publishers) to a time-limited statutory right, initially vested in the author, enabling authors to claim economic benefit from their creations.
% ABSENT_VOICES: The broader public, who previously enjoyed a more expansive 'public domain' for copying and adapting works, were not directly represented in the legislative process that established these new rights. Their interests in free access were subordinated to the new rights regime.
% DISAPPEARANCE_RATIONALE: If this statutory framework vanished, the entire modern intellectual property system would collapse. Authors would lose their primary means of economic support from their creations, publishers would have no exclusive rights to sell, and the market for creative works would revert to a chaotic state, likely dominated by those with printing presses rather than creators.
% FOUNDING_PROBLEM: The lack of clear, enforceable rights for authors to benefit from their creations, and the desire to regulate the publishing industry by moving away from perpetual common law monopolies towards a statutory, time-limited system.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and economists corroborate that the problem of incentivizing creation and regulating publishing was live. The Stationers' Company's prior monopoly was seen as stifling innovation and competition, while authors sought greater control over their works. The statute was a direct response to these pressures, attested by parliamentary records and contemporary legal commentary.
narrative_ontology:disappearance_verdict(ip_category_emergence__first_holding_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__first_holding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ip_category_emergence__first_holding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__first_holding_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.6) reflects the new costs imposed on the public and unlicensed printers, who previously had freer access to works. Suppression (0.7) is high because the new statutory rights required active enforcement against infringers, backed by legal penalties. Theater ratio is low (0.1) as the system was genuinely functional in establishing and enforcing these new rights, not merely performative. The claimed type is 'tangled_rope' because it solved a coordination problem (incentivizing authors, regulating publishing) while simultaneously creating asymmetric extraction from the public and rival printers.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of authors, this was a 'rope' – a mechanism for fair compensation and recognition. From the perspective of the public, it was a 'snare' – a new restriction on access to knowledge. The engine's classification as 'tangled_rope' captures this hybrid nature, acknowledging both the coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Authors and publishers (under the new statutory framework) are beneficiaries, gaining new or clarified rights and revenue streams. The public and unlicensed printers are victims, losing previous freedoms and facing new restrictions. The Stationers' Company, while still powerful, saw its perpetual monopoly curtailed, placing it in a complex, partially excluded position. Legal scholars act as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_practical_emergence,
    'Does the Statute of Anne mark the first ''holding'' of IP rights by authors, or the first ''thinkability'' of IP as a distinct legal category?',
    'Analysis of pre-1710 legal discourse for evidence of ''ownable expression'' concepts, independent of enforcement mechanisms. If such concepts were robustly present, the ''thinkability'' preceded ''holding''.',
    'If ''thinkability'' preceded ''holding'', this reading''s emphasis on the 1710 legal shift as the *origin* of authorial rights would be reclassified as a later stage of a longer conceptual evolution, potentially shifting its claimed_type to ''scaffold'' (transitional support for an already emerging concept).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_vs_practical_emergence, conceptual, 'Distinguishing between the conceptual emergence of IP and its practical legal instantiation.').

omega_variable(
    stationers_monopoly_vs_author_rights,
    'To what extent was the Statute of Anne a genuine shift to authorial rights, versus a re-codification of the Stationers'' Company''s monopoly under a new guise?',
    'Detailed historical analysis of enforcement patterns and economic flows post-1710, examining whether authors genuinely gained power or if publishers (often members of the Stationers'' Company) merely adapted their control under the new statutory framework.',
    'If the Stationers'' Company retained de facto control, the ''beneficiary'' status of authors would be reduced, and the ''extractiveness'' of the constraint would be re-attributed more directly to the publishers, potentially reclassifying it closer to a ''snare'' for authors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stationers_monopoly_vs_author_rights, empirical, 'Assessing the true beneficiaries of the 1710 shift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 1690, 1750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1690, ip_category_emergence__first_holding_reading, theater_ratio, 1690, 0.05).
narrative_ontology:measurement(ip_c_tr_t1700, ip_category_emergence__first_holding_reading, theater_ratio, 1700, 0.08).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__first_holding_reading, theater_ratio, 1710, 0.1).
narrative_ontology:measurement(ip_c_tr_t1720, ip_category_emergence__first_holding_reading, theater_ratio, 1720, 0.11).
narrative_ontology:measurement(ip_c_tr_t1730, ip_category_emergence__first_holding_reading, theater_ratio, 1730, 0.1).
narrative_ontology:measurement(ip_c_tr_t1740, ip_category_emergence__first_holding_reading, theater_ratio, 1740, 0.09).
narrative_ontology:measurement(ip_c_tr_t1750, ip_category_emergence__first_holding_reading, theater_ratio, 1750, 0.1).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1690, ip_category_emergence__first_holding_reading, base_extractiveness, 1690, 0.4).
narrative_ontology:measurement(ip_c_be_t1700, ip_category_emergence__first_holding_reading, base_extractiveness, 1700, 0.45).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__first_holding_reading, base_extractiveness, 1710, 0.6).
narrative_ontology:measurement(ip_c_be_t1720, ip_category_emergence__first_holding_reading, base_extractiveness, 1720, 0.62).
narrative_ontology:measurement(ip_c_be_t1730, ip_category_emergence__first_holding_reading, base_extractiveness, 1730, 0.61).
narrative_ontology:measurement(ip_c_be_t1740, ip_category_emergence__first_holding_reading, base_extractiveness, 1740, 0.59).
narrative_ontology:measurement(ip_c_be_t1750, ip_category_emergence__first_holding_reading, base_extractiveness, 1750, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1690, ip_category_emergence__first_holding_reading, suppression_requirement, 1690, 0.5).
narrative_ontology:measurement(ip_c_su_t1700, ip_category_emergence__first_holding_reading, suppression_requirement, 1700, 0.6).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__first_holding_reading, suppression_requirement, 1710, 0.7).
narrative_ontology:measurement(ip_c_su_t1720, ip_category_emergence__first_holding_reading, suppression_requirement, 1720, 0.72).
narrative_ontology:measurement(ip_c_su_t1730, ip_category_emergence__first_holding_reading, suppression_requirement, 1730, 0.71).
narrative_ontology:measurement(ip_c_su_t1740, ip_category_emergence__first_holding_reading, suppression_requirement, 1740, 0.69).
narrative_ontology:measurement(ip_c_su_t1750, ip_category_emergence__first_holding_reading, suppression_requirement, 1750, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__first_holding_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ip_category_emergence' kernel. This 'first_holding_reading' emphasizes the legal and practical shift in who could claim IP rights, particularly the author's entry into this set in 1710. It is linked to the 'thinkability_reading' (focusing on conceptual coherence) and 'synchronic_diachronic_seam' (examining the relationship between conceptual and practical emergence) as part of a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
