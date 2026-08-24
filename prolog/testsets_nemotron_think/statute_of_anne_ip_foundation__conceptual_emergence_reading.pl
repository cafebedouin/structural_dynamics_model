% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__conceptual_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: statute_of_anne_ip_foundation__conceptual_emergence_reading
 *   human_readable: Statute of Anne Copyright as Limited Regulatory Tool for Learning
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) created the first statutory copyright: a
 *   14-year term renewable once, vested in authors 'for the Encouragement of
 *   Learning.' This reading treats the statute as a genuine conceptual
 *   innovation — it made 'intellectual property' thinkable as a limited
 *   regulatory tool with a built-in sunset (the public domain), not as
 *   perpetual property. The constraint is the copyright system the statute
 *   instantiated: a scaffold meant to transition works from private monopoly
 *   to public commons. Over the interval 1710–1842, term extensions (1814: 28
 *   years or life; 1842: 42 years or life+7) and judicial interpretation
 *   progressively displaced the scaffold logic with a property logic, raising
 *   extractiveness and theater while the public domain destination receded.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.35).
domain_priors:suppression_score(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.45).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__conceptual_emergence_reading, scaffold).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__conceptual_emergence_reading, "Statute of Anne Copyright as Limited Regulatory Tool for Learning").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__conceptual_emergence_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:has_sunset_clause(statute_of_anne_ip_foundation__conceptual_emergence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'f62dc7c0-f52e-4251-b0c6-4f6723f99d91').
narrative_ontology:cs_kernel_codification('f62dc7c0-f52e-4251-b0c6-4f6723f99d91', formalized).
narrative_ontology:cs_authority_grounding('f62dc7c0-f52e-4251-b0c6-4f6723f99d91', lineage).
narrative_ontology:cs_interpretation_layer_present('f62dc7c0-f52e-4251-b0c6-4f6723f99d91').
narrative_ontology:cs_reading_relation('f62dc7c0-f52e-4251-b0c6-4f6723f99d91', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_reading_relation('f62dc7c0-f52e-4251-b0c6-4f6723f99d91', statute_of_anne_ip_foundation__entangled_event_reading, influences).
narrative_ontology:cs_axiom('f62dc7c0-f52e-4251-b0c6-4f6723f99d91', foundational, copyright_as_limited_regulatory_tool).
narrative_ontology:cs_axiom_status(copyright_as_limited_regulatory_tool, holdable).
narrative_ontology:cs_axiom_grounding('f62dc7c0-f52e-4251-b0c6-4f6723f99d91', copyright_as_limited_regulatory_tool, empirically_contingent).
narrative_ontology:cs_axiom('f62dc7c0-f52e-4251-b0c6-4f6723f99d91', foundational, public_domain_as_teleological_end).
narrative_ontology:cs_axiom_status(public_domain_as_teleological_end, holdable).
narrative_ontology:cs_axiom_grounding('f62dc7c0-f52e-4251-b0c6-4f6723f99d91', public_domain_as_teleological_end, deontological).
narrative_ontology:cs_reference_frame('f62dc7c0-f52e-4251-b0c6-4f6723f99d91', statutory_learning_mandate).
narrative_ontology:cs_drift_state('f62dc7c0-f52e-4251-b0c6-4f6723f99d91', post_1842_term_extension, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f62dc7c0-f52e-4251-b0c6-4f6723f99d91', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, publishers_booksellers).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, copyright_as_limited_regulatory_tool).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_domain_as_teleological_end).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, encouragement_of_learning_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the 1710 statute 'for the Encouragement of Learning,' vesting copyright in authors for 14+14 years instead of perpetuity. Sets the statutory term, enforcement mechanisms, and the public domain destination. Could amend the statute but faces political pressure from book trade interests.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, parliament_state, agenda_setter,
    institutional, generational, analytical, national).

% Gained statutory copyright for the first time, replacing dependence on Stationers' Company patronage. Can assign rights to publishers but retain reversion after first term. Benefit from limited monopoly but lack collective bargaining power; exit means returning to patronage or unsubsidized writing.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors, beneficiary,
    moderate, biographical, constrained, national).

% Lobbied for the statute to replace Stationers' perpetual monopoly with tradeable statutory terms. Buy authors' rights and enforce them against piracy. Benefit from predictable term-limited monopoly. Can diversify into other print markets; exit means shifting to uncopyrighted works or stationery.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, publishers_booksellers, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__conceptual_emergence_reading, publishers_booksellers, agenda_setter).

% Pays monopoly prices during the copyright term but gains guaranteed public domain access after term expires. Literacy rates rising; demand for cheap books high. Exit means piracy, manuscript circulation, or doing without — constrained by enforcement and availability.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public, beneficiary).

% Lost perpetual monopoly via royal charter; now competes as one publisher among many under statutory terms. Retains enforcement infrastructure (search/seizure) and Hall archives. Bears costs of adaptation; exit means dissolving or reinventing as a trade association — constrained by sunk institutional capital.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company, payer,
    organized, generational, constrained, national).

% Interpret the statute's 'encouragement of learning' purpose, term limits, and public domain destination. Donaldson v Beckett (1774) affirms statutory term over common law perpetuity. Their readings shape whether the constraint functions as scaffold or hardens into property.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, legal_scholars_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces the Stationers' Company's perpetual monopoly and private censorship with a state-granted, time-limited monopoly that coordinates publishing investment, author remuneration, and guaranteed eventual public domain access — solving the problem of knowledge lock-up under perpetual private control.
% TRANSFER_FUNCTION: Moves monopoly rents from the reading public to authors and publishers for a fixed statutory term (14+14 years), then moves the work itself into the public domain where it becomes a free resource for learning and reprinting.
% ABSENT_VOICES: Colonial subjects in India, Africa, and the Americas subject to British copyright extension without representation; women writers excluded from Stationers' Company and professional authorship; non-literate populations who bear enforcement costs but cannot access printed learning; Scottish and Irish publishers outside the London trade nexus.
% DISAPPEARANCE_RATIONALE: If the statute vanished overnight, the Stationers' Company would likely reassert common-law perpetual monopoly (as they argued in Donaldson v Beckett), authors would lose statutory reversion rights, and the guaranteed public domain pipeline would close — the book trade would revert to private perpetual control with no statutory learning mandate.
% FOUNDING_PROBLEM: The Stationers' Company's perpetual monopoly (via 1662 Licensing Act and royal charter) suppressed cheap reprints, controlled which texts were printed, and prevented the formation of a public domain — stifling the 'encouragement of learning' Parliament sought.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary pamphlets (e.g., 'The Case of the Booksellers' 1735, 'A Letter to a Member of Parliament' 1739) attest the statute was sold as breaking perpetual monopoly. Donaldson v Beckett (1774) Lords' opinions confirm the statute displaced common law perpetuity. Modern historians (Rose 2003, Deazley 2006, Patterson 1968) corroborate from outside the book trade that the founding problem — perpetual private monopoly — was structurally addressed but term extensions from 1814 onward recreate it.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__conceptual_emergence_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__conceptual_emergence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness starts low (0.25) because the term is short, reversion is mandatory, and the public domain is a real destination. It rises as terms lengthen and the public domain shrinks proportionally. Suppression is moderate — enforcement exists (Stationers' Hall searches, informers) but piracy persists and the term limit is a structural escape valve. Theater ratio rises sharply after 1800 as 'encouragement of learning' becomes rhetorical cover for term extensions that benefit publishers, not authors or the public. Accessibility collapse is moderate: during term, alternatives are suppressed; after term, they open fully. Resistance is moderate: public resistance to high prices, author resistance to publisher terms, Scottish/Irish resistance to London monopoly.
 *
 * PERSPECTIVAL GAP:
 *   From the Stationers' Company seat, the statute is a snare (loss of perpetual monopoly, forced into competitive statutory terms). From the author seat, it is a rope (first statutory right, reversion protects against publisher capture). From the public seat, it oscillates: scaffold during term (pay monopoly price for learning access), mountain after term (public domain as natural law). The engine computes these divergences from the structural data — the claimed type 'scaffold' reflects the statutory design, not every seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament is the agenda-setter (d ~0.15, beneficiary of legislative authority). Authors and publishers are beneficiaries during term (d ~0.3–0.4) but authors have constrained exit (reversion helps). Reading public is payer during term (d ~0.7) but beneficiary after (d ~0.2) — net directionality depends on time horizon. Stationers Company is payer (lost perpetual monopoly, d ~0.6) but retains enforcement role. Legal scholars are observers (d ~0.5, analytical). The scaffold structure means directionality flips at term end: the same constraint extracts from public then subsidizes public.
 *
 * MANDATROPHY ANALYSIS:
 *   The statute's mandate ('encouragement of learning') is structurally a scaffold: the constraint justifies itself by the transition it enables, not the steady state it maintains. Mandatrophy risk is high because the scaffold's sunset (public domain) is the very feature term extensions attack. By 1842, the theater ratio (0.60) signals the learning rationale has become performative cover for publisher rent-seeking. The founding problem (perpetual monopoly) is contested: legally dead (statute displaced it) but economically revived (long terms recreate it). Corroboration from outside beneficiaries (historians, Scottish trade, public petitions) confirms the mandatrophy dynamic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_novelty_vs_institutional_continuity,
    'Did the Statute of Anne create a genuinely new conceptual category (copyright as limited regulatory tool) or merely reallocate existing perpetual rights from Stationers to authors under a new label?',
    'Comparative analysis of pre-1710 discourse (Stationers'' petitions, licensing acts, common law arguments) vs. post-1710 statutory language and judicial interpretation. If ''copyright'' as a word and concept appears first in the statute and lacks pre-statute precedent, conceptual novelty is supported. If the statute''s machinery mirrors Stationers'' registers and common law ''literary property'' arguments, institutional continuity dominates.',
    'If conceptual novelty: the constraint is a genuine scaffold — a new regulatory instrument with designed sunset. If institutional continuity: the constraint is a tangled_rope from inception — reallocation of extraction rights with coordination cover story. This determines whether the scaffold classification applies to the statute''s design or only to its rhetorical framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_novelty_vs_institutional_continuity, conceptual, 'Whether the statute''s ''limited time'' language constitutes conceptual innovation or rhetorical packaging of existing monopoly structures.').

omega_variable(
    public_domain_as_designed_destination_vs_residual,
    'Was the public domain a teleological end designed into the statute (scaffold logic) or a residual category left over after monopoly expiration (property logic)?',
    'Textual analysis of ''for the Encouragement of Learning'' preamble, the 14+14 structure, the registration/deposit requirements, and the 1774 Donaldson v Beckett opinions. If the public domain is affirmatively constructed (deposit at libraries, mandatory reversion, no common law survival), it is a designed destination. If it is merely the absence of monopoly, it is residual.',
    'If designed destination: scaffold classification holds — the constraint''s function is the transition. If residual: the constraint is a rope or tangled_rope where the public domain is an externality, not a purpose. Affects whether term extensions are mandatrophy (betrayal of design) or natural evolution (property logic unfolding).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_domain_as_designed_destination_vs_residual, empirical, 'Whether the statute affirmatively constructs the public domain or merely lets monopoly lapse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__conceptual_emergence_reading, 1710, 1842).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(statute_anne_conceptual_tr_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1710, 0.1).
narrative_ontology:measurement(statute_anne_conceptual_tr_t1735, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1735, 0.15).
narrative_ontology:measurement(statute_anne_conceptual_tr_t1774, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1774, 0.2).
narrative_ontology:measurement(statute_anne_conceptual_tr_t1800, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1800, 0.3).
narrative_ontology:measurement(statute_anne_conceptual_tr_t1814, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1814, 0.45).
narrative_ontology:measurement(statute_anne_conceptual_tr_t1842, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1842, 0.6).

% Extraction over time
narrative_ontology:measurement(statute_anne_conceptual_be_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1710, 0.25).
narrative_ontology:measurement(statute_anne_conceptual_be_t1735, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1735, 0.3).
narrative_ontology:measurement(statute_anne_conceptual_be_t1774, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1774, 0.35).
narrative_ontology:measurement(statute_anne_conceptual_be_t1800, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1800, 0.4).
narrative_ontology:measurement(statute_anne_conceptual_be_t1814, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1814, 0.48).
narrative_ontology:measurement(statute_anne_conceptual_be_t1842, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1842, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(statute_anne_conceptual_su_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1710, 0.35).
narrative_ontology:measurement(statute_anne_conceptual_su_t1735, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1735, 0.4).
narrative_ontology:measurement(statute_anne_conceptual_su_t1774, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1774, 0.45).
narrative_ontology:measurement(statute_anne_conceptual_su_t1800, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1800, 0.5).
narrative_ontology:measurement(statute_anne_conceptual_su_t1814, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1814, 0.55).
narrative_ontology:measurement(statute_anne_conceptual_su_t1842, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1842, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__conceptual_emergence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.12).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__institutional_reallocation_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the Statute of Anne into three structurally distinct readings: (1) conceptual_emergence_reading — new regulatory category with designed public domain (scaffold); (2) institutional_reallocation_reading — rights transfer from Stationers to authors (tangled_rope); (3) entangled_event_reading — inseparable conceptual/institutional shift (hybrid). Each has different ε, beneficiaries, and temporal dynamics. Linked via affects_constraints for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
