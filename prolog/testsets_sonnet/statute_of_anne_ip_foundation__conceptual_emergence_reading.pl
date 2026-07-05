% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__conceptual_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Statute of Anne (1710) — Conceptual Emergence of Time-Limited Copyright
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is read here as the moment a durable
 *   conceptual category — copyright as a limited, purpose-justified statutory
 *   grant rather than a perpetual common-law property right — became
 *   available for legal reasoning. Before the statute, arguments about
 *   printed works operated inside a conceptual space that had no settled name
 *   or bounded shape for this kind of claim; the statute did not merely
 *   reassign an existing right, it introduced a new object into the space of
 *   thinkable legal claims. The mid-18th-century literary property cases
 *   (Millar v. Taylor, Donaldson v. Becket) are read, from this seat, as
 *   courts working out the implications of a concept that had only recently
 *   become available rather than as courts discovering a pre-existing natural
 *   right.
 *
 * KEY AGENTS:
 *   - the_reading_public: beneficiary (moderate/constrained) — gains access to works entering the public domain under the new bounded category
 *   - authors_as_a_class: beneficiary/payer (moderate/constrained) — gains conceptual recognition as rights-holder, pays via time-limitation
 *   - perpetual_monopoly_claimants: payer (organized/constrained) — loses the conceptual availability of a perpetual claim, not merely an asset
 *   - parliament_and_courts: agenda_setter (institutional/analytical) — defines and administers the new category going forward
 *   - legal_scholars_and_treatise_writers: observer (analytical/analytical) — traces and formalizes the conceptual shift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.28).
domain_priors:suppression_score(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.35).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__conceptual_emergence_reading, scaffold).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__conceptual_emergence_reading, "Statute of Anne (1710) — Conceptual Emergence of Time-Limited Copyright").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__conceptual_emergence_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:has_sunset_clause(statute_of_anne_ip_foundation__conceptual_emergence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'e179eb41-0018-4a87-9fe7-3c1194a86ad2').
narrative_ontology:cs_kernel_codification('e179eb41-0018-4a87-9fe7-3c1194a86ad2', formalized).
narrative_ontology:cs_authority_grounding('e179eb41-0018-4a87-9fe7-3c1194a86ad2', lineage).
narrative_ontology:cs_interpretation_layer_present('e179eb41-0018-4a87-9fe7-3c1194a86ad2').
narrative_ontology:cs_reading_relation('e179eb41-0018-4a87-9fe7-3c1194a86ad2', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_reading_relation('e179eb41-0018-4a87-9fe7-3c1194a86ad2', statute_of_anne_ip_foundation__entangled_event_reading, forecloses).
narrative_ontology:cs_axiom('e179eb41-0018-4a87-9fe7-3c1194a86ad2', foundational, copyright_is_a_newly_created_conceptual_object).
narrative_ontology:cs_axiom_status(copyright_is_a_newly_created_conceptual_object, holdable).
narrative_ontology:cs_axiom_grounding('e179eb41-0018-4a87-9fe7-3c1194a86ad2', copyright_is_a_newly_created_conceptual_object, conventional).
narrative_ontology:cs_axiom('e179eb41-0018-4a87-9fe7-3c1194a86ad2', foundational, conceptual_and_institutional_change_are_analytically_separable).
narrative_ontology:cs_axiom_status(conceptual_and_institutional_change_are_analytically_separable, holdable).
narrative_ontology:cs_axiom_grounding('e179eb41-0018-4a87-9fe7-3c1194a86ad2', conceptual_and_institutional_change_are_analytically_separable, conventional).
narrative_ontology:cs_reference_frame('e179eb41-0018-4a87-9fe7-3c1194a86ad2', pre_statutory_undefined_conceptual_space).
narrative_ontology:cs_drift_state('e179eb41-0018-4a87-9fe7-3c1194a86ad2', post_donaldson_v_becket_settlement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e179eb41-0018-4a87-9fe7-3c1194a86ad2', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, the_reading_public).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors_as_a_class).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, future_publishers_and_printers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, perpetual_monopoly_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors_as_a_class).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, copyright_as_limited_statutory_grant_doctrine).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, learning_as_public_good_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Prior to the statute, access to printed works depended on the Stationers' perpetual claim of a printing right that never terminated. Under the new conceptual scheme, works enter the public domain after a fixed term, so the public gains access to a growing common stock of texts once the term runs out. They do not administer the term; they simply receive expanded future access as works age out of protection.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, the_reading_public, beneficiary,
    moderate, generational, constrained, national).

% For the first time a legal category exists that names the author, rather than the printer, as the person the right is conceived around, even though authors typically still assign the right to a publisher for money. They benefit from being conceptually recognized as a rights-holder at all, a category that did not previously exist as a distinct legal object; they pay in that the right they now conceptually hold is explicitly time-limited rather than perpetual.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors_as_a_class, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors_as_a_class, payer).

% Parties whose prior understanding of printing rights as durable, quasi-property claims is conceptually foreclosed by the statute's new framing of copyright as inherently time-bound. They lose not a specific asset but the conceptual availability of the claim itself — 'perpetual copyright' stops being a coherent legal category to assert, which forecloses future argument, not merely present enforcement.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, perpetual_monopoly_claimants, payer,
    organized, generational, constrained, national).

% Parliament enacts the term-limited framework and courts (culminating in later cases confirming the statutory, non-perpetual character of the right) adjudicate what the new category means. They administer the conceptual space going forward — defining what falls inside 'copyright' as a statutory learning-incentive rather than a natural property right.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, parliament_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% Observe and later formalize the conceptual shift, tracing how a new legal category (copyright as regulatory tool) emerged where none existed before, distinguishing it in retrospect from the institutional-reallocation and entangled-event readings of the same historical event.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, legal_scholars_and_treatise_writers, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__conceptual_emergence_reading, the_reading_public).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__conceptual_emergence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, publicly legible category — a copyright with a fixed term justified by incentivizing learning and the production of useful books — replacing the absence of any such conceptual object with a legible, bounded legal instrument that both authors and the public can reason about.
% TRANSFER_FUNCTION: Moves conceptual availability: the claim of perpetual printing right is rendered unthinkable as valid law, while the claim of a time-limited, learning-justified right becomes newly thinkable and available to authors and, derivatively, publishers who take assignment from them.
% ABSENT_VOICES: The Stationers' Company as an institution (its reallocation-focused loss is the subject of the sibling institutional_reallocation_reading, not this one) is present here only as the class whose prior conceptual framework (perpetual right) is foreclosed; their institutional grievance is bracketed out of this reading by design, per the ε-invariance decomposition.
% DISAPPEARANCE_RATIONALE: If the conceptual category introduced by the statute had never emerged, subsequent copyright law would have had no bounded, learning-justified template to build from — the entire architecture of term limits, public domain, and statutory (rather than natural-property) grounding for intellectual production would lack its founding conceptual point of origin, and later doctrine would have had to invent it from elsewhere or not at all.
% FOUNDING_PROBLEM: Prior to 1710 there was no legal concept distinguishing a time-limited, purpose-justified regulatory grant from a durable property right in printed matter — the space of thinkable claims about printed works did not yet contain 'copyright' as we now use the term.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and treatise writers outside any beneficiary class (i.e., not authors, not publishers, not government) attest that the conceptual novelty is real and traceable in subsequent case law (e.g., the mid-18th-century literary property cases explicitly wrestling with whether a perpetual common-law right survived the statute). Some historians dispute whether the concept was genuinely new or whether it merely renamed a rights transfer already underway — that dispute is exactly what the sibling readings capture, and is not resolved from within this reading alone.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__conceptual_emergence_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__conceptual_emergence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low-moderate (0.28 at interval end) because this reading's central claim is about conceptual availability, not about a transfer of rents — the 'cost' borne by perpetual-monopoly claimants is the foreclosure of an argument, not a captured revenue stream, and that cost is real but structurally different from ordinary extraction. Suppression starts moderate (0.5) reflecting the genuine contestedness of the new category in its first decades (booksellers litigated vigorously to preserve a perpetual common-law right) and falls over the interval (to 0.35) as Donaldson v. Becket (1774) settles that copyright is a creature of statute, not perpetual common law — the conceptual space stabilizes and active suppression of the alternative claim becomes less necessary because the alternative becomes less assertable. Theater ratio stays low throughout (0.10 to 0.15): this is a conceptual-legal claim with little performative machinery layered on top of it; the courts' work is substantive doctrinal reasoning, not theater.
 *
 * DIRECTIONALITY LOGIC:
 *   The reading public and authors-as-a-class are the structural beneficiaries of the new conceptual object: the public gains an eventual public domain, authors gain a category that names them as the right's origin (even while typically assigning it away). Perpetual-monopoly claimants are the structural target: their loss is specifically the loss of the conceptual availability of a claim they previously could assert, which is why their exit_options are authored as 'constrained' rather than 'trapped' — they can still argue in other conceptual registers (trade custom, contract), but the specific claim of a perpetual common-law copyright becomes progressively less viable as courts settle the statutory reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the absence of a legal concept for time-limited, purpose-justified regulatory grants over printed works — is contested rather than cleanly dead or live: courts and legislatures continue extending, amending, and re-litigating copyright term and purpose (from 1710 through the Berne Convention and beyond), meaning the conceptual category the statute introduced remains actively used for its original purpose (incentivizing learning production) rather than having ossified into pure inertial persistence. This blocks a piton misreading — the concept is not merely surviving as theater, it is doing continuing genealogical work, hence the scaffold classification (the statute itself, as a specific enactment with a term structure, functions as transitional scaffolding for a durable conceptual architecture that outlives the particular statute).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_novelty_vs_relabeling,
    'Did the Statute of Anne genuinely introduce a new legal concept, or did it merely give a new statutory name and term-limit to a printing-trade practice that was already conceptually available as a de facto arrangement among Stationers?',
    'Close doctrinal-historical analysis of pre-1710 legal argument and treatise literature: if arguments structurally isomorphic to ''time-limited, purpose-justified grant'' appear before 1710 under different vocabulary, the novelty claim weakens; if the pre-1710 record shows only perpetual-property or trade-custom framings with no bounded-term concept, the novelty claim strengthens.',
    'If the concept was not genuinely new, this reading collapses toward the institutional_reallocation_reading (the sibling), since the apparent ''conceptual emergence'' would just be redescription of a reallocation already underway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_novelty_vs_relabeling, conceptual, 'Whether the conceptual category was genuinely novel or a relabeling of existing trade practice.').

omega_variable(
    which_framing_the_courts_actually_used,
    'Did the mid-18th century judges deciding Millar v. Taylor and Donaldson v. Becket experience themselves as adjudicating a genuinely new legal object, or as settling a dispute over who held an existing kind of right?',
    'Close reading of judicial opinions and contemporaneous legal commentary for explicit language about the novelty (or lack thereof) of the statutory category, cross-checked against later legal-historical scholarship.',
    'If judges'' own framing tracks the conceptual-emergence account, this reading gains direct corroboration; if their framing tracks a reallocation account, the corroboration this story claims from ''legal scholars outside the beneficiary class'' weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_framing_the_courts_actually_used, empirical, 'Whether contemporaneous judicial reasoning corroborates the conceptual-emergence framing.').

omega_variable(
    decomposition_boundary_with_entangled_event_reading,
    'Is it coherent to isolate a purely ''conceptual'' dimension of the 1710 event at all, or does the entangled_event_reading''s claim that conceptual and institutional change are inseparable defeat the premise of this decomposition?',
    'This is a framing-level (Omega_C) question rather than an empirically resolvable one: it depends on whether one accepts that historical events can be cleanly separated into conceptual and institutional dimensions for analytical purposes, or whether such separation is itself a distorting abstraction.',
    'If the entangled-event framing is correct, all three sibling readings (including this one) are artifacts of an illegitimate decomposition, and the family should be re-merged into a single constraint with a different, non-decomposed ε. If the decomposition is legitimate, the three-story family stands as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decomposition_boundary_with_entangled_event_reading, conceptual, 'Whether the conceptual/institutional decomposition of the 1710 event is itself defensible, per the entangled_event_reading''s objection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__conceptual_emergence_reading, 1710, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1710, 0.1).
narrative_ontology:measurement_basis(stat_tr_t1710, observed).
narrative_ontology:measurement(stat_tr_t1720, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1720, 0.11).
narrative_ontology:measurement_basis(stat_tr_t1720, observed).
narrative_ontology:measurement(stat_tr_t1735, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1735, 0.12).
narrative_ontology:measurement_basis(stat_tr_t1735, observed).
narrative_ontology:measurement(stat_tr_t1750, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1750, 0.13).
narrative_ontology:measurement_basis(stat_tr_t1750, observed).
narrative_ontology:measurement(stat_tr_t1762, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1762, 0.14).
narrative_ontology:measurement_basis(stat_tr_t1762, observed).
narrative_ontology:measurement(stat_tr_t1774, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1774, 0.15).
narrative_ontology:measurement_basis(stat_tr_t1774, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1710, 0.18).
narrative_ontology:measurement_basis(stat_be_t1710, observed).
narrative_ontology:measurement(stat_be_t1720, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1720, 0.2).
narrative_ontology:measurement_basis(stat_be_t1720, observed).
narrative_ontology:measurement(stat_be_t1735, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1735, 0.23).
narrative_ontology:measurement_basis(stat_be_t1735, observed).
narrative_ontology:measurement(stat_be_t1750, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1750, 0.25).
narrative_ontology:measurement_basis(stat_be_t1750, observed).
narrative_ontology:measurement(stat_be_t1762, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1762, 0.27).
narrative_ontology:measurement_basis(stat_be_t1762, observed).
narrative_ontology:measurement(stat_be_t1774, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1774, 0.28).
narrative_ontology:measurement_basis(stat_be_t1774, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1710, 0.5).
narrative_ontology:measurement_basis(stat_su_t1710, observed).
narrative_ontology:measurement(stat_su_t1720, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1720, 0.47).
narrative_ontology:measurement_basis(stat_su_t1720, observed).
narrative_ontology:measurement(stat_su_t1735, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1735, 0.42).
narrative_ontology:measurement_basis(stat_su_t1735, observed).
narrative_ontology:measurement(stat_su_t1750, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1750, 0.38).
narrative_ontology:measurement_basis(stat_su_t1750, observed).
narrative_ontology:measurement(stat_su_t1762, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1762, 0.36).
narrative_ontology:measurement_basis(stat_su_t1762, observed).
narrative_ontology:measurement(stat_su_t1774, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1774, 0.35).
narrative_ontology:measurement_basis(stat_su_t1774, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__conceptual_emergence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.05).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, institutional_reallocation_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, entangled_event_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the statute_of_anne_ip_foundation kernel, decomposed per the ε-invariance principle rather than averaged into a single measurement-dependent constraint. conceptual_emergence_reading (this story) claims the statute's structural novelty is a new legal concept, with beneficiary=public learning and victim=perpetual monopoly claimants. institutional_reallocation_reading claims the same event is better described as a transfer of an already-existing kind of right from the Stationers' Company to authors, with a different beneficiary/victim structure (authors vs. Stationers, not public-learning vs. perpetual-monopoly-claimants). entangled_event_reading rejects the decomposition itself, holding that conceptual and institutional change occurred as one inseparable event. All three are linked bidirectionally via affects_constraints to support contamination and consistency analysis across the family; none is the 'correct' reading — they are structurally distinct constraints sharing a historical origin point.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
