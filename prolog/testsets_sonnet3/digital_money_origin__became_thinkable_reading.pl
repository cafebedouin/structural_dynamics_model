% ============================================================================
% CONSTRAINT STORY: digital_money_origin__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__became_thinkable_reading, []).

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
 *   constraint_id: digital_money_origin__became_thinkable_reading
 *   human_readable: Digital Money's Origin as a Conceptual/Institutional Threshold Event
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This story reads the origin of digital money as the moment it became
 *   technically and institutionally CONCEIVABLE — the period, roughly 1960 to
 *   1990, when cryptographic research, mainframe-based clearing experiments,
 *   and central bank feasibility studies established a shared vocabulary for
 *   representing and transferring value without physical tokens. This is
 *   distinct from the moment any individual first held a non-physical
 *   monetary instrument in practical use (a later, use-anchored reading) and
 *   from the moment regulators formally counted digital instruments in
 *   statistical aggregates (a still-later, recognition-anchored reading).
 *   Locating the origin at conceivability pushes the date earlier and changes
 *   who counts as founding beneficiary and victim: the beneficiaries are the
 *   labs and institutions present when the conceptual architecture was set,
 *   and the victims are populations and traditions excluded from that
 *   conceptual room, whose absence shaped what digital money could even mean
 *   by the time implementation arrived.
 *
 * KEY AGENTS:
 *   - early_computer_scientists_and_cryptographers: primary agenda-setters who defined the conceptual vocabulary
 *   - central_bank_research_departments: institutional beneficiaries who absorbed the concept early to protect settlement authority
 *   - large_clearing_and_settlement_institutions: downstream beneficiaries who converted conceptual advantage into infrastructural capture
 *   - unbanked_populations_excluded_from_framing: primary victims of a conceptual architecture built without their participation
 *   - monetary_historians: analytical observers adjudicating between competing origin narratives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.52).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.44).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Digital Money's Origin as a Conceptual/Institutional Threshold Event").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, 'c2a29fdb-d925-4140-af1d-29efd0fa759f').
narrative_ontology:cs_kernel_codification('c2a29fdb-d925-4140-af1d-29efd0fa759f', distributed).
narrative_ontology:cs_authority_grounding('c2a29fdb-d925-4140-af1d-29efd0fa759f', expertise).
narrative_ontology:cs_interpretation_layer_present('c2a29fdb-d925-4140-af1d-29efd0fa759f').
narrative_ontology:cs_reading_relation('c2a29fdb-d925-4140-af1d-29efd0fa759f', digital_money_origin__first_held_reading, influences).
narrative_ontology:cs_reading_relation('c2a29fdb-d925-4140-af1d-29efd0fa759f', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('c2a29fdb-d925-4140-af1d-29efd0fa759f', foundational, technical_conceivability_constitutes_emergence).
narrative_ontology:cs_axiom_status(technical_conceivability_constitutes_emergence, holdable).
narrative_ontology:cs_axiom_grounding('c2a29fdb-d925-4140-af1d-29efd0fa759f', technical_conceivability_constitutes_emergence, conventional).
narrative_ontology:cs_axiom('c2a29fdb-d925-4140-af1d-29efd0fa759f', secondary, conceptual_architecture_precedes_and_bounds_practical_use).
narrative_ontology:cs_axiom_status(conceptual_architecture_precedes_and_bounds_practical_use, holdable).
narrative_ontology:cs_axiom_grounding('c2a29fdb-d925-4140-af1d-29efd0fa759f', conceptual_architecture_precedes_and_bounds_practical_use, empirically_contingent).
narrative_ontology:cs_reference_frame('c2a29fdb-d925-4140-af1d-29efd0fa759f', conceptual_feasibility_as_origin).
narrative_ontology:cs_drift_state('c2a29fdb-d925-4140-af1d-29efd0fa759f', post_cryptocurrency_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('c2a29fdb-d925-4140-af1d-29efd0fa759f', '').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_computer_scientists_and_cryptographers).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, central_bank_research_departments).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, large_clearing_and_settlement_institutions).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_theorists_of_electronic_funds_transfer).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, unbanked_populations_excluded_from_framing).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, non_western_monetary_traditions_excluded_from_framing).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, small_depository_institutions_without_technical_access).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, consumer_advocates_absent_from_early_design_rooms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Working in research labs and universities in the 1960s-1980s, they define what digital money COULD BE — cryptographic protocols, ledger formats, transmission standards — before any institution deploys it. Their conceptual choices (public-key cryptography, blind signatures, distributed ledgers) become the vocabulary everyone after them must use or explicitly reject. They set the terms of the conversation without needing to hold power over deployment.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_computer_scientists_and_cryptographers, agenda_setter,
    institutional, civilizational, analytical, global).

% Monitor and absorb the emerging conceptual frameworks early, running internal feasibility studies well before public rollout. This early positional advantage lets them shape which conceptual variant of digital money becomes institutionally legible, protecting their own settlement monopolies against variants (like fully anonymous cash-equivalents) that would threaten monetary control.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, central_bank_research_departments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__became_thinkable_reading, central_bank_research_departments, beneficiary).

% Positioned to capture first-mover advantage once the conceptual groundwork exists — their existing infrastructure and regulatory relationships let them translate the new conceptual possibility into proprietary rails before rivals or the public can. They benefit from a threshold that others do not yet see as crossed.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, large_clearing_and_settlement_institutions, beneficiary,
    powerful, generational, arbitrage, global).

% Had no seat in the labs, standards bodies, or central bank working groups where digital money's conceptual architecture was decided. The resulting designs assume bank accounts, credit histories, and stable identity documents as entry conditions — assumptions baked in at the conceptual stage that later become nearly impossible to dislodge once institutionalized.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, unbanked_populations_excluded_from_framing, payer,
    powerless, generational, trapped, global).

% Alternative conceptions of value transfer, informal credit networks, and community-based settlement systems were not consulted when the technical conceivability threshold was crossed in Western research institutions. Their absence from the founding conceptual moment means digital money's architecture reflects one civilizational assumption about what money is, universalized by default.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, non_western_monetary_traditions_excluded_from_framing, payer,
    powerless, civilizational, trapped, global).

% Lacked the research budgets or institutional relationships to participate in the conceptual formation period. By the time digital money's architecture stabilized, they faced adopting standards designed without their input or losing competitive ground to the institutions that shaped those standards from the start.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, small_depository_institutions_without_technical_access, payer,
    moderate, biographical, constrained, regional).

% Would have raised questions about privacy, surveillance capability, and access equity had they been present when the conceptual architecture was set. They enter the conversation only after implementation, when the foundational choices are already load-bearing and costly to reverse.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, consumer_advocates_absent_from_early_design_rooms, excluded,
    moderate, generational, constrained, national).

% Study the archival record of research papers, patent filings, and institutional memos to locate the moment digital money became conceivable, distinct from when it was first used or first regulated. Their account competes with alternative genealogies anchored to first use or to formal regulatory recognition.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishing a shared technical and institutional vocabulary for what digital money could be solves a genuine coordination problem: without common conceptual primitives (cryptographic signing, ledger formats, transmission protocols), no subsequent implementation, standard, or regulation could interoperate.
% TRANSFER_FUNCTION: The arrangement moves definitional authority — the power to say what digital money IS and is not — from the general public and excluded institutions toward the small set of research labs, cryptographers, and central bank departments present when the concept crystallized. That definitional authority later converts into infrastructural and regulatory advantage.
% ABSENT_VOICES: Unbanked populations, non-Western monetary traditions, small depository institutions, and consumer advocates were not present in the research labs or internal central bank working groups where the conceptual architecture formed. They would have argued for alternative primitives — offline usability, anonymity by default, community-ledger models — that the dominant conceptual frame did not prioritize.
% DISAPPEARANCE_RATIONALE: Early architects and central banks would say the conceptual groundwork is now so thoroughly absorbed into subsequent standards and regulation that 'undoing' the origin moment is meaningless — the world already rearranged around it decades ago. Excluded populations and historians of alternative monetary traditions would say the field remains structurally shaped by that early, narrow framing, and that surfacing an alternative origin story is not a return to an unchanged world but a genuine rearrangement of whose monetary imagination counts as history.
% FOUNDING_PROBLEM: Researchers needed a rigorous, implementable answer to the question 'can value be represented, verified, and transferred without physical tokens or a single trusted central ledger operator?' — a problem of cryptographic and institutional feasibility, prior to any question of adoption.
% FOUNDING_PROBLEM_CORROBORATION: Early cryptographers and central bank archives attest the conceptual problem was real and technically hard, and that solving it was necessary groundwork. Independent monetary historians and development economists studying financial inclusion attest that the 'thinkability' framing, examined from outside the labs, functioned simultaneously as a narrowing move — foreclosing alternative conceptions of digital value transfer that were technically thinkable in non-Western or informal-economy contexts but never entered the canonical record because those communities were not asked.
narrative_ontology:disappearance_verdict(digital_money_origin__became_thinkable_reading, contested).
narrative_ontology:founding_problem_status(digital_money_origin__became_thinkable_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__became_thinkable_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__became_thinkable_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52 at interval end) is moderate rather than severe because the founding conceptual work genuinely solved a hard coordination problem — someone had to specify how value transfer without physical tokens could work technically before any institution could build on it. But extraction is non-trivial because the conceptual choices made in that period systematically favored institutional continuity (bank-account-anchored identity, centralized ledger authority) over alternatives that would have served excluded populations, and those choices became load-bearing infrastructure that is now costly to revisit. Suppression (0.44) reflects that alternative conceptual framings were not actively banned so much as never solicited — a suppression-by-absence rather than suppression-by-force, which the theater ratio (0.28, rising modestly) partly captures as feasibility studies increasingly served institutional positioning rather than open technical inquiry as the period progressed.
 *
 * DIRECTIONALITY LOGIC:
 *   Early computer scientists and central bank research departments sit near the beneficiary end: their conceptual labor converts into definitional authority and, for the central banks, protective control over what digital money is permitted to become. Large clearing institutions benefit derivatively by being positioned to exploit the conceptual groundwork once it exists. Unbanked populations, non-Western monetary traditions, and small depository institutions sit near the target end: they bear the cost of an architecture built without their input, an architecture that becomes progressively harder to alter as later implementation and regulation build on top of it. Consumer advocates are marked excluded rather than payer because their absence, not their bearing of transferred costs directly, is the structurally salient fact for this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification resists two symmetrical mislabelings. Reading the conceptual-formation period as pure coordination (a rope) would erase the real asymmetry in who got to shape foundational assumptions and who paid the cost of exclusion later. Reading it as pure extraction (a snare) would erase the genuine, non-trivial technical problem the early researchers solved — the coordination function is real, not cover. Tangled rope captures both: a genuine coordination achievement (shared technical vocabulary enabling later interoperability) that simultaneously transferred definitional power asymmetrically to those present in the room, requiring no reconsideration or accountability mechanism (the active enforcement being the continued dominance of the original design assumptions across all downstream implementations) precisely because the founding period is treated as settled prehistory rather than contestable choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceivability_vs_construction_ambiguity,
    'Was digital money''s conceptual architecture discovered as the unique technically feasible solution, or constructed as one path among several equally feasible alternatives that were simply never pursued?',
    'Archival review of contemporaneous research proposals and rejected design alternatives (e.g., anonymity-preserving or offline-capable schemes proposed but not funded) to establish whether viable alternative conceptual architectures existed and were foreclosed by funding/institutional choices rather than technical infeasibility.',
    'If genuine technically-viable alternatives existed and were foreclosed by institutional preference rather than technical necessity, the extraction component of this reading should be weighted higher — the conceptual ''threshold'' was a choice, not a discovery. If the adopted architecture was genuinely the only feasible path, the coordination component dominates and extraction should be weighted lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceivability_vs_construction_ambiguity, empirical, 'Whether the conceptual architecture was uniquely determined or one of several foreclosed alternatives.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the disagreement between the three sibling readings of the digital_money_origin kernel live — is it a disagreement about facts (when did feasibility exist), about values (what counts as ''money'' existing — conception, use, or recognition), or about which actor''s experience should anchor historical periodization?',
    'This is inherently a conceptual/framing question, not resolvable by additional archival data alone — it would require explicit adjudication among historians of technology, historians of money, and regulatory economists about which anchoring criterion (conceivability, practical use, regulatory recognition) is the historiographically correct one for dating institutional emergence.',
    'If the disagreement is purely conceptual (about periodization criteria), all three readings remain simultaneously valid for different purposes and none forecloses the others — consistent with the coexists_with relations declared here. If deeper analysis revealed the readings actually disagree about verifiable facts (e.g., disputed patent dates), the readings could converge rather than remain permanently plural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the three-reading kernel split is a genuine framing pluralism or a resolvable factual dispute.').

omega_variable(
    exclusion_intentionality_ambiguity,
    'Was the exclusion of non-Western monetary traditions and unbanked populations from the conceptual formation period an active suppression (deliberate gatekeeping of research funding, publication venues, standards bodies) or a passive artifact of where computing and cryptographic research happened to be institutionally concentrated in this period?',
    'Comparative institutional history: examine whether alternative research programs from excluded regions/communities existed and were rejected/defunded (active) versus whether no comparable research infrastructure existed anywhere else to be excluded from (passive/structural).',
    'Active suppression would support a higher suppression score and strengthen the case for treating the exclusion as central to the constraint''s extractive character. Passive/structural absence would suggest the exclusion, while real in its consequences, is better modeled as a downstream inequality this constraint reproduces rather than one it actively manufactures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_intentionality_ambiguity, empirical, 'Whether exclusion from the conceptual formation period was actively enforced or structurally inherited.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 1960, 1990).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1960, digital_money_origin__became_thinkable_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(digi_tr_t1965, digital_money_origin__became_thinkable_reading, theater_ratio, 1965, 0.13).
narrative_ontology:measurement(digi_tr_t1970, digital_money_origin__became_thinkable_reading, theater_ratio, 1970, 0.16).
narrative_ontology:measurement(digi_tr_t1975, digital_money_origin__became_thinkable_reading, theater_ratio, 1975, 0.19).
narrative_ontology:measurement(digi_tr_t1980, digital_money_origin__became_thinkable_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(digi_tr_t1985, digital_money_origin__became_thinkable_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(digi_tr_t1990, digital_money_origin__became_thinkable_reading, theater_ratio, 1990, 0.28).

% Extraction over time
narrative_ontology:measurement(digi_be_t1960, digital_money_origin__became_thinkable_reading, base_extractiveness, 1960, 0.31).
narrative_ontology:measurement(digi_be_t1965, digital_money_origin__became_thinkable_reading, base_extractiveness, 1965, 0.36).
narrative_ontology:measurement(digi_be_t1970, digital_money_origin__became_thinkable_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(digi_be_t1975, digital_money_origin__became_thinkable_reading, base_extractiveness, 1975, 0.44).
narrative_ontology:measurement(digi_be_t1980, digital_money_origin__became_thinkable_reading, base_extractiveness, 1980, 0.47).
narrative_ontology:measurement(digi_be_t1985, digital_money_origin__became_thinkable_reading, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(digi_be_t1990, digital_money_origin__became_thinkable_reading, base_extractiveness, 1990, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1960, digital_money_origin__became_thinkable_reading, suppression_requirement, 1960, 0.2).
narrative_ontology:measurement(digi_su_t1965, digital_money_origin__became_thinkable_reading, suppression_requirement, 1965, 0.25).
narrative_ontology:measurement(digi_su_t1970, digital_money_origin__became_thinkable_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(digi_su_t1975, digital_money_origin__became_thinkable_reading, suppression_requirement, 1975, 0.34).
narrative_ontology:measurement(digi_su_t1980, digital_money_origin__became_thinkable_reading, suppression_requirement, 1980, 0.38).
narrative_ontology:measurement(digi_su_t1985, digital_money_origin__became_thinkable_reading, suppression_requirement, 1985, 0.41).
narrative_ontology:measurement(digi_su_t1990, digital_money_origin__became_thinkable_reading, suppression_requirement, 1990, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__became_thinkable_reading, information_standard).
narrative_ontology:boltzmann_floor_override(digital_money_origin__became_thinkable_reading, 0.05).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This constraint is the earliest-dated member of the digital_money_origin kernel family (conceivability-anchored). It structurally precedes and influences digital_money_origin__first_held_reading (the conceptual architecture constrains what forms of practical use become available to hold) and digital_money_origin__regulatory_recognition_reading (the conceptual vocabulary becomes the categories regulators later formalize). Each sibling story authors its own ε, beneficiary/victim structure, and claimed_type; they are not the same constraint measured differently — see commentary.kernel_context for the decomposition rationale.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
