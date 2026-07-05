% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__became_thinkable_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: electronic_money_emergence__became_thinkable_reading
 *   human_readable: Digital Money's Conceptual-Thinkability Emergence Reading
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   From the 1960s onward, electronic funds transfer systems, magnetic-stripe
 *   infrastructure, and cryptographic value-transfer schemes made 'money that
 *   is not physical currency' a thinkable, workable idea among engineers and
 *   researchers, well before any central bank statistical category (like M4
 *   vs M5 aggregates) or any single institution's ledger entry marked a
 *   formal threshold. This reading treats emergence as a slow diffusion of
 *   conceptual and technical possibility through research and engineering
 *   communities, with institutional recognition arriving as a decades-later
 *   lagging confirmation rather than the emergence event itself.
 *
 * KEY AGENTS:
 *   - early_electronic_payment_technologists: Primary beneficiary (moderate/mobile) — credited with founding technical groundwork
 *   - computer_science_and_cryptography_researchers: Primary beneficiary (moderate/mobile) — credited with conceptual priority
 *   - monetary_theorists_of_dematerialization: Analytical beneficiary (analytical/analytical) — intellectual vindication of diffusion historiography
 *   - central_bank_statisticians: Excluded institutional actor (institutional/constrained) — sidelined despite holding the measurement record
 *   - first_institutional_adopters: Excluded organized actor (organized/constrained) — sidelined despite rival claim to founding moment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__became_thinkable_reading, 0.18).
domain_priors:suppression_score(electronic_money_emergence__became_thinkable_reading, 0.12).
domain_priors:theater_ratio(electronic_money_emergence__became_thinkable_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__became_thinkable_reading, rope).
narrative_ontology:human_readable(electronic_money_emergence__became_thinkable_reading, "Digital Money's Conceptual-Thinkability Emergence Reading").
narrative_ontology:topic_domain(electronic_money_emergence__became_thinkable_reading, "economic_history/monetary_theory/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__became_thinkable_reading, '11e58664-237b-43b7-aa77-241708556381').
narrative_ontology:cs_kernel_codification('11e58664-237b-43b7-aa77-241708556381', distributed).
narrative_ontology:cs_authority_grounding('11e58664-237b-43b7-aa77-241708556381', distributed).
narrative_ontology:cs_reading_relation('11e58664-237b-43b7-aa77-241708556381', electronic_money_emergence__first_held_reading, coexists_with).
narrative_ontology:cs_reading_relation('11e58664-237b-43b7-aa77-241708556381', electronic_money_emergence__m4_m5_collapse_reading, influences).
narrative_ontology:cs_axiom('11e58664-237b-43b7-aa77-241708556381', foundational, conceptual_thinkability_precedes_institutional_recognition).
narrative_ontology:cs_axiom_status(conceptual_thinkability_precedes_institutional_recognition, holdable).
narrative_ontology:cs_axiom_grounding('11e58664-237b-43b7-aa77-241708556381', conceptual_thinkability_precedes_institutional_recognition, empirically_contingent).
narrative_ontology:cs_axiom('11e58664-237b-43b7-aa77-241708556381', foundational, emergence_is_gradual_diffusion_not_discrete_event).
narrative_ontology:cs_axiom_status(emergence_is_gradual_diffusion_not_discrete_event, holdable).
narrative_ontology:cs_axiom_grounding('11e58664-237b-43b7-aa77-241708556381', emergence_is_gradual_diffusion_not_discrete_event, conventional).
narrative_ontology:cs_reference_frame('11e58664-237b-43b7-aa77-241708556381', diffusion_based_technological_emergence).
narrative_ontology:cs_drift_state('11e58664-237b-43b7-aa77-241708556381', post_1990s_historiographical_consolidation, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('11e58664-237b-43b7-aa77-241708556381', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, early_electronic_payment_technologists).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, computer_science_and_cryptography_researchers).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, monetary_theorists_of_dematerialization).
narrative_ontology:constraint_vindicates(electronic_money_emergence__became_thinkable_reading, diffusion_model_of_monetary_innovation).
narrative_ontology:constraint_vindicates(electronic_money_emergence__became_thinkable_reading, conceptual_priority_over_institutional_measurement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engineers and systems designers working from the 1960s through the 1980s on electronic funds transfer, magnetic stripe cards, and early network payment protocols. They benefit from a reading that credits their conceptual and technical groundwork as the true site of emergence, ahead of when regulators or statisticians noticed anything worth counting. Their work diffused through banking back-offices and academic conferences well before any institution formally recognized a new monetary category.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, early_electronic_payment_technologists, beneficiary,
    moderate, generational, mobile, global).

% Researchers exploring digital signatures, blind signatures, and early cryptographic cash schemes (Chaum-era and prior) who established that electronic value transfer was conceptually and mathematically thinkable well before any central bank or statistical agency built an instrument to measure it. Their intellectual priority is elevated by this reading and diminished by a reading that dates emergence to first institutional custody.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, computer_science_and_cryptography_researchers, beneficiary,
    moderate, generational, mobile, global).

% Historians and theorists of money who argue that monetary forms emerge as social and technical possibilities before they become countable categories. This reading validates their diffusion-based historiography over event-based or measurement-based accounts; they have no material stake beyond intellectual vindication.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, monetary_theorists_of_dematerialization, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__became_thinkable_reading, monetary_theorists_of_dematerialization, observer).

% Statistical agencies whose M4/M5 aggregation work is the subject of a rival reading of this same kernel. Under the became_thinkable reading, their measurement categories are epiphenomenal to something that already happened diffusely; they are not consulted in this reading's account of when emergence occurred, even though their instruments are what most economic historians actually cite as evidence of electronic money's arrival.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, central_bank_statisticians, excluded,
    institutional, biographical, constrained, national).

% Banks, clearinghouses, or firms that were the first to formally hold or record dematerialized currency in institutional ledgers. The rival first_held_reading centers their moment; this reading treats their formal adoption as a lagging institutional confirmation of something already thinkable, sidelining their claim to be the emergence event.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, first_institutional_adopters, excluded,
    organized, biographical, constrained, national).

% Scholars who study how statistical categories construct rather than merely record economic reality. They watch the contest between the three readings of this kernel and can adjudicate, without a stake in any one reading's victory, whether emergence is better modeled as diffusion, event, or artifact.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, economic_historians_of_measurement, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared historical account of when a monetary technology became real, so that later regulatory, legal, and academic treatments of electronic money have a coherent origin story to build policy and scholarship on.
% TRANSFER_FUNCTION: Moves intellectual and historical credit toward technologists, cryptographers, and diffusion-theorists, and away from institutions and statisticians who might otherwise claim the founding moment for their own ledgers and instruments.
% ABSENT_VOICES: Central bank statisticians and first institutional adopters are structurally sidelined by this reading's own logic — their formal records are treated as lagging confirmations rather than the site of emergence itself, even though they hold the documentary evidence most historians actually cite.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the underlying diffusion of electronic payment concepts and technologies through the 1960s-1980s would remain historical fact; what would change is which community's origin story anchors textbooks, legal precedent on 'electronic money' definitions, and priority disputes in the history of computing. Whether that rearranges anything material is genuinely disputed among the three readings' respective communities.
% FOUNDING_PROBLEM: Historians and monetary theorists needed to explain why electronic money seemed to appear gradually and diffusely across decades rather than at a single legally or statistically identifiable moment, and needed a framework that did not force a false precision onto a genuinely diffuse technological and conceptual process.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology outside the beneficiary set (e.g., scholars of general-purpose technology diffusion in other domains such as electrification and telecommunications) corroborate that gradual conceptual-to-technical diffusion, decoupled from formal measurement adoption, is a recurring and independently observed pattern in technology history, not unique to money.
narrative_ontology:disappearance_verdict(electronic_money_emergence__became_thinkable_reading, contested).
narrative_ontology:founding_problem_status(electronic_money_emergence__became_thinkable_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__became_thinkable_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__became_thinkable_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__became_thinkable_reading_tests).
:- end_tests(electronic_money_emergence__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction and suppression are both low and rise only mildly over the interval (0.05→0.18 and near-negligible respectively) because this is fundamentally a historiographical-priority contest, not a resource-extraction structure: no party is coerced and no material rents are collected through the reading's operation. The modest theater_ratio increase (0.10→0.22) reflects growing retrospective narrative-building — as diffusion historiography matured into an academic subfield, some performative curation of the 'thinkability' narrative accreted, but the underlying claim remains substantially a genuine descriptive account rather than an extraction cover story. accessibility_collapse is moderate (0.35) because rival readings (first_held, m4_m5_collapse) remain fully live and contested; this is not a mountain — alternative framings persist robustly, which is exactly what the low resistance and moderate collapse jointly indicate.
 *
 * PERSPECTIVAL GAP:
 *   From the technologist/cryptographer seat, this reading is straightforwardly correct and coordination-generating: it lets a real technical history cohere. From the central-bank-statistician seat, the same reading looks like it discounts the only evidence they actually produced (formal statistical series), producing a seat-divergence in credibility assessment even though no material extraction occurs — this is a pure recognition-contest divergence, not an extraction divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are technologists, cryptographers, and diffusion-theorists who gain intellectual credit and historiographical validation from this reading being accepted — their directionality sits near the beneficiary end because the reading's acceptance costs them nothing and confers status. There are no true victims in the extractive sense; central bank statisticians and first institutional adopters are excluded from narrative centrality but bear no material cost, which is why victims[] is empty despite two excluded stakeholder seats — exclusion from a historiographical credit-allocation is not extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — explaining a genuinely diffuse technological emergence without forcing false precision — remains live; this is not a mandatrophied structure because the reading continues to do real explanatory work for historians of technology and has not ossified into unchallenged orthodoxy (the sibling readings remain fully active competitors, which is the diagnostic sign that mandatrophy has not set in).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Is the disagreement between became_thinkable_reading, first_held_reading, and m4_m5_collapse_reading a genuine empirical dispute about when emergence occurred, or a definitional dispute about what ''emergence'' means for a monetary technology?',
    'Comparative historiographical analysis: if all three readings agree on the same underlying timeline of events (technical breakthroughs, institutional adoptions, statistical category creation) and differ only in which event they designate as ''the'' emergence, the dispute is definitional. If they disagree about the actual dates or sequence of underlying events, it is empirical.',
    'A definitional resolution would mean all three readings are compatible descriptions of the same history under different framing conventions (coexists_with is then clearly correct for all pairs); an empirical resolution would require adjudicating which reading''s evidentiary timeline is more accurate, potentially foreclosing one reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the three-reading contest over electronic money''s emergence is definitional or empirical.').

omega_variable(
    diffusion_measurement_lag_magnitude,
    'How many years actually separated conceptual/technical thinkability (e.g., early cryptographic cash proposals, EFT systems) from the first formal institutional measurement categories (M4/M5-type distinctions)?',
    'Archival dating of specific technical milestones (e.g., Chaum''s blind signature papers, early EFT deployment dates) against the documented history of when statistical agencies formally introduced electronic-money-distinguishing categories.',
    'A short lag (a few years) would weaken this reading''s distinctiveness from first_held_reading; a long lag (decades, as the reading claims) would strongly support treating conceptual thinkability as structurally prior and analytically distinct from institutional measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffusion_measurement_lag_magnitude, empirical, 'The actual time gap between conceptual thinkability and institutional measurement, which this reading''s core claim depends on.').

omega_variable(
    beneficiary_group_naturalness,
    'Are the beneficiaries of this reading (technologists, cryptographers, diffusion-theorists) beneficiaries of a genuinely correct historical account, or is ''conceptual priority'' a self-serving credit-allocation narrative constructed by and for the research communities who did the early technical work?',
    'Independent assessment by historians of technology outside monetary economics of whether diffusion-based emergence accounts in OTHER technology domains (electrification, telecommunications) show the same pattern, which would suggest the framework is a general historiographical tool rather than a special pleading for this particular research community.',
    'If the diffusion framework is a general, domain-independent historiographical pattern, the beneficiary structure is incidental to a correct account. If it is specific to monetary technology and correlates suspiciously with which community gets credited, the reading functions partly as a status-allocation mechanism for its beneficiaries despite low measured extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_group_naturalness, conceptual, 'Whether the reading''s beneficiary structure reflects genuine historical accuracy or motivated credit-allocation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__became_thinkable_reading, 1960, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1960, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement_basis(elec_tr_t1960, observed).
narrative_ontology:measurement(elec_tr_t1968, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1968, 0.13).
narrative_ontology:measurement_basis(elec_tr_t1968, observed).
narrative_ontology:measurement(elec_tr_t1976, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1976, 0.16).
narrative_ontology:measurement_basis(elec_tr_t1976, observed).
narrative_ontology:measurement(elec_tr_t1984, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1984, 0.19).
narrative_ontology:measurement_basis(elec_tr_t1984, observed).
narrative_ontology:measurement(elec_tr_t1992, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1992, 0.21).
narrative_ontology:measurement_basis(elec_tr_t1992, observed).
narrative_ontology:measurement(elec_tr_t2000, electronic_money_emergence__became_thinkable_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement_basis(elec_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(elec_be_t1960, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1960, 0.05).
narrative_ontology:measurement_basis(elec_be_t1960, observed).
narrative_ontology:measurement(elec_be_t1968, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1968, 0.08).
narrative_ontology:measurement_basis(elec_be_t1968, observed).
narrative_ontology:measurement(elec_be_t1976, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1976, 0.11).
narrative_ontology:measurement_basis(elec_be_t1976, observed).
narrative_ontology:measurement(elec_be_t1984, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1984, 0.14).
narrative_ontology:measurement_basis(elec_be_t1984, observed).
narrative_ontology:measurement(elec_be_t1992, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1992, 0.16).
narrative_ontology:measurement_basis(elec_be_t1992, observed).
narrative_ontology:measurement(elec_be_t2000, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement_basis(elec_be_t2000, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(electronic_money_emergence__became_thinkable_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__became_thinkable_reading, information_standard).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__became_thinkable_reading, 0.03).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence__first_held_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the electronic_money_emergence kernel. first_held_reading locates emergence at first institutional custody of dematerialized currency (an event-based account); m4_m5_collapse_reading treats the M4/M5 statistical distinction itself as retroactively constructing the category (a measurement-artifact account). This reading (became_thinkable_reading) treats emergence as a decades-long diffusion of conceptual and technical possibility that precedes and is independent of both institutional custody and statistical measurement. The three readings share no single ε — each has its own beneficiary structure and its own claimed_type, linked here via network edges rather than reconciled into one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
