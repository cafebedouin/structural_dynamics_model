% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__conceptual_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Statute of Anne: Copyright as Limited Regulatory Tool (Conceptual Emergence Reading)
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'conceptual emergence' reading of
 *   the Statute of Anne, focusing on how the statute created a new conceptual
 *   space for copyright as a limited regulatory tool for public learning,
 *   rather than a perpetual property right. It emphasizes the shift in legal
 *   thought and the establishment of foundational principles that would shape
 *   future intellectual property law. The statute actively suppressed the
 *   prior regime of perpetual publisher monopolies, while coordinating author
 *   incentives with public benefit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.35).
domain_priors:suppression_score(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.75).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__conceptual_emergence_reading, rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__conceptual_emergence_reading, "Statute of Anne: Copyright as Limited Regulatory Tool (Conceptual Emergence Reading)").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__conceptual_emergence_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__conceptual_emergence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'f7e12628-c6c8-4297-990c-5f58c54aa88e').
narrative_ontology:cs_kernel_codification('f7e12628-c6c8-4297-990c-5f58c54aa88e', fixed_text).
narrative_ontology:cs_authority_grounding('f7e12628-c6c8-4297-990c-5f58c54aa88e', lineage).
narrative_ontology:cs_interpretation_layer_present('f7e12628-c6c8-4297-990c-5f58c54aa88e').
narrative_ontology:cs_reading_relation('f7e12628-c6c8-4297-990c-5f58c54aa88e', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_reading_relation('f7e12628-c6c8-4297-990c-5f58c54aa88e', statute_of_anne_ip_foundation__entangled_event_reading, forecloses).
narrative_ontology:cs_axiom('f7e12628-c6c8-4297-990c-5f58c54aa88e', foundational, copyright_as_limited_term_incentive).
narrative_ontology:cs_axiom_status(copyright_as_limited_term_incentive, holdable).
narrative_ontology:cs_axiom_grounding('f7e12628-c6c8-4297-990c-5f58c54aa88e', copyright_as_limited_term_incentive, instrumental).
narrative_ontology:cs_axiom('f7e12628-c6c8-4297-990c-5f58c54aa88e', foundational, public_domain_as_ultimate_goal).
narrative_ontology:cs_axiom_status(public_domain_as_ultimate_goal, holdable).
narrative_ontology:cs_axiom_grounding('f7e12628-c6c8-4297-990c-5f58c54aa88e', public_domain_as_ultimate_goal, deontological).
narrative_ontology:cs_reference_frame('f7e12628-c6c8-4297-990c-5f58c54aa88e', public_benefit_regulatory_framework).
narrative_ontology:cs_drift_state('f7e12628-c6c8-4297-990c-5f58c54aa88e', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f7e12628-c6c8-4297-990c-5f58c54aa88e', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, readers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the eventual entry of works into the public domain, fostering education, innovation, and cultural development. This reading emphasizes the statute's role in creating the conceptual space for this benefit.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning, beneficiary,
    powerless, generational, mobile, universal).

% Gained a statutory right to their works for a limited term, providing an incentive for creation. They 'pay' by having their rights expire, but this is framed as a necessary part of the public bargain.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors, payer).

% Lost their de facto perpetual monopoly over printing and publishing, becoming subject to the new limited term. They resisted the statute fiercely, as it fundamentally altered their business model and power.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company, payer,
    institutional, biographical, trapped, national).

% Enacted the statute, establishing a new legal framework for intellectual property. This body defined the conceptual boundaries of copyright as a limited regulatory tool.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, parliament, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from increased access to a wider variety of books and eventually, the ability to freely copy and adapt works once their copyright term expires. They are the ultimate consumers of the 'learning' facilitated by the statute.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, readers, beneficiary,
    powerless, immediate, mobile, local).

% Analyze the historical and conceptual impact of the Statute of Anne, interpreting its role in the evolution of intellectual property law and its foundational principles.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__conceptual_emergence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the incentives of authors to create new works with the public's interest in accessing and building upon those works after a limited period, by establishing a time-limited exclusive right.
% TRANSFER_FUNCTION: Transfers a temporary exclusive right from the public domain to authors for a limited term, in exchange for eventual public access; it also transferred the power to grant such rights from monopolistic publishers to authors.
% ABSENT_VOICES: Future digital rights holders who would argue for broader, more flexible rights in a networked age; indigenous communities whose traditional knowledge systems are not recognized by this framework.
% DISAPPEARANCE_RATIONALE: If the Statute of Anne and its conceptual framework vanished overnight, the concept of a time-limited, author-centric copyright would not have emerged. This would likely lead to either perpetual publisher monopolies (as before) or a chaotic public domain without author incentives, fundamentally altering the structure of modern IP law.
% FOUNDING_PROBLEM: The problem of perpetual monopolies held by booksellers (Stationers' Company) stifling new creation and public access to knowledge, alongside a lack of clear rights for authors.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and institutional economists widely corroborate the problem of publisher monopolies and the statute's intent to address it, drawing on parliamentary records and contemporary pamphlets from outside the Stationers' Company. The underlying tension between author incentive and public access remains a live debate in IP law.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__conceptual_emergence_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__conceptual_emergence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   The claimed type is 'rope' because the statute primarily functions as a coordination mechanism, balancing author incentives with public access to knowledge. Extraction (0.35) is present for authors (due to the limited term) and for the Stationers' Company (loss of perpetual rights), but it is framed as a necessary cost for the greater public good of learning. Suppression (0.75) is high, reflecting the active legislative force required to dismantle the entrenched perpetual monopoly. Theater ratio is low (0.10) as the statute was a genuine, effective legislative act. Accessibility collapse (0.60) reflects the collapse of perpetual rights while simultaneously opening access to the public domain after the term. Resistance (0.80) was high from the powerful Stationers' Company. The measurements show initial high suppression to overcome resistance, which then stabilizes as the new conceptual space takes hold.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Stationers' Company, the statute was a snare, extracting their established rights. From the perspective of authors and the public, it was a rope, coordinating incentives for a greater good. This story adopts the latter, 'conceptual emergence' reading, which frames the extraction from monopolists as a necessary component of the coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Public learning and authors are the primary beneficiaries, experiencing low effective extraction (or even subsidy) as the constraint enables their interests. The Stationers' Company is the primary target/victim, experiencing high effective extraction as their prior monopoly was suppressed. Parliament acts as the agenda-setter, defining the new conceptual space. Readers are indirect beneficiaries of the system's output.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_institutional_separability,
    'Is the conceptual emergence of copyright as a limited regulatory tool truly separable from the institutional reallocation of rights, or are they inextricably linked?',
    'Detailed historical analysis of parliamentary debates, legal commentaries, and economic impacts to determine if the conceptual shift could have occurred without the specific institutional changes, or vice-versa.',
    'If separable, this reading''s emphasis on conceptual emergence is robust. If inseparable, the ''entangled_event_reading'' gains strength, suggesting a more holistic understanding of the statute''s impact is required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_vs_institutional_separability, conceptual, 'Ambiguity regarding the separability of conceptual and institutional change.').

omega_variable(
    public_benefit_vs_private_enclosure_drift,
    'Has the balance between public learning and author incentive, as conceptually established by the Statute of Anne, been maintained in subsequent IP law, or has it drifted towards greater private enclosure?',
    'Empirical analysis of copyright term extensions, fair use doctrines, and licensing practices over centuries, comparing outcomes against the statute''s original conceptual intent.',
    'If significant drift towards private enclosure is observed, the ''rope'' classification of this foundational concept would need re-evaluation in light of its long-term institutional trajectory, potentially revealing a ''tangled_rope'' or ''snare'' in later periods.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_benefit_vs_private_enclosure_drift, empirical, 'Whether the foundational balance of the Statute of Anne has been preserved or eroded over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__conceptual_emergence_reading, 1710, 1740).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1710, 0.1).
narrative_ontology:measurement(stat_tr_t1715, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1715, 0.1).
narrative_ontology:measurement(stat_tr_t1720, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1720, 0.1).
narrative_ontology:measurement(stat_tr_t1725, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1725, 0.1).
narrative_ontology:measurement(stat_tr_t1730, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1730, 0.1).
narrative_ontology:measurement(stat_tr_t1735, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1735, 0.1).
narrative_ontology:measurement(stat_tr_t1740, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1740, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1710, 0.3).
narrative_ontology:measurement(stat_be_t1715, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1715, 0.32).
narrative_ontology:measurement(stat_be_t1720, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1720, 0.33).
narrative_ontology:measurement(stat_be_t1725, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1725, 0.34).
narrative_ontology:measurement(stat_be_t1730, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1730, 0.35).
narrative_ontology:measurement(stat_be_t1735, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1735, 0.35).
narrative_ontology:measurement(stat_be_t1740, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1740, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1710, 0.85).
narrative_ontology:measurement(stat_su_t1715, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1715, 0.8).
narrative_ontology:measurement(stat_su_t1720, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1720, 0.78).
narrative_ontology:measurement(stat_su_t1725, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1725, 0.76).
narrative_ontology:measurement(stat_su_t1730, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1730, 0.75).
narrative_ontology:measurement(stat_su_t1735, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1735, 0.75).
narrative_ontology:measurement(stat_su_t1740, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1740, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__conceptual_emergence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, copyright_term_extension_acts).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, fair_use_doctrine_interpretation).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, digital_millennium_copyright_act).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'statute_of_anne_ip_foundation' kernel, focusing on the conceptual emergence of copyright as a limited regulatory tool. Sibling readings include 'institutional_reallocation_reading' and 'entangled_event_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
