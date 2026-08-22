% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_occupation, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: temple_sacrifice_obligation__study_as_occupation
 *   human_readable: Study of Sacrifice Law as Legitimate Occupation of the Temple Obligation
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   Study of sacrifice law in the Talmud and post-Talmudic tradition is
 *   understood by the rabbinic authority structure as legitimate occupancy of
 *   the Temple sacrifice obligation during the period of Temple absence. The
 *   constraint treats intellectual mastery of the sacrificial corpus as the
 *   current form through which the eternal obligation remains bound and
 *   fulfilled. This reading instantiates one response to the foundational
 *   problem: the obligation cannot be abandoned (it is Torah); it cannot be
 *   physically performed (the Temple does not exist); therefore study
 *   constitutes its legitimate occupation until messianic restoration. Low
 *   extractiveness reflects the genuine coordination function (resolving an
 *   impossible obligation) and the absence of a victim class (those who study
 *   benefit from the framework, not from extraction). The authority structure
 *   absorbs the impossible situation (Temple absence) without surfacing the
 *   need for revision to the underlying obligation — an interpretive buffer
 *   that maintains normative continuity.
 *
 * KEY AGENTS:
 *   - Rabbinic learning community: Institutional authority that sets and transmits the interpretation; collects epistemic legitimacy.
 *   - Jewish practitioners: Organized beneficiaries who fulfill the obligation through study participation.
 *   - Temple authority tradition: Non-agent observer — the implicit hermeneutical continuity that grounds legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.15).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.08).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Study of Sacrifice Law as Legitimate Occupation of the Temple Obligation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious/halakhic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, '140f8455-e2ee-4f98-8cdb-d61ee1b4d239').
narrative_ontology:cs_kernel_codification('140f8455-e2ee-4f98-8cdb-d61ee1b4d239', fixed_text).
narrative_ontology:cs_authority_grounding('140f8455-e2ee-4f98-8cdb-d61ee1b4d239', lineage).
narrative_ontology:cs_interpretation_layer_present('140f8455-e2ee-4f98-8cdb-d61ee1b4d239').
narrative_ontology:cs_reading_relation('140f8455-e2ee-4f98-8cdb-d61ee1b4d239', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('140f8455-e2ee-4f98-8cdb-d61ee1b4d239', temple_sacrifice_obligation__study_as_archiving, influences).
narrative_ontology:cs_axiom('140f8455-e2ee-4f98-8cdb-d61ee1b4d239', foundational, study_occupies_obligation_directly).
narrative_ontology:cs_axiom_status(study_occupies_obligation_directly, holdable).
narrative_ontology:cs_axiom_grounding('140f8455-e2ee-4f98-8cdb-d61ee1b4d239', study_occupies_obligation_directly, deontological).
narrative_ontology:cs_axiom('140f8455-e2ee-4f98-8cdb-d61ee1b4d239', foundational, hermeneutical_continuity_with_temple_law).
narrative_ontology:cs_axiom_status(hermeneutical_continuity_with_temple_law, holdable).
narrative_ontology:cs_axiom_grounding('140f8455-e2ee-4f98-8cdb-d61ee1b4d239', hermeneutical_continuity_with_temple_law, conventional).
narrative_ontology:cs_reference_frame('140f8455-e2ee-4f98-8cdb-d61ee1b4d239', talmudic_hermeneutical_continuity).
narrative_ontology:cs_drift_state('140f8455-e2ee-4f98-8cdb-d61ee1b4d239', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('140f8455-e2ee-4f98-8cdb-d61ee1b4d239', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, rabbinic_learning_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, jewish_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets, transmits, and elaborates Talmudic and post-Talmudic sacrifice law through study, commentary, and legal reasoning. Treats the obligation to know and preserve this law as continuous with the Temple's sacrificial function. Collects intellectual authority and maintains institutional continuity through mastery of the corpus.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, rabbinic_learning_community, agenda_setter,
    institutional, civilizational, mobile, universal).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_occupation, rabbinic_learning_community, beneficiary).

% Fulfill the sacrificial obligation through intellectual participation in study. The framework allows them to maintain ritual purity of intent and halakhic compliance during the Temple's absence. They benefit from the interpretive elaboration and the structure that allows continued observance without physical performance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, jewish_practitioners, beneficiary,
    organized, generational, constrained, universal).

% The implicit authority structure grounding legitimacy in Torah prescription and Talmudic precedent. Treated as continuous with Temple law through hermeneutical application.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, temple_authority_tradition, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__study_as_occupation, temple_authority_tradition).

% The implicit temporal horizon within which this reading operates: study constitutes occupation of the obligation pending restoration, not permanent substitution.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, messianic_restoration_expectation, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__study_as_occupation, messianic_restoration_expectation).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains continuity of sacrificial law, knowledge, and intent during the period when the Temple does not exist and physical sacrifice is impossible. Coordinates the rabbinic and practitioner communities around a shared framework that preserves the obligation's normative force without violating the practical impossibility.
% TRANSFER_FUNCTION: Moves authority and epistemic legitimacy from Temple priesthood (now structurally absent) to the rabbinic learning community through interpretive mastery of the sacrificial corpus. The constraint transfers the locus of obligation-fulfillment from physical performance to intellectual study.
% ABSENT_VOICES: The priestly class whose functional role is displaced by the rabbinic substitution; competing readings that treat study as archiving (knowledge preservation only) rather than occupation (direct fulfillment); theologies that treat the obligation as suspended rather than occupied. These voices are marginal in the mainstream rabbinic consensus but represent coherent alternatives.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared — if study were no longer treated as occupying the obligation — the rabbinic framework for interpreting Temple law would face a crisis of meaning: either the obligation persists unfulfilled (demanding alternative restoration mechanisms), or it lapses (calling the entire sacrificial corpus into question as inapplicable). The world would require a new legitimacy claim.
% FOUNDING_PROBLEM: The Temple was destroyed in 70 CE. The sacrificial system prescribed in Torah cannot be performed. Yet the obligation to perform sacrifice is stated as eternal and binding. The obligation cannot be abandoned (it is Torah law); it cannot be performed (the Temple and priesthood do not exist); the contradiction must be resolved within the authority structure.
% FOUNDING_PROBLEM_CORROBORATION: This founding problem is attested by every major rabbinic authority from Talmudic through contemporary times; it appears as the explicit motivating question in the Mishnah and Gemara (BT Menachot 110a and surrounding passages); mainstream Jewish law codes (Maimonides, Shulchan Aruch) treat it as structurally unresolved and requiring continuous reinterpretation. The founding problem is also attested by scholars outside the rabbinic tradition (historians, phenomenologists of religion) as a genuine contradiction requiring explanation.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_occupation, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_occupation, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_occupation_tests).
:- end_tests(temple_sacrifice_obligation__study_as_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint solves a genuine coordination problem (what to do with an impossible obligation) without a victim class. Beneficiaries (rabbinic scholars, practitioners) collect authority and meaning, not rents extracted from those harmed. Suppression is minimal (0.08) because the constraint does not require coercive enforcement — alternative readings exist (messianic suspension, study as archiving) but are not defended by suppressive machinery. Theater is low (0.12) because intellectual study is the genuine substantive activity; performance is not merely theatrical maintenance. The measurements show remarkable stability over 2000 years — extractiveness hovering near 0.15 and theater near 0.12 — reflecting the constraint's durability and consensual adoption. The slight uptick in theater ratio over the first 500 years reflects the gradual elaboration of study protocols and textual commentary, adding formal ritual dimensions to the interpretive act, then stabilizing.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic authority seat and the practitioner seat should compute similarly from the structural data: both benefit from the framework, both hold low directionality. The engine should compute rope or weak-rope from both seats, not divergent types. The main seat-level divergence is epistemological authority: the rabbinic seat determines meaning; the practitioner seat receives it. But this is authority structure asymmetry (legitimate within a coordination framework), not extraction asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic learning community sits at low directionality (near 0.2 — beneficiary seat): they set the rules, transmit authority, and collect epistemic legitimacy. Jewish practitioners sit near symmetric (0.45–0.55): they fulfill the obligation through study (genuine benefit) but must participate in the framework as it is structured by the rabbinic authority. Neither seat is a target (high directionality) because there is no extraction mechanism — no one is paying a cost that flows elsewhere. The constraint's persistence depends on consensual adoption, not coercion.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is NOT present. The founding obligation (sacrifice the prescribed offerings) remains live, stated as eternal in Torah. The constraint (study occupies the obligation) is explicitly transient in this reading — it is meaningful ONLY pending messianic restoration. The rabbinic authority structure does not claim that study REPLACES sacrifice permanently; it claims study IS the occupation UNTIL the Temple returns. The framework explicitly absorbs the impossibility without rewriting the underlying obligation. Mandatrophy resolution is not needed here because the framework is reflexively aware of its own temporality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_occupation_vs_archiving_boundary,
    'Is study of sacrifice law genuinely fulfilling the obligation (occupying it, discharging it), or is it preserving knowledge while the obligation remains technically unfulfilled?',
    'Exegetical analysis of the authority sources: do they frame study as occupation (literal fulfillment) or as preservation (temporary placeholder until restoration)? Does mainstream rabbinic reasoning distinguish between learning the laws and actually performing the prescribed acts, and if so, on what basis?',
    'If study is occupation (literal fulfillment), the constraint is rope-classified and the obligation is satisfied through study. If study is archiving (knowledge preservation), the obligation remains technically unfulfilled and the constraint becomes more architecturally tangled — study is performing a function but not the obligation itself, which remains suspended or deferred.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_as_occupation_vs_archiving_boundary, empirical, 'Whether study fulfills the obligation or merely preserves it.').

omega_variable(
    messianic_temporality_embedded_or_explicit,
    'Is the messianic temporal horizon (restoration pending) explicitly part of the constraint''s structure, or is it implicit background that the authority structure does not surface?',
    'Survey of rabbinic texts: do authorities regularly invoke or mention messianic restoration as the frame within which study occupies the obligation, or is restoration background/assumed? Does the constraint become weaker or change type if the messianic horizon is removed?',
    'If the messianic horizon is explicit, the constraint is self-consciously transient and less likely to accumulate extraction (it cannot become permanent). If implicit, the constraint has organizational ambiguity: is study occupation or suspension? The constraint could drift toward piton (permanent form defended by inertia) if the restoration expectation atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_temporality_embedded_or_explicit, conceptual, 'Whether messianic restoration is structural to this reading or background assumption.').

omega_variable(
    reading_consensus_boundary,
    'How much institutional disagreement exists about whether study truly occupies the obligation? Do all major rabbinic authorities hold this reading, or is there significant dissent within the tradition?',
    'Comparative textual analysis of rabbinic codes and responsa literature: what fraction of major authorities explicitly endorse study as occupation? What do dissenters argue instead?',
    'Unanimous consensus would strengthen the rope classification (genuine coordination function, broad acceptance). Significant dissent would suggest the constraint is enforced by authority rather than consensus, raising suppression and extractiveness scores and potentially shifting the type toward tangled rope (some benefit from the reading, some are coordinated into it).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_consensus_boundary, empirical, 'Degree of authority consensus on study as occupancy.').

omega_variable(
    reading_kernel_foreclosure_ambiguity,
    'Does the study_as_occupation reading logically foreclose the messianic_suspension reading, or can both coexist within a single framework held by different authorities?',
    'Logical analysis: if study truly occupies the obligation, can the obligation simultaneously be in suspension? Or are these two fundamentally incompatible claims about the obligation''s status?',
    'If foreclosing: this reading rules out suspension within any framework that adopts it; the relation to messianic_suspension is forecloses. If compatible: different authorities can hold both readings simultaneously; the relation is coexists_with. The distinction affects how the constraint family decomposes across the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_foreclosure_ambiguity, conceptual, 'Logical foreclosure relationship between study_as_occupation and messianic_suspension readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(temp_tr_t0, projected).
narrative_ontology:measurement(temp_tr_t250, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 250, 0.09).
narrative_ontology:measurement_basis(temp_tr_t250, observed).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 500, 0.1).
narrative_ontology:measurement_basis(temp_tr_t500, observed).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1000, 0.11).
narrative_ontology:measurement_basis(temp_tr_t1000, observed).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1500, 0.12).
narrative_ontology:measurement_basis(temp_tr_t1500, observed).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 2000, 0.12).
narrative_ontology:measurement_basis(temp_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(temp_be_t0, projected).
narrative_ontology:measurement(temp_be_t250, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 250, 0.14).
narrative_ontology:measurement_basis(temp_be_t250, observed).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 500, 0.15).
narrative_ontology:measurement_basis(temp_be_t500, observed).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1000, 0.16).
narrative_ontology:measurement_basis(temp_be_t1000, observed).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1500, 0.15).
narrative_ontology:measurement_basis(temp_be_t1500, observed).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement_basis(temp_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(temp_su_t0, projected).
narrative_ontology:measurement(temp_su_t250, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 250, 0.06).
narrative_ontology:measurement_basis(temp_su_t250, observed).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 500, 0.07).
narrative_ontology:measurement_basis(temp_su_t500, observed).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 1000, 0.08).
narrative_ontology:measurement_basis(temp_su_t1000, observed).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 1500, 0.08).
narrative_ontology:measurement_basis(temp_su_t1500, observed).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 2000, 0.08).
narrative_ontology:measurement_basis(temp_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_occupation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__study_as_occupation, 0.12).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__messianic_suspension).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the temple_sacrifice_obligation kernel. The kernel is the Scriptural obligation to perform sacrifices in the Temple; it is contested because the Temple no longer exists and sacrifice cannot be physically performed. This reading (study_as_occupation) claims study fulfills the obligation. Sibling readings are: messianic_suspension (obligation suspended pending restoration, not fulfilled by study) and study_as_archiving (study preserves knowledge but does not fulfill the obligation). All three share the same referent (the standing arrangement under constraint: the obligation to sacrifice) and differ in what satisfies it. Each reading has its own ε value: study_as_occupation has low ε (genuine coordination function, no victims); messianic_suspension likely has moderate ε (obligation deferred creates interpretive strain); study_as_archiving likely has moderate-to-high ε (knowledge preserved but obligation unfulfilled creates asymmetric burden on those who know the laws but cannot perform). The constraint family is linked via network.affects_constraints; each reading is a separate constraint story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
