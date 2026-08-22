% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_archive, []).

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
 *   constraint_id: kodashim_obligation__study_as_archive
 *   human_readable: Kodashim Study as Communal Archive (Defunct-System Reading)
 *   domain: religious/textual_preservation
 *
 * SUMMARY:
 *   The order of Kodashim codifies the sacrificial system of a Temple
 *   destroyed in 70 CE. A standing institutional arrangement nonetheless
 *   directs a substantial share of elite textual study toward this corpus:
 *   yeshiva curricula assign it, examinations test it, and ordination
 *   standards presume mastery of it. This story instantiates the ARCHIVE
 *   reading of that arrangement: the corpus documents a defunct system, and
 *   its study functions as historical preservation and identity-maintenance
 *   rather than as legal obligation or cosmic enactment. On this reading the
 *   arrangement solves a real coordination problem (a dispersed community
 *   maintains cohesion, textual continuity, and boundary-marking through a
 *   shared canonical corpus) while simultaneously diverting scarce
 *   intellectual capital away from applicable law and banking legitimacy for
 *   the institutions that certify mastery. The claim/metric independence rule
 *   applies: claimed_type is authored from the structural analysis (both
 *   coordination and asymmetric extraction, actively enforced), and the
 *   metrics are authored from the arrangement's observed operation; the
 *   engine computes per-seat classifications from the structural data. KEY
 *   AGENTS (by structural relationship): - yeshiva_students: primary target
 *   (powerless/constrained) — bear the opportunity cost of study hours
 *   allocated to a non-operative corpus - ordination_track_students: target
 *   with partial offsetting gain (moderate/identity_locked) — trade years on
 *   non-operative material for communal credentials - rabbinic_leadership:
 *   agenda-setter and principal recipient (institutional/arbitrage) — sets
 *   curricula, administers ordination, accrues the legitimacy the continuity
 *   claim generates - diaspora_jewish_communities: beneficiary
 *   (organized/mobile) — collect identity-maintenance goods without
 *   administering the study system - practical_halakha_seekers: excluded
 *   voice (powerless/constrained) — hold live legal questions outside the
 *   curriculum-setting conversation - academic_jewish_studies_scholars:
 *   analytical observer (moderate/analytical) — historicize the corpus and
 *   corroborate the defunct-system premise from outside the study economy
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, 0.58).
domain_priors:suppression_score(kodashim_obligation__study_as_archive, 0.38).
domain_priors:theater_ratio(kodashim_obligation__study_as_archive, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, extractiveness, 0.58).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_archive, "Kodashim Study as Communal Archive (Defunct-System Reading)").
narrative_ontology:topic_domain(kodashim_obligation__study_as_archive, "religious/textual_preservation").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_archive, '27211228-6c91-42c0-9fad-82ac60d000ba').
narrative_ontology:cs_kernel_codification('27211228-6c91-42c0-9fad-82ac60d000ba', fixed_text).
narrative_ontology:cs_authority_grounding('27211228-6c91-42c0-9fad-82ac60d000ba', lineage).
narrative_ontology:cs_interpretation_layer_present('27211228-6c91-42c0-9fad-82ac60d000ba').
narrative_ontology:cs_reading_relation('27211228-6c91-42c0-9fad-82ac60d000ba', kodashim_obligation__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('27211228-6c91-42c0-9fad-82ac60d000ba', kodashim_obligation__study_as_preparation, forecloses).
narrative_ontology:cs_axiom('27211228-6c91-42c0-9fad-82ac60d000ba', foundational, sacrificial_corpus_normatively_inert).
narrative_ontology:cs_axiom_status(sacrificial_corpus_normatively_inert, holdable).
narrative_ontology:cs_axiom_grounding('27211228-6c91-42c0-9fad-82ac60d000ba', sacrificial_corpus_normatively_inert, empirically_contingent).
narrative_ontology:cs_axiom('27211228-6c91-42c0-9fad-82ac60d000ba', secondary, study_output_is_continuity_not_cosmos).
narrative_ontology:cs_axiom_status(study_output_is_continuity_not_cosmos, holdable).
narrative_ontology:cs_axiom_grounding('27211228-6c91-42c0-9fad-82ac60d000ba', study_output_is_continuity_not_cosmos, instrumental).
narrative_ontology:cs_reference_frame('27211228-6c91-42c0-9fad-82ac60d000ba', corpus_as_completed_record).
narrative_ontology:cs_drift_state('27211228-6c91-42c0-9fad-82ac60d000ba', contemporary_yeshiva_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('27211228-6c91-42c0-9fad-82ac60d000ba', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_archive, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, rabbinic_leadership).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, yeshiva_students).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, ordination_track_students).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, practical_halakha_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, ordination_track_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Spend their prime intellectual years working through sacrificial-law tractates that govern rituals no one can perform. What flows from them is attention and aptitude; what flows to them is membership in the interpretive community and fluency in its canonical method. Leaving the curriculum means leaving the institution, and usually the social world built around it; staying means the applicable-law competence they will eventually need arrives late and thin.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, yeshiva_students, payer,
    powerless, biographical, constrained, global).

% Advanced students on the path to communal certification. They bear the deepest investment in non-operative material — a decade or more — and receive in exchange a credential whose value presupposes the very curriculum that cost them. By the point the trade's terms become visible, their self-concept, marriage prospects, and career are constituted around mastery of the complete corpus; walking away would mean becoming someone else.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, ordination_track_students, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_archive, ordination_track_students, beneficiary).

% Roshei yeshiva and the decisor-establishment set curricula, examine candidates, and grant ordination. Their personal authority rests substantially on demonstrated command of the full corpus including its hardest, least applicable reaches, and their institutions' prestige rests on unbroken transmission. They could reallocate study hours by decree; doing so would undercut the legitimacy architecture their standing is built on. They bear almost none of the arrangement's opportunity costs themselves.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, rabbinic_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Dispersed communities receive cohesion, continuity, and a shared canonical identity from the study system without running it or staffing it. The corpus gives scattered congregations a common text and a common claim to antiquity. Their attachment is real but not exclusive: communities have re-anchored identity in language, land, or peoplehood before, and could again, at the cost of losing the antiquity claim the corpus uniquely supplies.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% Individuals with live questions — commerce, family, ritual practice in contemporary conditions — whose answers depend on exactly the applicable-law expertise the study system delays. They are not in the room where curricula are set, have no seat in the examination boards, and can only choose among whatever decisors the existing pipeline happens to produce. Their interest in reallocated scholar-hours is never voiced in the institutions that allocate them.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, practical_halakha_seekers, excluded,
    powerless, immediate, constrained, global).

% University-based historians and philologists study the sacrificial corpora as documents: they date redactions, map manuscript traditions, and note — among other things — that large stretches of the order lack Babylonian Talmudic commentary, evidence of its curricular marginality in antiquity. They hold no stake in the confessional study economy and no lever over its curricula, but their findings circulate freely and corroborate the defunct-system premise from outside.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, academic_jewish_studies_scholars, observer,
    moderate, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_archive, rabbinic_leadership).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared canonical corpus and the interpretive tradition that reads it, binding a geographically dispersed community across generations; preserves texts and reading practices that would otherwise decay; marks communal boundaries through a common body of mastered material.
% TRANSFER_FUNCTION: Moves elite study hours and intellectual attention — the scarcest resource in the textual economy — from currently applicable law toward a non-operative corpus; moves status, credentials, and institutional legitimacy upward to the leadership and academies that certify mastery of the complete canon.
% ABSENT_VOICES: Laypeople with live legal questions would object that scholar-years are being spent on material with no performative future while their questions queue; educators who built practical-halakha-weighted tracks would object that the pipeline undersupplies decisory competence. Both sit outside the roshei-yeshiva councils where curricula and ordination standards are actually set.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, yeshiva curricula would reallocate within a generation toward applicable law, ordination standards would be rewritten around practical competence, the academies would lose a principal pillar of their continuity-based prestige, and communal identity practices would re-anchor in language, land, or peoplehood — while the corpus itself would survive intact in libraries and university departments as history. Nothing cosmic changes; the communal economy of attention rearranges substantially.
% FOUNDING_PROBLEM: Preserving and transmitting the technical knowledge of the sacrificial system while the Temple stood: training priests in the mechanics of offerings, judges in valuation and disqualification, and owners in what they were legally required to bring — knowledge whose performance was imminent, mandatory, and consequential.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration comes from outside the beneficiary set: academic scholarship on the redaction and curricular history of the order attests that the performative system ended in 70 CE and that the corpus's study has been non-operative since; educators of practical-halakha tracks attest in practice, by building alternative pipelines, that the founding problem no longer governs training needs. The benefiting parties — the academies and their leadership — contest this, citing the performance and preparation framings under which the corpus retains force; no party inside the study economy attests the dead-status finding, which is itself signal.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_archive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_archive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_archive, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_obligation__study_as_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.58): the diverted resource is elite attention — the scarcest input in the textual economy — and the legitimacy it generates accrues to institutions rather than to those who pay in study years; but the arrangement also produces real goods (transmission, cohesion, philological preservation), which caps epsilon well below predatory levels. Suppression is moderate-low (0.38) and is a raw structural property, unscaled by power or scope: enforcement runs through curricular mandates, examination gates, and ordination requirements rather than coercion, with a minority internalized component (esteem structures that make de-emphasizing Kodashim feel like self-diminishment; roughly 70% structural gatekeeping, 30% internalized). Theater ratio (0.42) reflects a growing share of performative mastery-display — dialectical virtuosity on non-operative detail — alongside genuinely functional archival and identity work. Accessibility collapse is moderate (0.45): practical-halakha tracks, academic Jewish studies, and exit remain visible alternatives, but institutional gravity and credential pathways partially foreclose them. Resistance is low-moderate (0.30): periodic curricular-reform movements and individual exits, no sustained collective challenge — though the coalition potential of students is real and largely unrealized because the hierarchy controls both the curriculum and the credential. The temporal series run on ONE shared grid (T=0..50 in decade-scale units spanning the modern era of institutionalized yeshiva study, late 19th-century consolidation through the late 20th century); the rising suppression_requirement series traces the postwar standardization of curricula and examination machinery (an enforcement ratchet as the rebuilt yeshiva world hardened its canon), plateauing once standardization completed. End-state values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats diverge sharply. From the rabbinic-leadership seat the arrangement is sacred continuity it stewards — the corpus is the community's inheritance and mastery of it is the community's crown; this seat should compute a near-beneficiary profile. From the student seats the same arrangement is a sunk-cost structure: years committed to material with no performative future, redeemable only as status within the system itself. From the excluded seat (live questioners) it is misallocated scholarly labor. From the academic observer seat it is historiography — valuable, but as record, not as operative law. Identity-lock dynamics concentrate in the ordination track: the fusion is professional (path dependence — a decade invested, self-concept constituted as a master of the complete corpus) more than ideological; if the credential ceased to require Kodashim mastery, the lock would break within a generation as new cohorts optimized for the revised standard.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic leadership sits nearest the beneficiary pole: it collects the legitimacy flow and controls the levers, with arbitrage-grade exit (it can restructure the arrangement at will and bears little of its cost). Diaspora communities also sit near the beneficiary pole with mobile exit — they receive identity goods without administering anything, and could re-anchor identity elsewhere (language, land, peoplehood) at some cost. Yeshiva students sit near the target pole: they pay in forgone applicable competence, with constrained exit (leaving means leaving the community's status ladder). Ordination-track students are targets with a partial offset — their credential is minted from the very material that costs them — placing them somewhat short of full-target. Practical halakha seekers are structurally targeted in effect (their questions compete with diverted labor) but hold no seat in the arrangement. The beneficiary/victim declarations map onto these positions directly; no directionality override was needed because the derivation from declared structure plus exit options reproduces these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — training priests, judges, and owners for sacrificial performances that were legally imminent and mandatory — died with the Temple. The arrangement persisted by reassignment: the same study hours were re-justified first as vicarious performance, then as messianic preparation, and on this reading as archival identity-work. The founding_problem_status=dead combined with disappearance_verdict=world_rearranges is the honest mismatch: the arrangement persists after its mandate expired, sustained by a function its founders did not build it for. Declaring mandatrophy_resolved=true records that the original mandate is spent. Classification discipline prevents mislabeling in both directions: a pure-extraction reading would erase the genuine coordination achieved (dispersed communities really do cohere around this canon, and the texts really are preserved); a pure-coordination reading would erase the diversion cost borne by students and questioners. The tangled middle is where the structure actually sits — and the receipt surface (gains accruing to a named seat, fixing prohibitively expensive for the seat that could fix it) registers the capture pressure that keeps the arrangement from drifting back toward clean coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Which reading of the kodashim_obligation kernel correctly specifies the corpus''s present normative force: archive (this reading), performance, or preparation?',
    'Intra-traditional adjudication of which frameworks can absorb which premises, combined with a sociological mapping of which communities actually hold each reading and what each concedes about the others.',
    'The performance reading would collapse measured extraction toward the coordination floor (study IS the output, so diverted hours are not diverted); the preparation reading would tie extraction to restoration probability and shift the victim set toward those left unprepared for resumed performance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'This story instantiates one reading of a contested kernel; sibling readings instantiate different constraints with different epsilon and victim structures.').

omega_variable(
    identity_function_genuineness,
    'Is the identity-maintenance function of Kodashim study a genuine coordination output that offsets its diversion costs, or an identity-framed cover story for institutional legitimacy-banking?',
    'Comparative study of communities that reallocated elite study hours toward applicable law: if communal cohesion and transmission survived reallocation, the identity function is separable and excess diversion is higher; if cohesion degraded, the coordination is genuine.',
    'If separable, the arrangement leans toward pure extraction riding on a residual archive function; if inseparable, part of the measured cost is the price of the coordination itself and the tangled assessment stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_function_genuineness, conceptual, 'Whether the identity-coordination justification is load-bearing or decorative.').

omega_variable(
    restoration_contingency,
    'If Temple restoration became technically and politically live, would the studying community desire resumed sacrifice?',
    'Survey of communal leadership positions and liturgical practice under changed political conditions on the Temple Mount; analysis of how quickly curricular emphasis shifted in prior episodes of restored practice.',
    'If restoration is desired, the archive reading destabilizes toward preparation and the corpus regains a performative future; if undesired, the archive reading hardens and the preparation reading''s victim set evaporates entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_contingency, preference, 'The archive reading''s core premise (restoration structurally impossible/undesired) is preference-laden and politically contingent.').

omega_variable(
    opportunity_cost_magnitude,
    'What fraction of elite study hours actually goes to non-operative orders, and what would reallocation toward applicable law yield in decisory competence?',
    'Curricular time-allocation audits across yeshivot, plus outcome tracking of decisors trained under practical-halakha-weighted alternative tracks.',
    'A large diversion supports higher epsilon and strengthens the victim claims of students and questioners; a small diversion lowers epsilon toward a pure-coordination profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunity_cost_magnitude, empirical, 'Size of the diverted-intellectual-resources victim set.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_archive, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_archive, theater_ratio, 0, 0.28).
narrative_ontology:measurement(koda_tr_t10, kodashim_obligation__study_as_archive, theater_ratio, 10, 0.31).
narrative_ontology:measurement(koda_tr_t20, kodashim_obligation__study_as_archive, theater_ratio, 20, 0.34).
narrative_ontology:measurement(koda_tr_t30, kodashim_obligation__study_as_archive, theater_ratio, 30, 0.37).
narrative_ontology:measurement(koda_tr_t40, kodashim_obligation__study_as_archive, theater_ratio, 40, 0.4).
narrative_ontology:measurement(koda_tr_t50, kodashim_obligation__study_as_archive, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_archive, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(koda_be_t10, kodashim_obligation__study_as_archive, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(koda_be_t20, kodashim_obligation__study_as_archive, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(koda_be_t30, kodashim_obligation__study_as_archive, base_extractiveness, 30, 0.56).
narrative_ontology:measurement(koda_be_t40, kodashim_obligation__study_as_archive, base_extractiveness, 40, 0.57).
narrative_ontology:measurement(koda_be_t50, kodashim_obligation__study_as_archive, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_archive, suppression_requirement, 0, 0.26).
narrative_ontology:measurement(koda_su_t10, kodashim_obligation__study_as_archive, suppression_requirement, 10, 0.29).
narrative_ontology:measurement(koda_su_t20, kodashim_obligation__study_as_archive, suppression_requirement, 20, 0.33).
narrative_ontology:measurement(koda_su_t30, kodashim_obligation__study_as_archive, suppression_requirement, 30, 0.36).
narrative_ontology:measurement(koda_su_t40, kodashim_obligation__study_as_archive, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(koda_su_t50, kodashim_obligation__study_as_archive, suppression_requirement, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_archive, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_preparation).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'obligation to study Kodashim' covers three structurally distinct claims that cannot share one story, because measuring the corpus's present force one way yields near-zero extraction (performance: study is the function) and another way yields contingent extraction (preparation: force suspended pending restoration) while the archive reading yields moderate diversion-cost extraction. Each reading gets its own epsilon, beneficiaries, victims, and classification; this file is the archive member. The performance reading is upstream in internal prestige — its dictum-tradition is cited as warrant by institutions operating under all three readings — and therefore structurally influences its siblings; the archive reading depends on the defunct-system premise that the performance reading denies outright. Family members are linked via affects_constraints in all files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
