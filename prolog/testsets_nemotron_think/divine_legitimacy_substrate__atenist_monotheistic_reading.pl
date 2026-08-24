% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__atenist_monotheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__atenist_monotheistic_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: divine_legitimacy_substrate__atenist_monotheistic_reading
 *   human_readable: Atenist Monotheistic Revelation as Sole Source of Divine Legitimacy
 *   domain: ancient_history/religious_studies/political_economy
 *
 * SUMMARY:
 *   Akhenaten's Atenist revolution (c. 1353–1336 BCE) instituted history's
 *   first recorded state monotheism, declaring Aten the exclusive deity and
 *   the pharaoh his sole prophet. The constraint operated through boundary
 *   stelae at Akhetaten, the Great Hymn to Aten, and systematic erasure of
 *   Amun's name. It combined genuine ideological innovation (universal solar
 *   monotheism) with massive resource extraction from the Amun priesthood.
 *   The constraint collapsed within a generation of its founder's death, but
 *   its structural signature — centralized interpretive monopoly, temple
 *   economy seizure, suppression of alternatives — defines the Atenist
 *   reading of the divine legitimacy substrate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.78).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.87).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Monotheistic Revelation as Sole Source of Divine Legitimacy").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "ancient_history/religious_studies/political_economy").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, 'af87d3bb-2c7b-40e7-b59b-683e756d8e41').
narrative_ontology:cs_kernel_codification('af87d3bb-2c7b-40e7-b59b-683e756d8e41', fixed_text).
narrative_ontology:cs_authority_grounding('af87d3bb-2c7b-40e7-b59b-683e756d8e41', extraction).
narrative_ontology:cs_interpretation_layer_present('af87d3bb-2c7b-40e7-b59b-683e756d8e41').
narrative_ontology:cs_reading_relation('af87d3bb-2c7b-40e7-b59b-683e756d8e41', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('af87d3bb-2c7b-40e7-b59b-683e756d8e41', divine_legitimacy_substrate__folk_syncretistic_reading, forecloses).
narrative_ontology:cs_axiom('af87d3bb-2c7b-40e7-b59b-683e756d8e41', foundational, aten_exclusive_deity).
narrative_ontology:cs_axiom_status(aten_exclusive_deity, holdable).
narrative_ontology:cs_axiom_grounding('af87d3bb-2c7b-40e7-b59b-683e756d8e41', aten_exclusive_deity, deontological).
narrative_ontology:cs_axiom('af87d3bb-2c7b-40e7-b59b-683e756d8e41', foundational, pharaoh_sole_intermediary).
narrative_ontology:cs_axiom_status(pharaoh_sole_intermediary, holdable).
narrative_ontology:cs_axiom_grounding('af87d3bb-2c7b-40e7-b59b-683e756d8e41', pharaoh_sole_intermediary, deontological).
narrative_ontology:cs_reference_frame('af87d3bb-2c7b-40e7-b59b-683e756d8e41', atenist_divine_order).
narrative_ontology:cs_drift_state('af87d3bb-2c7b-40e7-b59b-683e756d8e41', post_amarna_restoration, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('af87d3bb-2c7b-40e7-b59b-683e756d8e41', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, royal_court).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_priesthoods).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_practitioners).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__atenist_monotheistic_reading, monotheistic_divine_order).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_sole_intermediary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims exclusive revelation of Aten as only true deity; dismantles Amun priesthood, seizes temple estates, redirects tribute to Aten institutions; presents himself as sole legitimate interpreter of divine will. His person fuses with the constraint — exit is structurally impossible without abdicating the throne and the revelation itself.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Gains control of redistributed temple wealth, administrative positions in new Aten bureaucracy, and proximity to the sole source of legitimacy. Some courtiers are genuine converts; others perform devotion for advancement. Exit is possible — they can flee or conform to restoration — but career capital is tied to the Atenist system.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, royal_court, beneficiary,
    powerful, biographical, mobile, national).

% The wealthiest and most powerful priesthood in Egypt, centered at Karnak. Their temples are closed, assets seized, statues defaced, name of Amun erased from monuments. Priests are displaced, imprisoned, or forced into hiding. The institution that rivaled pharaonic power for centuries is structurally dismantled — no exit preserves the institution.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood, payer,
    institutional, generational, trapped, national).

% Regional priesthoods of Mut, Khonsu, Ptah, Osiris, and local deities face temple closures, property confiscation, and prohibition of public festivals. Some flee to provincial strongholds; most are subordinated to Aten administration. Their religious authority and economic base are destroyed simultaneously.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_priesthoods, payer,
    organized, generational, trapped, regional).

% Village and household worshippers of Bes, Taweret, Hathor, ancestor spirits, and local deities. Public practice is banned; household shrines are targets of iconoclastic sweeps. Their religious identity is fused with daily life — birth, healing, protection, afterlife — making exit psychologically and socially near-impossible. They practice covertly or assimilate performatively.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Hittite, Mitanni, Babylonian, and Assyrian envoys accustomed to invoking Amun-Ra in treaties and correspondence. The theological shift disrupts diplomatic language and ideological common ground. They observe the internal upheaval, assess Egyptian weakness, and adjust treaty demands accordingly — they neither pay nor collect, but their operating framework is destabilized.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, foreign_diplomatic_corps, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unified monotheistic state ideology replacing fragmented polytheistic cults; centralized religious authority in pharaoh as sole intermediary, eliminating priestly mediation and rival power centers.
% TRANSFER_FUNCTION: Temple lands, agricultural estates, livestock, gold, labor corvée, and tribute income transferred from traditional priesthoods (especially Amun at Karnak) to pharaonic treasury and newly founded Aten institutions at Akhetaten.
% ABSENT_VOICES: Village elders maintaining household cults of Bes and Taweret; regional temple communities in Upper Egypt where Amun worship persisted covertly; foreign diplomatic corps whose treaty frameworks invoked Amun-Ra; the army officer corps whose traditional patron deities were suppressed.
% DISAPPEARANCE_RATIONALE: Upon Akhenaten's death, the constraint collapsed: Tutankhamun's Restoration Stela describes temples 'fallen into ruin,' gods 'ignored,' and the land 'in chaos.' The Amun priesthood was restored, Akhetaten abandoned, Aten monuments dismantled for fill, and the traditional religious economy reconstituted — the world rearranged itself completely around the restoration.
% FOUNDING_PROBLEM: Fragmented religious authority undermining pharaonic sovereignty; Amun priesthood at Karnak had become a rival power center controlling vast estates and oracle access; need for unified state ideology to centralize control over Egypt's imperial administration.
% FOUNDING_PROBLEM_CORROBORATION: The Amarna letters (EA 244-245) show vassal rulers confused by theological shift; Tutankhamun's Restoration Stela explicitly names the 'chaos' caused by neglect of traditional gods; Horemheb's Coronation Inscription frames the Atenist period as a deviation requiring correction; archaeological strata at Karnak show deliberate erasure followed by systematic restoration — all sources outside the Atenist beneficiary set.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__atenist_monotheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__atenist_monotheistic_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__atenist_monotheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint transferred the entire economic base of Egypt's dominant religious institution to the pharaoh. Suppression is very high (0.87) because persistence required active iconoclasm, name-erasure, and prohibition of alternatives — not mere persuasion. Theater ratio is high (0.72) because the performative devotion of the royal family (Amarna art's intimate solar worship) masked the material extraction. Accessibility collapse (0.82) reflects the systematic destruction of alternative cult infrastructure. Resistance (0.76) is evidenced by the rapidity and thoroughness of the post-Amarna restoration. The claimed type (tangled_rope) reflects genuine coordination (universal monotheism as state ideology) fused with asymmetric extraction (priesthood dispossession).
 *
 * PERSPECTIVAL GAP:
 *   From the pharaoh's seat, the constraint is a Mountain (divine revelation, natural law of monotheism). From the Amun priesthood's seat, it is a Snare (pure extraction via theological cover). From folk practitioners' seat, it is a Tangled Rope (some coordination — universal deity replacing confusing multiplicity — but overwhelming extraction of their protective deities). The engine computes this divergence; the authoring seat declares the structural data that produces it.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh sits at d≈0.05 (full beneficiary: constraint subsidizes his authority, collects its rents). Royal court at d≈0.2 (beneficiary with performance costs). Amun priesthood at d≈0.95 (full target: institution destroyed, wealth seized, no exit). Traditional priesthoods at d≈0.85 (targets with partial survival via concealment). Folk practitioners at d≈0.75 (targets but identity-locked — they bear suppression internally). Foreign diplomats at d≈0.5 (symmetric: framework disrupted but no extraction). The engine will derive these from beneficiary/victim declarations plus exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Amun priesthood rivalry) was real and live at inception. The Atenist solution destroyed the rival but created a system requiring total enforcement to persist. When enforcement ceased (Akhenaten's death), the constraint had no residual coordination value — the 'monotheistic substrate' did not survive as a self-sustaining arrangement. The mandate atrophied into pure performance within a single reign. The restoration was not a repair but a rejection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the divine_legitimacy_substrate a single kernel with multiple readings, or are these structurally distinct constraints that merely share vocabulary?',
    'Compare ε values across readings: if atenist_reading shows high extraction/suppression while amun_reading shows low extraction/coordination, they are distinct constraints linked by network.affects_constraints, not measurement variants of one constraint.',
    'If distinct constraints, each gets independent classification; if single kernel, the framework must model reading-indexed ε variance. The ε-invariance principle demands decomposition when ε changes with reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel frame correctly captures structural unity or masks constraint multiplicity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'For folk_practitioners, is the measured suppression (0.87) primarily structural (state iconoclasm, shrine destruction) or internalized (identity fusion with household gods making covert practice psychologically costly)?',
    'Post-Amarna suppression trajectory: if folk practice rebounds rapidly after restoration, suppression was primarily structural; if practices remain altered or syncretized, internalized component persists.',
    'If internalized, effective suppression exceeds structural measure — folk_practitioners carry the constraint''s suppression internally after exit, altering their seat''s χ computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for identity-locked folk practitioners.').

omega_variable(
    coordination_extraction_boundary,
    'Was the monotheistic coordination function (unified ideology, eliminated priestly mediation) genuine and separable from the extraction function (temple wealth seizure), or was coordination purely cover for extraction?',
    'Counterfactual: if Akhenaten had redistributed temple wealth to public granaries rather than royal treasury, would the monotheistic ideology have persisted? Compare with later monotheisms that lacked state extraction machinery.',
    'If inseparable, the constraint is a Snare (coordination is cover). If separable, it is a Tangled Rope (genuine coordination + asymmetric extraction). The classification hinges on this boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__atenist_monotheistic_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(atenist_mono_tr_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(atenist_mono_tr_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 3, 0.52).
narrative_ontology:measurement(atenist_mono_tr_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 6, 0.63).
narrative_ontology:measurement(atenist_mono_tr_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 9, 0.71).
narrative_ontology:measurement(atenist_mono_tr_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 12, 0.74).
narrative_ontology:measurement(atenist_mono_tr_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 17, 0.72).

% Extraction over time
narrative_ontology:measurement(atenist_mono_be_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(atenist_mono_be_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(atenist_mono_be_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 6, 0.71).
narrative_ontology:measurement(atenist_mono_be_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 9, 0.78).
narrative_ontology:measurement(atenist_mono_be_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 12, 0.81).
narrative_ontology:measurement(atenist_mono_be_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 17, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(atenist_mono_su_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(atenist_mono_su_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 3, 0.72).
narrative_ontology:measurement(atenist_mono_su_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 6, 0.81).
narrative_ontology:measurement(atenist_mono_su_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 9, 0.86).
narrative_ontology:measurement(atenist_mono_su_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 12, 0.89).
narrative_ontology:measurement(atenist_mono_su_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 17, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__atenist_monotheistic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.08).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood_economy).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, egyptian_imperial_administration).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate__amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate__folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% This constraint (atenist_monotheistic_reading) and its siblings (amun_polytheistic_reading, folk_syncretistic_reading) form a constraint family decomposing the divine_legitimacy_substrate kernel. The ε values differ substantially: atenist_reading ε≈0.78 (high extraction, active suppression), amun_reading ε≈0.25 (coordination with moderate extraction), folk_reading ε≈0.15 (low extraction, high accessibility). They are linked via network.affects_constraints because the atenist_reading's suppression directly targeted the amun_reading's institutional base and the folk_reading's practice space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_legitimacy_substrate__atenist_monotheistic_reading, institutional, 0.05).
constraint_indexing:directionality_override(divine_legitimacy_substrate__atenist_monotheistic_reading, powerless, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
