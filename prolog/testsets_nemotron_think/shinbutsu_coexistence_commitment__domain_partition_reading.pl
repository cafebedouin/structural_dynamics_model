% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__domain_partition_reading
 *   human_readable: Shinbutsu Domain Partition — Kami Govern Life/Purity/Harvest, Buddhas Govern Death/Salvation/Afterlife
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   The domain partition reading of shinbutsu coexistence holds that kami and
 *   Buddhist deities govern separate existential domains — kami for life,
 *   purity, harvest, and this-worldly flourishing; Buddhas for death,
 *   salvation, and afterlife — without requiring ontological unification.
 *   This arrangement persisted from the Nara period through the Edo period
 *   (8th–19th centuries) as a functional coexistence maintained through
 *   practice rather than doctrine. Shrines and temples operated in parallel,
 *   often sharing grounds (jingū-ji), with lay practitioners moving freely
 *   between them for different life-cycle needs. The constraint is the
 *   commitment to maintain this domain boundary rather than resolve it
 *   theologically.
 *
 * KEY AGENTS:
 *   - shinto_priesthood: Primary agenda_setter for kami domain (institutional/biographical/constrained) — maintains ritual authority over life/purity/harvest
 *   - buddhist_clergy: Primary agenda_setter for Buddha domain (institutional/biographical/constrained) — maintains doctrinal authority over death/salvation/afterlife
 *   - lay_practitioners: Primary beneficiary (organized/biographical/mobile) — accesses both systems for different existential needs
 *   - honji_suijaku_theologians: Excluded (moderate/biographical/trapped) — advocated ontological unification, marginalized by practice
 *   - meiji_restoration_state: Observer (institutional/generational/analytical) — forcibly imposed separation (shinbutsu bunri) in 1868
 *   - modern_scholars: Observer (analytical/civilizational/analytical) — analyze the arrangement from historical and theoretical distance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.18).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.12).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Shinbutsu Domain Partition — Kami Govern Life/Purity/Harvest, Buddhas Govern Death/Salvation/Afterlife").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious_studies/philosophy_of_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, 'ac3c4046-1200-4143-b1b6-5853a5858ea4').
narrative_ontology:cs_kernel_codification('ac3c4046-1200-4143-b1b6-5853a5858ea4', distributed).
narrative_ontology:cs_authority_grounding('ac3c4046-1200-4143-b1b6-5853a5858ea4', practice).
narrative_ontology:cs_interpretation_layer_present('ac3c4046-1200-4143-b1b6-5853a5858ea4').
narrative_ontology:cs_reading_relation('ac3c4046-1200-4143-b1b6-5853a5858ea4', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac3c4046-1200-4143-b1b6-5853a5858ea4', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('ac3c4046-1200-4143-b1b6-5853a5858ea4', foundational, domain_partition_sufficient_for_coexistence).
narrative_ontology:cs_axiom_status(domain_partition_sufficient_for_coexistence, holdable).
narrative_ontology:cs_axiom_grounding('ac3c4046-1200-4143-b1b6-5853a5858ea4', domain_partition_sufficient_for_coexistence, conventional).
narrative_ontology:cs_axiom('ac3c4046-1200-4143-b1b6-5853a5858ea4', foundational, ontological_unification_unnecessary_for_harmony).
narrative_ontology:cs_axiom_status(ontological_unification_unnecessary_for_harmony, holdable).
narrative_ontology:cs_axiom_grounding('ac3c4046-1200-4143-b1b6-5853a5858ea4', ontological_unification_unnecessary_for_harmony, deontological).
narrative_ontology:cs_reference_frame('ac3c4046-1200-4143-b1b6-5853a5858ea4', pragmatic_coexistence_practice).
narrative_ontology:cs_drift_state('ac3c4046-1200-4143-b1b6-5853a5858ea4', late_edo_nativist_critique, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ac3c4046-1200-4143-b1b6-5853a5858ea4', '2026-08-20T14:30:00Z').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_priesthood).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, lay_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, lay_practitioners).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__domain_partition_reading, pragmatic_coexistence_suffices_for_religious_peace).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__domain_partition_reading, ontological_unification_not_required_for_functional_harmony).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains ritual authority over kami domain (life, purity, harvest, this-worldly flourishing). Controls shrine networks, parishioner registers (ujiko), and festival calendars. Benefits from exclusive jurisdiction over these domains without Buddhist doctrinal interference. Exit is constrained by hereditary succession and institutional role — leaving the priesthood means abandoning the office, not just the constraint.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_priesthood, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_priesthood, beneficiary).

% Maintains doctrinal and ritual authority over Buddha domain (death, salvation, afterlife). Controls temple networks, parishioner registers (danka), and funerary rites. Benefits from exclusive jurisdiction over these domains without Shinto purity concerns. Exit is constrained by monastic vows, institutional hierarchy, and hereditary temple succession — leaving the clergy means abandoning the vocation.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_clergy, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_clergy, beneficiary).

% Accesses both systems for different life-cycle needs: shrine visits for births, weddings, harvest festivals; temple funerals, memorial services, afterlife assurance. Pays material support to both institutions (offerings, parishioner dues, funeral costs). Can shift emphasis between traditions, convert, or disengage — exit is mobile at individual level, though social pressure maintains dual affiliation as norm.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, lay_practitioners, beneficiary,
    organized, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, lay_practitioners, payer).

% Advocated ontological unification (kami as local manifestations of universal Buddhist truth). Their project was structurally incompatible with the domain partition — unification would dissolve the boundary the partition maintains. They were not suppressed by force but marginalized by practice: the partition operated at the level of ritual and institution, not theology, leaving no institutional seat for unificationist theology. Exit meant abandoning the theological project or moving to pure Buddhist institutional contexts.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, honji_suijaku_theologians, excluded,
    moderate, biographical, trapped, national).

% External observer that forcibly imposed shinbutsu bunri (separation of kami and Buddhas) in 1868, destroying the jingū-ji complex and redefining Shinto as non-religious state cult. Did not participate in the coexistence constraint but terminated it. Its structural relationship is post-hoc: it observed the arrangement, deemed it incoherent, and imposed a new constraint.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, meiji_restoration_state, observer,
    institutional, generational, analytical, national).

% Analyze the arrangement from historical, anthropological, and theoretical distance. Produce the competing readings (domain partition, syncretic fusion, incoherent bundle) as interpretive frameworks. Do not participate in the constraint but shape its retrospective classification. Exit is analytical — they can adopt any reading without material cost.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, modern_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Partitions existential authority between two powerful religious traditions to avoid doctrinal conflict: kami govern this-worldly life/purity/harvest; Buddhas govern other-worldly death/salvation/afterlife. Each tradition retains full authority in its domain without negotiating theological consistency with the other.
% TRANSFER_FUNCTION: Ritual authority, parishioner loyalty, and material support flow to each institution within its designated domain. Lay practitioners transfer resources to both — shrine offerings for life events, temple fees for death rituals. No transfer occurs between the institutions; the partition prevents competition for the same ritual occasions.
% ABSENT_VOICES: Honji suijaku theologians who sought ontological unification are the primary excluded voice — they would object that the partition is theologically incoherent and that kami must be understood as manifestations of Buddhist truth. Also absent: communities that practiced exclusive Shinto or exclusive Buddhism without dual affiliation (minority but present), and women whose ritual access was restricted in both traditions.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished overnight, the religious landscape would reorganize: either toward syncretic fusion (honji suijaku becoming institutional orthodoxy), toward competitive conflict (both traditions claiming full existential jurisdiction), or toward the Meiji-style forced separation. The partition structured centuries of Japanese religious practice; its removal would rearrange institutional boundaries, ritual economies, and lay affiliation patterns.
% FOUNDING_PROBLEM: How to accommodate two powerful, institutionally entrenched religious traditions — indigenous kami worship and imported Buddhism — without theological war, doctrinal synthesis, or mutual exclusion, in a polity where both claimed authority over existential matters.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (accommodation without synthesis) is historically documented in Nara/Heian period records (e.g., Jingikan records, temple-shrine complex formation) and corroborated by modern scholars outside the benefiting institutions (e.g., Kuroda Toshio's 'kenmitsu taisei' thesis, Teeuwen & Rambelli's edited volumes on shinbutsu shūgō). The benefiting institutions (Shinto priesthood, Buddhist clergy) self-assert the problem remains live; external scholarship treats it as historically specific and resolved by Meiji disruption.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__domain_partition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).
:- end_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the partition functions as genuine coordination: each tradition retains authority over its domain without extracting from the other. Suppression is low (0.12) because boundary maintenance operated through social practice and institutional habit, not active coercion — honji suijaku theology existed but was not systematically suppressed until Meiji. Theater ratio is minimal (0.08) — the arrangement performed its coordination function without elaborate performative maintenance. Accessibility collapse is moderate (0.35) — alternatives (pure Shinto, pure Buddhism, syncretic theology) remained conceptually available but were not institutionally realized at scale. Resistance is low (0.22) — the arrangement was broadly accepted until state intervention. The slight uptick in all metrics near 1500 (end of Edo) reflects increasing institutional rigidity and the rise of nativist (kokugaku) critique, but the constraint remained rope-like until external disruption.
 *
 * PERSPECTIVAL GAP:
 *   From the shinto_priesthood and buddhist_clergy seats, the constraint appears as stable coordination (rope) — each controls its domain, both benefit from mutual non-interference. From the lay_practitioner seat, it appears as beneficial pluralism with minor transaction costs. From the honji_suijaku_theologians seat (excluded), it appears as an enforced separation that suppresses theological resolution. The engine computes these per-seat types from the structural data; the claimed_type (rope) reflects the dominant institutional experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Both priesthoods are agenda_setters and beneficiaries — they set the ritual boundaries and collect authority/prestige/material support within their domains (d ~ 0.15). Lay practitioners are beneficiaries with mobile exit — they can emphasize one tradition over the other, convert, or disengage (d ~ 0.35). Honji suijaku theologians are excluded — their unificationist project was structurally incompatible with the partition, so they were not accommodated (d ~ 0.8 but excluded from coordination). The Meiji state is an external observer that ultimately imposed a different constraint (shinbutsu_bunri).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to accommodate two powerful religious traditions without theological war — was live for centuries. By the late Edo period, the problem was arguably dead (the traditions had stabilized), yet the arrangement persisted without sunset clause. However, this is not mandatrophy in the extractive sense: the constraint was not maintained by inertia despite harm, but because it continued to coordinate effectively. The Meiji disruption was exogenous, not an internal collapse. The arrangement resolved its mandatrophy by being forcibly replaced, not by atrophying into a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel shinbutsu_coexistence_commitment, and does the domain_partition_reading instantiate a distinct constraint from its siblings?',
    'Structural decomposition: if the domain partition reading yields a different ε, beneficiary/victim structure, or classification from the syncretic_fusion_reading or incoherent_bundle_reading, it is a distinct constraint per ε-invariance.',
    'If distinct, each reading gets its own constraint story with independent metrics; the kernel is a family of constraints linked by network.affects_constraints, not a single constraint with measurement-dependent classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment that this JSON instantiates exactly one reading of the shinbutsu_coexistence_commitment kernel.').

omega_variable(
    partition_stability_mechanism,
    'Was the domain partition maintained by genuine mutual accommodation (low suppression) or by implicit suppression of unificationist voices?',
    'Historical analysis of pre-Meiji discourse: evidence of active suppression of honji suijaku theologians vs. evidence of genuine pluralistic practice.',
    'If suppression was higher than authored, the constraint shifts toward tangled_rope; if genuinely low, rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_stability_mechanism, empirical, 'Whether boundary maintenance required coercion or emerged from practice.').

omega_variable(
    lay_practitioner_extraction,
    'Did lay practitioners bear hidden costs (double affiliation fees, ritual duplication, cognitive load) that constitute extraction not captured in the low ε?',
    'Economic history of temple/shrine parishioner obligations (danka/seidan systems); comparative analysis of single-tradition vs. dual-affiliation households.',
    'If significant hidden costs existed, ε rises and lay_practitioners shift from beneficiary toward payer, potentially reclassifying as tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lay_practitioner_extraction, empirical, 'Whether the coordination imposed diffuse costs on the laity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t0, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(shinbutsu_domain_partition_tr_t0, observed).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t300, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 300, 0.06).
narrative_ontology:measurement_basis(shinbutsu_domain_partition_tr_t300, observed).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t600, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 600, 0.07).
narrative_ontology:measurement_basis(shinbutsu_domain_partition_tr_t600, observed).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t900, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 900, 0.07).
narrative_ontology:measurement_basis(shinbutsu_domain_partition_tr_t900, observed).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1200, 0.08).
narrative_ontology:measurement_basis(shinbutsu_domain_partition_tr_t1200, observed).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t1500, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1500, 0.12).
narrative_ontology:measurement_basis(shinbutsu_domain_partition_tr_t1500, observed).

% Extraction over time
narrative_ontology:measurement(shinbutsu_domain_partition_be_t0, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(shinbutsu_domain_partition_be_t0, observed).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t300, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 300, 0.16).
narrative_ontology:measurement_basis(shinbutsu_domain_partition_be_t300, observed).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t600, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 600, 0.17).
narrative_ontology:measurement_basis(shinbutsu_domain_partition_be_t600, observed).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t900, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 900, 0.18).
narrative_ontology:measurement_basis(shinbutsu_domain_partition_be_t900, observed).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1200, 0.19).
narrative_ontology:measurement_basis(shinbutsu_domain_partition_be_t1200, observed).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t1500, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1500, 0.22).
narrative_ontology:measurement_basis(shinbutsu_domain_partition_be_t1500, observed).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_domain_partition_su_t0, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(shinbutsu_domain_partition_su_t0, observed).
narrative_ontology:measurement(shinbutsu_domain_partition_su_t300, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 300, 0.09).
narrative_ontology:measurement_basis(shinbutsu_domain_partition_su_t300, observed).
narrative_ontology:measurement(shinbutsu_domain_partition_su_t600, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 600, 0.1).
narrative_ontology:measurement_basis(shinbutsu_domain_partition_su_t600, observed).
narrative_ontology:measurement(shinbutsu_domain_partition_su_t900, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 900, 0.11).
narrative_ontology:measurement_basis(shinbutsu_domain_partition_su_t900, observed).
narrative_ontology:measurement(shinbutsu_domain_partition_su_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1200, 0.12).
narrative_ontology:measurement_basis(shinbutsu_domain_partition_su_t1200, observed).
narrative_ontology:measurement(shinbutsu_domain_partition_su_t1500, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1500, 0.18).
narrative_ontology:measurement_basis(shinbutsu_domain_partition_su_t1500, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__domain_partition_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_bunri_meiji_separation).

% DUAL FORMULATION NOTE:
% This story is one member of the shinbutsu_coexistence_commitment constraint family. The domain_partition_reading claims ε ≈ 0.18 (rope); the syncretic_fusion_reading likely claims lower ε (mountain/rope) but with different beneficiary structure (Buddhist clergy as primary); the incoherent_bundle_reading likely claims higher ε (snare/tangled_rope) with state/institutional power as beneficiary. The three readings are linked by network.affects_constraints and share the kernel_id in their respective cs_structure blocks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
