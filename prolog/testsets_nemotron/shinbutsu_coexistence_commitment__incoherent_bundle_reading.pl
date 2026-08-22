% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__incoherent_bundle_reading
 *   human_readable: Shinbutsu-shugo as Incoherent Bundle Maintained by Ambiguity
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   Shinbutsu-shugo (kami-buddha combination) is conventionally described as
 *   a syncretic fusion or a stable domain partition. This reading argues it
 *   was neither: it was an incoherent bundle — a practical arrangement that
 *   worked precisely because it refused to stabilize the kami-buddha
 *   relationship into any coherent ontology. The bundle was maintained by
 *   institutional power (shrine-temple complexes, shugendo lineages, court
 *   ritual office) that benefited from the ambiguity. Meiji bunri (separation
 *   of kami and buddhas) did not destroy a coherent system; it revealed the
 *   incoherence that the bundle's ambiguity had structurally suppressed. The
 *   constraint's extraction took the form of double parish dues, exclusion of
 *   coherent alternatives, and the labor of maintaining ritual participation
 *   in two overlapping systems. The theater ratio rose over time as the
 *   coordination function (sectarian peace) atrophied and the extraction
 *   function (institutional rent) expanded.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.72).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.68).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "Shinbutsu-shugo as Incoherent Bundle Maintained by Ambiguity").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "religious_studies/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '44305e35-7755-4236-98d7-73f69ef359a5').
narrative_ontology:cs_kernel_codification('44305e35-7755-4236-98d7-73f69ef359a5', distributed).
narrative_ontology:cs_authority_grounding('44305e35-7755-4236-98d7-73f69ef359a5', practice).
narrative_ontology:cs_interpretation_layer_present('44305e35-7755-4236-98d7-73f69ef359a5').
narrative_ontology:cs_reading_relation('44305e35-7755-4236-98d7-73f69ef359a5', shinbutsu_coexistence_commitment__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('44305e35-7755-4236-98d7-73f69ef359a5', shinbutsu_coexistence_commitment__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('44305e35-7755-4236-98d7-73f69ef359a5', foundational, no_stable_kami_buddha_ontology).
narrative_ontology:cs_axiom_status(no_stable_kami_buddha_ontology, holdable).
narrative_ontology:cs_axiom_grounding('44305e35-7755-4236-98d7-73f69ef359a5', no_stable_kami_buddha_ontology, empirically_contingent).
narrative_ontology:cs_axiom('44305e35-7755-4236-98d7-73f69ef359a5', foundational, ambiguity_as_institutional_strategy).
narrative_ontology:cs_axiom_status(ambiguity_as_institutional_strategy, holdable).
narrative_ontology:cs_axiom_grounding('44305e35-7755-4236-98d7-73f69ef359a5', ambiguity_as_institutional_strategy, instrumental).
narrative_ontology:cs_reference_frame('44305e35-7755-4236-98d7-73f69ef359a5', pre_meiji_ritual_polity).
narrative_ontology:cs_drift_state('44305e35-7755-4236-98d7-73f69ef359a5', bakumatsu_institutional_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('44305e35-7755-4236-98d7-73f69ef359a5', '2026-06-15T14:30:00Z').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_temple_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shugendo_lineages).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, court_ritual_office).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, local_parishioners).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, non_aligned_practitioners).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theoretical_coherence).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__incoherent_bundle_reading, ambiguity_as_institutional_strategy).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_bunri_as_revelation_not_creation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major shrine-temple complexes (e.g., Hie-Taisha/Enryaku-ji, Kasuga-Kofuku-ji) administered combined ritual economies — land, taxation, pilgrimage traffic, and mortuary services. They set the practical terms of coexistence: which kami shared space with which buddhas, which festivals followed which calendars, how donations were split. Their institutional identity fused Shinto and Buddhist functions so completely that disentangling them would dissolve the complex itself. Exit meant institutional suicide.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_temple_complexes, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_temple_complexes, beneficiary).

% Shugendo mountain ascetic lineages (e.g., Haguro, Omine) built their entire soteriology on the ambiguity: kami and buddhas were not merely coexisting but operationally interchangeable in practice. Their authority derived from navigating the boundary that the bundle refused to draw. They benefited from the bundle's refusal to stabilize — it let them claim both traditions' legitimacy without choosing. Meiji separation forced a choice that shattered their institutional coherence.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shugendo_lineages, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shugendo_lineages, agenda_setter).

% The Jingikan and later Jingi-kan managed state ritual through a deliberately unstable synthesis: the emperor as kami-descendant presiding over Buddhist-inflected court ceremonies. The office benefited from the bundle's ambiguity because it allowed the state to claim both indigenous legitimacy (kami) and cosmological sophistication (Buddhism) without resolving the tension. They maintained the bundle by refusing to codify the relationship — policy by precedent, not doctrine.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, court_ritual_office, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, court_ritual_office, beneficiary).

% Village households paid double dues — shrine parish membership (ujiko) and temple parish membership (danka) — for overlapping ritual services (birth, marriage, death, harvest). The bundle's ambiguity meant no single institution took full responsibility; gaps in coverage (e.g., pollution rites vs. funeral rites) fell on the household. They had no exit: leaving one parish invited social ostracism and spiritual danger; the bundle made both memberships mandatory.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, local_parishioners, payer,
    powerless, biographical, trapped, local).

% Groups that attempted coherent doctrinal positions — e.g., Yoshida Shinto's kami-centric theology, Nichiren's Lotus-exclusivist Buddhism, or nativist kokugaku scholars — were structurally excluded from the bundle's operation. The bundle's power depended on their marginalization: a coherent alternative would have exposed the bundle's incoherence. They were silenced through institutional pressure (temple registration laws, shrine affiliation requirements) rather than doctrinal refutation.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, non_aligned_practitioners, excluded,
    moderate, biographical, constrained, regional).

% The bundle sustained itself by treating the question 'what is the relationship between kami and buddhas?' as administratively settled (they coexist) while leaving it ontologically open. Any attempt to answer it coherently — whether through honji suijaku, domain partition, or separation — was treated as a threat to the practical arrangement. Theoretical coherence was the excluded voice that the bundle's ambiguity was designed to silence.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theoretical_coherence, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theoretical_coherence).

% Contemporary scholars of Japanese religion (e.g., Kuroda Toshio, Teeuwen, Rambelli) who read the pre-Meiji system not as a stable syncretism but as a managed incoherence. They see Meiji bunri not as destroying a living tradition but as forcing the bundle's suppressed contradictions into the open. Their analysis is the engine that computes the constraint's type from the structural data authored here.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, modern_scholar_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a minimally viable ritual economy across life-cycle events (birth at shrine, death at temple, harvest at both) without requiring any party to resolve the ontological status of kami vs. buddhas. The coordination was negative: it worked by NOT asking the question that would break the arrangement.
% TRANSFER_FUNCTION: Moved labor, land revenue, and ritual fees from local parishioners (double dues) and unaffiliated practitioners (exclusion costs) to shrine-temple complexes, shugendo lineages, and the court ritual office. The transfer was obscured by the bundle's refusal to name what was being transferred — 'ritual efficacy' served as the unquantified currency.
% ABSENT_VOICES: Coherent doctrinal positions (Yoshida Shinto, Nichiren Buddhism, kokugaku nativism) were excluded because their very coherence threatened the bundle's operational ambiguity. Non-aligned practitioners who sought single-tradition affiliation were pressured into dual membership. The bundle's stability required that no voice be allowed to ask 'which one is true?' inside the institutional space.
% DISAPPEARANCE_RATIONALE: When Meiji bunri forcibly separated kami and buddhas (1868–1872), the shrine-temple complexes fractured, shugendo lineages were forced to choose (many dissolved), parishioners were released from double dues but lost the ritual coverage the bundle provided, and the court ritual office was reconstituted as a purely Shinto institution. The world rearranged violently — the bundle's ambiguity had been the load-bearing structure.
% FOUNDING_PROBLEM: How to maintain a functional ritual polity across a population that practiced both kami-veneration and Buddhism without triggering the sectarian conflicts that devastated continental East Asia. The bundle solved this by refusing to legitimize any single answer to the kami-buddha relationship.
% FOUNDING_PROBLEM_CORROBORATION: Kuroda Toshio (1981) demonstrated that 'Shinto' as an independent tradition was a Meiji invention — the pre-Meiji system was a Buddhist-dominated ritual complex that used kami as local manifestations. The founding problem (sectarian peace via ambiguity) was solved by the bundle, but the solution outlived the problem: by the late Edo period, the ambiguity served institutional rent-extraction, not conflict prevention. Corroborated by non-beneficiary scholars (Teeuwen, Rambelli, Scheid) who read the bundle as a power arrangement, not a theological achievement.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the bundle extracted double ritual dues from parishioners and excluded coherent alternatives without providing proportional coordination value in the late Edo period. Suppression (0.68) is substantial because the bundle's persistence depended on institutional pressure (temple registration, shrine affiliation) that prevented coherent doctrinal positions from gaining institutional footholds. Theater ratio (0.78) is very high by 1872 because the coordination function (peacekeeping) had largely atrophied by the Bakumatsu period — the bundle persisted as theatrical performance of unity masking institutional rent-seeking. Accessibility collapse (0.42) is moderate: alternatives (pure Buddhism, pure kami-veneration) existed conceptually but were institutionally blocked. Resistance (0.55) is moderate: kokugaku and nativist movements mounted intellectual resistance but lacked institutional power until Meiji.
 *
 * PERSPECTIVAL GAP:
 *   From the shrine-temple complex seat (agenda_setter/beneficiary), the bundle appears as a brilliant pragmatic solution — the engine will compute low effective extraction for this seat because directionality derives from beneficiary status and identity_locked exit (d near 0). From the parishioner seat (payer, trapped), the same structure appears as enforced double taxation with no exit — high effective extraction (d near 1). From the shugendo seat (beneficiary/identity_locked), the bundle is their existential foundation — they cannot exit without losing their identity. The engine computes this seat divergence from the structural data; the claimed type (tangled_rope) reflects the analyst's judgment that genuine coordination (peacekeeping) and asymmetric extraction (double dues, exclusion) coexisted.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (shrine-temple complexes, shugendo, court office) collect ritual fees, land revenue, and institutional legitimacy from the bundle's ambiguity — their exit is identity_locked (the institution IS the bundle). Victims (parishioners, non-aligned practitioners) pay double dues and bear exclusion costs with trapped or constrained exit. Theoretical_coherence is an excluded non-agent — it bears no extraction but its exclusion is the bundle's structural condition. The directionality derivation chain reads beneficiary/victim + exit_options + power to compute d per seat; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sectarian peace via ambiguity) was live in the Sengoku/early Edo transition but dead by the mid-Edo period. The bundle persisted through mandatrophy: the arrangement that solved the founding problem became the vehicle for institutional extraction after the problem vanished. The Meiji collapse was not the destruction of a living tradition but the forced resolution of a zombie constraint. The high theater_ratio trajectory (0.35 → 0.78) tracks this mandatrophy: as the founding problem died, the performance of coherence intensified to mask the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bundle_vs_fusion_boundary,
    'Where does the historical evidence place the boundary between ''incoherent bundle maintained by power'' and ''genuinely believed syncretic theology'' across different institutional sites and time periods?',
    'Site-specific institutional histories (e.g., Haguro vs. Hie vs. Ise) compared against practitioner writings — do shugendo ascetics *believe* honji suijaku or *use* it? Do shrine priests *teach* fusion or *perform* it?',
    'If the bundle was genuinely believed at some sites/times, the extraction narrative weakens — the constraint becomes more rope-like (coordination via shared belief) at those nodes. If it was universally performative, the tangled_rope classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bundle_vs_fusion_boundary, empirical, 'Whether the bundle''s ambiguity was universally performative or partially believed').

omega_variable(
    meiji_as_revelation_or_creation,
    'Did Meiji bunri *reveal* pre-existing incoherence or *create* incoherence by imposing categories that the bundle had successfully avoided?',
    'Compare pre-Meiji dispute records (e.g., Yoshida vs. Buddhist establishment, kokugaku critiques) — were practitioners *experiencing* incoherence, or only *theorists*? Parishioner petitions during separation: did they seek clarity or resist disruption?',
    'If revelation: the bundle was always a tangled_rope (extraction masked by coordination). If creation: the bundle was a rope (genuine coordination via strategic ambiguity) that Meiji destroyed. This reading claims revelation; the omega names the empirical test.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(meiji_as_revelation_or_creation, conceptual, 'Whether Meiji separation exposed or invented the bundle''s incoherence').

omega_variable(
    coordination_value_of_ambiguity,
    'How much sectarian violence did the bundle *actually* prevent vs. how much institutional rent did it extract? The coordination function is claimed but not quantified.',
    'Counterfactual modeling: compare Japanese sectarian conflict rates (1600–1868) with continental East Asian comparators (Korea, China, Vietnam) where no comparable ambiguity bundle existed. Control for Tokugawa peace enforcement.',
    'If the bundle prevented significant violence, its coordination function was real and substantial — tangled_rope with high coordination value. If violence was prevented by Tokugawa policing, the bundle''s coordination claim is theater — extraction dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_value_of_ambiguity, empirical, 'Quantifying the bundle''s actual peacekeeping contribution').

omega_variable(
    commiter_frame_ambiguity,
    'Does this reading''s claim that ''ambiguity was a structural feature'' foreclose the possibility that some actors *within* the bundle genuinely experienced it as coherent?',
    'The reading_relations and axioms in cs_structure declare the structural relationships. This omega records the meta-question: can a reading that claims ''no stable ontology'' coexist with evidence that some participants experienced stability? The engine computes foreclosure from axiom contradictions; this omega flags the interpretive risk.',
    'If participant experience of coherence is empirically robust, the ''incoherent bundle'' claim may overreach — the constraint might be a constraint_family with site-specific types rather than a single typed constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commiter_frame_ambiguity, conceptual, 'Whether the reading''s structural claim forecloses participant-level coherence experience').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 1600, 1872).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_bundle_tr_t1600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1600, 0.35).
narrative_ontology:measurement(shinbutsu_bundle_tr_t1650, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1650, 0.42).
narrative_ontology:measurement(shinbutsu_bundle_tr_t1700, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1700, 0.51).
narrative_ontology:measurement(shinbutsu_bundle_tr_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1750, 0.6).
narrative_ontology:measurement(shinbutsu_bundle_tr_t1800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1800, 0.68).
narrative_ontology:measurement(shinbutsu_bundle_tr_t1850, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1850, 0.73).
narrative_ontology:measurement(shinbutsu_bundle_tr_t1868, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1868, 0.76).
narrative_ontology:measurement(shinbutsu_bundle_tr_t1872, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1872, 0.78).

% Extraction over time
narrative_ontology:measurement(shinbutsu_bundle_be_t1600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1600, 0.45).
narrative_ontology:measurement(shinbutsu_bundle_be_t1650, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1650, 0.52).
narrative_ontology:measurement(shinbutsu_bundle_be_t1700, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1700, 0.58).
narrative_ontology:measurement(shinbutsu_bundle_be_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1750, 0.63).
narrative_ontology:measurement(shinbutsu_bundle_be_t1800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1800, 0.68).
narrative_ontology:measurement(shinbutsu_bundle_be_t1850, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1850, 0.7).
narrative_ontology:measurement(shinbutsu_bundle_be_t1868, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1868, 0.71).
narrative_ontology:measurement(shinbutsu_bundle_be_t1872, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1872, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_bundle_su_t1600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1600, 0.4).
narrative_ontology:measurement(shinbutsu_bundle_su_t1650, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1650, 0.45).
narrative_ontology:measurement(shinbutsu_bundle_su_t1700, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1700, 0.5).
narrative_ontology:measurement(shinbutsu_bundle_su_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1750, 0.55).
narrative_ontology:measurement(shinbutsu_bundle_su_t1800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1800, 0.58).
narrative_ontology:measurement(shinbutsu_bundle_su_t1850, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1850, 0.62).
narrative_ontology:measurement(shinbutsu_bundle_su_t1868, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1868, 0.65).
narrative_ontology:measurement(shinbutsu_bundle_su_t1872, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1872, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__incoherent_bundle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_bunri_enforcement).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, tokugawa_temple_registration_system).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shugendo_institutional_identity).

% DUAL FORMULATION NOTE:
% This reading decomposes the 'shinbutsu-shugo' label into an incoherent bundle maintained by power (this story), a syncretic fusion ontology (syncretic_fusion_reading), and a domain partition arrangement (domain_partition_reading). The three stories form a constraint family: the bundle reading treats the other two as rhetorical strategies deployed *within* the bundle, not as the bundle's genuine structure. Meiji bunri enforcement is the downstream constraint that forcibly resolved the bundle's suppressed contradictions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
