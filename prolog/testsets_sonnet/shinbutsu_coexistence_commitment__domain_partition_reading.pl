% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Shinbutsu Domain-Partition Commitment (kami govern life/purity/harvest, Buddhas govern death/salvation)
 *   domain: religious/philosophical/historical
 *
 * SUMMARY:
 *   This constraint models the domain-partition reading of shinbutsu-shugo:
 *   kami and Buddhist deities are held to govern separate existential
 *   territories — kami over life, purity, and harvest; Buddhas over death,
 *   salvation, and the afterlife — without any claim that the two divine
 *   orders are ontologically unified. This is distinct from the syncretic
 *   fusion reading (honji suijaku, where kami are literally local
 *   manifestations of Buddhas — a claim of ontological identity this reading
 *   explicitly does not make) and from the incoherent bundle reading (which
 *   denies the arrangement was ever a coherent settlement at all, treating it
 *   as ambiguity sustained by institutional power until Meiji pressure broke
 *   it). The domain-partition reading treats the arrangement as a genuinely
 *   functional jurisdictional division: two independent systems, each
 *   authoritative in its own territory, coordinated through practice rather
 *   than doctrine. ε is low and stable because this reading claims minimal
 *   extraction — the coordination function (dividing ritual labor along
 *   life/death lines) is real and the material flows to each institution
 *   track the services it actually renders, not rents extracted through
 *   suppressed alternatives.
 *
 * KEY AGENTS:
 *   - shrine_priesthoods: agenda_setter/beneficiary (organized/constrained) — administer the life/purity/harvest domain
 *   - temple_clergy: agenda_setter/beneficiary (organized/constrained) — administer the death/salvation domain
 *   - village_ritual_specialists: beneficiary (moderate/constrained) — operate across both domains as occasion requires
 *   - lay_householders: beneficiary (powerless/constrained) — draw on both systems without needing them reconciled
 *   - systematic_theologians: excluded (moderate/analytical) — would press for ontological coherence but are structurally marginal
 *   - meiji_state_reformers: excluded (institutional/analytical) — later treat the partition as intolerable ambiguity
 *   - comparative_religion_scholars: observer (analytical/global) — study the arrangement as functioning pluralism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.28).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.22).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Shinbutsu Domain-Partition Commitment (kami govern life/purity/harvest, Buddhas govern death/salvation)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious/philosophical/historical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, '592a88a3-186a-4f1e-9fc3-ed7cc2e205bc').
narrative_ontology:cs_kernel_codification('592a88a3-186a-4f1e-9fc3-ed7cc2e205bc', distributed).
narrative_ontology:cs_authority_grounding('592a88a3-186a-4f1e-9fc3-ed7cc2e205bc', practice).
narrative_ontology:cs_interpretation_layer_present('592a88a3-186a-4f1e-9fc3-ed7cc2e205bc').
narrative_ontology:cs_reading_relation('592a88a3-186a-4f1e-9fc3-ed7cc2e205bc', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('592a88a3-186a-4f1e-9fc3-ed7cc2e205bc', shinbutsu_coexistence_commitment__incoherent_bundle_reading, influences).
narrative_ontology:cs_axiom('592a88a3-186a-4f1e-9fc3-ed7cc2e205bc', foundational, domain_jurisdiction_suffices_without_ontological_ranking).
narrative_ontology:cs_axiom_status(domain_jurisdiction_suffices_without_ontological_ranking, holdable).
narrative_ontology:cs_axiom_grounding('592a88a3-186a-4f1e-9fc3-ed7cc2e205bc', domain_jurisdiction_suffices_without_ontological_ranking, conventional).
narrative_ontology:cs_axiom('592a88a3-186a-4f1e-9fc3-ed7cc2e205bc', secondary, practice_based_coexistence_requires_no_doctrinal_settlement).
narrative_ontology:cs_axiom_status(practice_based_coexistence_requires_no_doctrinal_settlement, holdable).
narrative_ontology:cs_axiom_grounding('592a88a3-186a-4f1e-9fc3-ed7cc2e205bc', practice_based_coexistence_requires_no_doctrinal_settlement, instrumental).
narrative_ontology:cs_reference_frame('592a88a3-186a-4f1e-9fc3-ed7cc2e205bc', pre_honji_suijaku_dual_jurisdiction).
narrative_ontology:cs_drift_state('592a88a3-186a-4f1e-9fc3-ed7cc2e205bc', late_heian_theological_systematization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('592a88a3-186a-4f1e-9fc3-ed7cc2e205bc', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, shrine_priesthoods).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, temple_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, village_ritual_specialists).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, lay_householders).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__domain_partition_reading, functional_pluralism_without_theological_resolution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer kami rites governing birth, purity, harvest, and community life-cycle events. They maintain jurisdiction over the 'life' domain by declining to compete with temples over death ritual, which preserves their institutional niche without requiring doctrinal argument against Buddhist claims.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, shrine_priesthoods, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, shrine_priesthoods, beneficiary).

% Administer funerary rites, memorial services, and salvation-oriented practice governing death and the afterlife. They benefit from an uncontested monopoly over mortuary ritual, which the domain partition hands them without needing to displace kami worship from village life.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, temple_clergy, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, temple_clergy, beneficiary).

% Move fluidly between shrine and temple obligations depending on the occasion, drawing on kami blessing for planting and birth and Buddhist rites for death and ancestor memorial, without needing either tradition to explain the other. Their livelihood depends on the boundary holding, not on it being resolved.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, village_ritual_specialists, beneficiary,
    moderate, biographical, constrained, local).

% Participate in kami festivals for harvest and purification and in Buddhist funerals and memorial rites for the dead, using whichever practice fits the occasion. They have no stake in theological unification and would find a forced choice between systems a genuine loss of practical resources.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, lay_householders, beneficiary,
    powerless, biographical, constrained, local).

% Would press for an account of how a single cosmos can contain two independent divine hierarchies with no ontological subordination between them. Historically marginal to a religious culture organized around practice rather than doctrine, their objection is rarely voiced in the ritual centers themselves.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, systematic_theologians, excluded,
    moderate, civilizational, analytical, national).

% Later sought to forcibly separate kami worship from Buddhist practice (shinbutsu bunri) to construct a purified State Shinto, treating the long-standing domain partition as an intolerable ambiguity rather than a stable settlement. Not part of this reading's operative period but relevant as the eventual disruption it faces.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, meiji_state_reformers, excluded,
    institutional, generational, analytical, national).

% Study the domain-partition arrangement as a case of functioning religious pluralism achieved through jurisdictional division rather than doctrinal synthesis, comparing it to other traditions that manage multiple divine orders without insisting on a single coherent cosmology.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides ritual jurisdiction along existential lines — kami handle the domain of ongoing life (birth, purity, agricultural fertility), Buddhas handle the domain of death and what follows it — so that two distinct religious institutions can each serve the population's full life-cycle needs without competing for the same ritual occasions or requiring a shared cosmology.
% TRANSFER_FUNCTION: Distributes ritual authority and the material support (offerings, labor, land grants) that follows it: shrines receive support for life-affirming and agricultural rites, temples receive support for funerary and memorial rites. No systematic transfer runs from one institution to the other; each draws its own base from the same lay population for different occasions.
% ABSENT_VOICES: Systematic theologians pressing for ontological coherence are structurally sidelined in a religious culture organized around occasion-appropriate practice rather than doctrinal system; their objection surfaces mainly in learned commentary, rarely in the shrine or temple precinct itself.
% DISAPPEARANCE_RATIONALE: If the domain partition dissolved without being replaced by either fusion or forced separation, funerary and life-cycle ritual would have to be renegotiated institution by institution — either one system would have to claim the other's domain, or households would lose a functioning division of ritual labor that currently requires no theological justification to operate.
% FOUNDING_PROBLEM: Early religious life in Japan needed rites for both the ongoing concerns of the living (birth, harvest, purification) and the newly imported Buddhist framework for death and salvation; no single system covered both domains adequately, and forcing one to absorb the other risked destabilizing whichever institution lost its distinct function.
% FOUNDING_PROBLEM_CORROBORATION: Comparative religion scholars, working from outside both the shrine and temple institutions, attest that the partition functioned as a stable jurisdictional settlement for centuries without requiring doctrinal resolution. Meiji-era state reformers, also external to the ordinary operation of the arrangement, treated the same settlement as an intolerable ambiguity requiring forcible separation — their disagreement itself corroborates that the founding problem (dual ritual coverage without unification) was real and unresolved rather than settled doctrine.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness starts low (0.18) and rises only modestly (to 0.28) over the interval, reflecting gradual institutional consolidation (land grants, formalized parish systems) without any single actor capturing disproportionate rents — both shrine and temple institutions gain roughly proportionally to the ritual services they provide. Suppression is low (0.22) because neither institution needs to coerce participation in the other's domain; households move between them voluntarily by occasion. Theater ratio rises slowly (0.15 to 0.30) as ritual practice becomes more elaborate and institutionally formalized over centuries, but remains well below the level that would indicate the coordination function has hollowed out. Accessibility collapse (0.40) is moderate: alternatives to the two-track system existed in principle (folk practices outside both frameworks, later Confucian-inflected syntheses) but became progressively less visible as the partition normalized. Resistance (0.25) is low, consistent with a settlement that most participants experienced as convenient rather than imposed — the resistance that does appear comes from theologians and later from Meiji reformers, both excluded seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine priesthoods and temple clergy are declared beneficiaries and agenda-setters because each administers and draws material support from its own domain, but neither extracts from the other's domain or from lay practitioners beyond ordinary ritual fees — this keeps their derived directionality closer to symmetric than to a pure-target or pure-extractor position. Village ritual specialists and lay householders are beneficiaries because the two-track system expands their practical ritual options rather than constraining them; forcing a single system would be a net loss for them, not a liberation. No victim group is declared under this reading, because the domain-partition story does not claim anyone is being extracted from through the coexistence itself — the incoherent_bundle_reading would name different victims (those harmed by sustained ambiguity) and the syncretic_fusion_reading raises different questions about kami subordination, but those belong to sibling constraints, not this one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — needing both life-affirming and death/salvation ritual coverage without one system displacing the other — remains live in the sense that households still require both kinds of ritual service; the arrangement has not become a persisting shell around a defunct need. The 'contested' status reflects that different observers (comparative religionists vs. Meiji-era state reformers) reach opposite verdicts about whether the arrangement was a stable solution or an unstable ambiguity — that contest is exactly what the sibling readings (incoherent_bundle_reading) are built to capture, and this story deliberately does not try to resolve it internally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_stability_vs_convenient_fiction,
    'Was the life/death domain partition a genuinely stable settlement recognized as such by participants, or a convenient working fiction that avoided a harder question about how two independent divine hierarchies could coexist in one cosmos?',
    'Examine whether contemporaneous sources (temple and shrine records, popular ritual manuals) treat the boundary as a settled fact requiring no defense, versus whether boundary disputes (e.g. over funerary rights encroaching on shrine precincts, or shrine rites claimed by temples) recur across the period, which would suggest the partition was continually renegotiated rather than fixed.',
    'If the boundary was continually contested and required active management, this reading''s low suppression and low resistance scores would be understated, and the constraint would sit closer to the incoherent_bundle_reading''s territory. If the boundary was genuinely stable and unremarked, the domain_partition_reading is well-supported as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_stability_vs_convenient_fiction, empirical, 'Whether the domain partition was a settled fact or a continually managed ambiguity.').

omega_variable(
    reading_selection_criterion,
    'What structural signal justifies treating this constraint as a genuine functional partition rather than either an ontological unification (syncretic_fusion_reading) or a sustained incoherence (incoherent_bundle_reading) — given that all three readings describe the same historical practice of kami and Buddhist coexistence?',
    'The choice here is guided by the absence of a widely enforced honji suijaku doctrine in earliest-period practice (which would support fusion) and by the absence of documented widespread doctrinal crisis or forced resolution prior to Meiji (which would support incoherence). Later periods show honji suijaku theology becoming more prominent, suggesting the three readings may map to different historical phases rather than being permanently exclusive alternatives.',
    'If honji suijaku theology is shown to have been operative and authoritative from very early in the period this story covers, the domain_partition_reading''s claim of ''no ontological unification'' would be undermined for that phase, and the interval declared here may need to be narrowed to a pre-honji-suijaku period specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_criterion, conceptual, 'Whether the three kernel readings map to genuinely coextensive alternatives or to different historical phases of the same evolving practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 200, 0.17).
narrative_ontology:measurement(shin_tr_t400, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 400, 0.2).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 600, 0.23).
narrative_ontology:measurement(shin_tr_t800, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 800, 0.26).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1000, 0.28).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1200, 0.3).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(shin_be_t200, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 200, 0.2).
narrative_ontology:measurement(shin_be_t400, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 400, 0.22).
narrative_ontology:measurement(shin_be_t600, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 600, 0.24).
narrative_ontology:measurement(shin_be_t800, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 800, 0.26).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1000, 0.27).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1200, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_coexistence_commitment__domain_partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__domain_partition_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the shinbutsu_coexistence_commitment kernel, decomposed per the ε-invariance principle because the natural-language label 'shinbutsu-shugo' covers structurally distinct claims about ontology, coherence, and function. The domain_partition_reading (this file) authors low, stable ε on the claim that ritual jurisdiction was genuinely divided without requiring theological unification. The syncretic_fusion_reading authors a different ε profile appropriate to a claim of ontological identity between kami and Buddhas (honji suijaku), which raises different questions about doctrinal hierarchy and possible subordination of kami to Buddhist cosmology. The incoherent_bundle_reading authors yet another ε profile for the claim that the entire arrangement was sustained ambiguity rather than either a genuine partition or a genuine fusion, collapsing under Meiji-era pressure. All three are linked via affects_constraints because they compete for the same historical evidentiary base and a shift in scholarly consensus about one reading's plausibility structurally affects the others' relative weight.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
