% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__ontological_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__ontological_fusion_reading, []).

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
 *   constraint_id: simultaneous_veneration__ontological_fusion_reading
 *   human_readable: Honji-Suijaku Ontological Fusion Doctrine (Kami-as-Buddha-Manifestation)
 *   domain: religious/institutional/Japanese_history
 *
 * SUMMARY:
 *   Honji-suijaku ('original ground, manifest trace') theory, elaborated
 *   chiefly by Tendai and Shingon exegetes from roughly the ninth century
 *   onward, asserts that Japanese kami are local manifestations (suijaku) of
 *   universal Buddhist deities (honji) — Amaterasu as a trace of Dainichi
 *   Nyorai, Hachiman as a bodhisattva, and so on. Read as literal ontological
 *   claim (this story's reading), the theory does more than harmonize two
 *   traditions: it establishes which institution holds final interpretive
 *   authority over what a kami fundamentally is, and that authority accrued
 *   overwhelmingly to Buddhist temple establishments administering combined
 *   shrine-temple complexes (jingu-ji).
 *
 * KEY AGENTS:
 *   - buddhist_institutional_hierarchy: agenda-setting exegetical authority; issues honji rulings and administers combined temple-shrine complexes
 *   - indigenous_kami_cults: primary target; local cosmological autonomy dissolved into subordinate manifestation status
 *   - local_shrine_priesthoods: secondary payer; ritual authority made contingent on externally-issued doctrinal rulings
 *   - imperial_court_and_shogunal_authorities: institutional beneficiary using fused cosmology for state legitimation
 *   - comparative_religion_scholars: analytical observer evaluating sincerity vs. institutional function of the doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.71).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.62).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Honji-Suijaku Ontological Fusion Doctrine (Kami-as-Buddha-Manifestation)").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious/institutional/Japanese_history").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, '29519af9-31ce-40e1-8a92-a9d762c45a97').
narrative_ontology:cs_kernel_codification('29519af9-31ce-40e1-8a92-a9d762c45a97', formalized).
narrative_ontology:cs_authority_grounding('29519af9-31ce-40e1-8a92-a9d762c45a97', lineage).
narrative_ontology:cs_interpretation_layer_present('29519af9-31ce-40e1-8a92-a9d762c45a97').
narrative_ontology:cs_reading_relation('29519af9-31ce-40e1-8a92-a9d762c45a97', simultaneous_veneration__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('29519af9-31ce-40e1-8a92-a9d762c45a97', simultaneous_veneration__pragmatic_incoherence_reading, forecloses).
narrative_ontology:cs_axiom('29519af9-31ce-40e1-8a92-a9d762c45a97', foundational, kami_and_buddhas_share_single_underlying_nature).
narrative_ontology:cs_axiom_status(kami_and_buddhas_share_single_underlying_nature, holdable).
narrative_ontology:cs_axiom_grounding('29519af9-31ce-40e1-8a92-a9d762c45a97', kami_and_buddhas_share_single_underlying_nature, theological).
narrative_ontology:cs_axiom('29519af9-31ce-40e1-8a92-a9d762c45a97', secondary, buddhist_exegetical_authority_correctly_identifies_honji_suijaku_pairings).
narrative_ontology:cs_axiom_status(buddhist_exegetical_authority_correctly_identifies_honji_suijaku_pairings, holdable).
narrative_ontology:cs_axiom_grounding('29519af9-31ce-40e1-8a92-a9d762c45a97', buddhist_exegetical_authority_correctly_identifies_honji_suijaku_pairings, conventional).
narrative_ontology:cs_reference_frame('29519af9-31ce-40e1-8a92-a9d762c45a97', heian_era_exegetical_consensus).
narrative_ontology:cs_drift_state('29519af9-31ce-40e1-8a92-a9d762c45a97', meiji_restoration_shinbutsu_bunri, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('29519af9-31ce-40e1-8a92-a9d762c45a97', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, shingon_and_tendai_exegetes).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_cults).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, local_shrine_priesthoods).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, lay_worshippers).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, imperial_court_and_shogunal_authorities).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, lay_worshippers).
narrative_ontology:constraint_vindicates(simultaneous_veneration__ontological_fusion_reading, honji_suijaku_metaphysical_identity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major temple-shrine complexes (jingu-ji) and sectarian exegetes (Tendai, Shingon) author and administer the honji-suijaku framework, declaring specific kami to be traces (suijaku) of specific buddhas or bodhisattvas (honji). This doctrinal ruling determines ritual precedence, land and tax allocation between temple and shrine, and which priesthood controls interpretation of a local cult's meaning. The hierarchy loses nothing by asserting ontological identity — it gains interpretive jurisdiction over kami worship nationwide.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, beneficiary).

% Local kami traditions, often pre-dating Buddhist arrival by centuries, are reclassified as provisional, lesser manifestations of a Buddhist original. Their independent cosmological status is dissolved into a subordinate position within someone else's metaphysical system. Because the ruling is framed as revealed metaphysical truth rather than negotiated administrative arrangement, there is no venue to contest the reclassification on the cult's own terms — objecting requires either fluency in Buddhist doctrinal argument or is simply unheard.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_cults, payer,
    powerless, generational, trapped, local).

% Shrine keepers who administer kami worship find their ritual authority made contingent on a temple-side theological ruling about what their kami 'really is.' Where the jingu-ji institutional structure fuses temple and shrine administration, shrine priests report to or share revenue with the temple hierarchy. Some priesthoods benefit from the prestige of a favorable honji identification; most experience a net subordination of local practice to translated doctrine they did not author.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, local_shrine_priesthoods, payer,
    moderate, biographical, constrained, regional).

% Ordinary worshippers receive a unified cosmology that lets them venerate kami for this-worldly benefit and buddhas for salvation without feeling they are betraying either tradition — a real psychological and ritual convenience. They also inherit whatever hierarchy of sacred value the honji-suijaku ruling assigns to their local kami, which can diminish the perceived standing of a beloved local deity relative to imported Buddhist figures the worshipper never chose to rank above it.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, lay_worshippers, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, lay_worshippers, payer).

% State authorities use the fused cosmology to legitimate rule (the emperor as descendant of kami reinterpreted through Buddhist cosmic order) and to administer a single integrated religious-institutional apparatus rather than negotiating with two separate, potentially competing religious establishments. The ontological fusion claim is convenient state theology as much as it is theology.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, imperial_court_and_shogunal_authorities, beneficiary,
    institutional, civilizational, arbitrage, national).

% Later reformers who forcibly separated kami and buddhas (shinbutsu bunri, 1868) are not present in the honji-suijaku era's conversation but represent the eventual repudiation of this reading's premise — their absence from the medieval and early-modern discourse means the fusion doctrine faced no organized institutional challenge for roughly a millennium, only individual and localized friction.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, meiji_state_shinto_reformers, excluded,
    organized, generational, mobile, national).

% Modern scholars evaluate whether the historical honji-suijaku framework should be read as sincere metaphysics, functional syncretism, or institutional theology serving temple interests, without a stake in any shrine's revenue or ritual precedence.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, non-contradictory cosmological map that lets one population venerate both kami and buddhas without perceived religious conflict, and lets a fused temple-shrine institutional structure (jingu-ji) administer both cults under one interpretive roof rather than maintaining two competing religious bureaucracies.
% TRANSFER_FUNCTION: Moves interpretive authority over kami worship — and the ritual precedence, land grants, and tax revenue that follow interpretive authority — from independent local shrine priesthoods and kami-cult custodians to the Buddhist institutional hierarchy that issues the honji rulings.
% ABSENT_VOICES: The kami cults themselves, as maintained by illiterate or non-doctrinally-trained local custodians, have no seat in the exegetical debate that decides their own metaphysical status; the ruling is made in a textual register (Buddhist scholastic argument) the affected party did not participate in producing. Meiji-era Shinto purists who would later reject the fusion outright are also absent from this era's discourse — their critique postdates the arrangement by centuries.
% DISAPPEARANCE_RATIONALE: If the ontological-identity claim were withdrawn while joint veneration continued, shrine priesthoods would regain independent claim to define their own kami's nature, jingu-ji institutions would lose their doctrinal basis for combined administration and revenue-sharing, and land/precedence disputes currently settled by honji rulings would reopen as contests between independently-standing traditions — which is approximately what did happen at the Meiji separation, disruptively.
% FOUNDING_PROBLEM: Buddhism arriving in Japan needed a way to explain its relationship to entrenched, locally powerful kami cults without either provoking their custodians by declaring the kami false, or conceding that Buddhist teaching was merely one religion among equals competing with an older one.
% FOUNDING_PROBLEM_CORROBORATION: The founding accommodation problem (managing a new religion's arrival amid entrenched cults) had been resolved for centuries before Meiji reformers dismantled the arrangement in 1868 for essentially unrelated nationalist and political reasons — the separation edict's authors and modern historians of the period (e.g., scholarship on shinbutsu bunri) attest that by the early modern era the doctrine functioned as settled institutional administration and inherited theology rather than as an active answer to any live coexistence crisis; this corroboration comes from the state actors who dismantled it and from historians outside either the Buddhist or Shinto institutional lineages.
narrative_ontology:disappearance_verdict(simultaneous_veneration__ontological_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__ontological_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__ontological_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(simultaneous_veneration__ontological_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__ontological_fusion_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__ontological_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__ontological_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored as substantial (0.71 by the interval's end) because the ontological-fusion claim is not merely descriptive convenience but a jurisdictional ruling: it decides, as asserted metaphysical fact, whose deity is derivative of whose, and that decision routes ritual precedence and revenue through the party issuing the ruling. Suppression (0.62) reflects that the framework is sustained less by physical coercion than by the near-total absence of a legitimate register in which a kami cult could contest its own reclassification — the contest would have to be conducted in Buddhist scholastic terms the cult did not originate. Theater ratio rises across the interval (0.20 to 0.40) as the doctrine calcifies from an actively negotiated accommodation into routine institutional administration whose original persuasive work is largely done and whose remaining function is maintaining an inherited jurisdictional settlement.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist hierarchy's seat, honji-suijaku is settled metaphysical truth generating no felt extraction — it is simply what is real. From a local shrine priesthood's seat, the same doctrine is an externally imposed ranking that subordinates a locally sovereign tradition to an imported one, felt as extraction of interpretive and material authority. The engine should compute divergent seat-level types from this single structural description; the claimed_type (tangled_rope) reflects the analytical judgment that both a genuine coordination function (a livable joint cosmology) and asymmetric extraction (institutional interpretive monopoly) are simultaneously present and require active enforcement to hold, which is exactly why the pragmatic_incoherence sibling reading disputes that any such settled function was ever actually achieved.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist institutional hierarchy sits at the beneficiary end: it authors the ruling, administers the resulting combined institutions, and loses nothing by asserting identity rather than parity. Indigenous kami cults and local shrine priesthoods sit toward the target end: trapped or constrained exit, no alternative doctrinal venue, and a ruling issued about them rather than negotiated with them. Lay worshippers and the state occupy a genuinely mixed position — real coordination benefit (a livable, non-contradictory cosmology; simplified state administration) alongside inherited subordination effects they did not choose, which is why they are marked beneficiary with a payer secondary role rather than pure beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding accommodation problem — managing Buddhism's arrival alongside entrenched, locally powerful kami cults without provoking their custodians or conceding religious equality — was substantially resolved well before the Meiji era. By the time of shinbutsu bunri, the doctrine's operative function was inherited institutional administration (land tenure, ritual precedence, combined temple-shrine revenue) rather than active accommodation of live religious conflict. This is the R5 signature of a mandate that outlived its founding function while its institutional apparatus persisted — the tangled_rope classification, on this reading, captures a coordination arrangement whose original problem is dead while its extraction infrastructure remained fully active until forcibly dismantled by an external political act rather than by internal resolution or negotiated exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sincere_metaphysics_vs_institutional_theology,
    'Did the exegetes who articulated honji-suijaku genuinely believe in literal ontological identity between specific kami and specific buddhas, or was the doctrine primarily an institutionally convenient formalism that took on the rhetorical form of metaphysical assertion?',
    'Close textual analysis of Tendai/Shingon doctrinal writings for internal argumentative rigor and consistency versus ad hoc, jurisdiction-serving assignments; comparison of honji rulings against contemporaneous land/tax disputes to test correlation between doctrinal outcome and institutional benefit.',
    'If sincere, the extraction described here is a side effect of a genuine (if institutionally convenient) theological conviction rather than a fabricated cover story, which would lower confidence that suppression was primarily strategic rather than doctrinally motivated. If primarily instrumental, the tangled_rope reading strengthens toward snare-adjacent territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincere_metaphysics_vs_institutional_theology, conceptual, 'Whether the ontological identity claim was sincerely held metaphysics or institutionally motivated formalism.').

omega_variable(
    kernel_reading_selection_basis,
    'Among the three declared readings of the simultaneous_veneration kernel (domain_partition, ontological_fusion, pragmatic_incoherence), what historical or textual evidence would establish which reading best matches how a given historical population — as opposed to a given elite exegetical school — actually held the belief?',
    'Ethnographic and textual triangulation across social strata: elite doctrinal writings likely support ontological_fusion or domain_partition; popular practice records, votive inscriptions, and local shrine records may better support pragmatic_incoherence or domain_partition. No single reading should be assumed representative of the whole population across the full 1000-year interval.',
    'If the pragmatic_incoherence reading better describes lived popular practice while ontological_fusion describes only elite exegetical production, then the extraction and suppression authored in this story apply narrowly to the doctrinal-institutional stratum and should not be generalized to describe the beliefs of ordinary worshippers, who may have experienced something closer to unreflective dual practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, empirical, 'Which reading of the kernel best fits which stratum of historical practitioner, elite versus popular.').

omega_variable(
    meiji_separation_as_resolution_or_rupture,
    'Does the 1868 shinbutsu bunri separation constitute evidence that the ontological_fusion reading was never metaphysically stable (vindicating pragmatic_incoherence), or was it a politically motivated rupture of a genuinely stable millennium-long metaphysical settlement for reasons external to the doctrine''s internal coherence (nationalism, anti-Buddhist sentiment, state modernization)?',
    'Historical analysis of Meiji-era political motivations for shinbutsu bunri versus internal doctrinal critique literature predating the Restoration; presence or absence of significant pre-Meiji internal theological challenges to honji-suijaku would be decisive.',
    'If the separation was purely externally motivated (state modernization, anti-Buddhist nationalism) with no significant internal doctrinal collapse preceding it, this supports the ontological_fusion reading''s claim to have been a stable, functioning metaphysical settlement until forcibly ended — strengthening this story''s founding_problem_status as ''dead but externally terminated'' rather than ''always incoherent.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_as_resolution_or_rupture, empirical, 'Whether Meiji-era separation reflects doctrinal collapse or external political rupture of a stable arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__ontological_fusion_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(simu_tr_t200, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 200, 0.25).
narrative_ontology:measurement(simu_tr_t400, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 400, 0.3).
narrative_ontology:measurement(simu_tr_t600, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 600, 0.34).
narrative_ontology:measurement(simu_tr_t800, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 800, 0.37).
narrative_ontology:measurement(simu_tr_t1000, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1000, 0.4).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(simu_be_t200, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 200, 0.5).
narrative_ontology:measurement(simu_be_t400, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 400, 0.58).
narrative_ontology:measurement(simu_be_t600, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 600, 0.64).
narrative_ontology:measurement(simu_be_t800, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 800, 0.68).
narrative_ontology:measurement(simu_be_t1000, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1000, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(simu_su_t200, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 200, 0.42).
narrative_ontology:measurement(simu_su_t400, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 400, 0.5).
narrative_ontology:measurement(simu_su_t600, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 600, 0.55).
narrative_ontology:measurement(simu_su_t800, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 800, 0.59).
narrative_ontology:measurement(simu_su_t1000, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1000, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__ontological_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__ontological_fusion_reading, 0.08).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the simultaneous_veneration kernel per the ε-invariance principle: this story (ontological_fusion_reading, high ε ~0.71, tangled_rope) asserts literal metaphysical identity enforced through institutional interpretive monopoly; domain_partition_reading (lower ε expected) treats kami and buddhas as functionally distinct across separate life-domains, requiring no adjudicated identity claim; pragmatic_incoherence_reading (near-zero ε expected) holds that no metaphysical resolution was ever achieved and the arrangement persisted through absence of enforcement pressure rather than through any authority's active ruling. All three are linked via affects_constraints because they compete for the same historical evidentiary base and because institutional developments favoring one reading (e.g., an authoritative honji ruling) create downstream pressure on the others' plausibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
