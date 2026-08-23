% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__shafii_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__shafii_reading
 *   human_readable: Shafi'i Usul al-Fiqh: Hadith Authentication Prerequisite & Hierarchical Source Governance
 *   domain: legal/theological/intellectual
 *
 * SUMMARY:
 *   The Shafi'i reading of usul al-fiqh systematizes Islamic legal derivation
 *   into a strict hierarchical meta-discipline: Quran > authenticated hadith
 *   > qiyas (analogical reasoning, permitted only when authenticated hadith
 *   is absent) > ijma restricted to Companions' consensus. This hierarchy
 *   makes hadith authentication a prerequisite gate — legal derivation cannot
 *   proceed without passing through the isnad-based verification machinery
 *   operated by hadith transmission specialists. The constraint claims to be
 *   a genuine coordination mechanism (tangled_rope: solves the problem of
 *   unreliable legal sources by imposing authentication standards) but
 *   simultaneously extracts from jurists who claim authority through
 *   rationalist methods (ra'y, expansive qiyas, istihsan) by subordinating
 *   their methods to textual authentication. The beneficiary is the hadith
 *   transmission specialist class, whose gatekeeping authority is
 *   institutionalized; the victim is the rationalist jurist whose
 *   methodological autonomy is suppressed. The measurement series runs on a
 *   shared time grid from early formative period (t=0) to classical
 *   consolidation (t=300 AH/912 CE), showing extraction accumulation as the
 *   hierarchy hardens and theater rises as performative adherence to the
 *   meta-discipline replaces substantive methodological debate.
 *
 * KEY AGENTS:
 *   - hadith_transmission_specialists: Primary beneficiary (institutional/organized) — control the authentication gate, collect epistemic rents
 *   - shafii_madhhab_institutions: Secondary beneficiary/agenda_setter (institutional) — administer the meta-discipline, certify compliance
 *   - rationalist_jurists: Primary victim (organized/moderate) — bear exclusion from derivation when their methods fall outside the hierarchy
 *   - non_shafii_tradition_jurists: Secondary victim (organized) — their alternative source hierarchies are delegitimized by the Shafi'i framework's claim to systematic exclusivity
 *   - analytical_observer: Observer (analytical) — sees full structure from comparative jurisprudence seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, 0.62).
domain_priors:suppression_score(usul_al_fiqh_method__shafii_reading, 0.48).
domain_priors:theater_ratio(usul_al_fiqh_method__shafii_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__shafii_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__shafii_reading, "Shafi'i Usul al-Fiqh: Hadith Authentication Prerequisite & Hierarchical Source Governance").
narrative_ontology:topic_domain(usul_al_fiqh_method__shafii_reading, "legal/theological/intellectual").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__shafii_reading, '036c3458-97ad-456b-87d0-220a757af7e2').
narrative_ontology:cs_kernel_codification('036c3458-97ad-456b-87d0-220a757af7e2', formalized).
narrative_ontology:cs_authority_grounding('036c3458-97ad-456b-87d0-220a757af7e2', lineage).
narrative_ontology:cs_interpretation_layer_present('036c3458-97ad-456b-87d0-220a757af7e2').
narrative_ontology:cs_reading_relation('036c3458-97ad-456b-87d0-220a757af7e2', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('036c3458-97ad-456b-87d0-220a757af7e2', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('036c3458-97ad-456b-87d0-220a757af7e2', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('036c3458-97ad-456b-87d0-220a757af7e2', foundational, hadith_authentication_prerequisite).
narrative_ontology:cs_axiom_status(hadith_authentication_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('036c3458-97ad-456b-87d0-220a757af7e2', hadith_authentication_prerequisite, deontological).
narrative_ontology:cs_axiom('036c3458-97ad-456b-87d0-220a757af7e2', foundational, qiyas_subordination_to_authenticated_text).
narrative_ontology:cs_axiom_status(qiyas_subordination_to_authenticated_text, holdable).
narrative_ontology:cs_axiom_grounding('036c3458-97ad-456b-87d0-220a757af7e2', qiyas_subordination_to_authenticated_text, deontological).
narrative_ontology:cs_axiom('036c3458-97ad-456b-87d0-220a757af7e2', secondary, companions_ijma_restriction).
narrative_ontology:cs_axiom_status(companions_ijma_restriction, holdable).
narrative_ontology:cs_axiom_grounding('036c3458-97ad-456b-87d0-220a757af7e2', companions_ijma_restriction, conventional).
narrative_ontology:cs_axiom('036c3458-97ad-456b-87d0-220a757af7e2', foundational, systematized_usul_as_meta_discipline).
narrative_ontology:cs_axiom_status(systematized_usul_as_meta_discipline, holdable).
narrative_ontology:cs_axiom_grounding('036c3458-97ad-456b-87d0-220a757af7e2', systematized_usul_as_meta_discipline, conventional).
narrative_ontology:cs_reference_frame('036c3458-97ad-456b-87d0-220a757af7e2', classical_shafii_usul_system).
narrative_ontology:cs_drift_state('036c3458-97ad-456b-87d0-220a757af7e2', post_classical_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('036c3458-97ad-456b-87d0-220a757af7e2', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, shafii_madhhab_institutions).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, non_shafii_tradition_jurists).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, hadith_authentication_prerequisite).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, source_hierarchy_governance).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, companions_consensus_restriction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the isnad-based authentication machinery that the Shafi'i hierarchy makes prerequisite to all legal derivation. They evaluate transmitter reliability, grade hadith authenticity, and control which traditions enter the legal stream. Their authority is now structural — no derivation proceeds without their certification. They collect epistemic rents: scholarly prestige, institutional positions, and the power to include/exclude traditions that shape legal outcomes. Exit is arbitrage-grade: they can operate across madhhabs because all Sunni traditions require hadith authentication, though the Shafi'i hierarchy makes their gate most consequential.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, beneficiary,
    organized, generational, arbitrage, global).

% Administer the usul al-fiqh meta-discipline as a complete system: they certify who counts as a qualified jurist, define the boundaries of valid qiyas, police the hierarchy's enforcement, and legitimate the hadith specialists' gatekeeping. They benefit from the system's claim to systematic exclusivity — the Shafi'i madhhab's brand is 'the rational, systematic school.' Their identity is fused to the hierarchy: abandoning it would dissolve the institutional self-concept. Exit is identity_locked — they cannot leave without ceasing to be what they are.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, shafii_madhhab_institutions, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, shafii_madhhab_institutions, beneficiary).

% Jurists whose methodological authority derives from rationalist methods: ra'y (reasoned opinion), expansive qiyas (analogy extended beyond strict textual silence), istihsan (juristic preference for public interest). Under the Shafi'i hierarchy, their methods are subordinated — qiyas permitted only when authenticated hadith is absent, ra'y excluded, istihsan rejected. They bear the cost of exclusion: their derivations are delegitimized unless they route through the authentication gate. Their exit is identity_locked — their intellectual self-concept is constituted by rationalist methodology; adopting the Shafi'i hierarchy means abandoning their epistemic identity.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, rationalist_jurists, payer,
    organized, biographical, identity_locked, global).

% Jurists operating in Hanafi, Maliki, or Hanbali frameworks whose source hierarchies differ structurally. The Shafi'i meta-discipline's claim to systematic exclusivity structurally delegitimizes their alternatives: Hanafi ra'y becomes 'uncontrolled opinion,' Maliki 'amal becomes 'mere custom,' Hanbali textual maximalism becomes 'unsystematic rigidity.' They bear diffuse costs: their traditions must constantly defend against the Shafi'i framework's claim to be the only coherent usul. Exit is constrained — they maintain independent institutions but face the Shafi'i hierarchy as a dominant reference frame in cross-madhhab discourse.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, non_shafii_tradition_jurists, payer,
    organized, generational, constrained, global).

% Scholars of comparative Islamic law who analyze the four madhhabs as a constraint family. They see the full structure: shared kernel (usul_al_fiqh_method), divergent readings with different ε, different beneficiary/victim structures, different coordination/extraction balances. They neither collect nor pay — their seat is the analytical vantage from which the kernel's contested nature is visible.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, comparative_jurisprudence_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem of fabricated and unreliable traditions corrupting legal derivation by imposing a standardized authentication methodology (isnad criticism, transmitter grading, matn analysis) that all derivations must pass through.
% TRANSFER_FUNCTION: Moves epistemic authority and derivational legitimacy from rationalist jurists (who claim authority through reasoning methods) to hadith transmission specialists (who control the authentication gate). The transfer is not monetary but epistemic-institutional: the right to say 'this is law' shifts from the reasoner to the authenticator.
% ABSENT_VOICES: Early pre-systematization jurists (pre-150 AH) who practiced law without a formal usul hierarchy; lay Muslims whose legal access is mediated by whichever madhhab's gatekeepers control their region; women scholars excluded from the transmission chains that the hierarchy certifies as authoritative.
% DISAPPEARANCE_RATIONALE: If the Shafi'i hierarchy vanished overnight, hadith authentication would lose its prerequisite status — rationalist methods (ra'y, expansive qiyas, istihsan) would regain derivational authority, alternative hierarchies (Hanafi, Maliki, Hanbali) would no longer be structurally delegitimized by a dominant meta-discipline, and the hadith specialist class would lose its gatekeeping rent. The legal derivation landscape would reorganize around methodological pluralism.
% FOUNDING_PROBLEM: Early Islamic legal derivation (pre-150 AH) relied on heterogeneous, often unreliable traditions: fabricated hadith circulated widely, transmitter chains were uncritically accepted, and rationalist methods (ra'y) operated without textual discipline — producing legal chaos and potential corruption of divine law.
% FOUNDING_PROBLEM_CORROBORATION: The Shafi'i tradition attests the problem is still live (ongoing fabrication threats, need for systematic discipline). Hanafi and Maliki traditions attest the problem was substantially solved by their own methods (Hanafi: rigorous qiyas discipline; Maliki: Medinan practice as living verification) without requiring the Shafi'i authentication prerequisite. Comparative legal historians (outside all madhhabs) corroborate that by 300 AH, isnad science had matured across all traditions and the authentication problem was largely solved — the hierarchy's persistence reflects institutionalization, not ongoing necessity.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__shafii_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(usul_al_fiqh_method__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__shafii_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) is substantial because the authentication prerequisite creates a structural rent: any legal derivation must pass through the hadith specialist bottleneck, whose authority is now a prerequisite rather than an input. Suppression (0.48) is moderate — the constraint does not ban rationalist methods outright but subordinates them, making their exercise costly and institutionally marginal. Theater (0.35) rises over time as adherence to the usul meta-discipline becomes a performative credential for institutional legitimacy rather than a functional necessity. Accessibility collapse (0.58) reflects that once the hierarchy is accepted, alternatives (Hanafi ra'y, Maliki 'amal) are structurally marginalized but not eliminated — they persist as competing frameworks. Resistance (0.65) is high because rival madhhabs actively contest the Shafi'i hierarchy's claim to exclusive systematicity, and rationalist jurists within the tradition periodically push against authentication rigidity.
 *
 * PERSPECTIVAL GAP:
 *   From the hadith specialist seat, the constraint is coordination: authentication solves the genuine problem of fabricated traditions corrupting law. From the rationalist jurist seat, it is extraction: the authentication gate captures their methodological labor and converts it into rental income for transmitters. The Shafi'i institutional seat experiences it as both — they administer the coordination but also benefit from the extraction. The engine computes this divergence from the structural data: beneficiaries (transmission specialists) get low d → low χ; payers (rationalist jurists) get high d → high χ; agenda_setters (Shafi'i institutions) sit near symmetric but with institutional power to shape the hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: hadith_transmission_specialists (control the authentication bottleneck, collect epistemic authority rents) and shafii_madhhab_institutions (administer the meta-discipline, gain institutional legitimacy). Victims declared: rationalist_jurists (excluded from derivation authority when their methods are subordinated) and non_shafii_tradition_jurists (their alternative hierarchies are structurally delegitimized). The derivation chain assigns d near 0.15 to beneficiaries (institutional power + arbitrage-grade exit to other madhhabs), d near 0.85 to rationalist jurists (organized but identity_locked to their methodological commitments — exit means abandoning their intellectual tradition), and d near 0.75 to non-Shafi'i jurists (organized but constrained exit — they must maintain their own institutional structures against the Shafi'i claim to systematic exclusivity).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unreliable legal derivation from fabricated traditions — was genuine and live at t=0. By t=300, the problem is substantially solved (authentication standards are established, isnad science is mature), but the hierarchy persists and hardens. The constraint now extracts more than it coordinates: the gatekeeping function has become a rent-collection mechanism. This is classic mandatrophy — the mandate (ensure reliable derivation) has outlived its function, but the constraint persists because the beneficiary class (hadith specialists) has institutionalized its gatekeeping role and the agenda-setter (Shafi'i institutions) has built its legitimacy on the meta-discipline. The R5 genealogy interview (founding_problem_status = contested) captures this: the Shafi'i tradition claims the problem is live (new fabrication threats), while outside corroboration (Hanafi/Maliki practice, comparative analysis) shows the problem is substantially solved and the arrangement persists as extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_kernel_identity,
    'Is this constraint one reading of a contested kernel (usul_al_fiqh_method) rather than an independent constraint?',
    'Comparative structural analysis of sibling readings (hanafi_reading, maliki_reading, hanbali_reading) to confirm shared referent and divergent ε/beneficiary structures.',
    'If confirmed as a kernel reading, the constraint''s ε is reading-indexed over the fixed referent of the standing usul arrangement; cross-reading comparison becomes mandatory for corpus integrity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'Whether this story instantiates a kernel reading and thus carries committer-frame structure.').

omega_variable(
    natural_law_vs_constructed_hierarchy,
    'Is the source hierarchy (Quran > authenticated hadith > qiyas > Companion ijma) a discovered feature of revelation or a constructed methodological choice that benefits hadith specialists?',
    'Historical analysis of pre-Shafi''i legal practice; comparison with Hanafi and Maliki source hierarchies that do not require hadith authentication as prerequisite.',
    'If constructed, the constraint is a false summit candidate (Falm) — a coordination claim masking extraction by hadith gatekeepers. If natural-law, the hierarchy is Mountain from every seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_hierarchy, conceptual, 'Natural-law vs. constructed status of the Shafi''i source hierarchy.').

omega_variable(
    sibling_reading_relations,
    'What is the structural relationship between this Shafi''i reading and its sibling readings (Hanafi, Maliki, Hanbali)?',
    'Map each sibling''s core premise against this reading''s foundational axioms to determine foreclosure, coexistence, or influence.',
    'Determines cs_structure.reading_relations values and whether any sibling is logically foreclosed within a single framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_relations, conceptual, 'Structural relationship mapping to sibling readings of the usul_al_fiqh_method kernel.').

omega_variable(
    gatekeeping_extraction_boundary,
    'Where does hadith authentication''s coordination function (reliable transmission) end and its extraction function (gatekeeping rents for specialists) begin?',
    'Quantify the proportion of authentication activity that serves verification vs. exclusion; measure rents captured by transmission networks vs. verification costs.',
    'If extraction dominates, the constraint shifts toward snare; if coordination dominates, it remains tangled_rope. The ε value of 0.62 assumes a substantial but not dominant extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_extraction_boundary, empirical, 'Boundary between coordination and extraction in hadith authentication gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__shafii_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__shafii_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(usul_tr_t50, usul_al_fiqh_method__shafii_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement(usul_tr_t100, usul_al_fiqh_method__shafii_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement(usul_tr_t150, usul_al_fiqh_method__shafii_reading, theater_ratio, 150, 0.26).
narrative_ontology:measurement(usul_tr_t200, usul_al_fiqh_method__shafii_reading, theater_ratio, 200, 0.29).
narrative_ontology:measurement(usul_tr_t250, usul_al_fiqh_method__shafii_reading, theater_ratio, 250, 0.32).
narrative_ontology:measurement(usul_tr_t300, usul_al_fiqh_method__shafii_reading, theater_ratio, 300, 0.35).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__shafii_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(usul_be_t50, usul_al_fiqh_method__shafii_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(usul_be_t100, usul_al_fiqh_method__shafii_reading, base_extractiveness, 100, 0.48).
narrative_ontology:measurement(usul_be_t150, usul_al_fiqh_method__shafii_reading, base_extractiveness, 150, 0.52).
narrative_ontology:measurement(usul_be_t200, usul_al_fiqh_method__shafii_reading, base_extractiveness, 200, 0.56).
narrative_ontology:measurement(usul_be_t250, usul_al_fiqh_method__shafii_reading, base_extractiveness, 250, 0.59).
narrative_ontology:measurement(usul_be_t300, usul_al_fiqh_method__shafii_reading, base_extractiveness, 300, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__shafii_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(usul_su_t50, usul_al_fiqh_method__shafii_reading, suppression_requirement, 50, 0.35).
narrative_ontology:measurement(usul_su_t100, usul_al_fiqh_method__shafii_reading, suppression_requirement, 100, 0.4).
narrative_ontology:measurement(usul_su_t150, usul_al_fiqh_method__shafii_reading, suppression_requirement, 150, 0.43).
narrative_ontology:measurement(usul_su_t200, usul_al_fiqh_method__shafii_reading, suppression_requirement, 200, 0.45).
narrative_ontology:measurement(usul_su_t250, usul_al_fiqh_method__shafii_reading, suppression_requirement, 250, 0.47).
narrative_ontology:measurement(usul_su_t300, usul_al_fiqh_method__shafii_reading, suppression_requirement, 300, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__shafii_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__shafii_reading, 0.08).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the usul_al_fiqh_method constraint family (kernel). The Shafi'i reading (this story) imposes a strict hierarchical meta-discipline with hadith authentication as prerequisite. The Hanafi reading permits expansive analogical reasoning and juristic preference. The Maliki reading integrates Medinan practice and unrestricted public interest. The Hanbali reading maximally restricts to textual sources. Their ε values differ substantially: Shafi'i (0.62) and Hanbali (est. 0.55) are more extractive due to textual gatekeeping; Hanafi (est. 0.35) and Maliki (est. 0.4) are more coordination-oriented due to methodological pluralism. Cross-influence runs primarily from Shafi'i → others because the Shafi'i systematization (al-Risala) becomes the reference framework that other madhhabs must position themselves against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__shafii_reading, organized, 0.85).
constraint_indexing:directionality_override(usul_al_fiqh_method__shafii_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
