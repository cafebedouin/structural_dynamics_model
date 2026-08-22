% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__continuity_reading, []).

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
 *   constraint_id: classical_latin_standard__continuity_reading
 *   human_readable: Classical Latin Standard (Continuity Reading)
 *   domain: historical_linguistics/commitment_systems
 *
 * SUMMARY:
 *   The Classical Latin standard is a contested kernel with three readings:
 *   the continuity reading (this constraint) holds that correct Latin is the
 *   form legitimately transmitted through unbroken institutional practice,
 *   with linguistic drift recognized as development; the reconstruction
 *   reading holds that correct Latin is only the Classical form recoverable
 *   through philological archaeology, requiring rejection of medieval drift;
 *   the hybrid reading holds that both Classical fidelity and post-Classical
 *   institutional innovations are legitimate in their respective domains.
 *   This constraint instantiates the continuity reading—the framework through
 *   which ecclesiastical and institutional practitioners legitimize their
 *   actual practice as authentic development rather than corruption. The
 *   reading trades on the premise that linguistic legitimacy inheres in
 *   living transmission, not textual recovery. Suppression is moderate
 *   because the continuity reading does not aggressively delegitimize
 *   alternatives—it simply validates institutional practice. Extractiveness
 *   is moderate because gatekeeping occurs (institutional authorities control
 *   the standard) but without systematic denial that non-institutional speech
 *   is Latin at all (that denial is more characteristic of the reconstruction
 *   reading's delegitimization strategy).
 *
 * KEY AGENTS:
 *   - ecclesiastical_authorities: Institutional agenda-setter; set and defend the continuity standard through teaching and text selection.
 *   - institutional_latin_practitioners: Primary beneficiary; their everyday practice (with medieval innovations) is validated as correct Latin.
 *   - manuscript_copyists: Secondary beneficiary; scribal variants and textual evolution are read as legitimate development.
 *   - classical_philologists: Observer seat; their reconstruction projects sit orthogonal to the institutional transmission frame.
 *   - barbarism_excludees and vernacular_emergent_speakers: Excluded; their speech is branded as corruption, not recognized as legitimate development (the constraint's boundary-maintenance function).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__continuity_reading, 0.48).
domain_priors:suppression_score(classical_latin_standard__continuity_reading, 0.35).
domain_priors:theater_ratio(classical_latin_standard__continuity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__continuity_reading, rope).
narrative_ontology:human_readable(classical_latin_standard__continuity_reading, "Classical Latin Standard (Continuity Reading)").
narrative_ontology:topic_domain(classical_latin_standard__continuity_reading, "historical_linguistics/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__continuity_reading, '17a64598-94ee-492b-b603-99cad07a5872').
narrative_ontology:cs_kernel_codification('17a64598-94ee-492b-b603-99cad07a5872', distributed).
narrative_ontology:cs_authority_grounding('17a64598-94ee-492b-b603-99cad07a5872', lineage).
narrative_ontology:cs_interpretation_layer_present('17a64598-94ee-492b-b603-99cad07a5872').
narrative_ontology:cs_reading_relation('17a64598-94ee-492b-b603-99cad07a5872', classical_latin_standard__reconstruction_reading, coexists_with).
narrative_ontology:cs_reading_relation('17a64598-94ee-492b-b603-99cad07a5872', classical_latin_standard__hybrid_reading, influences).
narrative_ontology:cs_axiom('17a64598-94ee-492b-b603-99cad07a5872', foundational, transmission_legitimacy_primacy).
narrative_ontology:cs_axiom_status(transmission_legitimacy_primacy, holdable).
narrative_ontology:cs_axiom_grounding('17a64598-94ee-492b-b603-99cad07a5872', transmission_legitimacy_primacy, conventional).
narrative_ontology:cs_axiom('17a64598-94ee-492b-b603-99cad07a5872', foundational, drift_as_development).
narrative_ontology:cs_axiom_status(drift_as_development, holdable).
narrative_ontology:cs_axiom_grounding('17a64598-94ee-492b-b603-99cad07a5872', drift_as_development, deontological).
narrative_ontology:cs_reference_frame('17a64598-94ee-492b-b603-99cad07a5872', early_medieval_institutional_transmission).
narrative_ontology:cs_drift_state('17a64598-94ee-492b-b603-99cad07a5872', humanist_textual_recovery_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('17a64598-94ee-492b-b603-99cad07a5872', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__continuity_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, institutional_latin_practitioners).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, ecclesiastical_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, manuscript_copyists).
narrative_ontology:constraint_vindicates(classical_latin_standard__continuity_reading, linguistic_continuity_legitimacy).
narrative_ontology:constraint_vindicates(classical_latin_standard__continuity_reading, drift_as_development).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Priests, scholars, and ecclesiastical administrators who use Latin in living practice—liturgy, theological commentary, institutional correspondence. Benefit from a standard that legitimizes their actual practice as 'correct' Latin rather than degraded imitation. Their everyday usage (including post-Classical forms and innovations necessary for modern referents) is validated by the continuity framework rather than stigmatized as error.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, institutional_latin_practitioners, beneficiary,
    institutional, generational, mobile, continental).

% Church institutions (papal curia, monastic orders, episcopal courts) that set liturgical and administrative Latin standards through teaching, text selection, and institutional practice. Maintain the continuity reading through unbroken transmission in cathedral schools, monasteries, and universities. Defend the legitimacy of medieval Latin as authentic development, not corruption.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, ecclesiastical_authorities, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Scholars engaged in textual recovery and historical linguistics who study Classical Latin texts. They observe the constraint but sit outside the institutional practice frame; their primary commitment is to textual evidence and reconstruction rather than living transmission.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, classical_philologists, observer,
    organized, biographical, mobile, national).

% Scribes and manuscript-producing communities who transmit texts in daily work. Benefit from a standard that treats their own scribal practice, and the naturally-occurring variants in transmitted texts, as legitimate developments of Latin rather than mistakes to be corrected back to a frozen standard.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, manuscript_copyists, beneficiary,
    moderate, biographical, constrained, regional).

% Speakers and writers (lay people, peripheral communities, emerging vernacular speakers) whose usage patterns are branded as 'barbarisms' or corruptions rather than developments. Structurally silenced: they have no seat in the institutional transmission chain and no authority to defend their own speech as legitimate. Their exclusion is the constraint's boundary maintenance function.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, barbarism_excludees, excluded,
    powerless, biographical, trapped, local).

% Communities in post-Latin regions whose speech is intermediate between Late Latin and emerging Romance forms. Their native linguistic practice sits in the degradation zone—neither Classical nor institutionally legitimized as continuity. Locked into the position of speaking 'corrupted' Latin rather than having their speech recognized as a legitimate direction of development.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, vernacular_emergent_speakers, excluded,
    powerless, biographical, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes unbroken transmission of Latin knowledge and practice across generations: enables institutional actors (ecclesiastical hierarchy, monastic scholarly tradition) to maintain a shared communicative standard without requiring continuous reference back to ancient texts. Solves the problem: how does Latin stay a working language for institutional purposes across the Middle Ages and beyond?
% TRANSFER_FUNCTION: Transfers authority to set and enforce the standard from reconstructed textual authority (what Classical texts 'really' said) to living institutional transmission (what we have been doing, and it is legitimate). The flow is from marginalized non-institutional speakers toward institutional authorities who validate or deny linguistic legitimacy.
% ABSENT_VOICES: Speakers of peripheral, rural, and emerging-vernacular Latin have no seat in the transmission chain and cannot defend their usage patterns as legitimate development. Their speech is unheard in the institutional adjudication of 'correct' Latin; only institutional practitioners and ecclesiastical authorities speak in this kernel's hearing.
% DISAPPEARANCE_RATIONALE: If this constraint dissolved—if the authority to validate Latin no longer rested on institutional continuity but required philological reconstruction from texts, or if multiple competing standards were equally legitimate—the institutional practice of Latin would lose its epistemic grounding. Institutional actors would have to choose between defending their living practice against reconstructionist critique or abandoning Latin altogether. The ecclesiastical transmission chain that has held Latin as a working language would face a foundational legitimacy crisis.
% FOUNDING_PROBLEM: After the collapse of Classical administrative structures, Latin risked becoming a dead language, its correctness an unrecoverable historical fact. How could institutional actors (the Church, remaining literate communities) maintain and transmit Latin as a living, usable language across the early Medieval period without access to comprehensive Classical texts and without becoming completely isolated from their own present practice?
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical authorities and institutional practitioners attest the founding problem as live—they must reconcile Classical learning with contemporary institutional needs. Philologists and historians of Latin attest it is partially resolved: Classical texts are now comprehensively recovered (so a reconstructionist alternative is now possible). Contemporary defenders of the continuity reading (neo-Latin communities, ecclesiastical Latin practitioners) attest that living transmission remains the only way to keep Latin as a working language; textual archaeology alone produces a dead museum language, not a usable standard.
narrative_ontology:disappearance_verdict(classical_latin_standard__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(classical_latin_standard__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__continuity_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(classical_latin_standard__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(classical_latin_standard__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because institutional authorities do collect gatekeeping power—they decide what counts as legitimate Latin development—but the reading itself does not systematically delegitimize alternatives or deny their claim to be Latin. Suppression is lower (0.35) than extraction because the continuity reading tolerates drift as development; only gross divergence (barbarisms) is actively suppressed, and that suppression is mild by comparison to a reconstructionist standard that would brand all post-Classical form as error. Theater is low-to-moderate (0.22): the standard performs some institutional identity function (we are the keepers of authentic transmission), but the core function—enabling Latin to remain a working institutional language—is real. The measurement trajectory is flat to slightly rising, then declining slightly after 1200 (when textual recovery begins), reflecting the constraint's stability across the long medieval period and its challenge as reconstruction becomes possible. The grid metrics (suppression_requirement) track enforcement cost to maintain institutional consensus on continuity as legitimate—modest but rising as divergence increases, then stabilizing when the constraint solidifies into pedagogical consensus.
 *
 * PERSPECTIVAL GAP:
 *   From the ecclesiastical authority seat: this is a rope, genuine coordination enabling institutional communication across generations. From the institutional practitioner seat: this is closer to tangled rope—they get validation but must perform conformity. From the excluded vernacular speaker seat: this is a snare—they are trapped in a linguistic order that delegitimizes their speech without option to exit. The engine computes these per-seat types from the structural data; the story's claimed type (rope) reflects the reading's own self-understanding (institutional beneficiaries and the coordination function they emphasize), while the metrics and directionalities remain faithful to how extraction actually operates across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authorities sit at the beneficiary end (d ~ 0.2): they set the standard and collect gatekeeping authority without bearing systematic costs. Institutional practitioners sit near symmetric (d ~ 0.4–0.5): they benefit from validation of their practice but also bear costs—they must conform to institutional transmission norms to maintain their authority to speak Latin, and their speech is always subject to institutional review. Excluded vernacular speakers sit at the target end (d ~ 0.85): their native usage is branded as corruption, and they have no exit—they cannot abandon Latin (it is the language of institutional authority they must navigate) but cannot achieve legitimacy within the continuity reading. The derivation from beneficiary/victim + exit options produces these directionalities directly: ecclesiastical authorities are beneficiaries with high power and arbitrage exit (d low); institutional practitioners are beneficiaries with institutional power but constrained exit (d moderate); excluded speakers are victims with powerless status and identity-locked exit (d high).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to maintain Latin as a working institutional language after the collapse of Classical administrative structures) is live from the continuity reading's perspective: institutional actors still must solve this problem in every generation. The disappearance verdict is world_rearranges: if the continuity reading were abandoned and replaced with strict reconstructionism, institutional Latin would become a museum language—a massive epistemic reorganization. This prevents false mandatrophy: the constraint is not an atrophied function defended theatrically, but an active solution to an ongoing coordination problem (keeping Latin alive for institutional purposes). The theater_ratio stays low because the functional work is real, even if some performative identity-work (we are authentic keepers of tradition) rides alongside it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_transmission_vs_textual,
    'Does linguistic legitimacy inhere in unbroken institutional transmission (continuity reading), or in philological recovery of original form (reconstruction reading), or in both equally in different domains (hybrid reading)?',
    'Historical linguistics and institutional practice history: trace which standard(s) actually governed usage in different periods and communities; examine evidence of how practitioners justified their own speech (by appeal to transmission lineage, to textual authority, or both); study how the standards changed when classical texts became comprehensively available.',
    'If transmission proves the legitimate source, continuity reading is structurally sound. If textual recovery is privileged, the reading shifts toward reconstruction. If both are legitimate in different domains, the hybrid reading better captures the actual structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_transmission_vs_textual, conceptual, 'What is the actual source of Latin legitimacy—transmission, texts, or both?').

omega_variable(
    drift_as_development_vs_corruption,
    'Is post-Classical Latin drift best characterized as legitimate development (continuity frame) or as corruption of a fixed standard (reconstruction frame)?',
    'Systematic comparison of medieval Latin with Classical Latin and Romance languages: does medieval Latin form a coherent evolved system (supporting development narrative), or does it show random divergence and loss (supporting corruption narrative)? Do later Romance languages preserve medieval Latin patterns, suggesting continuity, or do they diverge orthogonally (suggesting corruption + independent development)?',
    'If medieval Latin is systematically coherent and ancestral to Romance, development framing is supported. If medieval Latin is unsystematic degradation with independent Romance emergence, corruption framing is more apt, and the hybrid or reconstruction readings gain standing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(drift_as_development_vs_corruption, empirical, 'Whether medieval Latin represents legitimate linguistic development or corruption.').

omega_variable(
    institutional_consensus_vs_natural_practice,
    'Is the continuity standard maintained by genuine institutional consensus among practitioners, or by institutional gatekeeping that suppresses alternatives?',
    'Textual and archaeological evidence of actual medieval Latin practice across diverse monasteries, regions, and time periods: does usage cluster tightly around a transmitted standard (suggesting consensus), or does it show high variance suppressed only by institutional correction (suggesting gatekeeping without consensus)?',
    'High institutional consensus supports the rope reading (genuine coordination); evidence of high variance with institutional suppression shifts it toward tangled_rope or snare (gatekeeping extracting legitimacy despite natural divergence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_consensus_vs_natural_practice, empirical, 'Whether the continuity standard represents genuine institutional consensus or imposed gatekeeping.').

omega_variable(
    kernel_reading_coexistence,
    'Can the continuity reading and the reconstruction reading coexist as live positions within the same institutional framework, or does one logically foreclose the other?',
    'Historical documentation and institutional analysis: examine whether any institutional actor (e.g., later humanists, contemporary neo-Latin scholars) has successfully held BOTH readings simultaneously, or whether they are forced into explicit opposition.',
    'If they coexist (different communities, different domains, different time periods), their relation is coexists_with. If one forecloses the other within any institutional framework, their relation is forecloses. If one creates structural pressure that reorganizes the field without eliminating the other, the relation is influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Logical compatibility of continuity and reconstruction readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__continuity_reading, 0, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t0, classical_latin_standard__continuity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(clas_tr_t0, observed).
narrative_ontology:measurement(clas_tr_t200, classical_latin_standard__continuity_reading, theater_ratio, 200, 0.14).
narrative_ontology:measurement_basis(clas_tr_t200, observed).
narrative_ontology:measurement(clas_tr_t400, classical_latin_standard__continuity_reading, theater_ratio, 400, 0.16).
narrative_ontology:measurement_basis(clas_tr_t400, observed).
narrative_ontology:measurement(clas_tr_t800, classical_latin_standard__continuity_reading, theater_ratio, 800, 0.19).
narrative_ontology:measurement_basis(clas_tr_t800, observed).
narrative_ontology:measurement(clas_tr_t1200, classical_latin_standard__continuity_reading, theater_ratio, 1200, 0.22).
narrative_ontology:measurement_basis(clas_tr_t1200, observed).
narrative_ontology:measurement(clas_tr_t1600, classical_latin_standard__continuity_reading, theater_ratio, 1600, 0.22).
narrative_ontology:measurement_basis(clas_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(clas_be_t0, classical_latin_standard__continuity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(clas_be_t0, observed).
narrative_ontology:measurement(clas_be_t200, classical_latin_standard__continuity_reading, base_extractiveness, 200, 0.42).
narrative_ontology:measurement_basis(clas_be_t200, observed).
narrative_ontology:measurement(clas_be_t400, classical_latin_standard__continuity_reading, base_extractiveness, 400, 0.45).
narrative_ontology:measurement_basis(clas_be_t400, observed).
narrative_ontology:measurement(clas_be_t800, classical_latin_standard__continuity_reading, base_extractiveness, 800, 0.48).
narrative_ontology:measurement_basis(clas_be_t800, observed).
narrative_ontology:measurement(clas_be_t1200, classical_latin_standard__continuity_reading, base_extractiveness, 1200, 0.5).
narrative_ontology:measurement_basis(clas_be_t1200, observed).
narrative_ontology:measurement(clas_be_t1600, classical_latin_standard__continuity_reading, base_extractiveness, 1600, 0.48).
narrative_ontology:measurement_basis(clas_be_t1600, observed).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t0, classical_latin_standard__continuity_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(clas_su_t0, observed).
narrative_ontology:measurement(clas_su_t200, classical_latin_standard__continuity_reading, suppression_requirement, 200, 0.31).
narrative_ontology:measurement_basis(clas_su_t200, observed).
narrative_ontology:measurement(clas_su_t400, classical_latin_standard__continuity_reading, suppression_requirement, 400, 0.33).
narrative_ontology:measurement_basis(clas_su_t400, observed).
narrative_ontology:measurement(clas_su_t800, classical_latin_standard__continuity_reading, suppression_requirement, 800, 0.35).
narrative_ontology:measurement_basis(clas_su_t800, observed).
narrative_ontology:measurement(clas_su_t1200, classical_latin_standard__continuity_reading, suppression_requirement, 1200, 0.36).
narrative_ontology:measurement_basis(clas_su_t1200, observed).
narrative_ontology:measurement(clas_su_t1600, classical_latin_standard__continuity_reading, suppression_requirement, 1600, 0.35).
narrative_ontology:measurement_basis(clas_su_t1600, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__continuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__continuity_reading, 0.06).
narrative_ontology:affects_constraint(classical_latin_standard__continuity_reading, classical_latin_standard__reconstruction_reading).
narrative_ontology:affects_constraint(classical_latin_standard__continuity_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% The classical_latin_standard kernel is instantiated by three separate constraint stories, one per reading. Each reading has distinct ε (legitimacy source), beneficiary structure (who controls the standard), and suppression profile (what alternatives are delegitimized). The continuity_reading (this constraint) privileges institutional transmission as the source of correctness; the reconstruction_reading privileges textual archaeology; the hybrid_reading combines both in domain-specific allocation. The three are linked by their shared kernel and by the reading_relations declared in cs_structure: continuity_reading coexists_with both siblings (different communities hold different readings), but reconstruction_reading influences the landscape by making textual validation possible. Do not merge these into one constraint with observable-dependent ε—each reading has its own stable ε, its own enforcement mechanism, and its own beneficiary/victim structure. They are genuinely distinct constraints interpreting the same cultural kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(classical_latin_standard__continuity_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
