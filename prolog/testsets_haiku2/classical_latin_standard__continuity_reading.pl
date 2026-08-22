% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: classical_latin_standard__continuity_reading
 *   human_readable: Classical Latin Standard (Continuity Reading)
 *   domain: philology/educational/commitment_system
 *
 * SUMMARY:
 *   The Classical Latin Standard is a contested kernel: what makes Latin
 *   'correct' and who has authority to define it? This constraint story
 *   instantiates the continuity reading: correctness is determined by the
 *   unbroken transmission of institutional practice, legitimately
 *   incorporating natural linguistic drift. This reading vests authority in
 *   the medieval Church and institutional scholars — they kept Latin alive
 *   and evolving, and their actual usage is the standard. The reading
 *   explicitly rejects the reconstruction reading's backward-looking appeal
 *   to Classical purity, and coexists with the hybrid reading that would
 *   partition domains. The continuity reading's beneficiaries are the
 *   institutional users (ecclesiastical and medieval scholars) whose practice
 *   becomes canonical by virtue of their gatekeeping role; the victims are
 *   minimal (barbarism-speakers excluded) because drift is accepted rather
 *   than delegitimized.
 *
 * KEY AGENTS:
 *   - ecclesiastical_latin_users: Institutional beneficiary — their evolved Latin forms are validated by continuity reading as legitimate development, not corruption.
 *   - medieval_institutional_scholars: Beneficiary and agenda-setter — they define standards through their own practice and curriculum; authority derives from transmission, not reconstruction.
 *   - classical_revival_scholars: Payer/excluded — their reconstruction project is delegitimized; they experience the continuity reading as hostile to philological methodology.
 *   - barbarism_speakers: Victim (minimal but real) — still excluded as unintelligible; the reading does not deny the existence of a threshold, only locates it at contemporary institutional standards rather than Classical forms.
 *   - reconstruction_reading_adherents: Excluded voice — their alternative framing is not foreclosed but is treated as methodologically misguided rather than alternative-but-valid.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__continuity_reading, 0.48).
domain_priors:suppression_score(classical_latin_standard__continuity_reading, 0.32).
domain_priors:theater_ratio(classical_latin_standard__continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__continuity_reading, rope).
narrative_ontology:human_readable(classical_latin_standard__continuity_reading, "Classical Latin Standard (Continuity Reading)").
narrative_ontology:topic_domain(classical_latin_standard__continuity_reading, "philology/educational/commitment_system").

domain_priors:requires_active_enforcement(classical_latin_standard__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__continuity_reading, '1f1e1b19-0c8b-4430-af73-e26c92087886').
narrative_ontology:cs_kernel_codification('1f1e1b19-0c8b-4430-af73-e26c92087886', distributed).
narrative_ontology:cs_authority_grounding('1f1e1b19-0c8b-4430-af73-e26c92087886', practice).
narrative_ontology:cs_interpretation_layer_present('1f1e1b19-0c8b-4430-af73-e26c92087886').
narrative_ontology:cs_reading_relation('1f1e1b19-0c8b-4430-af73-e26c92087886', classical_latin_standard__reconstruction_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f1e1b19-0c8b-4430-af73-e26c92087886', classical_latin_standard__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('1f1e1b19-0c8b-4430-af73-e26c92087886', foundational, institutional_transmission_grants_authority).
narrative_ontology:cs_axiom_status(institutional_transmission_grants_authority, holdable).
narrative_ontology:cs_axiom_grounding('1f1e1b19-0c8b-4430-af73-e26c92087886', institutional_transmission_grants_authority, conventional).
narrative_ontology:cs_axiom('1f1e1b19-0c8b-4430-af73-e26c92087886', foundational, linguistic_drift_is_legitimate_development).
narrative_ontology:cs_axiom_status(linguistic_drift_is_legitimate_development, holdable).
narrative_ontology:cs_axiom_grounding('1f1e1b19-0c8b-4430-af73-e26c92087886', linguistic_drift_is_legitimate_development, empirically_contingent).
narrative_ontology:cs_reference_frame('1f1e1b19-0c8b-4430-af73-e26c92087886', unbroken_institutional_transmission).
narrative_ontology:cs_drift_state('1f1e1b19-0c8b-4430-af73-e26c92087886', renaissance_textual_recovery_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1f1e1b19-0c8b-4430-af73-e26c92087886', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__continuity_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, ecclesiastical_latin_users).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, medieval_institutional_scholars).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, continuous_transmission_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(classical_latin_standard__continuity_reading, classical_revival_scholars).
narrative_ontology:constraint_victim(classical_latin_standard__continuity_reading, barbarism_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Church and its institutional apparatus inherited Latin as its liturgical and administrative language. Under the continuity reading, developments in ecclesiastical Latin (new vocabulary for sacramental practice, technical theological terms, phonetic shifts in pronunciation across regions and centuries) are recognized as legitimate continuations of living practice rather than corruptions. This reading validates their actual usage and need not require continuous backward-reference to Classical texts.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, ecclesiastical_latin_users, beneficiary,
    institutional, civilizational, constrained, universal).

% Medieval monastic, cathedral, and early university scholars who transmitted and developed Latin across centuries. They set curriculum, copied texts, created new works, and established the institutional practices that kept Latin alive. The continuity reading grants them authority to define what 'correct' Latin is — it is what they actually wrote and used, sanctified by the fact of unbroken institutional transmission. Their gate-keeping is mild: excluding grossly unintelligible 'barbarisms' but welcoming productive linguistic development.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, medieval_institutional_scholars, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__continuity_reading, medieval_institutional_scholars, agenda_setter).

% The abstract institutional authority of the Church as custodian of the transmitted tradition. This is not a person or even a named group, but the claim that legitimacy derives from institutional continuity itself — the fact that an unbroken chain of authorized speakers and writers kept the language alive and evolving. The continuity reading vests correctness in this transmitted authority rather than in any single reconstructed norm.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, continuous_transmission_authority, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(classical_latin_standard__continuity_reading, continuous_transmission_authority).

% Renaissance and later humanists who sought to recover and privilege Classical Latin by studying surviving texts and reconstructing pre-medieval norms. They experience the continuity reading as hostile: it treats their reconstruction effort as unnecessary and their rejection of medieval Latin as a denial of legitimate development. They are 'payers' in the sense that the continuity reading delegitimizes their scholarly project and makes institutional gatekeeping harder for them — medieval Latin cannot be simply dismissed as corrupt if drift is legitimate.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, classical_revival_scholars, payer,
    powerful, biographical, constrained, continental).

% Those whose Latin diverges grossly from institutional norms — whether from regional dialect, incomplete education, or genuine incomprehension of grammatical rules. The continuity reading excludes them by labeling their speech 'barbarism' rather than legitimate drift. They bear the suppression of being excluded from the educated/institutional Latin-speaking community without recourse to claim that their divergence is a legitimate development.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, barbarism_speakers, payer,
    powerless, biographical, trapped, local).

% Cathedral schools, monastic scriptoriums, universities, and the Church's bureaucracy. They teach Latin, define correct usage in their curricula, and model it in their own writing and administrative documents. The continuity reading empowers them to define standards by reference to their own institutional practice — they embody the continuous transmission. They enforce through curriculum and through social sanction of those who deviate too far from the institution-modeled standard.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, institutional_gatekeepers, agenda_setter,
    institutional, generational, analytical, continental).

% Scholars and purists who hold the reconstruction reading — that true Latin correctness resides only in the Classical texts and medieval drift is legitimate corruption that should be reversed. They are excluded from the continuity reading's legitimacy framework: their reconstruction methodology is treated as anachronistic, their rejection of medieval development as denial of how language actually works, their textual archaism as scholastic pretention rather than genuine correctness.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, reconstruction_reading_adherents, excluded,
    powerful, biographical, constrained, global).

% Scholars seeking a middle position: Classical norms for certain domains (formal rhetoric, philosophical discourse, legal contracts) and Medieval/ecclesiastical norms for others (theology, practical administration, liturgy). They observe both the continuity and reconstruction readings and propose that both are legitimate in different contexts. The continuity reading does not necessarily foreclose this position, but it grants no special weight to Classical purity.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, hybrid_reading_advocates, observer,
    powerful, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__continuity_reading, institutional_gatekeepers).
narrative_ontology:fixing_cost_class(classical_latin_standard__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single, mutually-intelligible written and spoken language across centuries and regions by anchoring its standards in the living practice of the institutional community that uses it, rather than in a fixed canonical text. Allows the language to evolve as needed (new vocabulary, phonetic shifts) while maintaining enough structural continuity that speaker-communities separated by generations can still understand each other and each other's texts.
% TRANSFER_FUNCTION: Transfers authority over correctness from textual sources (recovered Classical texts) to the institutional transmission itself — the Church, monasteries, scholars. Those who control the teaching and writing of Latin in living institutional contexts define what 'correct' is, by virtue of their role in the continuous chain. The continuity reading privileges the institutional gatekeepers' actual practice over philological reconstruction.
% ABSENT_VOICES: Reconstruction reading scholars, who would argue that correctness must be grounded in the best attainable Classical sources and that post-Classical drift, however continuous, is nevertheless corruption and should be reversed. They are excluded from the legitimacy framework of the continuity reading — their voice is heard as antiquarianism or purism rather than as a valid alternative standard. Regional speakers and populations far from institutional centers, who develop their own Latin variants, are also absent — only the educated institutional Latin-using community participates in the definition of correctness.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished — if institutional authority and practice ceased to be accepted as the ground of correctness — Latin would need a new reference standard. Scholars would revert fully to textual reconstruction (the reconstruction reading), institutional transmission would lose its legitimating force, and post-medieval Latin developments would be re-read as corruptions rather than legitimate evolution. The institutional community's current prerogative to define standards would collapse.
% FOUNDING_PROBLEM: Latin needed to remain a functional language for the educated and institutional world across centuries while natural linguistic drift (phonetic change, vocabulary shift, grammatical simplification) was inevitable. A purely textual standard tied to recovered Classical sources would require scholars to un-learn or suppress the natural forms they actually used; a purely descriptive standard that changed with every region would make institutional communication incoherent across space and time.
% FOUNDING_PROBLEM_CORROBORATION: Medieval and ecclesiastical scholars attest that they needed a stable enough standard for inter-regional communication and institutional coherence, but also needed to integrate developments in their own actual practice. Later scholars outside the continuity tradition (historians of linguistics, classicists studying medieval Latin) observe that medieval scribes, theologians, and administrators clearly did need functional norms that balanced innovation and continuity — the constraint is attested by behavior, not by dispute.
narrative_ontology:disappearance_verdict(classical_latin_standard__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.48) because the continuity reading does establish gatekeeping authority — institutional scholars and ecclesiastical writers control what counts as correct Latin, and those outside the institutional community are constrained to learn it their way. But the extractiveness is tempered by the reading's explicit legitimation of drift; no party is told their evolved forms are corruptions requiring suppression, only that the institution's forms define the standard. Suppression is notably lower (0.32) than in reconstruction reading (expected higher: ~0.65–0.75) because alternatives (medieval variants, regional dialects) are not delegitimized as barbarisms against a Classical absolute, but rather as less institutionally sanctioned. Theater is moderate (0.28) because the constraint does real work (maintaining institutional coherence across time), but increasingly faces challenge from reconstruction scholars and later-period print culture that will privilege Classical texts. Measurements plateau around midpoint (9–15 in the interval) as the constraint stabilizes under late-medieval institutional weight and before major disruption from Renaissance printing. The shared time grid ensures all three metrics are authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The reconstruction-reading seat experiences the same constraint as Snare (pure institutional gatekeeping, delegitimization of their project, suppression of their alternatives), while the continuity-reading seat experiences it as Rope (legitimate coordination of institutional communication with allowance for drift). This divergence is endemic to the kernel contest and should compute per-seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical institutional users (powerful, institutional power atom, constrained exit) are beneficiaries — they get to define their own language's norms without submitting to Classical purists. Medieval scholars (institutional, arbitrage exit) are both beneficiaries (they set the standard) and agenda-setters (they enforce it through teaching). Classical-revival scholars (powerful, biographical horizon, constrained exit) are payers — they cannot pursue their reconstruction project without institutional approval, and the continuity reading denies that project legitimacy. Barbarism-speakers (powerless, trapped exit) are victims at a sharp threshold — excluded, but the reading's logic does not grant them recourse to claim legitimate development. The hybrid-reading observers (analytical power, analytical exit) are neither beneficiaries nor payers, but they occupy a structural position that should compute neutral directionality, watching both the continuity and reconstruction readings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining functional Latin across drift) is live: the constraint solves it. But the legitimacy grounding is unstable — the continuity reading's claim that institutional transmission is the source of authority conflicts directly with the reconstruction reading's claim that Classical texts are. This is not mandatrophy (the founding problem has not died), but it is a permanent legitimacy contest. The reading does not resolve the contest; it chooses a side. The omega variable 'legitimacy_source_under_dispute' captures this irreducible uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_under_dispute,
    'Is the authority to define correct Latin located in the transmitted institutional practice (continuity reading), in recovered Classical texts (reconstruction reading), or partitioned by domain (hybrid reading)?',
    'The question has no empirical resolution — it is a normative claim about epistemic authority. Different scholarly communities and later institutional regimes (Renaissance, Enlightenment, modern academia) will choose differently. The resolution is historical (which reading wins institutional backing) rather than discovered.',
    'If continuity reading authority holds, medieval and ecclesiastical Latin are legitimate and corruption-free; if reconstruction reading authority displaces it, medieval Latin is retroactively re-read as corrupted and requiring correction. If hybrid reading becomes institutional standard, both are legitimate in different contexts. The classification shifts as authority shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_source_under_dispute, conceptual, 'Irreducible dispute over the source of normative authority for Latin correctness.').

omega_variable(
    drift_as_development_or_decay,
    'Are the phonetic, grammatical, and lexical changes that occurred in Latin between Classical and medieval periods instances of legitimate linguistic development or of linguistic decay/corruption?',
    'No empirical test distinguishes development from decay — the distinction is a normative framing. Modern historical linguistics describes the changes as natural phonetic evolution and productive morphological simplification (development framing); Classical purists describe them as loss of distinction and vulgarization (decay framing). The two descriptions are compatible factually but incompatible normatively.',
    'If drift is framed as development, the continuity reading''s legitimation of medieval forms stands; if framed as decay, reconstruction reading gains force. The classification is sensitive to this framing choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(drift_as_development_or_decay, preference, 'Normative framing of linguistic change as development or decay.').

omega_variable(
    institutional_vs_textual_authority_asymmetry,
    'Does granting institutional transmission authority over textual sources create an asymmetric extraction mechanism where medieval scholars can enforce their practice because they control institutions, even if Classical texts remain available to challenge them?',
    'Historical audit: measure the degree to which institutional gatekeeping (curriculum, manuscript control, teaching authority) actually prevented access to Classical texts and alternative readings. High control = stronger extraction; low control = weaker. Specific markers: availability of Classical texts in scriptoriums, citation of Classical sources in medieval works, evidence of deliberate exclusion vs. simple scarcity.',
    'If institutional control is high and deliberate, the extraction component of the constraint is higher than authored (0.48), making it closer to Tangled Rope or even Snare. If control is incidental (Classical texts are rare because copying is expensive and institutional priority is elsewhere, not because reconstruction is suppressed), the authored extraction holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_textual_authority_asymmetry, empirical, 'Degree to which institutional gatekeeping actively suppresses Classical-text authority vs. merely prioritizing institutional practice.').

omega_variable(
    barbarism_threshold_stability,
    'Is the threshold that distinguishes legitimate drift from excluded ''barbarism'' stable and principled in the continuity reading, or does it shift with institutional convenience?',
    'Analysis of medieval and ecclesiastical grammatical texts, teaching documents, and corrective marks on manuscripts. Identify stated rules for what counts as barbarism, compare across institutions and centuries, measure consistency. High consistency = stable threshold; high variance = threshold drifts with institutional interest.',
    'If threshold is unstable, the suppression component is underestimated (actual suppression higher than authored 0.32) because the constraint excludes arbitrarily, not by principled standard. If threshold is stable, the authored suppression is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(barbarism_threshold_stability, empirical, 'Whether the barbarism exclusion threshold is stable or shifts with institutional convenience.').

omega_variable(
    kernel_reading_foreclosure_status,
    'Do the continuity reading and reconstruction reading actually foreclose each other — logically incompatible within any single framework — or do they merely coexist as rival positions that different parties adopt?',
    'Logical analysis: both readings can be true in a single framework if authority is partitioned (continuity for Church/institutional use, reconstruction for scholarly textual study). If such partitioning is coherent, they coexist; if partitioning requires abandoning the core claim of either reading (e.g., that IT, not the other, has ultimate authority), they foreclose. The hybrid reading tests this: can one coherently hold ''both practices are legitimate in their domains'' while also holding ''continuity is THE authority'' or ''reconstruction is THE authority''?',
    'If readings foreclose each other, the reading_relations in cs_structure should use ''forecloses''; if they truly coexist, ''coexists_with'' is correct. Classification consequence: foreclosure signals an intra-kernel schism (the kernel cannot hold both); coexistence signals a genuine multi-party contest within a single kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_status, conceptual, 'Logical status of the relationship between continuity and reconstruction readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__continuity_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clsc_continuity_tr_t0, classical_latin_standard__continuity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(clsc_continuity_tr_t3, classical_latin_standard__continuity_reading, theater_ratio, 3, 0.24).
narrative_ontology:measurement(clsc_continuity_tr_t6, classical_latin_standard__continuity_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(clsc_continuity_tr_t9, classical_latin_standard__continuity_reading, theater_ratio, 9, 0.28).
narrative_ontology:measurement(clsc_continuity_tr_t12, classical_latin_standard__continuity_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(clsc_continuity_tr_t15, classical_latin_standard__continuity_reading, theater_ratio, 15, 0.28).

% Extraction over time
narrative_ontology:measurement(clsc_continuity_be_t0, classical_latin_standard__continuity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clsc_continuity_be_t3, classical_latin_standard__continuity_reading, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(clsc_continuity_be_t6, classical_latin_standard__continuity_reading, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(clsc_continuity_be_t9, classical_latin_standard__continuity_reading, base_extractiveness, 9, 0.48).
narrative_ontology:measurement(clsc_continuity_be_t12, classical_latin_standard__continuity_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(clsc_continuity_be_t15, classical_latin_standard__continuity_reading, base_extractiveness, 15, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(clsc_continuity_su_t0, classical_latin_standard__continuity_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(clsc_continuity_su_t3, classical_latin_standard__continuity_reading, suppression_requirement, 3, 0.29).
narrative_ontology:measurement(clsc_continuity_su_t6, classical_latin_standard__continuity_reading, suppression_requirement, 6, 0.31).
narrative_ontology:measurement(clsc_continuity_su_t9, classical_latin_standard__continuity_reading, suppression_requirement, 9, 0.32).
narrative_ontology:measurement(clsc_continuity_su_t12, classical_latin_standard__continuity_reading, suppression_requirement, 12, 0.32).
narrative_ontology:measurement(clsc_continuity_su_t15, classical_latin_standard__continuity_reading, suppression_requirement, 15, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__continuity_reading, 0.12).
narrative_ontology:affects_constraint(classical_latin_standard__continuity_reading, classical_latin_standard__reconstruction_reading).
narrative_ontology:affects_constraint(classical_latin_standard__continuity_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% The classical_latin_standard kernel admits three distinct constraint stories: continuity_reading (this file, authority vested in institutional transmission), reconstruction_reading (authority vested in recovered Classical texts, drift is corruption), and hybrid_reading (authority partitioned by domain). Each story has distinct ε, beneficiary/victim structure, and expected per-seat type divergence. The stories are linked because they are readings of the same kernel and because institutional adoption of one reading reshapes the others' feasibility conditions. This reading (continuity) influences both siblings by establishing institutional authority as a legitimate base; the reconstruction reading would influence this one by making Classical texts newly authoritative; the hybrid reading influences both by proposing partition as an escape from full foreclosure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
