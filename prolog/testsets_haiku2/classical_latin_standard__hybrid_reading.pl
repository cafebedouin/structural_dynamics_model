% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__hybrid_reading, []).

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
 *   constraint_id: classical_latin_standard__hybrid_reading
 *   human_readable: Classical Latin Standard (Hybrid Reading)
 *   domain: linguistic/institutional
 *
 * SUMMARY:
 *   The hybrid reading of the Classical Latin standard emerges in the
 *   12th–13th centuries as ecclesiastical and educational institutions seek
 *   to maintain Latin's prestige while accommodating their own technical
 *   vocabulary. Unlike pure Classical reconstruction (which would require
 *   abandoning medieval innovations) or pure continuity reading (which would
 *   accept all medieval drift as legitimate), the hybrid reading claims
 *   fidelity to Classical norms while recognizing 'legitimate technical
 *   extensions' in liturgical, theological, and scientific domains. This
 *   reading instantiates a tangled rope: it genuinely solves a coordination
 *   problem (allowing multi-domain communication under a unified standard)
 *   while asymmetrically extracting institutional authority from medieval
 *   continuity users and peripheral speakers. The hybrid boundary between
 *   acceptable 'technical extension' and unacceptable 'barbarism' is enforced
 *   by institutional power, not by transparent linguistic principle, making
 *   it an active-enforcement arrangement where the distinction between
 *   coordination and extraction depends on one's structural position.
 *
 * KEY AGENTS:
 *   - ecclesiastical_institutions: Beneficiary and enforcer (Church and monastic scriptoria define and enforce the standard; theological neologisms are accommodated as legitimate)
 *   - classical_education_authorities: Beneficiary (universities and grammar schools teach the hybrid standard and position themselves as arbiters of correctness)
 *   - technical_specialists: Beneficiary (medical writers, legal specialists, naturalists adopt the standard to legitimize their technical vocabulary)
 *   - medieval_continuity_users: Victim (clerics and local administrators trained in medieval transmission find their usage delegitimized without explicit principle)
 *   - peripheral_vernacular_speakers: Victim (Romance-influenced Latin speakers lack access to institutional education and institutional status to protect their forms)
 *   - textual_authorities: Observer/enforcer (manuscript custodians authenticate Classical texts that ground the standard's legitimacy)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, 0.48).
domain_priors:suppression_score(classical_latin_standard__hybrid_reading, 0.56).
domain_priors:theater_ratio(classical_latin_standard__hybrid_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__hybrid_reading, "Classical Latin Standard (Hybrid Reading)").
narrative_ontology:topic_domain(classical_latin_standard__hybrid_reading, "linguistic/institutional").

domain_priors:requires_active_enforcement(classical_latin_standard__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__hybrid_reading, '30447d7e-2997-454f-aedf-81de2fd1514c').
narrative_ontology:cs_kernel_codification('30447d7e-2997-454f-aedf-81de2fd1514c', fixed_text).
narrative_ontology:cs_authority_grounding('30447d7e-2997-454f-aedf-81de2fd1514c', extraction).
narrative_ontology:cs_interpretation_layer_present('30447d7e-2997-454f-aedf-81de2fd1514c').
narrative_ontology:cs_reading_relation('30447d7e-2997-454f-aedf-81de2fd1514c', classical_latin_standard__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('30447d7e-2997-454f-aedf-81de2fd1514c', classical_latin_standard__reconstruction_reading, influences).
narrative_ontology:cs_axiom('30447d7e-2997-454f-aedf-81de2fd1514c', foundational, fidelity_to_classical_plus_legitimate_innovation).
narrative_ontology:cs_axiom_status(fidelity_to_classical_plus_legitimate_innovation, holdable).
narrative_ontology:cs_axiom_grounding('30447d7e-2997-454f-aedf-81de2fd1514c', fidelity_to_classical_plus_legitimate_innovation, conventional).
narrative_ontology:cs_axiom('30447d7e-2997-454f-aedf-81de2fd1514c', secondary, institutional_gatekeeping_of_legitimacy).
narrative_ontology:cs_axiom_status(institutional_gatekeeping_of_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('30447d7e-2997-454f-aedf-81de2fd1514c', institutional_gatekeeping_of_legitimacy, deontological).
narrative_ontology:cs_reference_frame('30447d7e-2997-454f-aedf-81de2fd1514c', classical_textual_authority_with_institutional_flexibility).
narrative_ontology:cs_drift_state('30447d7e-2997-454f-aedf-81de2fd1514c', contemporary_institutional_entrenchment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('30447d7e-2997-454f-aedf-81de2fd1514c', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__hybrid_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, ecclesiastical_institutions).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, classical_education_authorities).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, technical_specialists).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, medieval_continuity_users).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, peripheral_vernacular_speakers).
narrative_ontology:constraint_vindicates(classical_latin_standard__hybrid_reading, classical_philological_authenticity).
narrative_ontology:constraint_vindicates(classical_latin_standard__hybrid_reading, institutional_latin_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Churches, monastic scriptoria, and theological faculties adopt the hybrid standard to maintain Latin's prestige while accommodating their own technical vocabularies (liturgical terms, theological neologisms, administrative usage). They enforce the standard through education and manuscript correction, legitimizing their own post-Classical developments as 'necessary technical extensions' while delegitimizing rival forms as 'barbarisms.' The standard allows them to claim fidelity to Classical authority while protecting institutional vocabulary from external criticism.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, ecclesiastical_institutions, beneficiary,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__hybrid_reading, ecclesiastical_institutions, agenda_setter).

% Universities and grammar schools teach the hybrid standard as the authoritative form. They benefit from the prestige conferred by Classical association while avoiding the pedagogical burden of requiring complete rejection of medieval usage. The standard legitimizes their curricula as 'preserving Classical truth' and positions teachers as arbiters of which developments count as legitimate extensions versus barbarisms.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, classical_education_authorities, beneficiary,
    institutional, generational, constrained, continental).

% Medical writers, legal specialists, and naturalists who work in Latin use the hybrid standard to justify their specialized vocabulary (anatomical terms, legal constructions, botanical nomenclature). They claim fidelity to Classical form while incorporating the technical developments their fields require. The standard's accommodation of domain-specific innovation within a Classical framework legitimizes their specialized usage and protects it from accusations of corruption.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, technical_specialists, beneficiary,
    powerful, biographical, mobile, continental).

% Writers and speakers trained in the medieval transmission—clerics, local administrators, craftspeople—whose Latin incorporates the full range of medieval innovations (morphological simplifications, frequency changes, new word formations). The hybrid standard delegitimizes much of their usage as 'barbarism' or 'corruption,' even when their forms derive from transparent linguistic evolution. They face pressure to abandon or hide forms they were taught, yet the boundary between acceptable 'technical extension' and unacceptable 'barbarism' is enforced by institutional authority, not by rule.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, medieval_continuity_users, payer,
    moderate, biographical, identity_locked, local).

% Communities in peripheral regions where Latin literacy coexists with Romance vernaculars and where Latin usage reflects heavy Romance influence (Iberian, Italian, Balkanic regions). Their Latin forms—which are continuous developments from Classical Latin through medieval drift—are systematically delegitimized as 'corrupted by barbarism' under the hybrid standard. They lack access to the institutional education that would teach the approved standard and lack the institutional position that would allow their forms to be recognized as 'technical extensions.'
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, peripheral_vernacular_speakers, payer,
    powerless, immediate, trapped, local).

% Scholars and theorists who advocate for Classical reconstruction (returning to Classical form by explicit rejection of medieval development) are systematically excluded from institutional authority. The hybrid reading accommodates enough medieval vocabulary that reconstructionists are outmaneuvered in debates about what counts as 'legitimate development'—the boundary is set by institutional convenience, not by principle, which reconstructionists refuse. Their position remains available as an alternative but is institutionally suppressed.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, philological_reconstructionists, excluded,
    institutional, generational, constrained, continental).

% Writers and educators who argue that medieval Latin is a legitimate living form—that linguistic drift is not corruption but natural development—find their position excluded from institutional authority. The hybrid reading co-opts the language of legitimacy ('we do recognize developments') while retaining the power to classify which developments are acceptable and which are barbarisms. Continuity advocates' position is structurally available but institutionally marginalized.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, continuity_advocates, excluded,
    moderate, biographical, identity_locked, local).

% Manuscript custodians, editors, and philologists who preserve and transmit Classical texts serve as the technical ground for the standard's legitimacy. They authenticate what counts as 'Classical' and thus what deviations require justification. Their role is framed as neutral preservation but functions as enforcement: the texts they privilege define the standard, and the standard in turn determines which texts are authoritative.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, textual_authorities, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__hybrid_reading, ecclesiastical_institutions).
narrative_ontology:fixing_cost_class(classical_latin_standard__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common referent for correctness in Latin writing across institutional domains (Church, education, technical fields), enabling communication between communities that would otherwise use divergent forms. The standard coordinates by anchoring to a shared Classical authority while permitting domain-specific vocabulary, allowing each institutional user to claim fidelity to Classical form while protecting their own innovations.
% TRANSFER_FUNCTION: Moves institutional legitimacy from medieval continuity users and peripheral speakers to ecclesiastical institutions, educational authorities, and technical specialists. Users whose Latin was formed by medieval transmission are classified as barbarous unless they undergo institutional retraining. The constraint transfers the right to define correctness from distributed medieval practice to centralized institutional authority.
% ABSENT_VOICES: Peripheral users whose Latin reflects Romance-influenced development (Iberian, Italian, Balkanic regions) are structurally excluded from the conversation about what counts as legitimate development. Continuity advocates who argue that medieval transmission is a live legitimate form are excluded from authority. Reconstructionists who would draw the boundary more sharply are outmaneuvered by the hybrid standard's strategic accommodation.
% DISAPPEARANCE_RATIONALE: If the hybrid standard and its enforcement disappeared, institutional users would face a crisis of legitimacy—their technical vocabularies would need separate justification, their manuscripts would lose a unified rubric for correction, and the educational system would need to choose between Classical reconstruction and medieval continuity rather than claiming to hold both. The distribution of authority over Latin correctness would revert to distributed medieval practice or require explicit commitment to an alternative principle.
% FOUNDING_PROBLEM: Medieval Latin had drifted substantially from Classical form through natural linguistic evolution, regional variation, and technical innovation. Institutional users needed a way to maintain Latin's prestige and intelligibility across regions and domains while accommodating the inevitable developments their own practice produced.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical and educational authorities attest the problem is live and the hybrid solution is necessary. Continuity advocates argue the founding problem was an invented crisis—medieval Latin was working fine as a living language until institutional authorities decided to delegitimize it for prestige reasons. Philologists outside the benefiting institutions attest that the hybrid boundary (which developments are legitimate, which are barbarous) is enforced by institutional convenience rather than by linguistic principle.
narrative_ontology:disappearance_verdict(classical_latin_standard__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(classical_latin_standard__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__hybrid_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(classical_latin_standard__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the constraint both solves coordination problems and asymmetrically distributes authority. The measurement series shows rising extractiveness from 1100–1550 (from 0.35 to 0.51), reflecting tightening institutional control and sharper policing of the Classical/barbarism boundary as the standard becomes pedagogically entrenched; from 1550–1700 extractiveness slightly declines as humanist philology produces more systematic Classical scholarship, making the boundary slightly more transparent (though still enforced). Suppression rises correspondingly (0.38→0.61, 1100–1550), tracking the enforcement infrastructure built around manuscript correction, educational curriculum, and institutional authority; from 1550–1700 it stabilizes around 0.56 as the suppressive machinery becomes institutionalized routine rather than explicit enforcement. Theater rises (0.22→0.41, 1100–1550) because the hybrid standard increasingly performs legitimacy—rhetoric about 'preserving Classical purity while accommodating necessary innovation'—while the actual mechanism is institutional gatekeeping; from 1550–1700 theater stabilizes as the performance becomes routine institutional practice. The accessibility_collapse (0.62) reflects that once the standard is institutionalized, alternatives become nearly invisible—medieval Latin looks barbarous, reconstructionist Latin looks pedantic, continuity reading looks heretical. The resistance (0.58) captures real opposition from continuity advocates and peripheral users, never fully suppressed because the standard can co-opt their language ('we do recognize legitimate developments').
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary and victim seats experience radically different constraints. From the ecclesiastical/educational seat, this is a rope—a coordination mechanism that enables intelligent communication while preserving prestige and protecting technical vocabulary. From the medieval continuity user's seat, this is a snare—institutional authority masquerading as principle, delegitimizing natural linguistic development and extracting the right to define correctness. From the peripheral speaker's seat, it is a pure snare with identity-locking: the forms they were taught are categorized as barbarous, yet the standard's accommodation of ecclesiastical and technical vocabulary makes it impossible to argue that 'all post-Classical forms are barbarous.' The boundary is set by institutional power, not by rule, and peripheral speakers lack the institutional position to argue for their forms. The engine computes per-seat classification from this structural data; the claimed type (tangled_rope) reflects the reading's position that coordination and asymmetric extraction coexist in one structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical institutions and educational authorities are structural beneficiaries (d near 0.0–0.15): they collect institutional authority, enforce the standard through education and manuscript control, and legitimize their own technical innovations. Technical specialists are moderate beneficiaries (d near 0.20–0.35): they benefit from institutional accommodation of their vocabularies but remain dependent on institutional gatekeeping. Medieval continuity users are structural targets (d near 0.75–0.85): their forms are delegitimized without transparent principle, they cannot exit the constraint without abandoning institutional participation, and their identity is partially constituted through medieval Latin practice (identity_locked). Peripheral speakers are complete targets (d near 0.90): they lack exit options, institutional access, and voice in the boundary-setting process. The hybrid reading produces seat divergence: from the institutional beneficiary seat, the constraint appears as enlightened accommodation of necessary development plus coordination; from the continuity user seat, it appears as institutional suppression of living linguistic practice. The engine computes this divergence from the structural data; the authoring seat (the reading as instantiated) declares the moderate asymmetry, not the unanimity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (medieval drift created communication barriers across regions and domains) was live in 1100–1200 but is substantively solved by 1300. However, the institutional machinery for enforcing the standard persists and intensifies (suppression_requirement rises from 0.38 to 0.61, 1100–1550) because the boundary-setting function has become lucrative for institutional actors—they collect authority, legitimize their own innovations, and maintain their gatekeeping power. The hybrid reading's strategic accommodation of ecclesiastical and technical vocabulary while delegitimizing medieval continuity is not a response to communication breakdown but a mechanism for institutional authority extraction. A genuine mandatrophy is present: the constraint's original coordination function persists (that is real), but enforcement intensity rises as institutional users leverage the standard to extract authority from competitors, suggesting the active-enforcement requirement is driven more by extraction-maintenance than by coordination-preservation. The theater_ratio rising from 0.22 to 0.41 (1100–1550) supports this: performance—the rhetoric of 'preserving Classical truth while accommodating innovation'—substitutes for transparency about the institutional gatekeeping mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_transparency_mechanism,
    'Is the boundary between ''legitimate technical extension'' and ''barbarism'' enforced by transparent linguistic principle or by institutional gatekeeping convenience?',
    'Examine the patterns of accommodation: do ecclesiastical/technical vocabularies receive systematic linguistic justification (etymology, morphological transparency, analogical formation) while medieval continuity forms receive dismissal? Or does justification track institutional status—forms used by powerful institutions are rationalized as legitimate, forms used by peripheral communities are delegitimized without principle?',
    'If enforced by transparent principle: the constraint is genuinely a tangled_rope with real coordination + moderate extraction. If enforced by gatekeeping convenience: the constraint is functionally a snare with a coordination facade, and the beneficiary set is narrower (institutional gatekeepers only, not all technical specialists). The classification shifts based on the mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_transparency_mechanism, empirical, 'Whether the hybrid standard''s boundary is principled or institutional-convenience-driven.').

omega_variable(
    identity_lock_depth_peripheral_speakers,
    'For peripheral Romance-influenced Latin speakers, is the suppression structural (economic barriers to institutional education, geographic isolation from authority centers) or internalized (speakers come to believe their forms are genuinely barbarous)?',
    'Post-exit observation: if peripheral speakers migrate to institutional centers and undergo Classical education, do they internalize the standard''s values (come to see their original forms as barbarous) or retain awareness of the naturalness of their linguistic development? Internalization would indicate deep identity-locking; retention of that awareness would indicate more structural suppression.',
    'If internalized: the constraint''s suppression is higher than the 0.56 structural measure suggests, and the peripheral speaker''s exit is not true exit (they carry the internalized constraint). If structural: the suppression is accurately measured, and exit from the constraint is possible once the barrier (lack of institutional education, isolation) is removed. The classification does not change, but the trajectory post-exit differs sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_depth_peripheral_speakers, empirical, 'Whether suppression of peripheral speakers is structural or internalized.').

omega_variable(
    hybrid_reading_vs_reconstruction_foreclosure,
    'Does the hybrid reading''s strategic accommodation of technical vocabulary logically foreclose the reconstruction reading''s core premise, or do both remain coherently available for different institutional contexts?',
    'Test whether a party could consistently hold both: ''Medieval drift in ecclesiastical domains is legitimate technical development (hybrid) AND Classical form is recoverable by rejecting all post-Classical drift (reconstruction).'' If coherent, the readings coexist. If incoherent, hybrid forecloses reconstruction.',
    'If coherent: the readings coexist_with each other (different institutions adopt different standards). If incoherent: hybrid forecloses reconstruction because hybrid says ''some post-Classical forms are legitimate'' and reconstruction says ''all post-Classical forms must be rejected.'' The network relation changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hybrid_reading_vs_reconstruction_foreclosure, conceptual, 'Foreclosure relation between hybrid and reconstruction readings.').

omega_variable(
    ecclesiastical_capture_of_legitimacy_standard,
    'Does the ecclesiastical institution benefit from the standard itself (independent of content), such that any standard that gives them gatekeeping authority would be advantageous, regardless of what principle it embodies?',
    'Compare ecclesiastical support for the hybrid standard to their hypothetical support for a pure reconstruction standard (which would require abandoning many ecclesiastical neologisms). If ecclesiastical support is equally strong for both, the institution captures the legitimacy-setting function independent of content. If support is conditional on accommodating ecclesiastical vocabulary, the institution benefits from the hybrid content specifically.',
    'If captured: the beneficiary from institutional gatekeeping is ecclesiastical organizations, and the extraction is pure authority-collection (the specific principle is secondary). If content-dependent: the beneficiary includes both ecclesiastical institutions and technical specialists, and the extraction is more asymmetrically distributed. The beneficiary set size and the extraction mechanism change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_capture_of_legitimacy_standard, conceptual, 'Whether ecclesiastical support tracks the hybrid principle or institutional gatekeeping authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__hybrid_reading, 1100, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t1100, classical_latin_standard__hybrid_reading, theater_ratio, 1100, 0.22).
narrative_ontology:measurement_basis(clas_tr_t1100, projected).
narrative_ontology:measurement(clas_tr_t1250, classical_latin_standard__hybrid_reading, theater_ratio, 1250, 0.28).
narrative_ontology:measurement_basis(clas_tr_t1250, observed).
narrative_ontology:measurement(clas_tr_t1400, classical_latin_standard__hybrid_reading, theater_ratio, 1400, 0.35).
narrative_ontology:measurement_basis(clas_tr_t1400, observed).
narrative_ontology:measurement(clas_tr_t1550, classical_latin_standard__hybrid_reading, theater_ratio, 1550, 0.41).
narrative_ontology:measurement_basis(clas_tr_t1550, observed).
narrative_ontology:measurement(clas_tr_t1650, classical_latin_standard__hybrid_reading, theater_ratio, 1650, 0.39).
narrative_ontology:measurement_basis(clas_tr_t1650, observed).
narrative_ontology:measurement(clas_tr_t1700, classical_latin_standard__hybrid_reading, theater_ratio, 1700, 0.38).
narrative_ontology:measurement_basis(clas_tr_t1700, observed).

% Extraction over time
narrative_ontology:measurement(clas_be_t1100, classical_latin_standard__hybrid_reading, base_extractiveness, 1100, 0.35).
narrative_ontology:measurement_basis(clas_be_t1100, projected).
narrative_ontology:measurement(clas_be_t1250, classical_latin_standard__hybrid_reading, base_extractiveness, 1250, 0.42).
narrative_ontology:measurement_basis(clas_be_t1250, observed).
narrative_ontology:measurement(clas_be_t1400, classical_latin_standard__hybrid_reading, base_extractiveness, 1400, 0.48).
narrative_ontology:measurement_basis(clas_be_t1400, observed).
narrative_ontology:measurement(clas_be_t1550, classical_latin_standard__hybrid_reading, base_extractiveness, 1550, 0.51).
narrative_ontology:measurement_basis(clas_be_t1550, observed).
narrative_ontology:measurement(clas_be_t1650, classical_latin_standard__hybrid_reading, base_extractiveness, 1650, 0.47).
narrative_ontology:measurement_basis(clas_be_t1650, observed).
narrative_ontology:measurement(clas_be_t1700, classical_latin_standard__hybrid_reading, base_extractiveness, 1700, 0.48).
narrative_ontology:measurement_basis(clas_be_t1700, observed).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t1100, classical_latin_standard__hybrid_reading, suppression_requirement, 1100, 0.38).
narrative_ontology:measurement_basis(clas_su_t1100, projected).
narrative_ontology:measurement(clas_su_t1250, classical_latin_standard__hybrid_reading, suppression_requirement, 1250, 0.48).
narrative_ontology:measurement_basis(clas_su_t1250, observed).
narrative_ontology:measurement(clas_su_t1400, classical_latin_standard__hybrid_reading, suppression_requirement, 1400, 0.56).
narrative_ontology:measurement_basis(clas_su_t1400, observed).
narrative_ontology:measurement(clas_su_t1550, classical_latin_standard__hybrid_reading, suppression_requirement, 1550, 0.61).
narrative_ontology:measurement_basis(clas_su_t1550, observed).
narrative_ontology:measurement(clas_su_t1650, classical_latin_standard__hybrid_reading, suppression_requirement, 1650, 0.58).
narrative_ontology:measurement_basis(clas_su_t1650, observed).
narrative_ontology:measurement(clas_su_t1700, classical_latin_standard__hybrid_reading, suppression_requirement, 1700, 0.56).
narrative_ontology:measurement_basis(clas_su_t1700, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__reconstruction_reading).

% DUAL FORMULATION NOTE:
% The classical_latin_standard kernel decomposes into three distinct constraints corresponding to three readings: continuity_reading (medieval transmission as live linguistic practice — a rope), reconstruction_reading (Classical recovery by rejecting medieval drift — a snare of textual authority), and hybrid_reading (this constraint — tangled rope of coordination + institutional extraction). The three readings instantiate different constraint types because they make fundamentally different normative claims about what developments are legitimate. Each reading has distinct beneficiary/victim structures, distinct enforcement mechanisms, and distinct extractiveness profiles. The readings are linked by kernel identity and by the reading_relations in cs_structure, not by causal dependency — they are alternative instantiations of a contested commitment, not serial revisions of a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(classical_latin_standard__hybrid_reading, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
