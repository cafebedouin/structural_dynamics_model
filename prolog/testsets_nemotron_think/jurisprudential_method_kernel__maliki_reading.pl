% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__maliki_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__maliki_reading
 *   human_readable: Maliki Jurisprudential Method: Medinan Practice as Living Tradition
 *   domain: legal_philosophy/islamic_jurisprudence/institutional_history
 *
 * SUMMARY:
 *   The Maliki reading of the jurisprudential method kernel holds that
 *   Islamic law derives from Qur'an and Hadith as concretely practiced by the
 *   early Medinan community ('amal ahl al-Madina). This living tradition is
 *   treated as a valid, independent source of law alongside scripture because
 *   Medina — where the Prophet lived, governed, and died — is held to have
 *   preserved his practice most faithfully. The constraint coordinates legal
 *   interpretation by anchoring it in a specific communal practice, solving
 *   the problem of how to derive concrete rulings from general revelation.
 *   However, it simultaneously extracts by privileging the Medinan scholarly
 *   lineage and their transmitted practice over other legitimate interpretive
 *   communities (Kufa, Basra, Mecca) and over rationalist methods (qiyas,
 *   istihsan). The constraint requires active enforcement through scholarly
 *   authority (ijaza, madrasa curriculum, qadi appointments) to maintain
 *   Medinan practice as the exclusive authenticator of prophetic sunna. The
 *   claimed type from within the tradition is 'rope' (pure coordination — the
 *   most faithful preservation); the authored metrics reveal substantial
 *   asymmetric extraction, making it a tangled_rope.
 *
 * KEY AGENTS:
 *   - medinan_scholars: Primary agenda_setter (institutional/generational/analytical) — define and transmit the Medinan practice, control the method's authoritative interpretation
 *   - maliki_jurists: Primary beneficiary (organized/biographical/identity_locked) — derive professional authority, institutional position, and interpretive monopoly from the method's dominance
 *   - hanafi_tradition: Primary payer (powerful/generational/constrained) — their rationalist methodology (qiyas, istihsan) is structurally marginalized as inferior to Medinan practice
 *   - shafii_tradition: Primary payer (powerful/generational/constrained) — their hadith-critical hierarchy competes with Medinan practice as the arbiter of sunna
 *   - hanbali_tradition: Primary payer (organized/generational/constrained) — their textual literalism rejects Medinan practice as bid'ah when it lacks explicit hadith backing
 *   - early_kufan_scholars: Excluded (moderate/biographical/trapped) — their substantive law was historically displaced by Medinan practice's claim to superior authenticity
 *   - comparative_legal_historians: Observer (analytical/civilizational/analytical) — analyze the method's historical contingency and structural function from outside the tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, 0.48).
domain_priors:suppression_score(jurisprudential_method_kernel__maliki_reading, 0.55).
domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__maliki_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Maliki Jurisprudential Method: Medinan Practice as Living Tradition").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "legal_philosophy/islamic_jurisprudence/institutional_history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, '3d06c30a-fd0d-4ccd-ae1d-2883cc65d844').
narrative_ontology:cs_kernel_codification('3d06c30a-fd0d-4ccd-ae1d-2883cc65d844', fixed_text).
narrative_ontology:cs_authority_grounding('3d06c30a-fd0d-4ccd-ae1d-2883cc65d844', lineage).
narrative_ontology:cs_interpretation_layer_present('3d06c30a-fd0d-4ccd-ae1d-2883cc65d844').
narrative_ontology:cs_reading_relation('3d06c30a-fd0d-4ccd-ae1d-2883cc65d844', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d06c30a-fd0d-4ccd-ae1d-2883cc65d844', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d06c30a-fd0d-4ccd-ae1d-2883cc65d844', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('3d06c30a-fd0d-4ccd-ae1d-2883cc65d844', foundational, medinan_practice_uniquely_authentic).
narrative_ontology:cs_axiom_status(medinan_practice_uniquely_authentic, holdable).
narrative_ontology:cs_axiom_grounding('3d06c30a-fd0d-4ccd-ae1d-2883cc65d844', medinan_practice_uniquely_authentic, conventional).
narrative_ontology:cs_axiom('3d06c30a-fd0d-4ccd-ae1d-2883cc65d844', foundational, living_tradition_valid_source).
narrative_ontology:cs_axiom_status(living_tradition_valid_source, holdable).
narrative_ontology:cs_axiom_grounding('3d06c30a-fd0d-4ccd-ae1d-2883cc65d844', living_tradition_valid_source, conventional).
narrative_ontology:cs_reference_frame('3d06c30a-fd0d-4ccd-ae1d-2883cc65d844', prophetic_medinan_community_practice).
narrative_ontology:cs_drift_state('3d06c30a-fd0d-4ccd-ae1d-2883cc65d844', post_shafii_methodological_standardization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3d06c30a-fd0d-4ccd-ae1d-2883cc65d844', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_traditions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, maliki_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, hanafi_tradition).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, shafii_tradition).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, hanbali_tradition).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__maliki_reading, medinan_practice_uniquely_authentic).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__maliki_reading, living_tradition_authority).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__maliki_reading, amal_ahl_al_madina_as_hujja).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define, transmit, and adjudicate the Medinan practice through ijaza chains, madrasa curricula, and qadi appointments. They control the method's authoritative interpretation and certify who speaks for the tradition. Their authority derives from the claim of unbroken transmission from the Prophet's community. Exit means abandoning the epistemic framework that constitutes their scholarly identity.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_scholars, agenda_setter,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, medinan_scholars, beneficiary).

% Derive professional standing, institutional positions (qadi, mufti, professor), and interpretive authority from the Maliki method's dominance in North/West Africa and parts of the Gulf. Their career path, intellectual formation, and communal recognition are fused with the method. Exit requires retraining in another madhhab — professionally and identity-costly.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, maliki_jurists, beneficiary,
    organized, biographical, identity_locked, global).

% Their rationalist methodology (qiyas, istihsan, ray) is structurally positioned as inferior to Medinan practice in the Maliki framework. They maintain a vast, coherent legal system across South/Central Asia, Turkey, and the Arab world, but within Maliki epistemology their derivations lack the 'Medinan seal.' Exit means accepting marginalization or adopting Maliki premises — constrained by their own institutional momentum.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, hanafi_tradition, payer,
    powerful, generational, constrained, global).

% Their hadith-critical hierarchy (Qur'an > Hadith > Ijma > Qiyas) competes directly with Maliki practice-as-source. Shafi'i's methodology was partly developed in response to Maliki claims. They dominate in Southeast Asia, East Africa, and parts of the Levant, but the Maliki method denies their hadith-critical arbiter primacy. Exit is constrained by their own global institutional weight.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, shafii_tradition, payer,
    powerful, generational, constrained, global).

% Their textual literalism (Qur'an/Hadith + Companion opinions only) treats Medinan practice without explicit hadith backing as bid'ah. They are structurally excluded from the Maliki epistemic circle because they reject the very premise of practice-as-independent-source. Dominant in parts of the Arabian Peninsula; exit means accepting a rival epistemology they consider heretical.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, hanbali_tradition, payer,
    organized, generational, constrained, global).

% Historical actors (2nd-3rd century AH) whose substantive law and practice were displaced by the rising claim that only Medinan practice authentically preserves the sunna. Figures like Ibn Abi Layla, al-Awza'i, early Hanafis. They are not present to object now; their exclusion is structural — the constraint's authenticity claim requires their marginalization. Exit was impossible: the epistemic field was reconfigured around them.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, early_kufan_scholars, excluded,
    moderate, biographical, trapped, regional).

% Analyze the Maliki method's historical development, structural function, and relationship to other schools from outside the tradition. They bear no professional cost or benefit from the constraint's operation. Their role is to map the coordination-extraction fusion, trace the historical contingency of the 'Medinan privilege' claim, and assess whether the method still solves a live coordination problem or has become extractive inertia.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__maliki_reading, medinan_scholars).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, community-anchored method for deriving concrete legal rulings from general revelation by treating the witnessed practice of the Prophet's city as a living, authoritative transmission of his sunna — solving the fragmentation problem of isolated hadith reports and rationalist speculation.
% TRANSFER_FUNCTION: Moves interpretive authority and professional recognition from non-Medinan traditions (rationalist, hadith-critical, literalist) to the Medinan scholarly lineage and their institutional successors, by making 'Medinan practice' the gatekeeping criterion for authenticity.
% ABSENT_VOICES: Early Kufan, Basran, and Meccan scholars whose living traditions were displaced by the Medinan authenticity claim; modern reformist scholars who argue for a non-madhhab, evidence-based usul that treats all regional practices as equally fallible historical data.
% DISAPPEARANCE_RATIONALE: If the Medinan practice privilege vanished overnight, the Maliki school would lose its distinctive epistemology and merge into a generic hadith-critical or rationalist framework; qadi appointments, madrasa curricula, and fatwa authority structures across North/West Africa would require reorganization; the Hanafi, Shafi'i, and Hanbali schools would lose their primary structural foil for 'practice vs. reason/text' debates.
% FOUNDING_PROBLEM: How to derive stable, concrete legal rulings from the Qur'an and Hadith without fragmenting into endless rationalist speculation (ray) or relying on isolated, potentially fabricated hadith reports — by anchoring derivation in the collective, witnessed practice of the community that lived with the Prophet.
% FOUNDING_PROBLEM_CORROBORATION: The Maliki school attests the problem is live: hadith fabrication remains a risk, rationalist extension remains unstable, and communal practice remains the best anchor (internal attestation). Shafi'i, Hanafi, and Hanbali scholars attest the problem is substantially solved by their own methods (hadith criticism, structured qiyas, textual literalism) and that the Medinan claim is a regional preference, not a universal solution (external corroboration for 'contested'). Modern legal historians (Schacht, Hallaq, Melchert) attest the problem was historically contingent — the 'Medinan practice' construct emerged in the 2nd century AH as a school-identity marker, not as a neutral coordination discovery (external scholarly corroboration).
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__maliki_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__maliki_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is medium: the constraint coordinates genuine legal derivation (solving the revelation-to-ruling problem) but extracts by making Medinan practice the gatekeeper of authenticity, capturing interpretive authority for one regional tradition. Suppression (0.55) is moderate: alternative methods are not banned but are delegitimized as 'less authentic' or 'innovative,' requiring active scholarly enforcement to maintain the hierarchy. Theater ratio (0.25) is low-moderate: the coordination function (stable legal derivation) is real and primary; the extractive overlay (regional privilege) is present but not the whole story. Accessibility collapse (0.52) reflects that alternatives exist but are structurally disadvantaged — a scholar trained in the Maliki method faces high identity/exit costs to adopt another framework. Resistance (0.45) is moderate: competing schools (Hanafi, Shafi'i, Hanbali) mounted sustained intellectual challenges but never displaced the Maliki method in its core regions. The claimed_type (tangled_rope) reflects the author's structural judgment: genuine coordination + asymmetric extraction + active enforcement. The Maliki school's own self-understanding would claim 'rope'; the divergence is the measurement.
 *
 * PERSPECTIVAL GAP:
 *   From the medinan_scholars (agenda_setter) seat, the constraint is experienced as rope: they see a faithful transmission solving a real coordination problem with minimal coercion. From the maliki_jurists (beneficiary) seat, it is also rope: they inherit a working method that grants them professional standing. From the hanafi/shafii/hanbali (payer) seats, it is experienced as snare or tangled_rope: their equally rigorous methods are treated as derivative or inferior because they lack the 'Medinan seal.' From the early_kufan_scholars (excluded) seat, it is snare: their living tradition was actively displaced by a competing claim to authenticity. From the comparative_legal_historians (observer) seat, the structural asymmetry is visible: a genuine coordination function (stable legal derivation) is fused with a regional privilege (Medina over Kufa/Basra) that extracts interpretive authority. The engine computes this seat divergence from the structural data — power, exit_options, and declared beneficiary/payer roles.
 *
 * DIRECTIONALITY LOGIC:
 *   The medinan_scholars and maliki_jurists are structural beneficiaries (d near 0.0): they collect interpretive authority, institutional control, and professional rents from the method's dominance. Their exit_options are analytical/identity_locked — they are invested in the method's truth. The hanafi_tradition, shafii_tradition, and hanbali_tradition are structural payers (d near 0.8-0.9): their interpretive claims are suppressed by the Medinan authenticity monopoly; their exit_options are constrained — they can maintain their schools but cannot claim equal authenticity within the Maliki framework. The early_kufan_scholars are excluded (d near 1.0): historically displaced, trapped in a superseded paradigm. The comparative_legal_historians are analytical (d = 0.5): they bear no cost or benefit from the constraint's operation. The directionality derivation follows from beneficiary/victim declarations plus exit modulation: beneficiaries with identity_locked exit get low d; payers with constrained exit get high d; excluded with trapped exit get maximal d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — deriving concrete law from general revelation in a way that preserves prophetic authority — remains live (founding_problem_status: contested). The Maliki method's coordination function is genuine: without a stable interpretive anchor, legal derivation fragments. However, the mandate has partially atrophied into extraction: the Medinan practice claim now functions more as a boundary-maintenance device for the Maliki school than as a neutral coordination solution. The constraint is not a piton because the coordination function is still actively used (qadis still apply it, jurists still derive rulings from it); it is not a snare because the coordination is not mere cover. It is a tangled_rope: the coordination and extraction are structurally fused. Mandatrophy is unresolved — the method persists because it still coordinates, but the extraction layer has grown.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Maliki method a genuine coordination solution (the best available anchor for prophetic sunna) or an extractive privileging of one regional tradition over others?',
    'Comparative historical analysis of early legal development in Medina vs. Kufa vs. Basra: if Medinan practice shows demonstrably superior continuity with prophetic practice, the coordination reading wins; if the difference is contingent/constructed, the extraction reading wins.',
    'If coordination: the constraint is rope (low ε, low suppression). If extraction: the constraint is snare or tangled_rope (medium ε, active suppression of alternatives). The current metrics split the difference (tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the Medinan practice''s epistemic privilege is historical fact or constructed authority.').

omega_variable(
    historical_fidelity_of_medinan_practice,
    'Did the early Medinan community actually preserve the Prophet''s practice more faithfully than Kufa, Basra, or Mecca?',
    'Hadith transmission analysis (isnad criticism), archaeological evidence of early practice, comparative fiqh reconstruction of pre-madhhab law.',
    'If yes: the constraint''s coordination function is empirically grounded, ε drops toward rope. If no: the constraint''s extraction is revealed as regional chauvinism, ε rises toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_fidelity_of_medinan_practice, empirical, 'Empirical verifiability of the Maliki school''s core historical claim.').

omega_variable(
    modern_mandatrophy_trajectory,
    'Has the Maliki method''s extraction layer grown relative to its coordination function in the modern period (post-colonial codification, nation-state adoption)?',
    'Track theater_ratio and suppression_requirement in modern Maliki-majority jurisdictions (North/West Africa, Gulf): rising theater + stable coordination = mandatrophy accumulation.',
    'If yes: the constraint drifts toward piton (coordination atrophies, theatrical maintenance remains). If no: it remains tangled_rope with stable coordination-extraction balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_mandatrophy_trajectory, empirical, 'Whether the constraint is accumulating extraction as its coordination context changes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maliki_reading_tr_t0, jurisprudential_method_kernel__maliki_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(maliki_reading_tr_t0, observed).
narrative_ontology:measurement(maliki_reading_tr_t30, jurisprudential_method_kernel__maliki_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(maliki_reading_tr_t30, observed).
narrative_ontology:measurement(maliki_reading_tr_t60, jurisprudential_method_kernel__maliki_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement_basis(maliki_reading_tr_t60, observed).
narrative_ontology:measurement(maliki_reading_tr_t90, jurisprudential_method_kernel__maliki_reading, theater_ratio, 90, 0.23).
narrative_ontology:measurement_basis(maliki_reading_tr_t90, observed).
narrative_ontology:measurement(maliki_reading_tr_t120, jurisprudential_method_kernel__maliki_reading, theater_ratio, 120, 0.25).
narrative_ontology:measurement_basis(maliki_reading_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(maliki_reading_be_t0, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(maliki_reading_be_t0, observed).
narrative_ontology:measurement(maliki_reading_be_t30, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement_basis(maliki_reading_be_t30, observed).
narrative_ontology:measurement(maliki_reading_be_t60, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement_basis(maliki_reading_be_t60, observed).
narrative_ontology:measurement(maliki_reading_be_t90, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 90, 0.46).
narrative_ontology:measurement_basis(maliki_reading_be_t90, observed).
narrative_ontology:measurement(maliki_reading_be_t120, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 120, 0.48).
narrative_ontology:measurement_basis(maliki_reading_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(maliki_reading_su_t0, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(maliki_reading_su_t0, observed).
narrative_ontology:measurement(maliki_reading_su_t30, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement_basis(maliki_reading_su_t30, observed).
narrative_ontology:measurement(maliki_reading_su_t60, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement_basis(maliki_reading_su_t60, observed).
narrative_ontology:measurement(maliki_reading_su_t90, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 90, 0.52).
narrative_ontology:measurement_basis(maliki_reading_su_t90, observed).
narrative_ontology:measurement(maliki_reading_su_t120, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 120, 0.55).
narrative_ontology:measurement_basis(maliki_reading_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__maliki_reading, 0.08).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the jurisprudential_method_kernel. The Maliki reading privileges communal practice ('amal) as a source; the Hanafi reading privileges rationalist extension (qiyas/istihsan); the Shafi'i reading privileges hadith-transmission hierarchy; the Hanbali reading privileges literal text. All four decompose the single colloquial label 'Islamic legal method' into structurally distinct constraints with different ε, beneficiaries, and victims. They are linked via affects_constraints because each reading's methodological claims structurally condition the others' legitimacy space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__maliki_reading, organized, 0.15).
constraint_indexing:directionality_override(jurisprudential_method_kernel__maliki_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
