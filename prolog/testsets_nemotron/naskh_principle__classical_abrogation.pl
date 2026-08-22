% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__classical_abrogation, []).

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
 *   constraint_id: naskh_principle__classical_abrogation
 *   human_readable: Classical Naskh (Abrogation) Principle — Chronological Supersession
 *   domain: religious_legal/hermeneutic
 *
 * SUMMARY:
 *   The classical naskh (abrogation) principle holds that later-revealed
 *   Quranic verses supersede earlier ones on the same legal topic, rendering
 *   the earlier verse's legal ruling inoperative while preserving its
 *   recitational and spiritual value. This reading became the dominant
 *   hermeneutic in Sunni usul al-fiqh from the 3rd/9th century onward,
 *   institutionalized through madrasa curricula, state qadi appointments, and
 *   codified fiqh. It solves a genuine coordination problem — how to derive
 *   consistent law from a revelation corpus containing apparently
 *   contradictory verses — but does so through a chronological supersession
 *   mechanism that extracts interpretive flexibility from later scholars and
 *   communities. The constraint is actively enforced: fatwa institutions,
 *   judicial precedent, and curricular gatekeeping suppress rival readings
 *   (contextual harmonization, progressive restriction). Beneficiaries are
 *   the classical ulama institutions, state sharia courts, and modern fiqh
 *   codification bodies that gain stable legal authority; victims are
 *   contextual exegetes, progressive reform movements, and minority sect
 *   interpretive traditions whose readings are excluded. The claim/metric gap
 *   is deliberate: the constraint is CLAIMED as tangled_rope (genuine
 *   coordination + asymmetric extraction) while the metrics reflect the
 *   historical trajectory from lower to higher extraction and suppression as
 *   the doctrine hardened into institutional orthodoxy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.62).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.71).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.62).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Naskh (Abrogation) Principle — Chronological Supersession").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "religious_legal/hermeneutic").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, 'b988e383-447b-4bbc-bffa-d4f5aeb5c9f5').
narrative_ontology:cs_kernel_codification('b988e383-447b-4bbc-bffa-d4f5aeb5c9f5', fixed_text).
narrative_ontology:cs_authority_grounding('b988e383-447b-4bbc-bffa-d4f5aeb5c9f5', lineage).
narrative_ontology:cs_interpretation_layer_present('b988e383-447b-4bbc-bffa-d4f5aeb5c9f5').
narrative_ontology:cs_reading_relation('b988e383-447b-4bbc-bffa-d4f5aeb5c9f5', naskh_principle__contextual_harmonization, forecloses).
narrative_ontology:cs_reading_relation('b988e383-447b-4bbc-bffa-d4f5aeb5c9f5', naskh_principle__progressive_restriction, influences).
narrative_ontology:cs_axiom('b988e383-447b-4bbc-bffa-d4f5aeb5c9f5', foundational, chronological_revelation_determines_legal_hierarchy).
narrative_ontology:cs_axiom_status(chronological_revelation_determines_legal_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('b988e383-447b-4bbc-bffa-d4f5aeb5c9f5', chronological_revelation_determines_legal_hierarchy, conventional).
narrative_ontology:cs_axiom('b988e383-447b-4bbc-bffa-d4f5aeb5c9f5', foundational, abrogated_verse_retains_spiritual_not_legal_force).
narrative_ontology:cs_axiom_status(abrogated_verse_retains_spiritual_not_legal_force, holdable).
narrative_ontology:cs_axiom_grounding('b988e383-447b-4bbc-bffa-d4f5aeb5c9f5', abrogated_verse_retains_spiritual_not_legal_force, conventional).
narrative_ontology:cs_axiom('b988e383-447b-4bbc-bffa-d4f5aeb5c9f5', secondary, nasikh_mansukh_catalog_is_closed_and_authoritative).
narrative_ontology:cs_axiom_status(nasikh_mansukh_catalog_is_closed_and_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('b988e383-447b-4bbc-bffa-d4f5aeb5c9f5', nasikh_mansukh_catalog_is_closed_and_authoritative, conventional).
narrative_ontology:cs_reference_frame('b988e383-447b-4bbc-bffa-d4f5aeb5c9f5', classical_usul_consensus_3rd_century).
narrative_ontology:cs_drift_state('b988e383-447b-4bbc-bffa-d4f5aeb5c9f5', modern_codification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b988e383-447b-4bbc-bffa-d4f5aeb5c9f5', '2026-08-10T14:30:00Z').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, classical_ulama_institutions).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, state_sharia_courts).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, fiqh_codification_bodies).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, contextual_exegetes).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, progressive_reform_movements).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, minority_sect_interpretive_traditions).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, divine_wisdom_in_gradual_legislation).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, legal_certainty_through_clear_hierarchy).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, chronological_revelation_as_interpretive_key).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the transmission of usul al-fiqh, define the canonical abrogation catalog (nasikh wa mansukh), staff the madrasa curricula and fatwa bodies that enforce the classical reading. They collect authority rents from being the gatekeepers of legal certainty. Their exit options are arbitrage-grade: they can migrate between institutional posts, state patronage, and transnational scholarly networks.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, classical_ulama_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Adjudicate using the classical naskh hierarchy as binding precedent. The doctrine gives them a clear, stable rule-set for consistent judgment — they benefit from legal certainty. But they are constrained by the classical chronology: they cannot adopt contextual or progressive readings without exceeding their institutional mandate. Exit means leaving the state judicial system.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, state_sharia_courts, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(naskh_principle__classical_abrogation, state_sharia_courts, beneficiary).

% Produce modern statute codes (e.g., family law, criminal law) grounded in classical fiqh. The classical naskh principle provides a determinate source hierarchy that makes codification tractable. They benefit from the coordination function. But they are bound by the classical abrogation catalog — they cannot 're-open' abrogated verses without scholarly backlash. Exit means producing codes without classical authority, losing legitimacy.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, fiqh_codification_bodies, beneficiary,
    organized, generational, constrained, national).

% Develop readings that resolve apparent contradictions through historical-contextual specification (asbab al-nuzul, maqasid, linguistic analysis) rather than chronological supersession. Their work is excluded from classical legal authority — it carries weight in academia but not in fatwa or qada. They pay with interpretive marginalization: their readings cannot generate binding law. Exit is identity_locked: their scholarly identity is constituted by the contextual method; abandoning it means ceasing to be the kind of scholar they are.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, contextual_exegetes, payer,
    organized, biographical, identity_locked, global).

% Seek to reform Islamic law on gender, penal, or financial issues by re-reading abrogated verses (e.g., 2:282 on testimony, 4:34 on discipline, 9:5 on warfare) as contextually superseded rather than permanently abrogated. They bear the cost of the classical catalog: the 'sword verse' and 'discipline verse' remain legally operative in classical fiqh, blocking reform. Exit is constrained: they can operate in civil society, academia, or minority jurisdictions, but cannot change the classical institutional consensus.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, progressive_reform_movements, payer,
    moderate, biographical, constrained, global).

% Hold alternative naskh theories: Zaydis reject naskh entirely; Ismailis use ta'wil (esoteric interpretation) to harmonize; some Shi'i usul restrict naskh to specific conditions. They are structurally excluded from the Sunni classical framework — their readings have no standing in Sunni courts or fatwa bodies. They are trapped: their tradition's interpretive commitments prevent adopting the classical reading, and the classical framework denies them entry.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, minority_sect_interpretive_traditions, excluded,
    moderate, generational, trapped, regional).

% Study the Quran's manuscript history, variant readings (qira'at), and chronological layering using philological and historical methods. They observe the constraint's operation from outside the tradition's authority structure. They neither collect nor pay — their work informs but does not bind the classical institutions. Their exit is analytical: they can engage or disengage without identity or institutional cost.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, quranic_text_critical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__classical_abrogation, classical_ulama_institutions).
narrative_ontology:fixing_cost_class(naskh_principle__classical_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves apparent contradictions in the Quranic legal corpus by establishing a clear chronological hierarchy: later verses supersede earlier ones on the same topic, yielding a determinate rule-set for legal derivation.
% TRANSFER_FUNCTION: Moves interpretive authority and legal determinacy from the open field of contextual/progressive readings to the closed catalog of classical abrogation pairs, concentrating hermeneutic power in the institutions that control the chronology and the nasikh wa mansukh literature.
% ABSENT_VOICES: The Quranic text's own polyphony — verses that speak in multiple registers (legal, ethical, theological, narrative) — is silenced when the chronological rule flattens them into a single legal hierarchy. Also absent: early companion readings that show no awareness of a systematic naskh doctrine (e.g., Ibn Mas'ud's codex, Ubayy's readings), and the lived practice of communities that harmonized verses contextually without abrogation.
% DISAPPEARANCE_RATIONALE: If the classical naskh principle vanished overnight, the entire edifice of classical fiqh built on the abrogation catalog would lose its determinate hierarchy. Courts would face open contradiction between verses. Codified statutes grounded in abrogated-verse rulings would lose their classical justification. Reform movements would gain immediate hermeneutic space. The legal order would reorganize around contextual harmonization or progressive restriction — or fragment into interpretive pluralism.
% FOUNDING_PROBLEM: Early Muslim communities faced practical legal contradictions in the Quranic corpus: e.g., the gradual prohibition of alcohol (2:219 → 4:43 → 5:90-91), the shift in qibla (2:142-144), the modification of inheritance rules (2:180 → 4:11-12). The classical naskh principle was built to resolve these into a coherent, applicable law.
% FOUNDING_PROBLEM_CORROBORATION: Classical usul texts (Shatibi, Jassas, Suyuti) attest the founding problem is live — new contradictions can emerge. Contextual exegetes (Fazlur Rahman, Nasr Abu Zayd, Abdelmajid Charfi) and progressive scholars (Abdullahi An-Na'im, Amina Wadud, Khaled Abou El Fadl) attest the founding problem is substantially solved by historical-contextual methods that do not require textual invalidation. Minority sect traditions (Zaydi, Ismaili) attest the problem was never solved by naskh in their frameworks. The classical reading's beneficiaries are the only ones claiming the problem requires chronological supersession.
narrative_ontology:disappearance_verdict(naskh_principle__classical_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__classical_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__classical_abrogation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(naskh_principle__classical_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__classical_abrogation, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__classical_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__classical_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__classical_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the doctrine's capture of interpretive space: the chronological rule closes off contextual and progressive readings that would otherwise be available, transferring hermeneutic authority to those who control the chronology and the abrogation catalog. Suppression (0.71) is high because maintaining the classical reading requires active exclusion — rival readings are not merely disfavored but treated as methodologically illegitimate in classical institutions. Theater ratio (0.28) is moderate: the coordination function (resolving contradictions) is real, but a growing share of the doctrine's operation serves to protect the institutional authority of classical usul. Accessibility collapse (0.58) is partial — alternative readings persist in marginalized traditions and modern academia — but the constraint makes them structurally inaccessible to mainstream legal authority. Resistance (0.44) is significant but contained: reform movements exist but operate largely outside the classical institutional perimeter.
 *
 * PERSPECTIVAL GAP:
 *   From the classical ulama seat, the constraint is genuine coordination: it resolves contradiction, preserves legal continuity, and embodies divine wisdom in gradual legislation. From the contextual exegete seat, it is extraction: a chronological heuristic elevated into dogma that silences the Quran's own contextual polyphony. From the state sharia court seat, it is legal certainty: a clear hierarchy that enables codification and predictable adjudication. From the progressive reform seat, it is a barrier: the abrogation catalog freezes historical rulings as eternal law. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical ulama institutions and state sharia courts are structural beneficiaries (d ≈ 0.15-0.25): they control the abrogation catalog, define the chronology, and collect the authority rents of legal certainty. Fiqa codification bodies are secondary beneficiaries (d ≈ 0.3): they gain a stable rule-set to codify but are constrained by the classical chronology. Contextual exegetes and progressive reform movements are targets (d ≈ 0.8-0.9): their readings are excluded from legal effect, their interpretive labor is devalued, and their exit options are constrained (identity_locked for those whose scholarly identity is fused to classical usul; trapped for those institutionally dependent). Minority sect traditions are excluded (d ≈ 0.95): their alternative naskh theories (e.g., Zaydi rejection of naskh, Ismaili ta'wil) are structurally outside the Sunni classical framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — resolving apparent contradictions in the revelation corpus — remains live (Quranic contradictions persist), but the classical solution has accumulated extraction: the chronological rule became a gatekeeping mechanism for interpretive authority, and the abrogation catalog expanded beyond necessity. The mandate has not atrophied (the coordination problem is real) but has been captured. This is tangled_rope, not piton: the coordination function is genuine and actively maintained, not merely performed. The theater ratio rise documents the capture, not atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_frame,
    'Is the classical naskh reading a structural feature of the Quranic text itself, or an interpretive framework imposed by early legal institutions to resolve contradictions?',
    'Comparative manuscript chronology and early tafsir transmission history: if the abrogation doctrine predates systematic chronology, it is an institutional imposition; if it emerges from the text''s own internal evidence, it is textually grounded.',
    'If institutional imposition, the constraint is a Snare extracting interpretive flexibility for legal certainty; if textually grounded, it approaches Mountain status (though enforcement still required). This reading''s ε=0.62 assumes the institutional view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_frame, conceptual, 'Whether the classical abrogation reading is textually intrinsic or institutionally constructed.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative readings (contextual harmonization, progressive restriction) structural (institutional exclusion, fatwa enforcement, curricular monopoly) or internalized (scholarly self-censorship, identity-fused commitment to classical usul)?',
    'Post-margin-of-tolerance suppression trajectory: if alternative readings re-emerge when institutional enforcement relaxes (e.g., in diaspora academic contexts), suppression was largely structural; if they remain marginal even without enforcement, internalization is significant.',
    'If internalized, effective suppression exceeds the structural measure — the constraint carries its own reproduction mechanism. Raises effective χ for excluded seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of non-classical readings.').

omega_variable(
    abrogation_scope_boundary,
    'Does the classical reading''s abrogation mechanism apply only to legal-ritual rulings (ahkam) or also to theological-ethical verses (aqidah, akhlaq)?',
    'Survey of classical usul al-fiqh manuals: majority restrict naskh to ahkam; minority extend to all verses. The scope boundary determines the constraint''s spatial_scope and victim set breadth.',
    'If extended to theological verses, victim set expands to include theological coherence seekers and the constraint''s extraction footprint widens substantially (higher ε, broader spatial_scope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abrogation_scope_boundary, conceptual, 'Scope boundary of the abrogation mechanism: legal-only vs. comprehensive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 0, 750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naskh_classical_tr_t0, naskh_principle__classical_abrogation, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(naskh_classical_tr_t0, observed).
narrative_ontology:measurement(naskh_classical_tr_t150, naskh_principle__classical_abrogation, theater_ratio, 150, 0.18).
narrative_ontology:measurement_basis(naskh_classical_tr_t150, observed).
narrative_ontology:measurement(naskh_classical_tr_t300, naskh_principle__classical_abrogation, theater_ratio, 300, 0.22).
narrative_ontology:measurement_basis(naskh_classical_tr_t300, observed).
narrative_ontology:measurement(naskh_classical_tr_t450, naskh_principle__classical_abrogation, theater_ratio, 450, 0.25).
narrative_ontology:measurement_basis(naskh_classical_tr_t450, observed).
narrative_ontology:measurement(naskh_classical_tr_t600, naskh_principle__classical_abrogation, theater_ratio, 600, 0.27).
narrative_ontology:measurement_basis(naskh_classical_tr_t600, observed).
narrative_ontology:measurement(naskh_classical_tr_t750, naskh_principle__classical_abrogation, theater_ratio, 750, 0.28).
narrative_ontology:measurement_basis(naskh_classical_tr_t750, observed).

% Extraction over time
narrative_ontology:measurement(naskh_classical_be_t0, naskh_principle__classical_abrogation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(naskh_classical_be_t0, observed).
narrative_ontology:measurement(naskh_classical_be_t150, naskh_principle__classical_abrogation, base_extractiveness, 150, 0.48).
narrative_ontology:measurement_basis(naskh_classical_be_t150, observed).
narrative_ontology:measurement(naskh_classical_be_t300, naskh_principle__classical_abrogation, base_extractiveness, 300, 0.55).
narrative_ontology:measurement_basis(naskh_classical_be_t300, observed).
narrative_ontology:measurement(naskh_classical_be_t450, naskh_principle__classical_abrogation, base_extractiveness, 450, 0.59).
narrative_ontology:measurement_basis(naskh_classical_be_t450, observed).
narrative_ontology:measurement(naskh_classical_be_t600, naskh_principle__classical_abrogation, base_extractiveness, 600, 0.61).
narrative_ontology:measurement_basis(naskh_classical_be_t600, observed).
narrative_ontology:measurement(naskh_classical_be_t750, naskh_principle__classical_abrogation, base_extractiveness, 750, 0.62).
narrative_ontology:measurement_basis(naskh_classical_be_t750, observed).

% Suppression requirement over time
narrative_ontology:measurement(naskh_classical_su_t0, naskh_principle__classical_abrogation, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(naskh_classical_su_t0, observed).
narrative_ontology:measurement(naskh_classical_su_t150, naskh_principle__classical_abrogation, suppression_requirement, 150, 0.55).
narrative_ontology:measurement_basis(naskh_classical_su_t150, observed).
narrative_ontology:measurement(naskh_classical_su_t300, naskh_principle__classical_abrogation, suppression_requirement, 300, 0.62).
narrative_ontology:measurement_basis(naskh_classical_su_t300, observed).
narrative_ontology:measurement(naskh_classical_su_t450, naskh_principle__classical_abrogation, suppression_requirement, 450, 0.67).
narrative_ontology:measurement_basis(naskh_classical_su_t450, observed).
narrative_ontology:measurement(naskh_classical_su_t600, naskh_principle__classical_abrogation, suppression_requirement, 600, 0.7).
narrative_ontology:measurement_basis(naskh_classical_su_t600, observed).
narrative_ontology:measurement(naskh_classical_su_t750, naskh_principle__classical_abrogation, suppression_requirement, 750, 0.71).
narrative_ontology:measurement_basis(naskh_classical_su_t750, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__classical_abrogation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(naskh_principle__classical_abrogation, 0.12).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__contextual_harmonization).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__progressive_restriction).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, classical_usul_al_fiqh_authority).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, sharia_codification_modern_state).

% DUAL FORMULATION NOTE:
% This constraint (classical_abrogation) and its two siblings form the naskh_principle constraint family. The classical reading claims the kernel's authority for a chronological supersession hierarchy; the siblings contest this reading's structural premises. The classical reading's ε=0.62 reflects its institutional capture of the interpretive space; the siblings would author substantially lower ε for their readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naskh_principle__classical_abrogation, institutional, 0.2).
constraint_indexing:directionality_override(naskh_principle__classical_abrogation, organized, 0.85).
constraint_indexing:directionality_override(naskh_principle__classical_abrogation, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
