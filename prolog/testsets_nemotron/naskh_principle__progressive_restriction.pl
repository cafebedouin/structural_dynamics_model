% ============================================================================
% CONSTRAINT STORY: naskh_principle__progressive_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__progressive_restriction, []).

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
 *   constraint_id: naskh_principle__progressive_restriction
 *   human_readable: Progressive Restriction Reading of Naskh (Quranic Abrogation)
 *   domain: Islamic Jurisprudence / Quranic Hermeneutics / Legal Theory
 *
 * SUMMARY:
 *   This constraint story models the 'progressive restriction' (tadarruj)
 *   reading of Quranic abrogation (naskh) as a distinct structural claim: the
 *   Quran's movement from permissive to restrictive rulings (e.g., on
 *   alcohol, warfare, inheritance, slavery) reflects a divine pedagogical
 *   strategy of gradual moral elevation, not textual invalidation. Earlier
 *   verses remain 'valid but suspended' — transitional accommodations to
 *   seventh-century Arabian capacities — while later verses express the final
 *   divine intent. This reading structurally benefits reformist juristic
 *   actors seeking to align Islamic law with contemporary human rights norms,
 *   while extracting compliance costs from traditionalist jurists and
 *   practitioners who rely on earlier permissive texts for current practice.
 *   The constraint is actively enforced through institutional fatwa
 *   hierarchies, academic gatekeeping, and state-appointed fiqh councils that
 *   marginalize classical literalist readings. Over the observed interval
 *   (roughly late 19th century to present), extractiveness has risen as the
 *   reading has been operationalized for modern legal reform, theater has
 *   increased as the pedagogical framing becomes ritualized in interfaith and
 *   human rights discourse, and suppression requirements have grown as
 *   traditionalist resistance institutionalizes.
 *
 * KEY AGENTS:
 *   - reformist_ulema: Primary beneficiary (institutional/moderate) — gains interpretive authority and modern relevance
 *   - progressive_legal_theorists: Beneficiary (organized/biographical) — builds scholarly careers on evolutionary readings
 *   - contemporary_fiqh_councils: Agenda setter (institutional/generational) — codifies the reading into positive law
 *   - classical_literalist_jurists: Primary victim (organized/biographical) — loses textual authority and institutional position
 *   - traditionalist_madhab_adherents: Victim (powerless/trapped) — bears compliance costs without exit from madhab loyalty
 *   - permissive_verse_reliant_practitioners: Victim (moderate/constrained) — e.g., those using earlier verses on slavery, alcohol, warfare for contemporary practice
 *   - anti_reform_polemicists: Excluded (powerful/trapped) — structural opposition with no voice in reformist institutions
 *   - analytical_observer: Observer (analytical/civilizational/universal) — sees full structural field
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.62).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.48).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.62).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, tangled_rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Progressive Restriction Reading of Naskh (Quranic Abrogation)").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "Islamic Jurisprudence / Quranic Hermeneutics / Legal Theory").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, '5434203b-26f8-42c9-be27-dc6421454690').
narrative_ontology:cs_kernel_codification('5434203b-26f8-42c9-be27-dc6421454690', fixed_text).
narrative_ontology:cs_authority_grounding('5434203b-26f8-42c9-be27-dc6421454690', lineage).
narrative_ontology:cs_interpretation_layer_present('5434203b-26f8-42c9-be27-dc6421454690').
narrative_ontology:cs_reading_relation('5434203b-26f8-42c9-be27-dc6421454690', naskh_principle__classical_abrogation, coexists_with).
narrative_ontology:cs_reading_relation('5434203b-26f8-42c9-be27-dc6421454690', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_axiom('5434203b-26f8-42c9-be27-dc6421454690', foundational, divine_pedagogy_gradual_elevation).
narrative_ontology:cs_axiom_status(divine_pedagogy_gradual_elevation, holdable).
narrative_ontology:cs_axiom_grounding('5434203b-26f8-42c9-be27-dc6421454690', divine_pedagogy_gradual_elevation, theological).
narrative_ontology:cs_axiom('5434203b-26f8-42c9-be27-dc6421454690', foundational, earlier_verses_valid_but_suspended).
narrative_ontology:cs_axiom_status(earlier_verses_valid_but_suspended, holdable).
narrative_ontology:cs_axiom_grounding('5434203b-26f8-42c9-be27-dc6421454690', earlier_verses_valid_but_suspended, theological).
narrative_ontology:cs_reference_frame('5434203b-26f8-42c9-be27-dc6421454690', classical_abrogation_framework).
narrative_ontology:cs_drift_state('5434203b-26f8-42c9-be27-dc6421454690', post_colonial_modernity_encounter, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5434203b-26f8-42c9-be27-dc6421454690', '').
narrative_ontology:cs_kernel_id(naskh_principle__progressive_restriction, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, reformist_ulema).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, progressive_legal_theorists).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, contemporary_fiqh_councils).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, interfaith_dialogue_practitioners).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, classical_literalist_jurists).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, traditionalist_madhab_adherents).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, permissive_verse_reliant_practitioners).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, anti_reform_polemicists).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, divine_pedagogy_doctrine).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, evolutionary_sharia_concept).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, contextual_revelation_principle).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, legal_moral_development_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain interpretive authority and institutional relevance by championing the progressive restriction reading. They control key fiqh councils, academic positions, and state advisory roles. Their exit options include moving to secular legal academia or interfaith organizations where the reading's capital transfers. They collect the reading's gains in legitimacy and resources.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, reformist_ulema, beneficiary,
    institutional, generational, mobile, global).

% Build scholarly careers, publication records, and grant funding on evolutionary Quranic hermeneutics. They operate in university centers, think tanks, and international networks. Their exit is high — they can pivot to comparative law, human rights advocacy, or secular theory with minimal loss. They benefit from the reading's academic currency without bearing its traditionalist enforcement costs.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, progressive_legal_theorists, beneficiary,
    organized, biographical, arbitrage, global).

% Codify the progressive restriction reading into positive law (family law codes, banking regulations, criminal reforms). They are state-appointed bodies with enforcement power via fatwa authority and legislative influence. Their exit is constrained by institutional mandate and state dependence. They administer the constraint and capture its regulatory rents.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, contemporary_fiqh_councils, agenda_setter,
    institutional, generational, constrained, national).

% Lose textual authority, madhab coherence, and institutional position when the progressive restriction reading is imposed. Their professional identity is fused to the classical abrogation framework — exit means abandoning their scholarly self-concept and community recognition. They bear the transition cost without consent.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, classical_literalist_jurists, payer,
    organized, biographical, identity_locked, global).

% Lay followers of traditional madhabs who inherit the compliance costs of reformist rulings (e.g., new family law codes that restrict polygamy, modify inheritance, ban corporal punishment) without scholarly capacity to contest them. Their exit is trapped by community, family, and geographic embeddedness. They pay the diffuse cost of a transition they did not choose.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, traditionalist_madhab_adherents, payer,
    powerless, biographical, trapped, regional).

% Practitioners who cite earlier permissive verses for contemporary practice — e.g., those using Quranic permission for defensive warfare, gradual alcohol prohibition, or regulated slavery as precedents for modern contexts. They face fatwa rejection, social censure, and legal penalty. Exit requires abandoning their interpretive method or migrating to traditionalist jurisdictions.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, permissive_verse_reliant_practitioners, payer,
    moderate, immediate, constrained, local).

% Activists and media figures who attack the progressive restriction reading as heretical innovation. They have audiences and resources but are structurally excluded from fiqh councils, academic journals, and state advisory bodies. Their exclusion is the enforcement object — the constraint's legitimacy depends on their marginalization.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, anti_reform_polemicists, excluded,
    powerful, biographical, trapped, global).

% Sees the full structural field: the coordination function (Quranic ethics meeting modernity), the extraction asymmetry (traditionalists pay, reformists collect), the enforcement machinery (fiqh councils, fatwa hierarchies, state patronage), and the identity-lock dynamics that prevent traditionalist exit. No stake in the outcome.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies the Quran's ethical trajectory with modern legal and human rights demands by framing earlier permissive rulings as transitional divine pedagogy rather than permanent law, allowing jurists to maintain textual fidelity while evolving substantive rulings.
% TRANSFER_FUNCTION: Moves interpretive authority, institutional legitimacy, and regulatory control from classical literalist jurists and traditional madhab structures to reformist ulema, progressive legal theorists, and state-appointed fiqh councils. The transfer is legitimated through the pedagogical narrative.
% ABSENT_VOICES: Traditionalist lay communities in non-state spheres (e.g., rural scholars, madrasa networks outside state control, diaspora communities maintaining classical practice) would object to the characterization of their lived madhab as 'transitional' but are not represented in fiqh councils, academic discourse, or interfaith forums where this reading is authorized.
% DISAPPEARANCE_RATIONALE: If the progressive restriction reading vanished overnight, reformist fiqh councils would lose their primary hermeneutic for modern legislation, traditionalist jurists would regain interpretive dominance in state institutions, family law codes in multiple countries would face immediate revision pressure, and the interfaith/human rights discourse on 'Islamic compatibility' would lose its main Quranic warrant. The world of Islamic legal authority would rearrange fundamentally.
% FOUNDING_PROBLEM: The crisis of Quranic authority under colonial modernity: how to maintain the Quran as binding divine law while accommodating nation-state legal systems, abolition of slavery, gender equality norms, and religious freedom — without conceding that the text contains obsolete or immoral rulings.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by non-beneficiary sources: colonial-era fatwa records (e.g., Ottoman Shaykh al-Islam responses to Tanzimat), early reformist writings (Muhammad Abduh, Rashid Rida) explicitly framing the problem as 'saving the Quran from obsolescence,' and contemporary traditionalist jurists who agree the problem exists but reject this reading's solution. No single party owns the problem statement.
narrative_ontology:disappearance_verdict(naskh_principle__progressive_restriction, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__progressive_restriction, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__progressive_restriction, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(naskh_principle__progressive_restriction, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__progressive_restriction, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__progressive_restriction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__progressive_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__progressive_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62) reflects that the reading extracts significant compliance costs from traditionalist actors while delivering concentrated interpretive authority to reformist elites. Suppression (0.48) is moderate: enforcement operates through scholarly consensus formation and institutional gatekeeping rather than direct coercion, but the cost of dissent is professional marginalization. Theater ratio (0.38) captures the growing gap between the reading's active juristic work (genuine coordination of Quranic ethics with modern legal systems) and its performative deployment in identity-politics and interfaith contexts. Accessibility collapse (0.42) is partial: alternative readings (classical abrogation, contextual harmonization) remain live and institutionally supported in major madhabs. Resistance (0.55) is substantial: traditionalist networks maintain parallel scholarly ecosystems and state patronage in several jurisdictions. The claimed type (tangled_rope) reflects genuine coordination (unifying revelation with moral progress) coupled with asymmetric extraction (traditionalists pay the transition cost).
 *
 * PERSPECTIVAL GAP:
 *   The reformist ulema seat experiences this as a rope (genuine coordination solving the problem of Quranic relevance); the classical literalist seat experiences it as a snare (their textual authority is extracted without consent); the traditionalist lay adherent seat experiences it as a tangled rope (they value the madhab's coherence but bear unchosen costs); the analytical observer sees the full structural asymmetry. The engine computes this divergence from the declared power/exit/beneficiary structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (reformist_ulema, progressive_legal_theorists, contemporary_fiqh_councils) collect interpretive authority, institutional resources, and modern legitimacy — directionality d near 0.1-0.2. Victims (classical_literalist_jurists, traditionalist_madhab_adherents, permissive_verse_reliant_practitioners) bear costs of retraining, loss of textual warrant, and institutional marginalization — directionality d near 0.7-0.9. Excluded anti-reform polemicists are structurally locked out (d=1.0) — their exclusion is the enforcement mechanism. Interfaith practitioners sit near symmetric (d~0.5): genuine coordination benefit, diffuse cost. The progressive restriction reading's pedagogical framing reduces effective extraction for beneficiaries (they experience it as subsidy) and amplifies it for victims (they experience it as imposed transition).
 *
 * MANDATROPHY ANALYSIS:
 *   The reading was founded to solve the coordination problem of maintaining Quranic authority while adapting Islamic law to modernity (colonial encounter, nation-state formation, human rights regime). That founding problem remains LIVE — the tension between textual fixity and moral evolution persists. However, the reading's coordination function shows mandatrophy signals: theater ratio rising from 0.12 to 0.38 suggests the pedagogical frame increasingly serves identity-signaling rather than active juristic problem-solving. The constraint persists not because the coordination problem is solved, but because the reformist institutional ecosystem now depends on the reading for its legitimacy — a classic piton drift pattern. The omega on mandatrophy_boundary captures this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this reading a distinct constraint with its own ε, or a hermeneutical frame applied to the same kernel as classical_abrogation?',
    'Compare effective extraction profiles across seats: if classical_abrogation and progressive_restriction produce materially different χ for the same agents, they are distinct constraints; if χ profiles align, they are the same constraint viewed from different angles.',
    'If distinct constraints, the ε-invariance principle requires separate stories with independent ε; if same constraint, the reading frame belongs in commentary only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the progressive restriction reading instantiates a structurally distinct constraint from classical abrogation').

omega_variable(
    beneficiary_structure_contestation,
    'Do reformist ulema and contemporary fiqh councils genuinely benefit from this reading, or do they primarily use it strategically while bearing hidden costs?',
    'Track institutional outcomes over time: do progressive legal rulings based on this reading lead to sustained authority gains and resource flows for the endorsing bodies, or do they trigger backlash that negates benefits?',
    'If benefits are illusory or offset by backlash costs, the declared beneficiaries may be misidentified; the constraint could be a snare for its apparent beneficiaries or a piton for the tradition overall.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_structure_contestation, empirical, 'Whether the declared beneficiaries actually capture net gains from this reading''s operation').

omega_variable(
    suppression_mechanism,
    'Is the suppression of earlier permissive verse applications structural (institutional exclusion, fatwa control) or internalized (scholarly self-censorship, legitimacy anxiety)?',
    'Post-reform trajectory analysis: if scholars who cite permissive verses face formal sanction vs. informal marginalization; whether the constraint persists without active enforcement.',
    'If internalized, effective suppression is higher than structural measures suggest; if structural, suppression requires continuous institutional effort and may decay under resource pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism for practitioners of earlier permissive readings').

omega_variable(
    mandatrophy_boundary,
    'Does this reading resolve a genuine coordination problem (unifying Quranic ethics with modern legal demands) or has it outlived its transitional function?',
    'Assess whether the ''progressive restriction'' frame still enables new legal solutions or has become a ritualized citation pattern divorced from active juristic work.',
    'If coordination function is live, the reading is a rope/tangled_rope; if atrophied, it trends toward piton; if purely extractive for reformist legitimacy, snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_boundary, preference, 'Whether the progressive restriction reading retains active coordination function or has become theatrical maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naskh_prog_restr_tr_t0, naskh_principle__progressive_restriction, theater_ratio, 0, 0.12).
narrative_ontology:measurement(naskh_prog_restr_tr_t25, naskh_principle__progressive_restriction, theater_ratio, 25, 0.18).
narrative_ontology:measurement(naskh_prog_restr_tr_t50, naskh_principle__progressive_restriction, theater_ratio, 50, 0.25).
narrative_ontology:measurement(naskh_prog_restr_tr_t75, naskh_principle__progressive_restriction, theater_ratio, 75, 0.32).
narrative_ontology:measurement(naskh_prog_restr_tr_t100, naskh_principle__progressive_restriction, theater_ratio, 100, 0.38).

% Extraction over time
narrative_ontology:measurement(naskh_prog_restr_be_t0, naskh_principle__progressive_restriction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(naskh_prog_restr_be_t25, naskh_principle__progressive_restriction, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(naskh_prog_restr_be_t50, naskh_principle__progressive_restriction, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(naskh_prog_restr_be_t75, naskh_principle__progressive_restriction, base_extractiveness, 75, 0.55).
narrative_ontology:measurement(naskh_prog_restr_be_t100, naskh_principle__progressive_restriction, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(naskh_prog_restr_su_t0, naskh_principle__progressive_restriction, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(naskh_prog_restr_su_t25, naskh_principle__progressive_restriction, suppression_requirement, 25, 0.32).
narrative_ontology:measurement(naskh_prog_restr_su_t50, naskh_principle__progressive_restriction, suppression_requirement, 50, 0.38).
narrative_ontology:measurement(naskh_prog_restr_su_t75, naskh_principle__progressive_restriction, suppression_requirement, 75, 0.44).
narrative_ontology:measurement(naskh_prog_restr_su_t100, naskh_principle__progressive_restriction, suppression_requirement, 100, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__progressive_restriction, identity_coordination).
narrative_ontology:boltzmann_floor_override(naskh_principle__progressive_restriction, 0.08).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, contextual_harmonization).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, quranic_ethics_modernity_interface).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, fiqh_council_authority_structure).

% DUAL FORMULATION NOTE:
% Part of the naskh_principle constraint family. This reading (progressive_restriction) differs from classical_abrogation in ε (0.62 vs ~0.35) because classical abrogation treats earlier verses as legally null (lower extraction from traditionalists who already accept supersession) while progressive restriction treats them as valid-but-suspended (higher extraction from those who cite them for practice). It differs from contextual_harmonization in suppression (0.48 vs ~0.15) because harmonization requires no enforcement — all readings coexist. The three stories share the kernel but instantiate distinct constraints with independent ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naskh_principle__progressive_restriction, institutional, 0.15).
constraint_indexing:directionality_override(naskh_principle__progressive_restriction, organized, 0.75).
constraint_indexing:directionality_override(naskh_principle__progressive_restriction, powerless, 0.85).
constraint_indexing:directionality_override(naskh_principle__progressive_restriction, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
