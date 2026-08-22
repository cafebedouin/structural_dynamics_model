% ============================================================================
% CONSTRAINT STORY: latin_correctness__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__rupture_reading, []).

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
 *   constraint_id: latin_correctness__rupture_reading
 *   human_readable: Classical Latin Purity Standard (Rupture Reading)
 *   domain: intellectual_history/philology
 *
 * SUMMARY:
 *   The classical Latin purity standard, in the rupture reading, asserts that
 *   'correct' Latin is the Latin of classical antiquity as reconstructed from
 *   ancient texts, and that medieval usage represents linguistic corruption
 *   requiring correction. This reading emerged during the Renaissance as
 *   humanist scholars recovered classical texts and positioned themselves as
 *   authorities on authentic Latin. The constraint enforces this standard
 *   through pedagogy, textual criticism, manuscript acquisition, and
 *   delegitimization of medieval practice. The rupture reading positions
 *   medieval scholars as victims of a standard that retroactively invalidates
 *   their intellectual heritage. This is ONE reading of a contested kernel
 *   (the status of Latin across its history); sibling readings
 *   (continuity_reading, hybrid_reading) offer alternative frameworks where
 *   medieval Latin is adaptive evolution, not corruption, or where different
 *   standards apply to different domains.
 *
 * KEY AGENTS:
 *   - humanist_elite: Renaissance scholars and philologists who control textual authority; beneficiaries of the classical purity standard
 *   - medieval_scholars: whose intellectual works are now positioned as evidence of linguistic corruption; victims
 *   - technical_domain_practitioners: physicians, lawyers, theologians requiring medieval forms for functional terminology; victims/constrained beneficiaries
 *   - vernacular_adjacent_writers: authors bridging Latin and emerging national languages; partially mobile victims
 *   - ecclesiastical_authority: the Church as preserver of Latin and enforcer of pedagogical standards; ambivalent agenda-setter
 *   - hybrid/continuity advocates: excluded from authority-setting; would redistribute the standard if admitted
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__rupture_reading, 0.78).
domain_priors:suppression_score(latin_correctness__rupture_reading, 0.71).
domain_priors:theater_ratio(latin_correctness__rupture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__rupture_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__rupture_reading, "Classical Latin Purity Standard (Rupture Reading)").
narrative_ontology:topic_domain(latin_correctness__rupture_reading, "intellectual_history/philology").

domain_priors:requires_active_enforcement(latin_correctness__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__rupture_reading, '108a080a-9b53-4954-a29b-3c23293e8045').
narrative_ontology:cs_kernel_codification('108a080a-9b53-4954-a29b-3c23293e8045', fixed_text).
narrative_ontology:cs_authority_grounding('108a080a-9b53-4954-a29b-3c23293e8045', extraction).
narrative_ontology:cs_interpretation_layer_present('108a080a-9b53-4954-a29b-3c23293e8045').
narrative_ontology:cs_reading_relation('108a080a-9b53-4954-a29b-3c23293e8045', latin_correctness__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('108a080a-9b53-4954-a29b-3c23293e8045', latin_correctness__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('108a080a-9b53-4954-a29b-3c23293e8045', foundational, classical_latin_is_fixed_standard).
narrative_ontology:cs_axiom_status(classical_latin_is_fixed_standard, holdable).
narrative_ontology:cs_axiom_grounding('108a080a-9b53-4954-a29b-3c23293e8045', classical_latin_is_fixed_standard, conventional).
narrative_ontology:cs_axiom('108a080a-9b53-4954-a29b-3c23293e8045', foundational, medieval_latin_is_corruption_not_evolution).
narrative_ontology:cs_axiom_status(medieval_latin_is_corruption_not_evolution, holdable).
narrative_ontology:cs_axiom_grounding('108a080a-9b53-4954-a29b-3c23293e8045', medieval_latin_is_corruption_not_evolution, empirically_contingent).
narrative_ontology:cs_reference_frame('108a080a-9b53-4954-a29b-3c23293e8045', classical_latin_purity).
narrative_ontology:cs_drift_state('108a080a-9b53-4954-a29b-3c23293e8045', modern_linguistic_science, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('108a080a-9b53-4954-a29b-3c23293e8045', '').
narrative_ontology:cs_kernel_id(latin_correctness__rupture_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, humanist_elite).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, classical_philology_establishment).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, medieval_scholars).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, technical_domain_practitioners).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, vernacular_adjacent_writers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, technical_domain_practitioners).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, manuscript_preservers).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, ecclesiastical_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Renaissance humanists and classical philologists who control textual authority and pedagogical gatekeeping. They set the standard that 'correct' Latin is classical Latin, requiring reconstruction from ancient sources. They commission manuscript hunts, publish critical editions, and train the next generation of scholars to judge medieval usage as degradation. Their prestige and institutional position depend on classical Latin remaining the unmarked standard against which all other Latin is measured as deviation.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, humanist_elite, agenda_setter,
    institutional, generational, arbitrage, continental).

% Medieval scholars whose intellectual heritage and written works are delegitimized by the rupture reading. They are positioned as having 'corrupted' a language they actually stewarded through centuries of use. Their texts are now read as evidence of linguistic failure rather than adaptation. Escape would require either abandoning their own tradition or challenging the authority structure itself—both identity-threatening moves. They carry the suppression internalized: a medieval scholar who internalizes the humanist judgment experiences their own work as linguistically inadequate.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, medieval_scholars, payer,
    moderate, biographical, identity_locked, continental).

% Medical, legal, theological, and scientific practitioners who use Latin as their technical lingua franca. Medieval Latin forms and coinages are their working vocabulary—they need words for new concepts (diseases, legal procedures, theological distinctions) that classical Latin lacks. The rupture standard forces them to either conform to classical purity (crippling technical clarity) or accept being delegitimized as non-standard speakers. They gain some benefit from Latin's remaining unified across domains, but pay a steep cost in functional constraint.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, technical_domain_practitioners, payer,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__rupture_reading, technical_domain_practitioners, beneficiary).

% Authors transitioning between Latin and emerging vernacular languages (Italian, French, Spanish, English). Medieval Latin's flexibility and vernacular-proximity made it a usable bridge language. The rupture standard's insistence on classical purity makes medieval Latin less useful for code-switching and technical writing; it pushes writers toward exclusive vernacular use or toward classical Latin (which lacks vernacular-adjacent vocabulary). They have the most mobility: if excluded from classical Latin's prestige, they can shift entirely to vernacular, which some eventually do.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, vernacular_adjacent_writers, payer,
    moderate, biographical, mobile, continental).

% Monastic and cathedral scriptoria, libraries, and early-modern collectors who preserve both classical and medieval texts. The humanist project increases demand for classical manuscripts (commissioning hunts, funding acquisitions), which can enhance the libraries' prestige and resources. However, the delegitimization of medieval texts can reduce the perceived value of their medieval collections. They benefit selectively from the standard's enforcement.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, manuscript_preservers, beneficiary,
    moderate, biographical, constrained, continental).

% The Catholic Church, as the preserver of Latin and the institutional framework for clerical Latin education. The rupture standard serves the Church's interest in standardizing and controlling ecclesiastical Latin, but also constrains it: medieval liturgical and theological Latin is now positioned as corrupted, potentially undermining the authority of medieval theological traditions. The Church's relation to the standard is ambivalent—it enforces classical norms pedagogically while defending medieval texts doctrinally.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, ecclesiastical_authority, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__rupture_reading, ecclesiastical_authority, beneficiary).

% Scholars and practitioners who would prefer a hybrid or continuity reading—those who argue medieval Latin is a legitimate evolution or that different domains should have different standards. They are excluded from authority-setting roles and their arguments are dismissed as uncritical or untrained. If admitted, they would propose opening the standard to medieval forms and establishing domain-specific norms, which would redistribute authority and prestige.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, textual_competitors, excluded,
    moderate, biographical, trapped, continental).

% Historical linguists, comparative philologists, and intellectual historians who can step outside the dispute and observe its structural properties—how the rupture reading concentrates authority, what alternatives are foreclosed, how the enforcement mechanism operates, and what temporal costs accumulate.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__rupture_reading, humanist_elite).
narrative_ontology:fixing_cost_class(latin_correctness__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified textual standard for Latin across Christendom and the scholarly world: a fixed referent (classical texts) against which all Latin usage can be measured, judged, and taught. Enables a single curriculum and a coherent intellectual tradition traceable to ancient authority.
% TRANSFER_FUNCTION: Transfers authority, prestige, and interpretive power from medieval scholars and practitioners to the humanist elite and classical philology establishment. Medieval scholars' intellectual capital is revalued downward; humanist scholars' expertise in 'correct' reconstruction and textual criticism is revalued upward. Resources (manuscripts, patronage, pedagogical authority) flow toward classical philology.
% ABSENT_VOICES: Medieval scholars themselves, now positioned as evidence of corruption rather than as parties to the conversation. Practitioners in technical domains (medicine, law, theology) whose actual working vocabulary is delegitimized. Hybrid-standard advocates and continuity-reading defenders are excluded from authority-setting and dismissed as untrained. If these voices were present and heard, they would challenge the presupposition that classical purity is the only legitimate standard.
% DISAPPEARANCE_RATIONALE: If the rupture reading and its enforcement apparatus vanished overnight, medieval Latin texts would regain prestige and interpretive credibility; technical domains would reinstate medieval coinages and forms without shame; multiple standards (domain-specific, evolutionary, hybrid) would coexist without hierarchical judgment. The unified classical standard would dissolve into pluralism, and the authority currently concentrated in the humanist elite would disperse.
% FOUNDING_PROBLEM: The rediscovery of classical texts in the late medieval/early Renaissance period created a perception that contemporary (medieval) Latin usage had drifted from the standard set by Cicero, Virgil, and other canonical authors. The problem was framed as: how do we recover the purity of ancient usage and maintain it as the authoritative standard?
% FOUNDING_PROBLEM_CORROBORATION: Humanist scholars and classical philologists attest the problem is still live: contemporary Latin usage (and medieval usage in retrospect) is indeed corrupt and requires constant vigilance to maintain classical standards. However, medieval historians, linguists studying language change, and practitioners in technical domains attest the problem is reframed: medieval Latin was not corruption but adaptation; the 'drift' was functional evolution, not degradation. Linguistic science supports the continuity/evolution reading over the rupture reading.
narrative_ontology:disappearance_verdict(latin_correctness__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__rupture_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) and rising across the interval because the rupture reading concentrates interpretive authority and prestige in the humanist elite. Medieval scholars lose interpretive standing; their texts are read as corrupted rather than adapted. The constraint extracts intellectual authority from medieval tradition and transfers it to classical philology. Suppression is sustained (0.71) because it operates through pedagogical authority: the standard is taught as objective rather than chosen, medieval Latin is presented as failed latin rather than legitimate variant, and alternative standards are excluded from scholarly conversation. Theater is moderate (0.42): the actual enforcement work includes genuine scholarly labor (manuscript hunts, critical editions, training), but the labor increasingly defends the exclusion of medieval forms and practitioners rather than producing new knowledge. The measurement series tracks the Renaissance period through early modernity (0–500 years from initial contact with classical texts), showing extraction and suppression rising as the humanist standard consolidates institutional power.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist elite's seat, the constraint is genuine coordination: they have recovered objective classical standards and are transmitting them faithfully. From the medieval scholar's seat, the same constraint is extraction: their intellectual work is revalued downward, their tradition is pathologized, and they are offered no path to legitimacy within the new standard. From the technical practitioner's seat, the constraint is coercive: it demands purity that makes their work harder, forcing a choice between functional clarity and prestige. The engine computes these divergent types from the structural positions and power atoms; the authored metrics do not preempt that computation.
 *
 * DIRECTIONALITY LOGIC:
 *   The humanist elite sit at the beneficiary end of the directionality spectrum (d ≈ 0.1–0.2): they collect prestige, institutional authority, and interpretive power. Medieval scholars sit at the target end (d ≈ 0.85–0.95): their intellectual heritage is delegitimized, their status is downgraded, and they are identity-locked into a tradition now marked as corrupted. Technical practitioners sit near the symmetric point (d ≈ 0.5–0.6): they gain a unified standard but lose functional vocabulary. Ecclesiastical authority is conflicted (d ≈ 0.4): the Church benefits from standardized Latin pedagogy but is also constrained by classical purity norms applied to medieval theological texts. The engine should compute these divergences from the structural data—beneficiary concentration among humanists, victim concentration among medieval practitioners, and the identity-lock mechanism binding medieval scholars to a tradition they cannot exit without epistemic crisis.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is the Renaissance recovery of classical texts and the perception that medieval usage had drifted. At t=0, the founding problem is live: there is genuine concern about linguistic standardization and classical recovery. By t=500, the founding problem status becomes contested: classical texts are recovered, schools teach classical forms, but medieval scholars and linguistic science attest that the founding problem has been reframed. The constraint's justification (recover classical purity) is decoupled from its actual operation (maintain humanist authority and devalue medieval practice). The 'corruption' of medieval Latin is now recognized by linguists as systematic change, not random degradation. The founding_problem_status='contested' + disappearance_verdict='world_rearranges' signals mandatrophy: the arrangement persists (world would rearrange if it vanished) but its mandate (recover authentic classical Latin) is dead or defeated—the classical Latin has been recovered; the ongoing constraint now serves only to exclude medieval forms and maintain humanist prestige. This is a candidate for zombie-constraint classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_structural_rival,
    'Is this constraint a reading of the latin_correctness kernel, or is it itself the kernel?',
    'Historical-genealogical inquiry: trace which formulation (rupture, continuity, or hybrid) was prior in European intellectual history, which was reaction to which, and whether a pre-formulation commitment exists that all three readings interpret.',
    'If the rupture reading IS the kernel (the foundational fixed text), then continuity and hybrid readings are derived alternatives; if all three are readings of a deeper kernel (''what is the status of medieval Latin?''), then the rupture reading is not immune to being deposed by a competing reading that better fits empirical or normative evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_structural_rival, conceptual, 'Whether the rupture reading is a reading of a kernel or the kernel itself.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.71) structural (enforcement by authority) or internalized (medieval scholars self-suppressing through belief in the standard''s legitimacy)?',
    'Post-enforcement trajectory analysis: if suppression persists after institutional enforcement weakens, the internalization hypothesis is supported; if suppression decays with enforcement, it is primarily structural.',
    'If internalized, medieval scholars'' victims-position is reinforced by their own acceptance of the standard''s legitimacy—exit would require epistemic breaks, not just institutional change. If structural, exit becomes possible with institutional reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in the classical standard''s enforcement.').

omega_variable(
    extraction_vs_coordination_inseparability,
    'Is the coordination function (unified textual standard) structurally inseparable from the extraction function (humanist prestige/authority concentration)?',
    'Thought experiment: could a unified Latin standard exist without classical purity requirement? Could medieval forms be admitted to domain-specific legitimacy without dissolving standardization? Empirical test: hybrid regimes (domain-specific norms) in other standardization domains.',
    'If separable, the extraction is pure rent-seeking riding on genuine coordination; if inseparable, part of the measured extraction is the cost of unification itself. This affects whether a remedy would split the standard (losing coordination benefit) or redistribute authority (keeping coordination, changing beneficiaries).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_coordination_inseparability, conceptual, 'Whether the classical purity standard is structurally required for Latin unification or represents a particular choice of which purity to enforce.').

omega_variable(
    competing_reading_empirical_viability,
    'Do the continuity and hybrid readings have sufficient historical and linguistic evidence to support them as live alternatives, or are they defeated by the rupture reading''s evidence base?',
    'Comparative historical-linguistic evidence: manuscripts, attested usage patterns, internal linguistic change rates, domain-specific terminology development. If medieval texts show systematic innovation rather than random corruption, continuity hypothesis is supported.',
    'If sibling readings are empirically viable, the rupture reading is one reading among rivals, not an objective standard—its enforcement power derives from institutional authority, not from truth. If the rupture reading is empirically defeated, the constraint undergoes type reclassification or mandatrophy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_reading_empirical_viability, empirical, 'Whether sibling readings have empirical support sufficient to unseat the rupture reading''s authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__rupture_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t0, latin_correctness__rupture_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lati_tr_t50, latin_correctness__rupture_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(lati_tr_t100, latin_correctness__rupture_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement(lati_tr_t200, latin_correctness__rupture_reading, theater_ratio, 200, 0.36).
narrative_ontology:measurement(lati_tr_t350, latin_correctness__rupture_reading, theater_ratio, 350, 0.41).
narrative_ontology:measurement(lati_tr_t500, latin_correctness__rupture_reading, theater_ratio, 500, 0.42).

% Extraction over time
narrative_ontology:measurement(lati_be_t0, latin_correctness__rupture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lati_be_t50, latin_correctness__rupture_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(lati_be_t100, latin_correctness__rupture_reading, base_extractiveness, 100, 0.61).
narrative_ontology:measurement(lati_be_t200, latin_correctness__rupture_reading, base_extractiveness, 200, 0.72).
narrative_ontology:measurement(lati_be_t350, latin_correctness__rupture_reading, base_extractiveness, 350, 0.76).
narrative_ontology:measurement(lati_be_t500, latin_correctness__rupture_reading, base_extractiveness, 500, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t0, latin_correctness__rupture_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(lati_su_t50, latin_correctness__rupture_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement(lati_su_t100, latin_correctness__rupture_reading, suppression_requirement, 100, 0.59).
narrative_ontology:measurement(lati_su_t200, latin_correctness__rupture_reading, suppression_requirement, 200, 0.66).
narrative_ontology:measurement(lati_su_t350, latin_correctness__rupture_reading, suppression_requirement, 350, 0.69).
narrative_ontology:measurement(lati_su_t500, latin_correctness__rupture_reading, suppression_requirement, 500, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__rupture_reading, information_standard).
narrative_ontology:boltzmann_floor_override(latin_correctness__rupture_reading, 0.12).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% The latin_correctness kernel decomposes into three structurally distinct readings with different ε values, beneficiary/victim structures, and type classifications. The rupture_reading (this constraint) asserts classical purity as the standard; the continuity_reading asserts medieval evolution as legitimate; the hybrid_reading permits domain-specific standards. These are not the same constraint viewed from different angles—they have incompatible ε-referents (the standing arrangement under dispute is different for each reading). Each reading has its own narrative, stakeholders, and foundational axioms. They are linked via the kernel-contest structure: all three interpret the same kernel (the status of medieval Latin), but each reading yields a different constraint with different extractiveness, suppression, and classification. The rupture_reading is upstream in historical priority (Renaissance humanists formulated it first) and influences the other readings through institutional power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
