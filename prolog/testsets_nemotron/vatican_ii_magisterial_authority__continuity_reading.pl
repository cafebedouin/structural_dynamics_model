% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__continuity_reading
 *   human_readable: Vatican II Magisterial Authority — Continuity Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   The continuity reading of Vatican II asserts that the Council's sixteen
 *   documents, rightly interpreted, represent an organic development within
 *   the Church's unbroken tradition — no rupture with the prior magisterium,
 *   only deeper understanding. This reading was authoritatively proclaimed by
 *   Benedict XVI (2005 Christmas address) and codified in CDF documents
 *   (e.g., Dominus Iesus 2000, the 1985 Extraordinary Synod's final report).
 *   It functions as a constraint on all conciliar implementation: liturgical
 *   translations must preserve Latin normative texts (SC §36), religious
 *   freedom (DH) must be reconciled with the Syllabus of Errors via the
 *   thesis/hypothesis distinction or development of doctrine, and 'spirit of
 *   Vatican II' claims are declared unauthorized. The constraint is actively
 *   enforced through doctrinal reviews, appointment vetting, liturgical law
 *   (Summorum Pontificum / Traditionis Custodes), and canonical visitations.
 *   Its extraction falls on post-conciliar reformers and local churches; its
 *   beneficiaries are the Roman curia's interpretive office and
 *   traditionalist communities whose identity depends on the reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, 0.38).
domain_priors:suppression_score(vatican_ii_magisterial_authority__continuity_reading, 0.42).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__continuity_reading, "Vatican II Magisterial Authority — Continuity Reading").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__continuity_reading, '9496cca5-ad79-4de2-991b-0105f58fd916').
narrative_ontology:cs_kernel_codification('9496cca5-ad79-4de2-991b-0105f58fd916', fixed_text).
narrative_ontology:cs_authority_grounding('9496cca5-ad79-4de2-991b-0105f58fd916', lineage).
narrative_ontology:cs_interpretation_layer_present('9496cca5-ad79-4de2-991b-0105f58fd916').
narrative_ontology:cs_reading_relation('9496cca5-ad79-4de2-991b-0105f58fd916', vatican_ii_magisterial_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('9496cca5-ad79-4de2-991b-0105f58fd916', vatican_ii_magisterial_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('9496cca5-ad79-4de2-991b-0105f58fd916', foundational, conciliar_texts_constrain_implementation_to_preserve_preconciliar_doctrine).
narrative_ontology:cs_axiom_status(conciliar_texts_constrain_implementation_to_preserve_preconciliar_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('9496cca5-ad79-4de2-991b-0105f58fd916', conciliar_texts_constrain_implementation_to_preserve_preconciliar_doctrine, conventional).
narrative_ontology:cs_axiom('9496cca5-ad79-4de2-991b-0105f58fd916', foundational, organic_development_of_doctrine_excludes_rupture).
narrative_ontology:cs_axiom_status(organic_development_of_doctrine_excludes_rupture, holdable).
narrative_ontology:cs_axiom_grounding('9496cca5-ad79-4de2-991b-0105f58fd916', organic_development_of_doctrine_excludes_rupture, deontological).
narrative_ontology:cs_axiom('9496cca5-ad79-4de2-991b-0105f58fd916', secondary, latin_liturgy_preservation_mandate_binding).
narrative_ontology:cs_axiom_status(latin_liturgy_preservation_mandate_binding, holdable).
narrative_ontology:cs_axiom_grounding('9496cca5-ad79-4de2-991b-0105f58fd916', latin_liturgy_preservation_mandate_binding, conventional).
narrative_ontology:cs_axiom('9496cca5-ad79-4de2-991b-0105f58fd916', secondary, religious_freedom_reconcilable_with_syllabus_via_thesis_hypothesis).
narrative_ontology:cs_axiom_status(religious_freedom_reconcilable_with_syllabus_via_thesis_hypothesis, holdable).
narrative_ontology:cs_axiom_grounding('9496cca5-ad79-4de2-991b-0105f58fd916', religious_freedom_reconcilable_with_syllabus_via_thesis_hypothesis, conventional).
narrative_ontology:cs_reference_frame('9496cca5-ad79-4de2-991b-0105f58fd916', tridentine_magisterial_continuity).
narrative_ontology:cs_drift_state('9496cca5-ad79-4de2-991b-0105f58fd916', post_conciliar_reception_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9496cca5-ad79-4de2-991b-0105f58fd916', '2026-06-15T14:30:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, roman_curia_continuity_faction).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, traditionalist_laity).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, bishops_conferences_implementation_authors).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, post_conciliar_reform_practitioners).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, local_churches_cultural_adaptation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, bishops_conferences_implementation_authors).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, hermeneutic_of_continuity).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, organic_development_of_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, latin_liturgy_preservation_mandate).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, thesis_hypothesis_distinction_religious_freedom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the authoritative interpretation of conciliar texts through the Congregation for the Doctrine of the Faith and papal magisterium. Issues binding hermeneutical directives (e.g., 1985 Extraordinary Synod, Benedict XVI's 2005 Christmas address) that constrain how bishops implement Vatican II. Their professional identity and institutional authority are fused with the continuity thesis — abandoning it would dissolve the legitimacy of their interpretive office.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, roman_curia_continuity_faction, agenda_setter,
    institutional, generational, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, roman_curia_continuity_faction, beneficiary).

% Lay movements and communities (e.g., FSSP, ICKSP, traditionalist lay associations) who receive the continuity reading as protection for pre-conciliar liturgical and doctrinal forms. Their ecclesial identity is constituted through adherence to this reading; exit would require reconstituting their self-understanding as Catholics. They contribute materially and politically to sustain the reading's institutional enforcement.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, traditionalist_laity, beneficiary,
    organized, biographical, identity_locked, global).

% National and regional episcopal conferences that produce liturgical translations, catechetical directories, and pastoral plans. They benefit from a stable hermeneutical framework that legitimizes their implementation work and shields them from accusations of rupture. However, they bear the cost of enforcing Latin preservation (SC §36) and restricting inculturation where it conflicts with the continuity reading — a cost that falls disproportionately on churches in the Global South.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, bishops_conferences_implementation_authors, beneficiary,
    institutional, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, bishops_conferences_implementation_authors, payer).

% Theologians, liturgists, pastoral workers, and religious orders who implemented Vatican II in the 1965–1985 period under a 'spirit of the Council' hermeneutic. They bear the extraction of having their work retrospectively delegitimized, their formation investments devalued, and their ecclesial standing questioned. Exit options are constrained: leaving means abandoning a vocation; staying means accepting a hermeneutic that declares their life's work unauthorized.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, post_conciliar_reform_practitioners, payer,
    moderate, biographical, constrained, global).

% Particular churches in Africa, Asia, Oceania, and Latin America whose inculturation efforts (liturgical adaptation, indigenous leadership forms, contextual theology) are constrained by the continuity reading's insistence on textual fidelity to Latin originals and Roman normative practice. They are trapped because the universal magisterium's interpretive authority is structurally binding, and local exit (schism) is ecclesially unthinkable. The extraction is diffuse but structural: their cultural and spiritual resources are filtered through a Eurocentric hermeneutic they did not author.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, local_churches_cultural_adaptation, payer,
    moderate, generational, trapped, regional).

% Theologians (e.g., Schüssler Fiorenza, Haight, parts of the Concilium school), progressive laity, and some episcopal conferences who hold that Vatican II constituted a genuine rupture. They are excluded from the authoritative interpretive circle — their publications face doctrinal review, their appointments are blocked, their voices are absent from official hermeneutical documents. Their identity is fused with the rupture claim; they cannot 'moderate' into the continuity reading without abandoning their core theological conviction.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, rupture_reading_adherents, excluded,
    organized, biographical, identity_locked, global).

% Church historians, patristic scholars, and historians of doctrine who study the conciliar texts and their reception without institutional stake in the hermeneutical dispute. They observe the constraint's operation from outside the magisterial enforcement structure.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, historical_theological_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__continuity_reading, roman_curia_continuity_faction).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified hermeneutical key that allows the magisterium to authorize conciliar implementation without fragmentation — a single authoritative reading that binds 1.3 billion Catholics across 60 years, preventing schism and doctrinal drift.
% TRANSFER_FUNCTION: Moves interpretive authority and liturgical/doctrinal control from local churches and post-conciliar reform practitioners to the Roman center. The extraction is authority and cultural autonomy; the gain is centralized doctrinal coherence and institutional continuity.
% ABSENT_VOICES: The rupture_reading_adherents (theologians and communities who experienced Vatican II as a genuine break) and local_churches_cultural_adaptation (whose inculturation is constrained by Roman textual fidelity) are structurally excluded from the authoritative interpretive process. Their objection would challenge the continuity thesis itself, which the constraint exists to protect.
% DISAPPEARANCE_RATIONALE: If the continuity reading lost its magisterial enforcement overnight, episcopal conferences would immediately diversify liturgical translations, inculturation would accelerate, the traditionalist movements would lose their canonical protection, and the theological landscape would fragment into competing hermeneutics — the unified magisterial authority that the constraint sustains would dissolve.
% FOUNDING_PROBLEM: The Council (1962–1965) produced texts with ambiguous formulations (e.g., DH on religious freedom, SC on liturgy, LG on collegiality) that could be read as rupture. The post-conciliar chaos (liturgical experimentation, doctrinal dissent, mass defections 1965–1985) created an existential threat to magisterial credibility and Catholic unity. The continuity reading was constructed as the authoritative key to bind implementation to tradition.
% FOUNDING_PROBLEM_CORROBORATION: The continuity faction (Ratzinger/Benedict XVI, CDF documents, 1985 Synod) attests the founding problem remains live — secularism and relativism make hermeneutical stability more urgent. Post-conciliar reform practitioners (O'Malley, Alberigo, Komonchak, many episcopal conferences) attest the founding problem was the Council's own ambiguity, now resolved by time and reception; the continuity reading is a retroactive imposition. No neutral third party corroborates either account — the dispute is the field.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__continuity_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__continuity_reading_tests).
:- end_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects that the constraint does not primarily extract material resources but interpretive authority and cultural autonomy — a substantial but not total extraction. Suppression (0.42) is moderate: the constraint relies on magisterial authority and canonical penalties rather than physical coercion, but the penalties (loss of faculty, canonical sanction, ecclesial marginalization) are real within the system. Theater ratio (0.28) reflects genuine coordination function (unified hermeneutic preventing fragmentation) alongside performative maintenance (repeated re-articulation of the continuity thesis against evidence of textual ambiguity). Accessibility collapse (0.68) is high: within the Catholic system, the magisterial interpretation is structurally binding; alternatives exist only as dissent. Resistance (0.35) is significant but contained: the rupture reading persists in theology faculties and some episcopal conferences but has no magisterial purchase.
 *
 * PERSPECTIVAL GAP:
 *   From the Roman curia / agenda_setter seat, the constraint is genuine coordination (rope-like): it solves the real problem of hermeneutical fragmentation. From the post_conciliar_reform_practitioners / payer seat, it operates as extraction: their life's work is delegitimized by a reading they had no hand in authoring. From the local_churches_cultural_adaptation / payer seat, it is structural suppression: their inculturation is constrained by a Eurocentric textual fidelity they did not choose. The engine computes these divergences from the declared power/exit/role structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The Roman curia and traditionalist laity are structural beneficiaries (d near 0.0–0.2): they collect interpretive authority and identity-protection. Bishops conferences are mixed (d ~0.4): they gain legitimacy but bear enforcement costs. Post-conciliar reformers and local churches are targets (d ~0.7–0.9): they bear the authority transfer and cultural constraint. Rupture adherents are excluded (identity_locked, trapped in dissent). The observer sits at analytical (d=0.5). The continuity reading's core move — declaring the Council's ambiguous texts univocally continuous — is what creates the extraction: it converts textual ambiguity into centralized authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-conciliar chaos threatening unity) was real and live in 1985. Whether it remains live is contested. The continuity reading's persistence after the chaos subsided (post-2000) suggests mandatrophy: the constraint's coordination function (preventing schism) has attenuated, but its extraction function (centralizing authority, constraining inculturation) persists. The theater ratio rise (0.05→0.28) tracks this: more enforcement activity defends the reading's authority than solves hermeneutical fragmentation. The constraint is a tangled_rope because it genuinely coordinates (prevents doctrinal anarchy) AND extracts (transfers authority to Rome, constrains local churches).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_construction_ambiguity,
    'Is the continuity reading a genuine discovery of the Council''s intrinsic meaning, or a construction imposed by the magisterium to solve a governance crisis?',
    'Historical analysis of the 1985 Extraordinary Synod''s preparatory documents and the CDF''s internal deliberations 1985–2005; comparison with the Council''s own reception history 1965–1985.',
    'If constructed, the constraint is a snare disguised as a mountain (false summit candidate) — the ''unbroken tradition'' claim masks a power play. If discovered, the extraction is the price of genuine coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(continuity_vs_construction_ambiguity, conceptual, 'Whether the continuity thesis is descriptive (hermeneutical fact) or prescriptive (institutional imposition).').

omega_variable(
    thesis_hypothesis_coherence,
    'Does the thesis/hypothesis distinction (used to reconcile DH with the Syllabus) hold as a coherent theological principle, or is it an ad hoc device to preserve the continuity claim?',
    'Systematic theological evaluation: does the distinction operate consistently across other doctrinal domains (e.g., usury, slavery, death penalty), or only where continuity is threatened?',
    'If ad hoc, the continuity reading''s claim to organic development is undermined — the constraint''s coordination function rests on a fragile theological device. If coherent, the reading has deeper theological grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thesis_hypothesis_coherence, conceptual, 'Internal coherence of the continuity reading''s key hermeneutical tool.').

omega_variable(
    local_church_reception_trajectory,
    'Will the growing demographic weight of African and Asian churches (where inculturation demands are strongest) force a magisterial revision of the continuity reading, or will enforcement intensify?',
    'Demographic projection + analysis of recent synodal processes (2021–2024 Synod on Synodality) for signs of hermeneutical flexibility.',
    'If revision occurs, the constraint transitions toward scaffold or rope. If enforcement intensifies, it hardens toward snare. The trajectory determines the constraint''s lifecycle classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_church_reception_trajectory, empirical, 'Future structural pressure on the constraint from demographic shift.').

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural relationship to the kernel vatican_ii_magisterial_authority differ from its siblings?',
    'Structural comparison of the three readings'' beneficiary/victim sets, extraction profiles, and enforcement mechanisms.',
    'Documents the committer-frame logic: this reading forecloses rupture_reading, coexists_with composite_overdetermination_reading, and influences it by privileging continuity-compatible strands in magisterial reception.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committers-frame structural positioning of this reading within the kernel''s reading family.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__continuity_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vatican_ii_continuity_tr_t1965, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1965, 0.05).
narrative_ontology:measurement(vatican_ii_continuity_tr_t1975, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1975, 0.12).
narrative_ontology:measurement(vatican_ii_continuity_tr_t1985, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(vatican_ii_continuity_tr_t1995, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1995, 0.28).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2005, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2015, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2015, 0.27).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2025, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(vatican_ii_continuity_be_t1965, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1965, 0.15).
narrative_ontology:measurement(vatican_ii_continuity_be_t1975, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1975, 0.22).
narrative_ontology:measurement(vatican_ii_continuity_be_t1985, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement(vatican_ii_continuity_be_t1995, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(vatican_ii_continuity_be_t2005, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2005, 0.37).
narrative_ontology:measurement(vatican_ii_continuity_be_t2015, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2015, 0.39).
narrative_ontology:measurement(vatican_ii_continuity_be_t2025, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(vatican_ii_continuity_su_t1965, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1965, 0.1).
narrative_ontology:measurement(vatican_ii_continuity_su_t1975, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1975, 0.25).
narrative_ontology:measurement(vatican_ii_continuity_su_t1985, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1985, 0.45).
narrative_ontology:measurement(vatican_ii_continuity_su_t1995, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement(vatican_ii_continuity_su_t2005, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(vatican_ii_continuity_su_t2015, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2015, 0.43).
narrative_ontology:measurement(vatican_ii_continuity_su_t2025, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__continuity_reading, 0.1).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, tridentine_mass_canonical_status).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, religious_freedom_doctrinal_development).

% DUAL FORMULATION NOTE:
% This constraint is one of three in the vatican_ii_magisterial_authority kernel family. The continuity_reading and rupture_reading are mutually foreclosing (core premises contradict). The composite_overdetermination_reading describes the Council's textual genesis; both prescriptive readings (continuity, rupture) coexist with it as competing receptions. All three link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_magisterial_authority__continuity_reading, institutional, 0.15).
constraint_indexing:directionality_override(vatican_ii_magisterial_authority__continuity_reading, organized, 0.2).
constraint_indexing:directionality_override(vatican_ii_magisterial_authority__continuity_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
