% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__guarantor_reading, []).

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
 *   constraint_id: lausanne_minority_protections__guarantor_reading
 *   human_readable: Lausanne Minority Protections — Guarantor Reading (International Supervision Pathway)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   The guarantor reading of Lausanne minority protections treats the treaty
 *   as creating internationally supervised obligations enforceable through
 *   guarantor state diplomacy and the European Court of Human Rights, not
 *   solely through Turkish domestic interpretation. This reading emerged from
 *   the treaty's text (Articles 37-45 establishing League of Nations
 *   guarantee, later succeeded by UN and guarantor state mechanisms) and was
 *   crystallized in ECHR jurisprudence referencing the treaty as a 'special
 *   agreement' under Article 57 of the European Convention. The constraint
 *   functions as a low-extractiveness scaffold: it creates a genuine
 *   coordination pathway (international adjudication) but lacks direct
 *   enforcement — compliance depends on Turkish state willingness, diplomatic
 *   pressure, and EU accession conditionality. The Turkish state bears
 *   compliance costs (property restitution, minority school autonomy, clergy
 *   recognition) while minority communities and guarantor states benefit from
 *   the external recourse mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__guarantor_reading, 0.25).
domain_priors:suppression_score(lausanne_minority_protections__guarantor_reading, 0.35).
domain_priors:theater_ratio(lausanne_minority_protections__guarantor_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__guarantor_reading, scaffold).
narrative_ontology:human_readable(lausanne_minority_protections__guarantor_reading, "Lausanne Minority Protections — Guarantor Reading (International Supervision Pathway)").
narrative_ontology:topic_domain(lausanne_minority_protections__guarantor_reading, "international_law/religious_governance/minority_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__guarantor_reading, '8b3b5e05-9987-4e66-9863-876e4f088667').
narrative_ontology:cs_kernel_codification('8b3b5e05-9987-4e66-9863-876e4f088667', formalized).
narrative_ontology:cs_authority_grounding('8b3b5e05-9987-4e66-9863-876e4f088667', lineage).
narrative_ontology:cs_interpretation_layer_present('8b3b5e05-9987-4e66-9863-876e4f088667').
narrative_ontology:cs_reading_relation('8b3b5e05-9987-4e66-9863-876e4f088667', lausanne_minority_protections__restrictive_reading, forecloses).
narrative_ontology:cs_reading_relation('8b3b5e05-9987-4e66-9863-876e4f088667', lausanne_minority_protections__expansive_reading, influences).
narrative_ontology:cs_axiom('8b3b5e05-9987-4e66-9863-876e4f088667', foundational, treaty_obligations_are_internationally_supervised).
narrative_ontology:cs_axiom_status(treaty_obligations_are_internationally_supervised, holdable).
narrative_ontology:cs_axiom_grounding('8b3b5e05-9987-4e66-9863-876e4f088667', treaty_obligations_are_internationally_supervised, conventional).
narrative_ontology:cs_axiom('8b3b5e05-9987-4e66-9863-876e4f088667', secondary, guarantor_states_have_standing_to_enforce).
narrative_ontology:cs_axiom_status(guarantor_states_have_standing_to_enforce, holdable).
narrative_ontology:cs_axiom_grounding('8b3b5e05-9987-4e66-9863-876e4f088667', guarantor_states_have_standing_to_enforce, conventional).
narrative_ontology:cs_reference_frame('8b3b5e05-9987-4e66-9863-876e4f088667', lausanne_treaty_framework).
narrative_ontology:cs_drift_state('8b3b5e05-9987-4e66-9863-876e4f088667', contemporary_echr_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8b3b5e05-9987-4e66-9863-876e4f088667', '2026-08-03T12:00:00Z').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, minority_communities).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, guarantor_states).
narrative_ontology:constraint_victim(lausanne_minority_protections__guarantor_reading, turkish_state).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__guarantor_reading, treaty_obligations_are_internationally_supervised).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__guarantor_reading, guarantor_states_have_standing_to_enforce).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Non-Muslim minorities in Turkey (Greek Orthodox, Armenian, Jewish) who rely on the Lausanne Treaty's international supervision mechanism to protect their religious, educational, and property rights against domestic legal erosion. They can petition the ECHR and invoke guarantor state diplomacy but lack direct enforcement power. Their exit is constrained by territorial rootedness and communal identity.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, minority_communities, beneficiary,
    organized, generational, constrained, national).

% The Republic of Turkey as successor state to the Ottoman Empire, bound by the Lausanne Treaty. It administers minority protections domestically but resists international supervision as infringement on sovereignty. It bears compliance costs (property restitution, school autonomy, clergy recognition) and faces diplomatic pressure from guarantor states and ECHR judgments. Exit from treaty obligations is structurally blocked by international law and EU accession conditionality.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, turkish_state, agenda_setter,
    institutional, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, turkish_state, payer).

% Original signatories (UK, France, Italy, Japan) and later adherents who retain formal guarantor status under the treaty. They exercise diplomatic leverage through bilateral representations, EU frameworks, and international forums. They benefit from the treaty as a stable reference point for minority rights diplomacy and as a tool in broader Turkey-EU relations. Their exit is mobile — they can modulate engagement without legal consequence.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, guarantor_states, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, guarantor_states, beneficiary).

% The ECHR serves as the primary judicial mechanism for Lausanne protections, interpreting Article 14 (non-discrimination) and Protocol 1 Article 1 (property) in light of the treaty. It issues binding judgments against Turkey but lacks direct enforcement power. Its authority derives from the European Convention system, not the Lausanne Treaty directly. It operates as an analytical observer whose rulings reshape the constraint's operational meaning.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, european_court_human_rights, observer,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, european_court_human_rights, agenda_setter).

% Academic and practitioner community that interprets the treaty's scope, monitors compliance, and frames the doctrinal debate between restrictive, guarantor, and expansive readings. They do not collect rents or bear costs but shape the interpretive environment in which the constraint operates.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates an international adjudication pathway for minority rights disputes by coordinating guarantor state diplomacy with European human rights mechanisms, moving interpretive authority from solely domestic Turkish courts to a supervised international framework.
% TRANSFER_FUNCTION: Transfers diplomatic leverage and legal accountability from the purely domestic sphere to the international sphere: compliance burden shifts to the Turkish state (property restitution, institutional autonomy), protection benefit flows to minority communities, and guarantor states gain a standing diplomatic instrument in Turkey relations.
% ABSENT_VOICES: Individual minority members without access to ECHR or diplomatic channels (rural communities, elderly, non-literate); Turkish domestic courts formally excluded from final interpretive authority over treaty scope; non-signatory states with minority populations who lack guarantor standing.
% DISAPPEARANCE_RATIONALE: If the international supervision pathway vanished overnight, minority communities would lose their only external recourse against domestic legal erosion; the Turkish state would face no diplomatic consequences for restrictive interpretation; guarantor states would lose their primary legal instrument for minority rights engagement; the ECHR would lose its Lausanne-referenced jurisprudential anchor.
% FOUNDING_PROBLEM: The 1923 Lausanne Treaty needed to protect non-Muslim minorities in the new Turkish Republic from majority domination after the collapse of the Ottoman millet system, while preserving Turkish sovereignty over territory and population.
% FOUNDING_PROBLEM_CORROBORATION: The treaty text itself (Articles 37-45), ECHR Grand Chamber jurisprudence (e.g., Bozcaada Kimisis Monastery, Fener Rum Patriarchate cases), minority community testimonies documented by Human Rights Watch and ECMI, and international legal scholarship (e.g., Alexis Alexandris, Baskin Oran) outside the Turkish state or guarantor state apparatus.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__guarantor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lausanne_minority_protections__guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__guarantor_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__guarantor_reading_tests).
:- end_tests(lausanne_minority_protections__guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the constraint primarily creates a procedural pathway rather than extracting resources; the Turkish state's compliance costs are real but bounded by treaty scope. Suppression is moderate (0.35) because the constraint's persistence relies on diplomatic pressure and ECHR judgments, not coercive enforcement — Turkey can and does resist implementation (e.g., delayed property returns, non-recognition of Patriarchate ecumenical status). Theater ratio is low (0.20) because the ECHR mechanism and guarantor diplomacy have genuine functional activity, though a growing gap between judgments and implementation suggests performative compliance. Accessibility collapse (0.40) reflects that domestic alternatives exist but are structurally inadequate for minorities. Resistance (0.55) captures Turkey's sustained pushback against international supervision as sovereignty infringement.
 *
 * PERSPECTIVAL GAP:
 *   From the Turkish state's seat, the constraint appears as an externally imposed sovereignty infringement with high compliance costs and no reciprocal benefit — a snare-like experience. From minority communities' seat, it is a fragile but genuine coordination scaffold — the only pathway to rights protection. From guarantor states' seat, it is a low-cost diplomatic instrument. The engine computes this seat divergence from the structural data; the claimed scaffold type reflects the authoring seat's assessment that the coordination function is real but enforcement is provisional.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority communities are structural beneficiaries (d near 0.0): they gain external recourse without bearing enforcement costs. The Turkish state is the primary target (d near 1.0): it bears compliance costs and faces diplomatic pressure, with trapped exit (treaty-bound). Guarantor states sit near beneficiary (d ~0.15): they gain diplomatic leverage without significant cost. The ECHR is analytical (d=0.5): it adjudicates without collecting or paying. International scholars are analytical observers (d=0.5). The derivation chain from beneficiary/victim declarations + power + exit produces these directionalities without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting minorities in a new nation-state) remains live: minorities still exist, property disputes persist, and institutional autonomy is contested. The constraint has not atrophied into a piton because the coordination function (international adjudication) remains actively used — ECHR caseload on Lausanne-grounded claims continues. However, the lack of enforcement mechanism means the scaffold has not transitioned to a self-sustaining rope; it remains dependent on external diplomatic pressure. This is not mandatrophy (mandate outliving function) but mandate outpacing enforcement capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the guarantor reading a structurally distinct constraint from the restrictive and expansive readings, or merely an interpretive position within a single constraint?',
    'Test whether the three readings produce different ε values, different beneficiary/victim structures, and different stakeholder directionalities when operationalized. If ε differs materially (restrictive: near-zero extraction; guarantor: low extraction; expansive: moderate extraction), they are distinct constraints per ε-invariance.',
    'If distinct constraints, each gets its own classification and the kernel is a family linked by network.affects_constraints. If one constraint, the readings are observer perspectives on a single structural arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the kernel''s contested readings map to one constraint or a constraint family.').

omega_variable(
    enforcement_mechanism_gap,
    'Does the guarantor reading''s lack of direct enforcement mechanism make it a failed scaffold, or is diplomatic leverage + ECHR judgments a genuine (if soft) enforcement regime?',
    'Measure compliance rates on ECHR Lausanne-grounded judgments vs. non-Lausanne judgments over time. If compliance is systematically lower, the mechanism is functionally defective; if comparable, diplomatic leverage suffices.',
    'If defective, the constraint may be a piton (atrophied function maintained theatrically) rather than a scaffold. If functional, the scaffold classification holds with low extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_mechanism_gap, empirical, 'Whether the international supervision pathway constitutes real enforcement or performative gesture.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is Turkish resistance to international supervision structural (sovereignty doctrine, constitutional order) or internalized (nationalist narrative framing minorities as existential threat)?',
    'Post-exit suppression trajectory: if Turkey withdrew from ECHR or denounced treaty but domestic courts independently maintained protections, suppression is structural. If protections collapse without external pressure, internalized narrative dominates.',
    'If internalized, effective suppression is higher than structural measure — the constraint''s coercive force persists in domestic legal culture even without active international pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression mechanism in the Turkish state''s resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__guarantor_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lausanne_guarantor_tr_t0, lausanne_minority_protections__guarantor_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(lausanne_guarantor_tr_t0, observed).
narrative_ontology:measurement(lausanne_guarantor_tr_t25, lausanne_minority_protections__guarantor_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement_basis(lausanne_guarantor_tr_t25, observed).
narrative_ontology:measurement(lausanne_guarantor_tr_t50, lausanne_minority_protections__guarantor_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(lausanne_guarantor_tr_t50, observed).
narrative_ontology:measurement(lausanne_guarantor_tr_t75, lausanne_minority_protections__guarantor_reading, theater_ratio, 75, 0.2).
narrative_ontology:measurement_basis(lausanne_guarantor_tr_t75, observed).
narrative_ontology:measurement(lausanne_guarantor_tr_t100, lausanne_minority_protections__guarantor_reading, theater_ratio, 100, 0.2).
narrative_ontology:measurement_basis(lausanne_guarantor_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(lausanne_guarantor_be_t0, lausanne_minority_protections__guarantor_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(lausanne_guarantor_be_t0, observed).
narrative_ontology:measurement(lausanne_guarantor_be_t25, lausanne_minority_protections__guarantor_reading, base_extractiveness, 25, 0.2).
narrative_ontology:measurement_basis(lausanne_guarantor_be_t25, observed).
narrative_ontology:measurement(lausanne_guarantor_be_t50, lausanne_minority_protections__guarantor_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement_basis(lausanne_guarantor_be_t50, observed).
narrative_ontology:measurement(lausanne_guarantor_be_t75, lausanne_minority_protections__guarantor_reading, base_extractiveness, 75, 0.25).
narrative_ontology:measurement_basis(lausanne_guarantor_be_t75, observed).
narrative_ontology:measurement(lausanne_guarantor_be_t100, lausanne_minority_protections__guarantor_reading, base_extractiveness, 100, 0.25).
narrative_ontology:measurement_basis(lausanne_guarantor_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(lausanne_guarantor_su_t0, lausanne_minority_protections__guarantor_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(lausanne_guarantor_su_t0, observed).
narrative_ontology:measurement(lausanne_guarantor_su_t25, lausanne_minority_protections__guarantor_reading, suppression_requirement, 25, 0.3).
narrative_ontology:measurement_basis(lausanne_guarantor_su_t25, observed).
narrative_ontology:measurement(lausanne_guarantor_su_t50, lausanne_minority_protections__guarantor_reading, suppression_requirement, 50, 0.35).
narrative_ontology:measurement_basis(lausanne_guarantor_su_t50, observed).
narrative_ontology:measurement(lausanne_guarantor_su_t75, lausanne_minority_protections__guarantor_reading, suppression_requirement, 75, 0.35).
narrative_ontology:measurement_basis(lausanne_guarantor_su_t75, observed).
narrative_ontology:measurement(lausanne_guarantor_su_t100, lausanne_minority_protections__guarantor_reading, suppression_requirement, 100, 0.35).
narrative_ontology:measurement_basis(lausanne_guarantor_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__guarantor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__guarantor_reading, 0.1).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, turkish_minority_property_rights).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, ec_turkey_accession_conditionality).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, echr_religious_freedom_jurisprudence).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, istanbul_patriarchate_recognition).

% DUAL FORMULATION NOTE:
% This constraint is the guarantor_reading in the lausanne_minority_protections kernel family. The restrictive_reading (domestic-only) and expansive_reading (full institutional continuity) are sibling constraints with different ε values: restrictive ε≈0.05 (minimal extraction, no international mechanism), guarantor ε=0.25 (low extraction, procedural pathway), expansive ε≈0.45 (moderate extraction, substantive institutional guarantees). They are linked by network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lausanne_minority_protections__guarantor_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
