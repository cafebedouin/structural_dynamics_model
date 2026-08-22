% ============================================================================
% CONSTRAINT STORY: maat_order_principle__divine_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__divine_mandate_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: maat_order_principle__divine_mandate_reading
 *   human_readable: Ma'at as Divine Mandate of Pharaoh
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This constraint describes the 'divine mandate' reading of Ma'at in
 *   ancient Egypt, where cosmic order flows from the divine through the
 *   Pharaoh to society. In this interpretation, the Pharaoh embodies Ma'at
 *   and cannot, by definition, violate it, placing him above any constraint.
 *   This reading justifies absolute royal power and substantial extraction
 *   from the populace, framing it as a cosmic necessity for stability. The
 *   constraint is claimed as a Snare due to its high extraction and
 *   suppression, despite the official narrative presenting it as a natural,
 *   divinely ordained order.
 *
 * KEY AGENTS:
 *   - Pharaoh: Primary agenda-setter and beneficiary (institutional/arbitrage)
 *   - Royal Court Officials: Secondary beneficiaries (powerful/constrained)
 *   - Priestly Caste: Institutional beneficiaries (institutional/constrained)
 *   - Egyptian Commoners: Primary targets/payers (powerless/trapped)
 *   - Scribal Class: Payer/beneficiary, identity-locked into upholding the system (moderate/identity_locked)
 *   - Future Historians: Analytical observers (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, 0.85).
domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, 0.92).
domain_priors:theater_ratio(maat_order_principle__divine_mandate_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__divine_mandate_reading, snare).
narrative_ontology:human_readable(maat_order_principle__divine_mandate_reading, "Ma'at as Divine Mandate of Pharaoh").
narrative_ontology:topic_domain(maat_order_principle__divine_mandate_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__divine_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__divine_mandate_reading, '2c5776aa-9247-45e8-bace-a7641b248cd7').
narrative_ontology:cs_kernel_codification('2c5776aa-9247-45e8-bace-a7641b248cd7', formalized).
narrative_ontology:cs_authority_grounding('2c5776aa-9247-45e8-bace-a7641b248cd7', lineage).
narrative_ontology:cs_interpretation_layer_present('2c5776aa-9247-45e8-bace-a7641b248cd7').
narrative_ontology:cs_reading_relation('2c5776aa-9247-45e8-bace-a7641b248cd7', maat_order_principle__reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('2c5776aa-9247-45e8-bace-a7641b248cd7', maat_order_principle__distributed_maintenance_reading, forecloses).
narrative_ontology:cs_axiom('2c5776aa-9247-45e8-bace-a7641b248cd7', foundational, pharaoh_embodies_maat).
narrative_ontology:cs_axiom_status(pharaoh_embodies_maat, holdable).
narrative_ontology:cs_axiom_grounding('2c5776aa-9247-45e8-bace-a7641b248cd7', pharaoh_embodies_maat, theological).
narrative_ontology:cs_axiom('2c5776aa-9247-45e8-bace-a7641b248cd7', foundational, royal_action_is_maat).
narrative_ontology:cs_axiom_status(royal_action_is_maat, holdable).
narrative_ontology:cs_axiom_grounding('2c5776aa-9247-45e8-bace-a7641b248cd7', royal_action_is_maat, deontological).
narrative_ontology:cs_reference_frame('2c5776aa-9247-45e8-bace-a7641b248cd7', pharaonic_divine_authority).
narrative_ontology:cs_drift_state('2c5776aa-9247-45e8-bace-a7641b248cd7', contemporary_historical_analysis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2c5776aa-9247-45e8-bace-a7641b248cd7', '').
narrative_ontology:cs_kernel_id(maat_order_principle__divine_mandate_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, pharaoh).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, royal_court_officials).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, egyptian_commoners).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, scribal_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, scribal_class).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, priestly_caste).
narrative_ontology:constraint_vindicates(maat_order_principle__divine_mandate_reading, divine_right_of_kings).
narrative_ontology:constraint_vindicates(maat_order_principle__divine_mandate_reading, cosmic_harmony_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The divine ruler, source and embodiment of Ma'at. Benefits from the absolute legitimacy and power derived from this interpretation, which places him beyond reproach and justifies all royal action as inherently aligned with cosmic order. Collects all surplus and labor.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, pharaoh, agenda_setter,
    institutional, generational, arbitrage, national).

% Administer the Pharaoh's will and benefit from their proximity to power. Their authority and wealth are directly tied to the Pharaoh's absolute rule and the divine mandate interpretation of Ma'at. They enforce royal decrees and collect taxes/tributes.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, royal_court_officials, beneficiary,
    powerful, biographical, constrained, national).

% Bear the full burden of labor, taxation, and conscription, justified as their contribution to maintaining cosmic order through the Pharaoh. They have no recourse against royal decrees, as the Pharaoh cannot, by definition, violate Ma'at. Their lives are entirely subject to the royal will.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, egyptian_commoners, payer,
    powerless, immediate, trapped, local).

% Are responsible for documenting and interpreting Ma'at, but within the strict confines of the divine mandate reading. They benefit from their literacy and administrative roles but are identity-locked into upholding the Pharaoh's absolute authority, even when it leads to hardship for commoners. Any deviation from the official interpretation is suppressed.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, scribal_class, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, scribal_class, beneficiary).

% Maintain the religious rituals and narratives that reinforce the Pharaoh's divine status and the cosmic necessity of his rule. They benefit from state patronage and their role as intermediaries between the divine and the earthly realm, which is strengthened by this reading of Ma'at.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, priestly_caste, beneficiary,
    institutional, generational, constrained, national).

% Analyze the historical records and archaeological evidence to reconstruct the functioning of Ma'at and its various interpretations. They are outside the system and can critically assess the claims of divine mandate against the lived realities of ancient Egyptian society.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, future_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rigid social and political hierarchy, ensuring stability and order by centralizing all authority and legitimacy in the Pharaoh, who is believed to embody cosmic justice.
% TRANSFER_FUNCTION: Transfers all surplus wealth, labor, and decision-making power from the common populace to the Pharaoh and his court, justified as necessary for the maintenance of cosmic order (Ma'at).
% ABSENT_VOICES: Any dissenting voices or alternative interpretations of Ma'at that would challenge the Pharaoh's absolute authority are suppressed. These would include those advocating for distributed responsibility or reciprocal obligations, who are silenced by the state's enforcement mechanisms.
% DISAPPEARANCE_RATIONALE: If this interpretation of Ma'at vanished, the entire political and social structure of ancient Egypt would collapse. The Pharaoh's legitimacy would evaporate, leading to widespread unrest, challenges to authority, and a complete reorganization of power and resource distribution. The cosmic justification for the existing order would be gone.
% FOUNDING_PROBLEM: To establish and maintain a stable, unified state in ancient Egypt, preventing chaos and ensuring the prosperity of the land through a divinely sanctioned ruler.
% FOUNDING_PROBLEM_CORROBORATION: The Pharaoh and priestly caste attest that the problem of maintaining cosmic order and preventing chaos is perpetually live, requiring constant vigilance and absolute rule. Future historians, from outside the benefiting parties, corroborate that the problem of state stability was indeed central to ancient Egyptian society, but contest whether the divine mandate reading was the only or most effective solution, noting its extractive consequences.
narrative_ontology:disappearance_verdict(maat_order_principle__divine_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__divine_mandate_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__divine_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(maat_order_principle__divine_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__divine_mandate_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__divine_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(maat_order_principle__divine_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because the Pharaoh's divine mandate allows for near-total appropriation of resources and labor without accountability. Suppression (0.92) is severe, as any challenge to the Pharaoh's authority or the divine mandate interpretation of Ma'at is met with state coercion, and alternatives are almost entirely collapsed (accessibility_collapse 0.90). Resistance is low (0.10) due to the overwhelming power and ideological control. The theater ratio (0.40) reflects that while some royal actions genuinely contribute to order (e.g., infrastructure), a significant portion of the 'maintenance of Ma'at' is performative justification for extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the Pharaoh's and priestly caste's perspective, this is a Mountain or Rope, a natural and beneficial order. From the commoners' perspective, it is a clear Snare, an extractive system enforced by divine decree. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Pharaoh and royal court are full beneficiaries (d near 0.0) as they directly control and profit from the system. Commoners are full targets (d near 1.0) with no exit and bearing all costs. The scribal class is identity-locked: they benefit from their position but are also targets of the ideological suppression that prevents them from challenging the system. The priestly caste benefits from reinforcing the divine narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Snare, not a Mountain, because its persistence depends on active enforcement and suppression of alternatives, not on inherent naturalness. The 'divine mandate' is a constructed justification for extraction, not an unchangeable cosmic law. The high suppression and extractiveness, coupled with identifiable beneficiaries and victims, clearly indicate a Snare, preventing mislabeling it as a natural or purely coordinative constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_legitimacy,
    'Is the Pharaoh''s embodiment of Ma''at a genuine natural law, or a constructed ideological claim that benefits identifiable agents?',
    'Comparative historical analysis of other ancient civilizations'' legitimacy claims, and archaeological evidence of material conditions for commoners vs. royal court.',
    'If a constructed claim, the constraint is a Snare, as authored. If genuinely a natural law (unlikely given the beneficiaries), it would be a Mountain, and the extraction would be reclassified as inherent cost of cosmic order.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_legitimacy, conceptual, 'Ambiguity between natural law and constructed ideological justification for power.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state coercion, lack of alternatives) or internalized (belief in divine order, fear of cosmic chaos)?',
    'Analysis of historical records for evidence of active rebellion vs. passive acceptance, and the role of religious indoctrination in maintaining compliance.',
    'If primarily internalized, the constraint''s effective suppression is higher than the structural measure suggests, as commoners carry the suppression with them. If primarily structural, the state''s coercive capacity is the dominant factor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in maintaining the divine mandate.').

omega_variable(
    pharaoh_as_source_or_subject,
    'Is the Pharaoh truly the source of Ma''at, or is he also subject to its principles, even if implicitly?',
    'Analysis of wisdom texts and non-royal narratives for evidence of expectations placed on the Pharaoh''s conduct, independent of his self-proclaimed divine status.',
    'If the Pharaoh is also subject, this reading forecloses the ''divine mandate'' and shifts towards the ''reciprocity'' or ''distributed maintenance'' readings, reducing the claimed type''s extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharaoh_as_source_or_subject, conceptual, 'Whether the Pharaoh is above or within the Ma''at system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__divine_mandate_reading, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__divine_mandate_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(maat_tr_t300, maat_order_principle__divine_mandate_reading, theater_ratio, 300, 0.35).
narrative_ontology:measurement(maat_tr_t600, maat_order_principle__divine_mandate_reading, theater_ratio, 600, 0.4).
narrative_ontology:measurement(maat_tr_t900, maat_order_principle__divine_mandate_reading, theater_ratio, 900, 0.42).
narrative_ontology:measurement(maat_tr_t1200, maat_order_principle__divine_mandate_reading, theater_ratio, 1200, 0.41).
narrative_ontology:measurement(maat_tr_t1500, maat_order_principle__divine_mandate_reading, theater_ratio, 1500, 0.4).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__divine_mandate_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(maat_be_t300, maat_order_principle__divine_mandate_reading, base_extractiveness, 300, 0.8).
narrative_ontology:measurement(maat_be_t600, maat_order_principle__divine_mandate_reading, base_extractiveness, 600, 0.85).
narrative_ontology:measurement(maat_be_t900, maat_order_principle__divine_mandate_reading, base_extractiveness, 900, 0.87).
narrative_ontology:measurement(maat_be_t1200, maat_order_principle__divine_mandate_reading, base_extractiveness, 1200, 0.86).
narrative_ontology:measurement(maat_be_t1500, maat_order_principle__divine_mandate_reading, base_extractiveness, 1500, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__divine_mandate_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(maat_su_t300, maat_order_principle__divine_mandate_reading, suppression_requirement, 300, 0.85).
narrative_ontology:measurement(maat_su_t600, maat_order_principle__divine_mandate_reading, suppression_requirement, 600, 0.9).
narrative_ontology:measurement(maat_su_t900, maat_order_principle__divine_mandate_reading, suppression_requirement, 900, 0.93).
narrative_ontology:measurement(maat_su_t1200, maat_order_principle__divine_mandate_reading, suppression_requirement, 1200, 0.92).
narrative_ontology:measurement(maat_su_t1500, maat_order_principle__divine_mandate_reading, suppression_requirement, 1500, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__divine_mandate_reading, identity_coordination).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__reciprocity_reading).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Ma'at order principle' kernel. Its high extractiveness and suppression contrast sharply with other readings that emphasize distributed responsibility or reciprocal obligations. All readings are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
