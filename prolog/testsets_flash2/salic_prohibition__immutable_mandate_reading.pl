% ============================================================================
% CONSTRAINT STORY: salic_prohibition__immutable_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__immutable_mandate_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: salic_prohibition__immutable_mandate_reading
 *   human_readable: Salic Law: Immutable Divine/Natural Mandate Reading
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint story models the 'immutable mandate' reading of Salic
 *   Law, where it is interpreted as an unchangeable divine or natural law
 *   embedded in the dynastic constitution, categorically excluding female
 *   heirs. This reading legitimizes challenges to female succession and even
 *   preventive war to enforce agnatic priority. The claimed type is 'snare'
 *   because the coordination story (stable succession) is cover for the
 *   systematic extraction of power and status from female and cognatic lines,
 *   maintained by active enforcement and suppression of alternatives. The
 *   metrics reflect this high extraction and suppression, with a rising
 *   theater ratio as the 'natural law' justification becomes increasingly
 *   performative over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.85).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.9).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, snare).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Law: Immutable Divine/Natural Mandate Reading").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, '4801d24a-28aa-4e6d-bc19-9e40e40b430e').
narrative_ontology:cs_kernel_codification('4801d24a-28aa-4e6d-bc19-9e40e40b430e', fixed_text).
narrative_ontology:cs_authority_grounding('4801d24a-28aa-4e6d-bc19-9e40e40b430e', lineage).
narrative_ontology:cs_interpretation_layer_present('4801d24a-28aa-4e6d-bc19-9e40e40b430e').
narrative_ontology:cs_reading_relation('4801d24a-28aa-4e6d-bc19-9e40e40b430e', salic_prohibition__sovereign_override_reading, forecloses).
narrative_ontology:cs_reading_relation('4801d24a-28aa-4e6d-bc19-9e40e40b430e', salic_prohibition__cognatic_reversion_reading, forecloses).
narrative_ontology:cs_axiom('4801d24a-28aa-4e6d-bc19-9e40e40b430e', foundational, agnatic_primogeniture_divine_mandate).
narrative_ontology:cs_axiom_status(agnatic_primogeniture_divine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('4801d24a-28aa-4e6d-bc19-9e40e40b430e', agnatic_primogeniture_divine_mandate, theological).
narrative_ontology:cs_axiom('4801d24a-28aa-4e6d-bc19-9e40e40b430e', foundational, female_rule_unnatural_disorder).
narrative_ontology:cs_axiom_status(female_rule_unnatural_disorder, holdable).
narrative_ontology:cs_axiom_grounding('4801d24a-28aa-4e6d-bc19-9e40e40b430e', female_rule_unnatural_disorder, deontological).
narrative_ontology:cs_reference_frame('4801d24a-28aa-4e6d-bc19-9e40e40b430e', ancient_frankish_custom_divinely_ordained).
narrative_ontology:cs_drift_state('4801d24a-28aa-4e6d-bc19-9e40e40b430e', enlightenment_era_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4801d24a-28aa-4e6d-bc19-9e40e40b430e', '').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_dynastic_lines).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, male_nobility).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_heirs).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, cognatic_dynastic_lines).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, liberal_reformers).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, divine_right_of_kings_agnatic_priority).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, natural_law_male_primogeniture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the male-line royal families whose legitimacy and power are directly derived from and maintained by the Salic prohibition. They actively enforce the exclusion of female heirs, viewing it as a divine or natural right. Their identity is fused with this agnatic principle.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_dynastic_lines, agenda_setter,
    institutional, generational, identity_locked, national).

% Individuals who, by birthright, would otherwise be in line for succession but are categorically excluded by the Salic prohibition. They bear the direct cost of lost power and status, with no legal recourse within this framework.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_heirs, payer,
    powerless, biographical, trapped, national).

% Benefit from the agnatic system by having greater access to positions of power and influence within the court and government, as female rule would potentially shift patronage and power structures. They are invested in maintaining the status quo.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, male_nobility, beneficiary,
    powerful, generational, constrained, national).

% Royal or noble families that trace their lineage through female lines and would have a claim to succession under a cognatic system. They are victims of the Salic prohibition, seeing their potential claims nullified, but are often too weak to challenge the dominant agnatic power.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, cognatic_dynastic_lines, payer,
    moderate, generational, constrained, national).

% Provides theological and moral justification for the Salic prohibition, often framing it as divine will or natural order. Their authority and influence are intertwined with the maintenance of traditional, male-dominated power structures.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, traditionalist_clergy, beneficiary,
    institutional, civilizational, identity_locked, national).

% Advocate for gender equality in succession and challenge the divine/natural law framing of Salic Law. They bear the costs of political marginalization and repression for their views, facing an entrenched institutional power structure.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, liberal_reformers, payer,
    organized, biographical, constrained, national).

% Neighboring kingdoms or empires that adhere to or benefit from agnatic succession. They may intervene militarily or diplomatically to prevent female succession in a Salic kingdom, viewing it as a threat to regional stability or an opportunity for territorial gain.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, foreign_powers_agnatic, agenda_setter,
    institutional, generational, arbitrage, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit restrictive, line of succession, preventing internal dynastic disputes over female claims and providing a stable, predictable (male) leadership structure for the realm.
% TRANSFER_FUNCTION: Transfers the right to rule, and all associated power, wealth, and legitimacy, exclusively to male heirs, diverting it from female heirs and their cognatic lines to agnatic lines and their supporting male nobility.
% ABSENT_VOICES: Female heirs and their supporters are systematically silenced or delegitimized; their claims are dismissed as contrary to divine/natural order. Any alternative legal or social frameworks that would permit female rule are suppressed.
% DISAPPEARANCE_RATIONALE: If the immutable mandate of Salic Law vanished, numerous female claims to thrones would immediately become legitimate, triggering widespread dynastic crises, potential civil wars, and a complete reordering of European royal houses and political alliances. The entire system of succession would need to be renegotiated.
% FOUNDING_PROBLEM: To prevent fragmentation of Frankish lands through female inheritance and to ensure military leadership by male rulers in a martial society, establishing a clear, undisputed male line of succession.
% FOUNDING_PROBLEM_CORROBORATION: Agnatic dynastic lines and traditionalist clergy assert the problem of maintaining male-line purity and martial leadership is still live. Liberal reformers and cognatic lines argue the original Frankish context is dead, and the law persists as a tool for male power consolidation, with historical analysis from outside the benefiting parties supporting this view.
narrative_ontology:disappearance_verdict(salic_prohibition__immutable_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__immutable_mandate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__immutable_mandate_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__immutable_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__immutable_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the law systematically diverts immense power and wealth from female lines to male lines, with no compensation. Suppression is very high (0.90) as any challenge to this 'immutable' law is met with severe political, social, and even military force. The theater ratio is substantial (0.60) because while the law provides a clear succession, the 'divine/natural' justification becomes increasingly performative as Enlightenment ideas challenge such claims, yet the exclusion persists due to entrenched power interests. Resistance is high (0.70) from those excluded, but accessibility collapse is also high (0.80) because the institutional and ideological barriers to challenging the law are formidable.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of agnatic dynastic lines, this is a foundational, immutable law ensuring stability and divine order. From the perspective of female heirs and liberal reformers, it is a coercive, extractive mechanism designed to maintain male power, cloaked in a theatrical 'natural law' justification. The engine's classification as a snare from the victim's seat captures this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Agnatic dynastic lines and traditionalist clergy are clear beneficiaries and agenda-setters, as their power and legitimacy are directly tied to this interpretation of Salic Law. Female heirs and cognatic lines are direct victims, suffering categorical exclusion. Liberal reformers are also victims, as their efforts to modernize succession are suppressed. Foreign powers adhering to agnatic principles can act as agenda-setters by intervening to enforce the law, benefiting from regional stability or opportunities for influence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (stable succession) is still 'live' but its 'immutable divine law' justification has become increasingly theatrical. The classification as a snare prevents mislabeling this as a 'rope' (pure coordination) by highlighting the systematic extraction and suppression inherent in the 'immutable mandate' reading. The high theater ratio indicates that the performance of divine/natural justification is a significant part of its persistence, even as its genuine coordination function is increasingly questioned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_political_origin,
    'Is the Salic prohibition truly a divine or natural law, or is its ''immutable'' status a political construct to maintain agnatic power?',
    'Historical-critical analysis of primary sources, theological scholarship on divine mandates, and comparative legal studies of succession laws across cultures and eras.',
    'If proven a political construct, the ''immutable mandate'' reading loses its primary legitimizing force, weakening the constraint''s ideological suppression and potentially reclassifying it as a more overt snare or even a piton if its enforcement becomes purely inertial.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_vs_political_origin, conceptual, 'Ambiguity between divine/natural origin and political construction of Salic Law''s immutability.').

omega_variable(
    enforcement_cost_vs_benefit,
    'Does the cost of actively enforcing the Salic prohibition (e.g., through military intervention or political repression) outweigh the benefits of ''stable'' agnatic succession for the realm as a whole, or only for the benefiting dynastic lines?',
    'Economic and social cost-benefit analysis of historical succession crises, including wars of succession, compared to the costs of adopting cognatic succession.',
    'If enforcement costs for the realm are shown to be prohibitive relative to diffuse benefits, it would highlight the concentrated benefits to agnatic lines and further solidify the ''snare'' classification, potentially pushing it towards a piton if the costs become unsustainable for the agenda-setters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_vs_benefit, empirical, 'Whether enforcement costs justify the claimed benefits of agnatic succession.').

omega_variable(
    kernel_reading_divergence,
    'Given the ''salic_prohibition'' kernel, how do the ''immutable_mandate_reading'', ''sovereign_override_reading'', and ''cognatic_reversion_reading'' structurally diverge in their impact on succession and power distribution?',
    'Comparative analysis of the axioms, reference frames, and drift states of each reading, identifying specific points of contradiction or influence.',
    'Understanding the precise structural delta between readings clarifies the contested nature of the kernel and the specific mechanisms by which each reading either forecloses, coexists with, or influences the others, informing the overall stability and contestability of the ''salic_prohibition'' kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Structural differences between the various readings of Salic Law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 1500, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t1500, salic_prohibition__immutable_mandate_reading, theater_ratio, 1500, 0.4).
narrative_ontology:measurement(sali_tr_t1600, salic_prohibition__immutable_mandate_reading, theater_ratio, 1600, 0.5).
narrative_ontology:measurement(sali_tr_t1700, salic_prohibition__immutable_mandate_reading, theater_ratio, 1700, 0.6).
narrative_ontology:measurement(sali_tr_t1800, salic_prohibition__immutable_mandate_reading, theater_ratio, 1800, 0.65).
narrative_ontology:measurement(sali_tr_t1900, salic_prohibition__immutable_mandate_reading, theater_ratio, 1900, 0.6).

% Extraction over time
narrative_ontology:measurement(sali_be_t1500, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1500, 0.75).
narrative_ontology:measurement(sali_be_t1600, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1600, 0.8).
narrative_ontology:measurement(sali_be_t1700, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1700, 0.85).
narrative_ontology:measurement(sali_be_t1800, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1800, 0.88).
narrative_ontology:measurement(sali_be_t1900, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1900, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t1500, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1500, 0.8).
narrative_ontology:measurement(sali_su_t1600, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1600, 0.85).
narrative_ontology:measurement(sali_su_t1700, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1700, 0.9).
narrative_ontology:measurement(sali_su_t1800, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1800, 0.92).
narrative_ontology:measurement(sali_su_t1900, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1900, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
