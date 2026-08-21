% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__composite_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__composite_reading
 *   human_readable: Honor Satisfaction Mechanism (Composite Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This constraint describes the decline of dueling as a mechanism for honor
 *   satisfaction, viewed through a 'composite reading' that emphasizes
 *   multiple, distinct, and interacting pressures: state monopoly on
 *   violence, the rise of bourgeois norms, the influence of insurance, and a
 *   fundamental category-shift in how honor was conceived. This reading
 *   argues that dueling did not simply fade but was actively suppressed and
 *   replaced by alternative mechanisms, leading to a high degree of
 *   extraction from those who still adhered to the older honor code. The
 *   claimed type is 'tangled_rope' because it involved both a genuine
 *   coordination function (reducing private violence) and asymmetric
 *   extraction (from honor-seekers by the state and bourgeois elites).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.68).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.75).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Honor Satisfaction Mechanism (Composite Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, 'ed3d6ced-3e78-4f4f-acbd-25c264055f56').
narrative_ontology:cs_kernel_codification('ed3d6ced-3e78-4f4f-acbd-25c264055f56', implicit).
narrative_ontology:cs_authority_grounding('ed3d6ced-3e78-4f4f-acbd-25c264055f56', extraction).
narrative_ontology:cs_interpretation_layer_present('ed3d6ced-3e78-4f4f-acbd-25c264055f56').
narrative_ontology:cs_reading_relation('ed3d6ced-3e78-4f4f-acbd-25c264055f56', honor_satisfaction_mechanism__decline_reading, influences).
narrative_ontology:cs_reading_relation('ed3d6ced-3e78-4f4f-acbd-25c264055f56', honor_satisfaction_mechanism__contraction_reading, influences).
narrative_ontology:cs_axiom('ed3d6ced-3e78-4f4f-acbd-25c264055f56', foundational, honor_is_socially_constructed_and_mutable).
narrative_ontology:cs_axiom_status(honor_is_socially_constructed_and_mutable, holdable).
narrative_ontology:cs_axiom_grounding('ed3d6ced-3e78-4f4f-acbd-25c264055f56', honor_is_socially_constructed_and_mutable, empirically_contingent).
narrative_ontology:cs_axiom('ed3d6ced-3e78-4f4f-acbd-25c264055f56', foundational, state_monopoly_on_violence_is_legitimate).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('ed3d6ced-3e78-4f4f-acbd-25c264055f56', state_monopoly_on_violence_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('ed3d6ced-3e78-4f4f-acbd-25c264055f56', traditional_honor_code_with_dueling).
narrative_ontology:cs_drift_state('ed3d6ced-3e78-4f4f-acbd-25c264055f56', late_19th_century, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('ed3d6ced-3e78-4f4f-acbd-25c264055f56', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, state_legal_apparatus).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, bourgeois_elites).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, honor_seekers).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, lower_gentry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, insurance_companies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively suppressed dueling through legal prohibitions and enforcement, while simultaneously offering alternative, state-sanctioned mechanisms for honor satisfaction (e.g., courts of honor, legal redress for slander). Benefited from consolidating its monopoly on violence and dispute resolution.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Promoted new norms of civility and legalistic dispute resolution, which delegitimized dueling as a 'barbaric' practice. This shift reinforced their social status and reduced the personal risk associated with honor disputes, as their social capital was not tied to martial prowess.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, bourgeois_elites, beneficiary,
    powerful, biographical, mobile, national).

% Individuals whose social standing and personal identity were deeply intertwined with the traditional code of honor, which often necessitated dueling to redress perceived slights. Faced increasing legal penalties and social opprobrium for engaging in duels, yet felt compelled by their identity to do so.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, honor_seekers, payer,
    moderate, immediate, identity_locked, local).

% A social class that historically relied on dueling to assert and defend their honor, often lacking the economic or political capital of higher elites. They bore the brunt of state suppression and the erosion of traditional honor codes, finding fewer legitimate avenues for honor satisfaction.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, lower_gentry, payer,
    powerless, biographical, constrained, regional).

% Benefited from the decline of dueling by reducing payouts for death or injury, and by promoting a culture of risk aversion that further undermined the practice. Their financial interests aligned with the state's efforts to suppress dueling.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, insurance_companies, beneficiary,
    organized, biographical, arbitrage, national).

% Advocated for the abolition of dueling on moral and social grounds, contributing to the normative shift against the practice. Their efforts influenced public opinion and legislative action.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, social_reformers, observer,
    moderate, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a multi-faceted system for managing honor disputes, transitioning from private violence (dueling) to state-controlled legal and social mechanisms, thereby coordinating social order and reducing lethal conflict.
% TRANSFER_FUNCTION: Transferred the authority for honor satisfaction from individuals and private codes to the state and emerging bourgeois norms. It extracted the right to private violence from honor-bound individuals and transferred social legitimacy to legalistic redress.
% ABSENT_VOICES: Traditionalists and those deeply embedded in the honor culture, particularly from the lower gentry, who saw the state's alternatives as inadequate or illegitimate for true honor satisfaction. Their voices were increasingly marginalized by legal suppression and normative shifts.
% DISAPPEARANCE_RATIONALE: If the composite mechanisms (state monopoly, bourgeois norms, insurance, category-shift) had not emerged, dueling or similar forms of private violence would likely have persisted as a primary means of honor satisfaction, leading to a different social and legal landscape regarding personal reputation and conflict resolution.
% FOUNDING_PROBLEM: The problem of uncontrolled private violence stemming from honor disputes, which challenged state authority and social stability.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars corroborate that the problem of private violence and the need for state monopoly on force was a live concern for emerging nation-states. Sociologists attest to the ongoing evolution of social norms around conflict resolution.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__composite_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_satisfaction_mechanism__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__composite_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the state and bourgeois elites successfully imposed their preferred mechanisms for honor satisfaction, effectively extracting the right to private violence from individuals and imposing new social costs on those who resisted. Suppression is also high, reflecting the legal prohibitions and social stigmatization of dueling. Theater ratio is moderate, as the state's 'alternatives' for honor satisfaction were often performative or inadequate for those deeply committed to the traditional honor code, serving more to legitimize state power than to genuinely satisfy honor. The increasing extractiveness and suppression over time reflect the hardening of state control and the entrenchment of new social norms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and bourgeois elites, the decline of dueling was a triumph of civilization and order (a 'rope' or even 'mountain' of progress). From the perspective of honor seekers, it was a coercive imposition that extracted their fundamental right to defend their reputation (a 'snare'). This composite reading acknowledges both the coordination function and the extractive asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legal apparatus and bourgeois elites are clear beneficiaries and agenda-setters, as they gained power and social legitimacy from the decline of dueling. Honor seekers and the lower gentry are victims, as they lost a primary means of asserting their social standing and faced severe penalties for adherence to traditional codes. Insurance companies benefited from reduced risk. Social reformers acted as observers and advocates for the new norms.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (reducing private violence) remains live, but the mechanisms for achieving it shifted from direct suppression of dueling to a broader redefinition of honor itself. The 'tangled_rope' classification prevents mislabeling the state's actions as pure extraction, acknowledging the genuine coordination problem of private violence, while also highlighting the asymmetric costs borne by those whose honor was tied to the older system. The persistence of 'honor seekers' despite high suppression indicates the deep identity-lock that made exit from the traditional code difficult.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relative_weight_of_mechanisms,
    'What was the relative causal weight of state monopoly, bourgeois norms, insurance, and category-shift in the decline of dueling?',
    'Comparative historical analysis across different national contexts with varying strengths of each mechanism, or counterfactual modeling.',
    'A stronger weighting of state monopoly would emphasize the coercive aspect (more snare-like); stronger bourgeois norms would highlight the cultural shift (more rope-like for new elites); stronger category-shift would suggest a more ''mountain''-like inevitability of cognitive change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relative_weight_of_mechanisms, empirical, 'Determining the primary drivers of dueling''s decline.').

omega_variable(
    honor_redefinition_legitimacy,
    'To what extent were the state-sanctioned alternatives for honor satisfaction (e.g., courts of honor) genuinely legitimate and effective for all social strata, or primarily a tool for elite control?',
    'Analysis of historical records, personal correspondence, and legal outcomes across different social classes to assess perceived efficacy and fairness of alternative mechanisms.',
    'If alternatives were widely legitimate, the extraction from honor seekers is mitigated (more rope-like); if they were primarily tools of elite control, the extraction is amplified (more snare-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_redefinition_legitimacy, conceptual, 'Assessing the legitimacy and effectiveness of alternative honor satisfaction mechanisms.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, social exclusion) or internalized (honor seekers'' self-concept shifting away from dueling)?',
    'Post-exit suppression trajectory: if adherence to dueling norms persists after legal barriers are removed (e.g., in subcultures or historical reenactments), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more insidious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the decline of dueling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 1600, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1600, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(hono_tr_t1650, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1650, 0.2).
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1700, 0.3).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1750, 0.4).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1800, 0.45).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1850, 0.45).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1900, 0.45).

% Extraction over time
narrative_ontology:measurement(hono_be_t1600, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1600, 0.4).
narrative_ontology:measurement(hono_be_t1650, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1650, 0.5).
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1700, 0.58).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1750, 0.63).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1800, 0.66).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1850, 0.68).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1900, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1600, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1600, 0.3).
narrative_ontology:measurement(hono_su_t1650, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1650, 0.45).
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1700, 0.6).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1750, 0.7).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1800, 0.75).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1850, 0.75).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1900, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'honor_satisfaction_mechanism' kernel. This 'composite_reading' emphasizes multiple, distinct, and interacting pressures leading to the decline of dueling, in contrast to the 'decline_reading' (focus on gradual reduction) and 'contraction_reading' (focus on cognitive impossibility).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
