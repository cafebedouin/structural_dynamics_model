% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__originalist_reading, []).

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
 *   constraint_id: all_men_created_equal__originalist_reading
 *   human_readable: Originalist Reading of 'All Men Are Created Equal'
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   This constraint represents the 'originalist reading' of the phrase 'all
 *   men are created equal' from the US Declaration of Independence. In this
 *   reading, the scope of 'men' is strictly limited to the social taxonomy
 *   understood by the 18th-century founders, primarily propertied white men.
 *   This interpretation serves to bound the expansion of equality, benefiting
 *   those who align with or descend from the founding elite, while actively
 *   extracting from and suppressing the rights of historically excluded
 *   groups. The constraint is claimed as a Snare due to its high
 *   extractiveness and active suppression of alternative interpretations and
 *   rights claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, 0.85).
domain_priors:suppression_score(all_men_created_equal__originalist_reading, 0.9).
domain_priors:theater_ratio(all_men_created_equal__originalist_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, snare).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "Originalist Reading of 'All Men Are Created Equal'").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, 'a3eb16c7-130e-443d-8b16-6eb94bc475cb').
narrative_ontology:cs_kernel_codification('a3eb16c7-130e-443d-8b16-6eb94bc475cb', fixed_text).
narrative_ontology:cs_authority_grounding('a3eb16c7-130e-443d-8b16-6eb94bc475cb', lineage).
narrative_ontology:cs_interpretation_layer_present('a3eb16c7-130e-443d-8b16-6eb94bc475cb').
narrative_ontology:cs_reading_relation('a3eb16c7-130e-443d-8b16-6eb94bc475cb', all_men_created_equal__universalist_reading, forecloses).
narrative_ontology:cs_reading_relation('a3eb16c7-130e-443d-8b16-6eb94bc475cb', all_men_created_equal__textualist_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('a3eb16c7-130e-443d-8b16-6eb94bc475cb', foundational, original_public_meaning_supremacy).
narrative_ontology:cs_axiom_status(original_public_meaning_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('a3eb16c7-130e-443d-8b16-6eb94bc475cb', original_public_meaning_supremacy, conventional).
narrative_ontology:cs_axiom('a3eb16c7-130e-443d-8b16-6eb94bc475cb', foundational, judicial_restraint_as_constitutional_duty).
narrative_ontology:cs_axiom_status(judicial_restraint_as_constitutional_duty, holdable).
narrative_ontology:cs_axiom_grounding('a3eb16c7-130e-443d-8b16-6eb94bc475cb', judicial_restraint_as_constitutional_duty, deontological).
narrative_ontology:cs_reference_frame('a3eb16c7-130e-443d-8b16-6eb94bc475cb', founding_era_social_taxonomy).
narrative_ontology:cs_drift_state('a3eb16c7-130e-443d-8b16-6eb94bc475cb', contemporary_civil_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a3eb16c7-130e-443d-8b16-6eb94bc475cb', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, founding_elite_descendants).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, originalist_legal_scholars).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, conservative_political_factions).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, african_americans).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, women).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, indigenous_peoples).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, non_propertied_men).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, original_intent_doctrine).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, judicial_restraint_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the preservation of historical power structures and interpretations that legitimize their inherited status and influence. This reading maintains the historical narrative that justifies their position.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, founding_elite_descendants, beneficiary,
    institutional, generational, arbitrage, national).

% Actively interpret and promote the constraint, shaping legal discourse and judicial appointments. Their careers and intellectual authority are tied to the persistence of this reading. They benefit from its continued application in law.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, originalist_legal_scholars, agenda_setter,
    institutional, biographical, constrained, national).

% Leverage this reading to justify policies that limit the expansion of rights or maintain traditional social hierarchies, aligning with their political agenda and electoral base. They gain political capital from its legal and cultural dominance.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, conservative_political_factions, beneficiary,
    organized, immediate, mobile, national).

% Historically and presently bear the brunt of this narrow interpretation, which denied and continues to deny their full personhood and equal rights, leading to systemic disadvantages. Exit from this structural position is extremely difficult.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, african_americans, payer,
    powerless, generational, trapped, national).

% Were excluded from the original scope of 'all men' and continue to face legal and social barriers rooted in this historical interpretation, impacting reproductive rights, economic equality, and political representation.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, women, payer,
    powerless, generational, trapped, national).

% Were not considered within the original framework of equality, leading to ongoing struggles for sovereignty, land rights, and recognition of their distinct legal and cultural identities.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, indigenous_peoples, payer,
    powerless, generational, trapped, national).

% While eventually included, their initial exclusion from the franchise and full civic participation demonstrates the original narrowness of the 'all men' definition, highlighting the constraint's historical extractive nature.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, non_propertied_men, payer,
    powerless, biographical, constrained, national).

% Advocate for an expansive, evolving interpretation of equality, arguing that the originalist reading is morally anachronistic and legally unjust. Their arguments are often marginalized in dominant legal and political discourse shaped by originalism.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, universalist_legal_scholars, excluded,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, historically grounded framework for constitutional interpretation, aiming to prevent judicial overreach and ensure fidelity to the founders' original intent, thereby coordinating legal precedent.
% TRANSFER_FUNCTION: Transfers interpretive authority and the scope of rights from contemporary societal values and evolving moral principles to the perceived intent of 18th-century founders, benefiting those aligned with that historical power structure.
% ABSENT_VOICES: Historically excluded groups (African Americans, women, Indigenous peoples) were absent from the founding discourse and continue to be marginalized by interpretations that privilege original intent over their lived experiences and claims to equality. Universalist legal scholars are often excluded from mainstream judicial appointments and influential legal bodies.
% DISAPPEARANCE_RATIONALE: If the originalist reading vanished overnight, the legal landscape would undergo a profound transformation. Judicial decisions would likely shift towards more expansive interpretations of rights, historically marginalized groups would find greater legal recourse, and the political balance of power would be significantly altered as the justification for many conservative policies would erode.
% FOUNDING_PROBLEM: To establish a stable republican government with a clear, limited scope of federal power, ensuring that the foundational principles of the new nation, including a specific understanding of 'equality,' were preserved against future political or judicial reinterpretation.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and conservative political factions attest the problem is live, arguing for the necessity of judicial restraint and adherence to original meaning to prevent an 'unmoored' judiciary. Civil rights advocates and universalist scholars attest the founding problem (of defining and limiting equality) is dead in its original form, having been superseded by evolving moral consensus and constitutional amendments, and that the originalist reading now serves to perpetuate historical injustices. Historical documents and legal analyses from outside the benefiting parties corroborate the narrow 18th-century understanding of 'equality' and its subsequent expansion through amendments and social movements.
narrative_ontology:disappearance_verdict(all_men_created_equal__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(all_men_created_equal__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__originalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because this reading actively denies equal rights and opportunities to a broad range of groups, channeling benefits to a narrow segment of society. Suppression is very high (0.9) as it requires active legal and political enforcement to maintain a narrow interpretation against evolving societal norms and demands for universal equality. Theater ratio is moderate-high (0.6) because while there's a genuine intellectual effort in historical interpretation, a significant portion of the discourse serves to legitimize the exclusion and maintain the status quo rather than genuinely seeking a just application of the principle. Accessibility collapse is 0.7 because while legal challenges and social movements offer some avenues, the entrenched nature of originalist jurisprudence makes fundamental reinterpretation extremely difficult. Resistance is 0.8, reflecting the ongoing, intense struggle by marginalized groups and their allies against this restrictive interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiaries perceive this as a necessary, stable framework for constitutional governance, preventing judicial activism. The victims experience it as an active mechanism of oppression and exclusion, perpetuating historical injustices. The engine's classification will highlight this divergence, likely computing a Snare for the victims and a more benign type for the beneficiaries, despite the claimed type being Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding elite descendants, originalist legal scholars, and conservative political factions are beneficiaries (low d) as this reading preserves their power, intellectual authority, and political agenda. African Americans, women, Indigenous peoples, and non-propertied men are victims (high d) as their rights and claims to equality are actively denied or constrained by this interpretation. Universalist legal scholars are excluded (high d) as their arguments are marginalized by the dominant discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The originalist reading prevents mislabeling extraction as coordination by explicitly identifying the beneficiaries and victims of its narrow scope. It highlights how a claim of 'fidelity' to a historical mandate can become a mechanism for ongoing extraction when the original mandate itself was deeply flawed and exclusionary. The persistence of this reading, despite the 'founding problem' of defining equality being contested as 'dead' in its original form, points to its function as a Snare rather than a genuine coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_scope_ambiguity,
    'Is ''original intent'' a fixed, discoverable historical fact, or is it itself subject to interpretation and selection, making the originalist reading a constructed rather than discovered constraint?',
    'Historical and philosophical analysis of the founders'' diverse and often contradictory views on equality, and the inherent limitations of recovering a singular ''intent'' across multiple authors and decades.',
    'If original intent is found to be inherently ambiguous or selectively applied, the ''mountain'' aspect of the originalist claim (that it''s an unchangeable historical truth) collapses, strengthening its classification as a constructed Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_scope_ambiguity, conceptual, 'Ambiguity of ''original intent'' as a fixed interpretive anchor.').

omega_variable(
    legitimacy_of_historical_exclusion,
    'Does the historical exclusion of certain groups from the definition of ''all men'' constitute a legitimate boundary for contemporary rights, or is it a historical injustice that must be actively remedied?',
    'Societal consensus, legislative action, and judicial precedent that explicitly repudiate historical exclusions and affirm an expansive, evolving understanding of equality.',
    'If historical exclusion is deemed illegitimate, the moral and legal justification for the originalist reading''s restrictive application erodes, leading to its reclassification as a more severe Snare or even a Piton if it persists purely by inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_historical_exclusion, preference, 'Moral and legal legitimacy of historical exclusions in defining equality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t0, all_men_created_equal__originalist_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(all__tr_t60, all_men_created_equal__originalist_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(all__tr_t120, all_men_created_equal__originalist_reading, theater_ratio, 120, 0.5).
narrative_ontology:measurement(all__tr_t180, all_men_created_equal__originalist_reading, theater_ratio, 180, 0.55).
narrative_ontology:measurement(all__tr_t240, all_men_created_equal__originalist_reading, theater_ratio, 240, 0.6).

% Extraction over time
narrative_ontology:measurement(all__be_t0, all_men_created_equal__originalist_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(all__be_t60, all_men_created_equal__originalist_reading, base_extractiveness, 60, 0.75).
narrative_ontology:measurement(all__be_t120, all_men_created_equal__originalist_reading, base_extractiveness, 120, 0.8).
narrative_ontology:measurement(all__be_t180, all_men_created_equal__originalist_reading, base_extractiveness, 180, 0.83).
narrative_ontology:measurement(all__be_t240, all_men_created_equal__originalist_reading, base_extractiveness, 240, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t0, all_men_created_equal__originalist_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(all__su_t60, all_men_created_equal__originalist_reading, suppression_requirement, 60, 0.85).
narrative_ontology:measurement(all__su_t120, all_men_created_equal__originalist_reading, suppression_requirement, 120, 0.88).
narrative_ontology:measurement(all__su_t180, all_men_created_equal__originalist_reading, suppression_requirement, 180, 0.89).
narrative_ontology:measurement(all__su_t240, all_men_created_equal__originalist_reading, suppression_requirement, 240, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, universalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, textualist_paradox_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, civil_rights_legislation).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, voting_rights_act).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'all_men_created_equal' kernel. This originalist reading emphasizes historical intent and narrow scope, contrasting with universalist and textualist paradox readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
