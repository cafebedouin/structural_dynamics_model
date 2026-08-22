% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__behavioral_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__behavioral_control_reading, []).

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
 *   constraint_id: hoa_covenant_scope__behavioral_control_reading
 *   human_readable: HOA Covenant as Aesthetic Uniformity and Behavioral Conformity Enforcement
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This story instantiates the behavioral_control_reading of the
 *   hoa_covenant_scope kernel: the covenant's real operative function, on
 *   this reading, is enforcing aesthetic and behavioral uniformity — not
 *   primarily coordinating shared infrastructure (the sibling
 *   coordination_reading) and not primarily extracting board revenue via fine
 *   proliferation (the sibling extraction_reading), though all three readings
 *   describe the same standing document and enforcement apparatus. Under this
 *   reading, architectural review discretion has expanded from preventing
 *   genuine value-depressing neglect into policing subjective taste,
 *   lifestyle choices, and even political/personal expression (yard signs,
 *   flags), with the majority's comfort as the operative standard rather than
 *   any demonstrated value linkage.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.42).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.61).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant as Aesthetic Uniformity and Behavioral Conformity Enforcement").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, '8291a09b-fb25-4df2-af19-c29ccb507d2f').
narrative_ontology:cs_kernel_codification('8291a09b-fb25-4df2-af19-c29ccb507d2f', fixed_text).
narrative_ontology:cs_authority_grounding('8291a09b-fb25-4df2-af19-c29ccb507d2f', extraction).
narrative_ontology:cs_interpretation_layer_present('8291a09b-fb25-4df2-af19-c29ccb507d2f').
narrative_ontology:cs_reading_relation('8291a09b-fb25-4df2-af19-c29ccb507d2f', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('8291a09b-fb25-4df2-af19-c29ccb507d2f', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('8291a09b-fb25-4df2-af19-c29ccb507d2f', foundational, aesthetic_uniformity_is_legitimate_value_protection).
narrative_ontology:cs_axiom_status(aesthetic_uniformity_is_legitimate_value_protection, holdable).
narrative_ontology:cs_axiom_grounding('8291a09b-fb25-4df2-af19-c29ccb507d2f', aesthetic_uniformity_is_legitimate_value_protection, instrumental).
narrative_ontology:cs_axiom('8291a09b-fb25-4df2-af19-c29ccb507d2f', secondary, majority_taste_may_bind_dissenting_owners).
narrative_ontology:cs_axiom_status(majority_taste_may_bind_dissenting_owners, holdable).
narrative_ontology:cs_axiom_grounding('8291a09b-fb25-4df2-af19-c29ccb507d2f', majority_taste_may_bind_dissenting_owners, conventional).
narrative_ontology:cs_reference_frame('8291a09b-fb25-4df2-af19-c29ccb507d2f', developer_era_neglect_prevention_standard).
narrative_ontology:cs_drift_state('8291a09b-fb25-4df2-af19-c29ccb507d2f', contemporary_expansive_arc_discretion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8291a09b-fb25-4df2-af19-c29ccb507d2f', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics_practitioners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, political_speech_displayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, architectural_review_committee).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Homeowners who already prefer, or have converged on, the covenant's aesthetic and behavioral norms (uniform lawn treatments, muted exterior colors, no visible personal expression). They experience the covenant as validating their own preferences and as protecting their property values from what they perceive as unsightly or declassing deviation by neighbors. They rarely file violations against themselves and readily vote to expand or tighten the architectural review guidelines.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, conformist_majority, beneficiary,
    organized, biographical, mobile, local).

% Serve on or have close relationships with the architectural review committee and board. They interpret ambiguous covenant language ('tasteful,' 'harmonious,' 'in keeping with neighborhood character') to ratify their own aesthetic preferences and lifestyle norms as the community standard, then enforce that interpretation against others through fines, liens, and mandated remediation.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, agenda_setter,
    organized, generational, mobile, local).

% Want to paint a house an unusual color, keep a native-plant or vegetable-forward yard instead of turf, park a work vehicle in the driveway, or otherwise deviate visibly from the norm. They face escalating fines, mandatory remediation orders, and liens for noncompliance. Selling to exit means absorbing a loss on a house forced into conformity or accepting the stigma of an active violation on the property record; the covenant runs with the land, so there is no opt-out short of moving.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners, payer,
    moderate, biographical, constrained, local).

% Homeowners whose taste, culture, religious practice (e.g., outdoor shrines, specific holiday displays), or economic constraints (visible clotheslines, older vehicles, DIY repairs) fall outside the board's notion of 'harmonious.' They often cannot afford the remediation costs the board demands and accumulate fines and liens that can eventually threaten the home itself. Their exit is effectively foreclosed by the cost of both compliance and departure.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics_practitioners, payer,
    powerless, biographical, trapped, local).

% Homeowners who want to display yard signs, flags, or other political or personal expression. The covenant's aesthetic-uniformity provisions are read to prohibit or restrict these displays as 'visual clutter' or 'discordant with neighborhood character,' effectively suppressing speech through property-aesthetics language rather than a speech-restriction clause naming itself as such.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, political_speech_displayers, payer,
    powerless, immediate, constrained, local).

% The specific body that adjudicates what counts as compliant. Its members are volunteer or semi-professional homeowners who apply subjective standards case by case, with wide discretion and little formal accountability beyond the board that appoints them. They set the boundary of acceptable aesthetics and behavior in practice, even where the written covenant is vague.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, architectural_review_committee, agenda_setter,
    organized, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__behavioral_control_reading, architectural_review_committee, beneficiary).

% People considering purchasing into the community are shown the covenant but rarely see how it is actually enforced in practice or against whom. They have no voice in how the standard has been interpreted historically and discover the real scope of behavioral control only after buying in.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, prospective_buyers, excluded,
    powerless, immediate, mobile, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__behavioral_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is a genuine underlying coordination problem the covenant COULD solve — preventing a single owner's neglect or extreme deviation from depressing neighboring property values — but under this reading the covenant's actual operation extends well past that problem into policing subjective taste, lifestyle choices, and expressive conduct that has no demonstrated effect on value.
% TRANSFER_FUNCTION: Moves discretion over aesthetic and behavioral standards from individual homeowners to the board/ARC, and moves money (fines, remediation costs, legal fees) and psychological cost (fear of violation, self-censorship) from nonconforming and marginal-taste homeowners to the enforcement apparatus and, indirectly, to the comfort of the conforming majority.
% ABSENT_VOICES: Nonconformist and marginal-aesthetics homeowners rarely sit on the board or ARC because those bodies self-select for people who already hold and enjoy enforcing the majority aesthetic; prospective buyers are not shown enforcement history before purchase; renters and non-owner occupants, who are bound by the same rules but cannot vote, are entirely absent from governance.
% DISAPPEARANCE_RATIONALE: If the aesthetic and behavioral enforcement provisions vanished overnight, yard displays, exterior colors, plantings, and lifestyle-visible choices would diversify rapidly; the ARC would lose its primary function and likely its rationale for existing; fine revenue tied to aesthetic violations would disappear; conformist and board-aligned homeowners would lose the mechanism that currently protects their preferred visual environment from neighbors' choices.
% FOUNDING_PROBLEM: Mid-20th-century developers and early homeowner associations sought to prevent visible property neglect and value-depressing eyesores (junk cars, unmaintained structures, incompatible commercial use) in newly built residential subdivisions, in a context where zoning alone was seen as insufficient.
% FOUNDING_PROBLEM_CORROBORATION: Real estate economists and some board members attest that maintaining baseline upkeep and preventing severe neglect still protects value, a live problem. Fair housing advocates, ACLU-affiliated speech litigation records, and independent studies of HOA enforcement patterns (cited in state legislative hearings on HOA reform) attest that enforcement has substantially shifted from neglect-prevention to subjective taste and expression policing with no demonstrated value linkage — corroboration from outside the board and conformist-majority seats.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__behavioral_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__behavioral_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hoa_covenant_scope__behavioral_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__behavioral_control_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__behavioral_control_reading_tests).
:- end_tests(hoa_covenant_scope__behavioral_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42 at interval end) because the harm here is primarily behavioral/expressive suppression and cost of remediation/fines, not the raw revenue-extraction the sibling extraction_reading would score higher. Suppression is authored higher (0.61) because the mechanism depends on continuously active, discretionary enforcement against a class of nonconforming and marginal-aesthetic owners who have no realistic exit short of selling at a loss. Theater ratio is moderate-low (0.30): some enforcement genuinely addresses neglect, but a rising share targets subjective taste and expression with declining connection to any measurable value effect.
 *
 * PERSPECTIVAL GAP:
 *   From the board/ARC seat, this reads as legitimate, majority-ratified standard-setting protecting shared value. From the marginal_aesthetics_practitioners and political_speech_displayers seats, the identical clause ('harmonious with neighborhood character') operates as open-ended discretionary suppression of whatever the reviewing committee happens to personally disfavor. The engine should compute these seats differently from the same structural data — that divergence is the phenomenon this reading isolates.
 *
 * DIRECTIONALITY LOGIC:
 *   Conformist_majority and board_aligned_homeowners sit near the beneficiary end: they already hold the enforced preferences and collect the psychic and (they claim) financial benefit of imposed uniformity without bearing its costs. Nonconformist_homeowners, marginal_aesthetics_practitioners, and political_speech_displayers sit near the target end: they bear fines, remediation costs, and self-censorship, and their exit is constrained or trapped because the covenant runs with the land and selling under an active violation or lien imposes a direct financial penalty.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing visible neglect and value-depressing eyesores) is only partly live: baseline upkeep enforcement retains genuine function, but the covenant's aesthetic-taste and lifestyle-restriction provisions have outrun any demonstrated connection to that founding problem, while the enforcement apparatus built to serve it persists and has broadened its reach. This is exactly the divergence the founding_problem_status='contested' captures, distinguishing genuine residual coordination from accreted behavioral control riding on the same instrument.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aesthetic_standard_vs_value_link,
    'Is there any demonstrated empirical link between the specific subjective aesthetic standards enforced (paint color palettes, lawn treatment, permitted plantings, sign/flag restrictions) and actual property value outcomes, or is the value-maximization justification a post-hoc rationalization for enforcing majority taste?',
    'Comparative real-estate valuation studies across otherwise similar HOA and non-HOA communities, controlling for baseline maintenance standards, isolating aesthetic-taste provisions from neglect-prevention provisions.',
    'If no link is found for taste-specific provisions (as opposed to neglect-prevention provisions), the behavioral_control_reading''s classification as extractive-of-nonconformists strengthens; if a link is found, some of the measured extraction should properly be reattributed to the coordination_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aesthetic_standard_vs_value_link, empirical, 'Whether aesthetic uniformity enforcement actually protects value or merely encodes majority preference.').

omega_variable(
    reading_boundary_ambiguity,
    'Where exactly does the behavioral_control_reading''s enforcement activity end and the extraction_reading''s fine-revenue/power-consolidation activity begin, given that the same ARC decisions and fines serve both functions simultaneously in practice?',
    'Disaggregate fine and enforcement-action records by whether the underlying complaint concerns (a) genuine neglect/externality, (b) subjective taste/behavior with no revenue optimization signal, or (c) enforcement patterns correlated with board discretionary targeting or fine-schedule escalation.',
    'Reclassifying some enforcement actions from this reading to the extraction_reading would lower this story''s ε and its suppression figure, since the residual behavioral-control core is narrower than the full enforcement apparatus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'The two extractive sibling readings (behavioral control, revenue extraction) may not be cleanly separable in the underlying enforcement data.').

omega_variable(
    speech_suppression_scope,
    'Should political/personal-expression suppression (yard signs, flags) be treated as part of THIS reading''s aesthetic-uniformity function, or does it constitute a structurally distinct free-expression constraint that itself deserves decomposition into its own story?',
    'Examine whether covenant language and enforcement history treat sign/flag restrictions under the same ''aesthetic harmony'' clause and ARC discretion as paint-color and landscaping restrictions, or under a separately drafted and separately enforced speech provision.',
    'If enforcement data shows sign/flag suppression is adjudicated by a distinct process or clause, per the ε-invariance principle this should be split into its own constraint story rather than folded into the aesthetic-uniformity reading authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speech_suppression_scope, conceptual, 'Whether speech suppression via covenant is the same constraint as aesthetic-taste enforcement or a distinct one improperly merged here.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(hoa__tr_t8, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(hoa__tr_t16, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(hoa__tr_t24, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(hoa__tr_t32, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(hoa__tr_t40, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(hoa__be_t8, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(hoa__be_t16, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(hoa__be_t24, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(hoa__be_t32, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(hoa__be_t40, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hoa__su_t8, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(hoa__su_t16, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(hoa__su_t24, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(hoa__su_t32, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(hoa__su_t40, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__behavioral_control_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__behavioral_control_reading, 0.08).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the hoa_covenant_scope kernel, decomposed per the ε-invariance principle rather than authored as a single constraint with a measurement parameter. coordination_reading covers the genuine shared-infrastructure/externality function (lower ε, likely rope or tangled_rope). extraction_reading covers revenue generation and board power consolidation via fine proliferation and selective enforcement (likely tangled_rope or snare, beneficiary=board_members concentrated). behavioral_control_reading (this file) isolates the aesthetic-uniformity and lifestyle/speech-conformity enforcement function, with conformist_majority and board_aligned_homeowners as beneficiaries and nonconformists/marginal-aesthetics/speech-displayers as victims. All three readings share the same covenant text and enforcement body but diverge sharply in ε, beneficiary/victim structure, and classification, which is why they are linked via affects_constraints rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
