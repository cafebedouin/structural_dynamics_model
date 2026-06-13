% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__contextual_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__contextual_necessity, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: humane_treatment_standard__contextual_necessity
 *   human_readable: Contextual Necessity Reading: Enhanced Interrogation Under National Security Imperative
 *   domain: international_humanitarian_law/state_security
 *
 * SUMMARY:
 *   This constraint instantiates the 'contextual necessity' reading of the
 *   contested humane-treatment standard kernel. Common Article 3 of the
 *   Geneva Conventions establishes a floor for detainee protection that
 *   states nominally accept. The contextual necessity reading interprets that
 *   floor as a baseline subject to override when operational security
 *   imperatives so require: humane treatment becomes context-dependent,
 *   security agencies gain discretion to define it, and high-value detainees
 *   face conditional protection. This reading competes with absolute
 *   prohibition (no circumstances permit enhanced interrogation) and
 *   proportionality balancing (security and dignity are always weighed). The
 *   structural delta from the other readings: discretion shifts to the
 *   security apparatus; victims are narrowed to those assessed as high-value
 *   targets; the victim set shrinks because low-value detainees retain
 *   baseline protection while high-value ones do not.
 *
 * KEY AGENTS:
 *   - Security agencies: institutional agenda-setter with discretionary authority over technique classification and necessity determination; benefits from the flexibility the reading provides
 *   - High-value detainees: powerless payer, trapped, subjected to enhanced interrogation justified by necessity; their status as targets is determined unilaterally by security apparatus
 *   - Detainees in operational theaters: powerless payer, trapped in regional contexts where baseline review is minimal and contextual humane treatment is operationally determined
 *   - ICRC and humanitarian bodies: beneficiary-observer, constrained by dependence on state cooperation; mandate to enforce standards conflicts with discretionary interpretation
 *   - States party to convention: institutional agenda-setter and beneficiary; interpret Common Article 3 through security doctrine and preserve discretion
 *   - Domestic courts and oversight bodies: observer, institutionally captured through deference to executive security judgment and dependence on state-provided evidence
 *   - Torture prohibition advocates: excluded from interpretation process, systematically kept out by operational security classification and framing of necessity as executive determination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.78).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.81).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.78).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Contextual Necessity Reading: Enhanced Interrogation Under National Security Imperative").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "international_humanitarian_law/state_security").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, '4b2c6d8d-1136-4356-9fa0-4ec8627d6d3d').
narrative_ontology:cs_kernel_codification('4b2c6d8d-1136-4356-9fa0-4ec8627d6d3d', fixed_text).
narrative_ontology:cs_authority_grounding('4b2c6d8d-1136-4356-9fa0-4ec8627d6d3d', extraction).
narrative_ontology:cs_interpretation_layer_present('4b2c6d8d-1136-4356-9fa0-4ec8627d6d3d').
narrative_ontology:cs_reading_relation('4b2c6d8d-1136-4356-9fa0-4ec8627d6d3d', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('4b2c6d8d-1136-4356-9fa0-4ec8627d6d3d', humane_treatment_standard__proportionality_balancing, influences).
narrative_ontology:cs_axiom('4b2c6d8d-1136-4356-9fa0-4ec8627d6d3d', foundational, necessity_exception_doctrine).
narrative_ontology:cs_axiom_status(necessity_exception_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('4b2c6d8d-1136-4356-9fa0-4ec8627d6d3d', necessity_exception_doctrine, deontological).
narrative_ontology:cs_axiom('4b2c6d8d-1136-4356-9fa0-4ec8627d6d3d', foundational, security_agency_discretion_legitimacy).
narrative_ontology:cs_axiom_status(security_agency_discretion_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('4b2c6d8d-1136-4356-9fa0-4ec8627d6d3d', security_agency_discretion_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('4b2c6d8d-1136-4356-9fa0-4ec8627d6d3d', common_article_3_contextual_baseline).
narrative_ontology:cs_drift_state('4b2c6d8d-1136-4356-9fa0-4ec8627d6d3d', contemporary_post_911_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4b2c6d8d-1136-4356-9fa0-4ec8627d6d3d', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, state_apparatus).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, high_value_detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, detainees_in_operational_theaters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, international_humanitarian_bodies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, states_party_to_convention).
narrative_ontology:constraint_vindicates(humane_treatment_standard__contextual_necessity, security_imperative_doctrine).
narrative_ontology:constraint_vindicates(humane_treatment_standard__contextual_necessity, necessity_exception_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and apply Common Article 3 through an operational security lens. Define 'humane treatment' contextually — what constitutes acceptable interrogation intensity depends on the detainee's intelligence value, the operational urgency, and the assessed threat level. They justify enhanced techniques as necessary for extracting actionable intelligence that prevents mass casualties. Discretion over technique classification and necessity determination resides with them.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, security_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Individuals assessed to possess time-sensitive intelligence about ongoing threats. Subjected to enhanced interrogation techniques (sleep deprivation, stress positions, environmental manipulation, sensory overstimulation) justified under necessity. Their status as 'high-value' becomes the criterion that conditional protections withdraw. They have no mechanism to contest the assessment or exit the constraint.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, high_value_detainees, payer,
    powerless, immediate, trapped, local).

% Captured combatants and detainees in active conflict zones where administrative review is minimal. Humane treatment becomes situational — the operational environment, resource scarcity, and perceived threat intensity determine what baseline protections apply. Release, transfer, or long-term detention are options controlled entirely by detaining authorities; resistance meets further restriction.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, detainees_in_operational_theaters, payer,
    powerless, biographical, trapped, regional).

% Organizations like the ICRC that monitor detainee treatment face a structural dilemma: they can observe conditions and report violations, but under this reading their authority to define violations is limited by the security apparatus's necessity claims. They benefit from access to detention sites (which requires institutional cooperation) but their mandate to enforce absolute standards is constrained by security discretion.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, international_humanitarian_bodies, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__contextual_necessity, international_humanitarian_bodies, observer).

% Signatories to the Geneva Conventions who interpret Common Article 3 through domestic security doctrine. This reading grants them the structural authority to balance detainee protections against state security; they define when necessity overrides baseline humane treatment and what enhanced techniques are permitted. The constraint persists because states benefit from the discretion it preserves.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, states_party_to_convention, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__contextual_necessity, states_party_to_convention, beneficiary).

% Judicial and oversight bodies tasked with holding states accountable face structural capture: they depend on state-provided evidence about necessity claims, they lack real-time access to interrogation decisions, and their authority to overturn security agency determinations is bounded by deference to executive judgment on national security. They can review after the fact but cannot effectively prevent the constraint's operation.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, domestic_courts_and_human_rights_bodies, observer,
    organized, generational, constrained, national).

% Human rights organizations, legal scholars, and NGOs that reject the necessity framing entirely. They are systematically excluded from the interpretive process — their objections are framed as ideological rather than legal, their empirical claims about interrogation efficacy are treated as uninformed, and their participation in security determinations is precluded on grounds of operational sensitivity. They would argue for absolute prohibition but have no seat at the constraint-setting table.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, torture_prohibition_advocates, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__contextual_necessity, security_agencies).
narrative_ontology:fixing_cost_class(humane_treatment_standard__contextual_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state security apparatus discretion with a baseline of 'humane' treatment: establishes a procedural framework for detainee handling that permits operational judgment to modulate protection levels based on intelligence value and urgency, while nominally retaining Common Article 3 compliance.
% TRANSFER_FUNCTION: Transfers discretionary authority from objective humane-treatment standards to security agencies: the right to define 'humane' in context shifts from universal baseline to situational determination. Moves detainees from protected status to conditional status based on assessed intelligence value. Extracts compliance from detainees through enhanced interrogation justified as necessary.
% ABSENT_VOICES: Torture prohibition advocates, detainee legal representatives, and international human rights monitoring bodies that would contest necessity claims are structurally excluded from the interpretation process. They are kept out by operational security classification, by the designation of necessity as an executive judgment immune from external second-guessing, and by the framing of baseline protections as context-dependent rather than universal. Their absence makes the necessity claim unchallengeable.
% DISAPPEARANCE_RATIONALE: If the contextual necessity reading and its discretionary framework disappeared overnight, security agencies would lose the interpretive flexibility to justify enhanced techniques; detainees would revert to absolute Common Article 3 protections; interrogation programs would contract; state intelligence gathering would require alternative methods; the institutional capacity to balance security urgency against detainee protections would collapse and be replaced by prohibition. The global security architecture depends on this discretion.
% FOUNDING_PROBLEM: Common Article 3 was written for interstate conflict contexts with predictable, bounded detention. Contemporary operational realities include non-state actors, cell-based networks with time-critical intelligence, asymmetric warfare where detainee information can prevent mass casualty attacks within hours, and long-term detention in resource-constrained zones. A rigid baseline applied across all contexts is operationally unworkable.
% FOUNDING_PROBLEM_CORROBORATION: Security agencies and state governments attest the problem is live and severe — cite specific historical cases where delayed interrogation permitted attacks, assert that rigid baseline hampers intelligence gathering, argue necessity exceptions are standard in other legal domains (medical ethics, self-defense law). ICRC and human rights bodies contest the founding problem framing itself — assert that the empirical premise (that enhanced interrogation yields reliable actionable intelligence superior to rapport-based methods) is unsupported; cite studies showing enhanced techniques produce false confessions; argue the problem is overstated to justify predetermined practices. Academic interrogation research from outside both camps is mixed but leans toward skepticism of necessity claims.
narrative_ontology:disappearance_verdict(humane_treatment_standard__contextual_necessity, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__contextual_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(humane_treatment_standard__contextual_necessity, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__contextual_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__contextual_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.58 to 0.78 over the interval) because the constraint transfers discretionary authority to the security apparatus — what counts as 'humane' is no longer objective but situationally determined, allowing increasingly intensive extraction from detainees justified by necessity claims. Suppression is similarly high (0.64 to 0.81) because the constraint's persistence depends on actively suppressing the absolute prohibition reading and its advocates; detainees are trapped without recourse; and oversight bodies lack real-time intervention capacity. Theater ratio is moderate and rises (0.25 to 0.42) because while enhanced interrogation is genuinely functional for intelligence extraction, an increasing share of enforcement activity is devoted to justifying techniques against criticism rather than defending against actual imminent threats — the necessity framing becomes increasingly theatrical as it shifts from specific operational cases to standing doctrine. The measurement series documents the constraint's hardening: extractiveness plateaus at t=30, suggesting the framework has reached steady state; suppression requirements rise through t=25 then plateau, indicating enforcement infrastructure has solidified; theater rises through t=25 then stabilizes, showing rhetorical justification has become routinized. All three metrics share one time grid (every metric authored at every time point) so the engine can detect simultaneous shifts.
 *
 * PERSPECTIVAL GAP:
 *   The security agency seat and the detainee seats should compute wildly differently. From the agency perspective, the arrangement solves a genuine coordination problem (how to extract operationally necessary information within some baseline constraint); from the detainee perspective it is pure extraction with a fig-leaf of protection. The agency views necessity as a legitimate exception; detainees view it as the erasure of protection itself. Domestic courts sit between, structurally captured: they acknowledge necessity exceptions in theory but defer to the agency's necessity determinations in practice. The engine computes this per-seat divergence from the structural data — the reading itself does not resolve which perspective is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Security agencies are direct beneficiaries (d near 0.0 — they collect discretionary authority, control the interpretation, benefit from flexibility). High-value detainees are full targets (d near 1.0 — extraction is intensive, exit is impossible, identity-locked by virtue of the assessment itself). Detainees in operational theaters are high-d payers (trapped, powerless, subject to contextual humane treatment that dissolves baseline protection). ICRC and humanitarian bodies sit at moderate d (0.4–0.5) — they benefit from institutional access and coordination function (baseline remains nominally in place) but bear costs through mandate constraint and effective neutering of enforcement capacity. States themselves are complex: as signatories they nominally shoulder duty (moderate d), but as beneficiaries of the discretion they collect (d closer to beneficiary end). Domestic courts are near symmetric (0.5) — they participate in the constraint's legitimation but are also captured by it. Torture prohibition advocates are structurally invisible in this reading (excluded, analytical seat, d undefined because they collect nothing and bear no contractual cost, though they bear moral and epistemic cost).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is explicitly tangled rope, not snare, because a genuine coordination function (extracting time-critical intelligence that prevents mass casualty attacks) exists alongside the asymmetric extraction. The classification is tight but defensible on the structural data: beneficiaries exist (security apparatus, state), victims exist (detainees), active enforcement is required to sustain it (suppression of absolute prohibition advocates, maintenance of necessity discretion against international challenge), and genuine coordination happens (baseline humane treatment exists for low-value detainees; only the conditional narrowing on high-value targets is the extractive overlay). If the founding problem is genuine (time-critical intelligence prevents attacks), the coordination is real. If the founding problem is overstated to justify predetermined practices (enhanced interrogation is not actually more effective than rapport-based methods), the constraint collapses toward snare (the coordination story is cover). The three-way kernel contest (absolute prohibition, contextual necessity, proportionality) prevents mandatrophy resolution: no single axis (necessity exception, balanced determination, absolute baseline) commands unanimous acceptance, so the constraint persists because the three readings remain locked in stalemate rather than converging on any single verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_empirics_vs_doctrine,
    'Do enhanced interrogation techniques actually produce superior actionable intelligence compared to rapport-based or other methods, or is the necessity claim primarily a doctrinal justification for predetermined practices?',
    'Comparative empirical research on interrogation outcomes, declassified effectiveness assessments, natural experiments where states have abandoned enhanced techniques and measured intelligence collection impact. External auditors (not security agencies, not prohibition advocates) analyzing interrogation efficacy.',
    'If enhanced techniques produce materially superior intelligence, the coordination function is real and the constraint remains tangled rope. If they produce equivalent or inferior results, the founding problem claim dissolves and the constraint collapses toward pure snare — the necessity framing is rhetorical cover for extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_empirics_vs_doctrine, empirical, 'Whether necessity claims rest on genuine interrogation efficacy or are doctrinal justifications.').

omega_variable(
    discretion_boundary_instability,
    'What prevents the security apparatus''s discretionary authority to define ''humane'' in necessity cases from expanding to routine cases, progressively narrowing the victim set and detaching the reading from its contextual premise?',
    'Documentation of refusals to apply enhanced techniques in non-necessity scenarios; audited case records showing necessity determinations remain bounded to time-critical intelligence; longitudinal analysis of the scope creep of techniques across threat-level classifications.',
    'If the boundary holds, the reading remains distinguishable from snare (contextualized extraction is bounded). If discretion systematically expands beyond necessity cases, the reading has collapsed toward snare and the ''contextual'' qualifier is nominal — the constraint is actually unlimited discretion dressed in necessity language.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discretion_boundary_instability, empirical, 'Whether discretionary authority remains bounded to necessity cases or expands toward routine interrogation.').

omega_variable(
    absolute_prohibition_foreclosure,
    'Does the contextual necessity reading logically foreclose the absolute prohibition reading, or do both remain coherent within different state frameworks?',
    'Comparative constitutional and treaty interpretation: can a state that adopts contextual necessity as its Common Article 3 reading acknowledge the absolute prohibition reading as internally legitimate? Are the core premises genuinely contradictory, or do they represent different valid choices about how to bound state power?',
    'If foreclosure is genuine, the two readings cannot coexist in any single state''s legal order. If coexistence is possible, they are alternative readings held by different states and neither is logically eliminated. This determines whether the kernel contest is resolvable (one reading wins) or permanent (both remain live).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_prohibition_foreclosure, conceptual, 'Whether contextual necessity and absolute prohibition logically foreclose each other or can coexist.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of torture prohibition advocates structural (they are excluded by institutional rules and classification barriers) or partly internalized (they have internalized deference to executive security judgment such that they self-suppress)?',
    'Analysis of participation patterns when classification barriers are lowered; examination of advocacy outcomes when these actors gain institutional access; post-exit trajectory analysis to distinguish structural from internalized suppression.',
    'If structural, removing institutional barriers could reactivate advocacy and shift the reading toward proportionality balancing. If internalized, suppression persists even when barriers lower — the constraint has durably captured even those excluded from it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of advocates is structural exclusion or internalized deference.').

omega_variable(
    kernel_reading_choice_as_false_summit,
    'Is the contextual necessity reading presented as a natural reading of Common Article 3''s text, or is it a constructed reading that benefits the security apparatus by preserving discretion?',
    'Linguistic and historical analysis of the Common Article 3 text and its negotiation; comparison of the reading to the text''s surface structure; examination of whether the reading was adopted because it fits the text or because it preserves institutional interests.',
    'If the reading is textually justified, it is legitimate interpretation. If it is a false summit — a constructed reading benefiting a specific actor and presented as natural — the constraint becomes a candidate for FSM reclassification and unmasking of the beneficiary structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_choice_as_false_summit, conceptual, 'Whether contextual necessity is a natural reading of Common Article 3 or a constructed interpretation benefiting security agencies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__contextual_necessity, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(huma_tr_t0, observed).
narrative_ontology:measurement(huma_tr_t5, humane_treatment_standard__contextual_necessity, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(huma_tr_t5, observed).
narrative_ontology:measurement(huma_tr_t10, humane_treatment_standard__contextual_necessity, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(huma_tr_t10, observed).
narrative_ontology:measurement(huma_tr_t15, humane_treatment_standard__contextual_necessity, theater_ratio, 15, 0.35).
narrative_ontology:measurement_basis(huma_tr_t15, observed).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__contextual_necessity, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(huma_tr_t20, observed).
narrative_ontology:measurement(huma_tr_t25, humane_treatment_standard__contextual_necessity, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(huma_tr_t25, observed).
narrative_ontology:measurement(huma_tr_t30, humane_treatment_standard__contextual_necessity, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(huma_tr_t30, observed).
narrative_ontology:measurement(huma_tr_t35, humane_treatment_standard__contextual_necessity, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(huma_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__contextual_necessity, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(huma_be_t0, observed).
narrative_ontology:measurement(huma_be_t5, humane_treatment_standard__contextual_necessity, base_extractiveness, 5, 0.64).
narrative_ontology:measurement_basis(huma_be_t5, observed).
narrative_ontology:measurement(huma_be_t10, humane_treatment_standard__contextual_necessity, base_extractiveness, 10, 0.69).
narrative_ontology:measurement_basis(huma_be_t10, observed).
narrative_ontology:measurement(huma_be_t15, humane_treatment_standard__contextual_necessity, base_extractiveness, 15, 0.73).
narrative_ontology:measurement_basis(huma_be_t15, observed).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__contextual_necessity, base_extractiveness, 20, 0.76).
narrative_ontology:measurement_basis(huma_be_t20, observed).
narrative_ontology:measurement(huma_be_t25, humane_treatment_standard__contextual_necessity, base_extractiveness, 25, 0.77).
narrative_ontology:measurement_basis(huma_be_t25, observed).
narrative_ontology:measurement(huma_be_t30, humane_treatment_standard__contextual_necessity, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(huma_be_t30, observed).
narrative_ontology:measurement(huma_be_t35, humane_treatment_standard__contextual_necessity, base_extractiveness, 35, 0.78).
narrative_ontology:measurement_basis(huma_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__contextual_necessity, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(huma_su_t0, observed).
narrative_ontology:measurement(huma_su_t5, humane_treatment_standard__contextual_necessity, suppression_requirement, 5, 0.68).
narrative_ontology:measurement_basis(huma_su_t5, observed).
narrative_ontology:measurement(huma_su_t10, humane_treatment_standard__contextual_necessity, suppression_requirement, 10, 0.72).
narrative_ontology:measurement_basis(huma_su_t10, observed).
narrative_ontology:measurement(huma_su_t15, humane_treatment_standard__contextual_necessity, suppression_requirement, 15, 0.75).
narrative_ontology:measurement_basis(huma_su_t15, observed).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__contextual_necessity, suppression_requirement, 20, 0.78).
narrative_ontology:measurement_basis(huma_su_t20, observed).
narrative_ontology:measurement(huma_su_t25, humane_treatment_standard__contextual_necessity, suppression_requirement, 25, 0.8).
narrative_ontology:measurement_basis(huma_su_t25, observed).
narrative_ontology:measurement(huma_su_t30, humane_treatment_standard__contextual_necessity, suppression_requirement, 30, 0.81).
narrative_ontology:measurement_basis(huma_su_t30, observed).
narrative_ontology:measurement(huma_su_t35, humane_treatment_standard__contextual_necessity, suppression_requirement, 35, 0.81).
narrative_ontology:measurement_basis(huma_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(humane_treatment_standard__contextual_necessity, 0.18).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__proportionality_balancing).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, intelligence_extraction_justification).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, detainee_status_determination).

% DUAL FORMULATION NOTE:
% Part of the humane_treatment_standard constraint family. This reading (contextual_necessity) instantiates discretionary authority interpretation; the absolute_prohibition reading instantiates non-derogable baseline interpretation; the proportionality_balancing reading instantiates structured balancing interpretation. All three are readings of the same kernel (Common Article 3's 'humane treatment' clause) with different ε values and structural consequences. Contextual necessity is the most extractive (highest ε) because it concentrates discretion; absolute prohibition is the least extractive (lowest ε) because it removes discretion entirely; proportionality balancing sits between (medium ε) because it distributes discretion across reviewable determinations. Each reading affects the others: adoption of contextual necessity suppresses absolute prohibition advocacy; evidence of necessity empirics affects the boundary between contextual necessity and proportionality balancing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(humane_treatment_standard__contextual_necessity, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
