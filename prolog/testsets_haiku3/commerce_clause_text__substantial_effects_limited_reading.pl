% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__substantial_effects_limited_reading, []).

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
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Commerce Clause Substantial Effects Limited Reading
 *   domain: constitutional/federalism
 *
 * SUMMARY:
 *   The substantial effects limited reading of the commerce clause is a
 *   contested judicial doctrine holding that federal power extends to
 *   intrastate economic activity with substantial effects on interstate
 *   commerce, PROVIDED the regulation is genuinely economic (not disguised
 *   police power) and meets a jurisdictional nexus requirement. The
 *   constraint operates through boundary policing: courts distinguish
 *   economic from non-economic regulation and genuine from pretextual
 *   commerce rationales. The reading sits between the originalist narrow
 *   reading (commerce limited to trade crossing borders) and the expansive
 *   federal reading (all economically consequential activity). The story
 *   captures the constraint AS THIS READING FRAMES IT: the boundary-enforcing
 *   mechanism that permits federal reach into genuinely economic local
 *   activity while nominally reserving state police power for non-economic
 *   matters. Other readings see the same constitutional text through
 *   different frames and structure their constraints accordingly.
 *
 * KEY AGENTS:
 *   - Federal economic regulators (EPA, DOL, FTC, NLRB) — agenda-setter; set the boundary between economic and non-economic; benefit from authority to reach intrastate economic activity
 *   - Interstate commerce participants (national firms, multi-state markets) — beneficiary; access uniform federal floors on labor, environment, competition that prevent fragmented state regulation
 *   - State governments defending police power — payer; lose autonomy over local activity recharacterized as economic and substantially affecting commerce
 *   - Courts (Supreme Court especially) — agenda-setter; police the economic/non-economic distinction and jurisdictional nexus requirement through doctrine
 *   - Local regulatory autonomy defenders (cities, community groups) — payer; face preemption when local regulation is reframed as commerce regulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.58).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.42).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Commerce Clause Substantial Effects Limited Reading").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, '3c27bd7c-f340-444a-a84a-3f5d08bdd4c7').
narrative_ontology:cs_kernel_codification('3c27bd7c-f340-444a-a84a-3f5d08bdd4c7', fixed_text).
narrative_ontology:cs_authority_grounding('3c27bd7c-f340-444a-a84a-3f5d08bdd4c7', lineage).
narrative_ontology:cs_interpretation_layer_present('3c27bd7c-f340-444a-a84a-3f5d08bdd4c7').
narrative_ontology:cs_reading_relation('3c27bd7c-f340-444a-a84a-3f5d08bdd4c7', commerce_clause_text__expansive_federal_reading, influences).
narrative_ontology:cs_reading_relation('3c27bd7c-f340-444a-a84a-3f5d08bdd4c7', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_axiom('3c27bd7c-f340-444a-a84a-3f5d08bdd4c7', foundational, economic_noneconomic_distinction_constitutionally_meaningful).
narrative_ontology:cs_axiom_status(economic_noneconomic_distinction_constitutionally_meaningful, holdable).
narrative_ontology:cs_axiom_grounding('3c27bd7c-f340-444a-a84a-3f5d08bdd4c7', economic_noneconomic_distinction_constitutionally_meaningful, conventional).
narrative_ontology:cs_axiom('3c27bd7c-f340-444a-a84a-3f5d08bdd4c7', foundational, jurisdictional_nexus_requirement_genuine_limit).
narrative_ontology:cs_axiom_status(jurisdictional_nexus_requirement_genuine_limit, holdable).
narrative_ontology:cs_axiom_grounding('3c27bd7c-f340-444a-a84a-3f5d08bdd4c7', jurisdictional_nexus_requirement_genuine_limit, empirically_contingent).
narrative_ontology:cs_axiom('3c27bd7c-f340-444a-a84a-3f5d08bdd4c7', secondary, state_police_power_reserved_for_non_economic_regulation).
narrative_ontology:cs_axiom_status(state_police_power_reserved_for_non_economic_regulation, holdable).
narrative_ontology:cs_axiom_grounding('3c27bd7c-f340-444a-a84a-3f5d08bdd4c7', state_police_power_reserved_for_non_economic_regulation, deontological).
narrative_ontology:cs_reference_frame('3c27bd7c-f340-444a-a84a-3f5d08bdd4c7', enumerated_federal_power_with_state_police_reservation).
narrative_ontology:cs_drift_state('3c27bd7c-f340-444a-a84a-3f5d08bdd4c7', contemporary_2025, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3c27bd7c-f340-444a-a84a-3f5d08bdd4c7', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, federal_economic_regulators).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, interstate_commerce_participants).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, state_police_power_interests).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, local_regulatory_autonomy_defenders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, local_regulatory_autonomy_defenders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal agencies (EPA, DOL, FTC, etc.) regulate genuinely economic intrastate activity on the theory that it substantially affects interstate commerce. They set the jurisdictional boundary by litigating the nexus requirement and distinguishing economic from non-economic regulation. They benefit from stable commerce clause authority to reach local activity without pretextual relabeling.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_economic_regulators, agenda_setter,
    institutional, generational, analytical, national).

% National businesses and interstate market participants benefit from uniform federal regulation of local activity that substantially affects national markets. The constraint permits federal floors on labor standards, pollution, safety, and anti-competitive practices that their interstate competitors would otherwise face with fragmented state regulation. They have exit to compliant states and to lobbying for federal minimum standards.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, interstate_commerce_participants, beneficiary,
    powerful, generational, mobile, national).

% States asserting traditional police power over local health, safety, morals, and local land use face federal preemption when activity is recharacterized as economic and substantial-effects-connected. They lose regulatory autonomy over genuinely local matters when they are reframed as affecting interstate commerce. The constraint's category boundary policing is the mechanism that strips authority.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, state_police_power_interests, payer,
    institutional, generational, constrained, regional).

% Cities and localities defending zoning, environmental protection, and community standards against federal preemption under commerce clause reach. They bear the cost of having local regulations voided when deemed pretextual commerce regulation. Some benefit when federal baselines prevent a race to the bottom (labor standards, environmental minimums).
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, local_regulatory_autonomy_defenders, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, local_regulatory_autonomy_defenders, beneficiary).

% Courts (especially Supreme Court) police the economic/non-economic and pretextual/genuine distinctions through doctrine (rational basis, strict scrutiny, dormant commerce clause analysis). They maintain the categorical framework that defines the constraint's scope and enforce the jurisdictional nexus requirement. Their doctrine determinations set boundaries for all other actors.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, boundary_enforcement_courts, agenda_setter,
    institutional, generational, analytical, national).

% Advocates for local moral, cultural, and community regulation (obscenity, family law, cultural preservation, religious liberty) are excluded from the commerce clause conversation because the constraint treats these as non-economic and thus outside federal reach. They would argue the economic/non-economic distinction is itself a political choice, not a natural boundary, but that argument stays outside the reading's framework.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, non_economic_regulation_advocates, excluded,
    moderate, biographical, constrained, local).

% Scholarship communities offering alternative readings of the commerce clause text. Originalists argue substantial effects doctrine strays from the historical meaning (trade crossing borders). They contribute to the legal conversation but their preferred framework competes with the substantial effects reading rather than being subsumed within it.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, originalist_originalism_scholars, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__substantial_effects_limited_reading, federal_economic_regulators).
narrative_ontology:fixing_cost_class(commerce_clause_text__substantial_effects_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified national market by allowing federal regulation of genuinely economic intrastate activity that substantially affects interstate commerce, preventing a prisoner's dilemma where states would otherwise impose incoherent local requirements on multi-state firms.
% TRANSFER_FUNCTION: Transfers regulatory authority from states and localities to federal agencies over categorically economic activity deemed to have substantial interstate effects. States retain police power for non-economic regulation but lose autonomy over local economic activity recharacterized as affecting commerce.
% ABSENT_VOICES: Communities and movements defending non-economic local regulation (cultural, moral, zoning autonomy) are excluded because the reading treats non-economic regulation as outside the commerce clause frame entirely — they would contend the economic/non-economic boundary is itself a political choice, not a constitutional given.
% DISAPPEARANCE_RATIONALE: If the substantial effects doctrine and its economic/non-economic boundary vanished, federal commerce clause reach would collapse to trade crossing borders (the originalist reading) or expand to all activity with aggregate economic effects (the expansive reading). Interstate businesses would face fragmented state regulation, or federal police power would expand massively. Market organization would reorganize fundamentally around either state or federal supremacy.
% FOUNDING_PROBLEM: Early commerce clause doctrine created uncertainty about federal reach into local economic activity: could Congress regulate local manufacturing that affected interstate trade? Could states impose protectionist local regulations? The substantial effects test (Wickard, Gonzales) solved the problem by allowing federal reach into local activity when genuinely economically connected, while preserving state police power for non-economic regulation.
% FOUNDING_PROBLEM_CORROBORATION: Federal regulators and interstate commerce interests attest the problem remains live (fragmented state regulation hampers national markets). State governments and originalist scholars attest the problem is solved and the doctrine has metastasized to justify federal reach over non-economic activity dressed as economic (family law, local land use, cultural standards reframed as economic). Constitutional law scholarship and Supreme Court dissents document the contest; neither side is dominant.
narrative_ontology:disappearance_verdict(commerce_clause_text__substantial_effects_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__substantial_effects_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__substantial_effects_limited_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.58: the constraint operates a real coordination function (unified national market, prevention of a regulatory prisoner's dilemma) but also transfers significant authority from states to federal agencies. The boundary-policing mechanism itself is extractive — it requires states to accept federal characterization of what counts as genuinely economic. The measurement series shows stability in extractiveness from 2010–2025, with a gentle rise 1995–2010 as the boundary doctrine hardened post-Wickard and Gonzales. Suppression at 0.42: the constraint requires active court enforcement of the economic/non-economic distinction and jurisdictional nexus test, but does not require coercion against unwilling states at the enforcement stage (states accommodate federal preemption through formal supremacy doctrine). Theater at 0.31 reflects increasing rhetorical justification of federal reach — courts cite aggregate effects rhetoric more elaborately over time, while the underlying boundary-enforcement mechanism remains the same. Accessibility collapse at 0.72: once a court determines an activity is economic and substantially affects interstate commerce, alternatives for state regulation collapse nearly completely (preemption is near-total, short of constitutional amendment). Resistance at 0.68: states mount consistent legal and political resistance to federal expansion, originalist scholars challenge the doctrine, and recent court signals suggest the boundary may be narrowing again (Commerce Clause Case law 2020s, NFIB dissents). The constraint is actively contested.
 *
 * PERSPECTIVAL GAP:
 *   Federal seat: the constraint is genuine coordination solving market fragmentation and enabling national firms to plan. State seat: the constraint is extraction via boundary redefinition — what was formerly pure state police power (local economic regulation) is recharacterized as commerce regulation subject to federal preemption. The reading's own framework assumes the economic/non-economic distinction is meaningful and constitutionally grounded (not contingent). A sibling reading (expansive federal) would say the distinction is unstable and economically nonsensical (all substantive regulation affects economic incentives). The originalist reading would say the boundary should be much narrower (only trade crossing borders, not all economically consequential activity). Each reading has a different ε for the same constitutional text — the referent is the standing arrangement the reading describes, assessed by its own lights.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal economic regulators (institutional power, analytical exit) sit as beneficiaries — they set the agenda, police the boundary, and expand federal reach. Interstate commerce participants (powerful, mobile exit) are beneficiaries — they benefit from uniform regulation and can exit to compliant jurisdictions. States (institutional power, constrained exit) are payers — they lose autonomy over local economic regulation and cannot exit without constitutional amendment. Local defenders (moderate power, constrained exit) are payers — they lose the ability to regulate local economic activity as purely local. The directionality derivation should put federal seats near d=0.2 (beneficiary) and state seats near d=0.75 (payer). Courts as boundary-enforcers sit near d=0.5 (symmetric: they coordinate the system but also wield the suppressive power of preemption doctrine).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented state regulation of economically interconnected activity) remains live in federal regulator and interstate commerce participant assessment, but is increasingly challenged as solved or as having been replaced by federal overreach into non-economic matters (family law, local land use, cultural standards reframed as economic). The theater ratio rise from 0.22 to 0.31 signals a growing gap between the cosmetic justification (substantial effects on interstate commerce) and the mechanical enforcement (boundary policing that favors federal reach). Courts have signaled narrowing — NFIB v. Sebelius (2012) and Shelby County (2013) suggest the boundary may be tightening. This is not yet a full mandatrophy (the founding problem is not completely dead, the constraint is still enforced by a legitimized institution), but the divergence between the stated coordination function and the actual boundary-expansion mechanism is widening. An omega captures this: is the constraint primarily coordination-via-boundary or extraction-via-category?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_noneconomic_boundary_contingency,
    'Is the economic/non-economic distinction a constitutionally grounded, objective boundary, or a contingent categorization that reflects current regulatory priorities?',
    'Genealogical analysis of the boundary''s historical emergence (Commerce Clause Case law trajectory), comparative analysis of what counts as ''economic'' across courts and regulatory eras, and formal attempt to derive the boundary from first principles of the commerce clause text.',
    'If objective and natural, the constraint''s suppression and extraction are legitimate category-enforcement. If contingent, the boundary-policing mechanism is a pure extraction device masquerading as neutral application of a constitutional distinction. The classification would shift from tangled_rope (coordination + extraction) toward snare (boundary drawn pretextually).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_noneconomic_boundary_contingency, conceptual, 'Whether the economic/non-economic distinction is a constitutionally natural boundary or a contingent policy choice.').

omega_variable(
    substantial_effects_scope_creep,
    'Has the jurisdictional nexus requirement (substantial effects on interstate commerce) actually constrained federal reach, or has it become a formality that permits federal regulation of nearly all intrastate economic activity?',
    'Empirical survey of commerce clause challenges to federal regulation 1995–2025: what fraction of regulations have been invalidated for lack of substantial effects? What activities have been held to substantially affect interstate commerce?',
    'If the nexus is genuinely constraining (>5% invalidation rate, meaningful categories of activity held non-affecting), the constraint enforces real limits. If it is nearly universally satisfied (<1% invalidation, all economically consequential activity held substantially affecting), the constraint is theater — the pretextual non-pretextual distinction collapses and federal reach is effectively unlimited.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantial_effects_scope_creep, empirical, 'Whether the substantial effects test actually limits federal commerce clause reach or has become de facto unlimited.').

omega_variable(
    state_police_power_preservation_actual,
    'Do states meaningfully retain police power over genuinely non-economic matters (family law, cultural regulation, moral rules), or has the federal police power expanded to encompass these via commerce clause characterization?',
    'Case law analysis: are there categories of regulation that courts recognize as genuinely non-economic and beyond federal commerce clause reach? Instances where states have successfully defended non-economic regulation against federal preemption?',
    'If states retain meaningful non-economic policing autonomy, the reading''s coordination/extraction balance is accurate. If the non-economic exception has been absorbed into federal police power (via commerce clause or other enumerated powers), the coordination function is illusory — the constraint only appears to preserve state power; it actually transfers all substantive regulation to federal hands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_police_power_preservation_actual, empirical, 'Whether state police power is genuinely preserved for non-economic regulation or has been subsumed into federal authority.').

omega_variable(
    kernel_reading_alternative_framing,
    'Does the expansive_federal_reading foreclose this reading''s core premise (that the economic/non-economic distinction is constitutionally meaningful), or can both readings coexist in different interpretive communities?',
    'Assessment of whether holding the expansive reading''s core premise (all economically consequential activity is within federal reach) logically requires rejecting this reading''s core premise (some activity is non-economic and reserved to states) within the same decision-making framework. Or do they coexist as readings that different judicial coalitions and legal communities simply hold simultaneously?',
    'If the readings foreclose each other, the engine should compute the relationship as forecloses (rare). If they coexist as live positions in different parts of the legal system, the relationship is coexists_with (more likely). This determines the network topology among the constraint family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framing, conceptual, 'Whether the expansive and substantial-effects readings are logically incompatible or can coexist as different live interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2005, 0.26).
narrative_ontology:measurement(comm_tr_t2010, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(comm_tr_t2015, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(comm_tr_t2020, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2020, 0.31).
narrative_ontology:measurement(comm_tr_t2025, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2025, 0.31).

% Extraction over time
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2000, 0.51).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2005, 0.54).
narrative_ontology:measurement(comm_be_t2010, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2010, 0.56).
narrative_ontology:measurement(comm_be_t2015, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2015, 0.57).
narrative_ontology:measurement(comm_be_t2020, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement(comm_be_t2025, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2000, 0.37).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2005, 0.39).
narrative_ontology:measurement(comm_su_t2010, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(comm_su_t2015, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2015, 0.41).
narrative_ontology:measurement(comm_su_t2020, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement(comm_su_t2025, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__substantial_effects_limited_reading, 0.12).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, dormant_commerce_clause_preemption).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, state_regulatory_autonomy_constraint).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-member constraint family sharing the commerce_clause_text kernel. The three readings (originalist_narrow, substantial_effects_limited, expansive_federal) each instantiate distinct constraint structures with different ε values, beneficiary structures, and suppression mechanisms. Each story stands alone as a coherent constraint; the network links trace the genealogical relationships (which reading influences which) and the kernel identity that unifies them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_text__substantial_effects_limited_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
