% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__individual_right_reading, []).

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
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading (Heller/Bruen framework)
 *   domain: constitutional_law/rights_jurisprudence
 *
 * SUMMARY:
 *   The individual-right reading of the Second Amendment (cemented in Heller
 *   2008, incorporated in McDonald 2010, methodologically entrenched in Bruen
 *   2022) operates as a constraint that removes firearms regulation from
 *   democratic politics and places it under a judicial history-and-tradition
 *   test. The reading claims to coordinate a natural right of self-defense;
 *   its operation extracts regulatory authority from states and localities
 *   while concentrating interpretive power in a federal judiciary shaped by
 *   the gun-rights movement. The claimed type is tangled_rope — genuine
 *   coordination (decentralized defense capacity) fused with asymmetric
 *   extraction (state regulatory foreclosure, concentrated human cost on
 *   powerless populations).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.68).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.42).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment Individual Right Reading (Heller/Bruen framework)").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional_law/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, 'ffc81d38-c191-493a-b548-5a0f69638498').
narrative_ontology:cs_kernel_codification('ffc81d38-c191-493a-b548-5a0f69638498', fixed_text).
narrative_ontology:cs_authority_grounding('ffc81d38-c191-493a-b548-5a0f69638498', lineage).
narrative_ontology:cs_interpretation_layer_present('ffc81d38-c191-493a-b548-5a0f69638498').
narrative_ontology:cs_reading_relation('ffc81d38-c191-493a-b548-5a0f69638498', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('ffc81d38-c191-493a-b548-5a0f69638498', second_amendment_scope__civic_right_reading, coexists_with).
narrative_ontology:cs_axiom('ffc81d38-c191-493a-b548-5a0f69638498', foundational, individual_self_defense_natural_right).
narrative_ontology:cs_axiom_status(individual_self_defense_natural_right, holdable).
narrative_ontology:cs_axiom_grounding('ffc81d38-c191-493a-b548-5a0f69638498', individual_self_defense_natural_right, deontological).
narrative_ontology:cs_axiom('ffc81d38-c191-493a-b548-5a0f69638498', foundational, second_amendment_codifies_preexisting_right).
narrative_ontology:cs_axiom_status(second_amendment_codifies_preexisting_right, holdable).
narrative_ontology:cs_axiom_grounding('ffc81d38-c191-493a-b548-5a0f69638498', second_amendment_codifies_preexisting_right, deontological).
narrative_ontology:cs_axiom('ffc81d38-c191-493a-b548-5a0f69638498', secondary, history_tradition_test_exhaustive).
narrative_ontology:cs_axiom_status(history_tradition_test_exhaustive, holdable).
narrative_ontology:cs_axiom_grounding('ffc81d38-c191-493a-b548-5a0f69638498', history_tradition_test_exhaustive, conventional).
narrative_ontology:cs_reference_frame('ffc81d38-c191-493a-b548-5a0f69638498', founding_era_militia_right).
narrative_ontology:cs_drift_state('ffc81d38-c191-493a-b548-5a0f69638498', post_bruen_2022, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ffc81d38-c191-493a-b548-5a0f69638498', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, all_adult_citizens).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, gun_rights_organizations).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, state_legislatures).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, local_governments).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, public_health_authorities).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, gun_violence_victims).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, individual_self_defense_right).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, originalist_interpretation_method).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, textualist_second_amendment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain constitutionally protected access to firearms for self-defense without militia connection. The right is enforced through judicial invalidation of restrictions. Political exit requires constitutional amendment or court composition change; geographic exit (moving to friendlier jurisdictions) is available but partial.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, all_adult_citizens, beneficiary,
    organized, generational, constrained, national).

% Direct commercial beneficiary of expanded market access and reduced regulatory compliance costs. Actively funds litigation and advocacy shaping the reading's scope. Can redirect capital internationally if domestic regime shifts; holds structural leverage over regulatory conversation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_industry, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__individual_right_reading, firearms_industry, agenda_setter).

% Architect and maintain the individual-right reading through strategic litigation (Heller, McDonald, Bruen), amicus networks, and legislative grading. Collect membership dues and political capital from the reading's enforcement. Organizational survival depends on the reading's vitality; exit would mean mission dissolution.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_rights_organizations, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__individual_right_reading, gun_rights_organizations, beneficiary).

% Lose regulatory authority over firearms; laws on possession, carry, licensing, and prohibited categories face strict scrutiny and frequent invalidation. Political cost of compliance is high (primary challenges); institutional exit requires federal constitutional amendment or court packing — practically unavailable.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, state_legislatures, payer,
    institutional, biographical, constrained, national).

% Bear enforcement costs and liability for gun violence while stripped of local regulatory tools (preemption doctrines, state-law constraints, now federal constitutional floor). No meaningful exit: cannot leave jurisdiction, cannot regulate, must absorb externalities.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, local_governments, payer,
    moderate, biographical, trapped, regional).

% Firearms research restricted (Dickey Amendment legacy); injury prevention treated as regulation subject to strict scrutiny rather than public health authority. Data collection and intervention design constrained by constitutional avoidance. Exit means abandoning population-level injury prevention mission.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, public_health_authorities, payer,
    moderate, biographical, constrained, national).

% Bear the concentrated human cost of the reading's regulatory foreclosure — shootings, suicides, domestic violence fatalities enabled by inaccessible restrictions. No individual exit; collective exit requires political mobilization against organized, well-resourced opposition.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_violence_victims, payer,
    powerless, immediate, trapped, local).

% Authors and applies the reading through precedent (Heller, McDonald, Bruen, Rahimi). Interprets "text, history, and tradition" test; determines which historical analogs validate modern regulations. Not a direct beneficiary or payer; institutional legitimacy tied to perceived neutrality of method.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Provide the intellectual architecture for the reading — original public meaning, founding-era history, textual analysis. Professional credibility and institutional placement (clerkships, professorships, think tanks) depend on the reading's academic legitimacy.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, legal_scholars_originalist, observer,
    analytical, generational, analytical, national).

% Argue for collective-right or civic-reading frameworks; excluded from doctrinal dominance since Heller. Their exclusion is structural — the reading's methodological commitments (originalism, textualism) treat their approach as illegitimate rather than contestable.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, legal_scholars_living_constitutionalist, excluded,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a decentralized self-defense capacity across the polity by guaranteeing individual access to effective arms, reducing dependence on state protection monopolies and enabling collective resistance to tyranny.
% TRANSFER_FUNCTION: Transfers regulatory authority over firearms from state/local governments to the federal judiciary applying a history-and-tradition test; transfers risk of gun violence from regulated markets to unregulated individuals; transfers political capital to gun-rights organizations that enforce the reading.
% ABSENT_VOICES: Communities disproportionately impacted by gun violence (urban Black and Latino communities, domestic violence survivors, suicide loss families) are structurally excluded from the historical-analog methodology that defines the right's scope — their experience is not a "historical tradition" the test recognizes. Future generations who will inherit the regulatory foreclosure are also absent.
% DISAPPEARANCE_RATIONALE: If the individual-right reading vanished overnight, states would regain plenary authority to regulate firearms (licensing, bans, carry restrictions, storage mandates). The firearms market would contract under compliance costs. Gun violence epidemiology would shift as supply-side interventions become legally viable. The political coalition built around the reading would lose its constitutional anchor.
% FOUNDING_PROBLEM: The founding generation feared a standing army and disarmed citizenry; the Second Amendment was designed to preserve the militia system by protecting the people's right to keep and bear arms suitable for militia service — an individual right but one structurally tied to collective defense.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (e.g., Scalia in Heller, Amar, Volokh) attest the founding problem was individual self-defense as a natural right. Historians of the founding era (e.g., Cornell, Rakove, Waldman) and the dissenting justices in Heller (Stevens, Breyer) attest the founding problem was militia preservation — the individual right reading is a 20th-century reconstruction. The historical consensus outside the benefiting parties supports the militia-tied reading.
narrative_ontology:disappearance_verdict(second_amendment_scope__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__individual_right_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) reflects the breadth of regulatory authority transferred from democratic institutions to courts, and the human cost externalized to communities with no exit. Suppression (0.42) is moderate — the constraint operates through judicial invalidation rather than direct coercion, but the Bruen test's rigidity suppresses regulatory experimentation. Theater (0.28) is rising as the history-and-tradition test becomes a formalistic screen for policy preferences. Accessibility collapse (0.55) is partial: some regulations survive (felon prohibitions, sensitive places), but the space for innovation is narrowing. Resistance (0.72) is high from state legislatures, public health authorities, and affected communities — but resistance is channeled into litigation the reading's methodology is designed to defeat.
 *
 * PERSPECTIVAL GAP:
 *   From the citizen-beneficiary seat, the constraint appears as a liberty guarantee against state overreach — a rope-like coordination of self-defense. From the state-legislature payer seat, it appears as a snare — judicial preemption of democratic policy with no accountability. From the gun-violence-victim seat, it appears as a pure extraction mechanism — their bodies are the cost of the reading's theoretical purity. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the hybrid reality.
 *
 * DIRECTIONALITY LOGIC:
 *   All adult citizens are structural beneficiaries (gain enforceable right, d ~ 0.15). Firearms industry and gun-rights organizations are concentrated beneficiaries and agenda-setters (collect rents and control doctrine, d ~ 0.05). State legislatures, local governments, and public health authorities are institutional payers (lose authority, bear costs, d ~ 0.85). Gun violence victims are powerless payers (bear concentrated harm, no exit, d ~ 0.95). Federal judiciary sits near analytical (d ~ 0.5) — neither collects nor pays directly but legitimates the structure. Originalist scholars are analytical beneficiaries (career capital from the reading). Living-constitutionalist scholars are excluded.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem (militia preservation against standing armies) is historically dead — the militia system has been replaced by a professional military and National Guard. The reading persists because it was repurposed to serve a new coordination function (individual self-defense) that benefits organized, powerful actors. The mandate has atrophied but the constraint intensifies — classic mandatrophy where the form outlives the function and is repurposed for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    history_tradition_test_coherence,
    'Does the Bruen history-and-tradition test have a coherent, determinate methodology, or is it an open-ended license for judicial policy-making?',
    'Track lower-court applications of Bruen: if judges reach wildly divergent results on similar regulations using the same test, the methodology is indeterminate. Empirical study of post-Bruen decisions.',
    'If indeterminate, the constraint''s suppression is higher than measured (judicial discretion masquerading as method) and extraction is judge-dependent rather than rule-dependent — the constraint becomes a vehicle for ideological capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(history_tradition_test_coherence, conceptual, 'Methodological coherence of the history-and-tradition test').

omega_variable(
    regulatory_foreclosure_vs_innovation,
    'Does the reading foreclose ALL meaningful firearms regulation, or does it permit a substantial regulatory space (licensing, background checks, dangerous-weapon bans) that makes the extraction partial rather than total?',
    'Catalog post-Bruen judicial outcomes: which regulations survive and which fall. Measure the regulatory surface area remaining.',
    'If regulatory space remains substantial, the constraint is more rope-like (coordination with bounded extraction). If nearly all regulation falls, it approaches snare (extraction with coordination as cover).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_foreclosure_vs_innovation, empirical, 'Scope of surviving regulatory authority under the individual-right reading').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the individual-right reading''s core premise (individual right unconnected to militia) logically foreclose the collective-right reading within a single constitutional framework, or do they occupy different interpretive registers that could theoretically coexist?',
    'Analyze the logical structure: if the Second Amendment''s text (''the right of the people'') grammatically entails an individual right, the collective reading is foreclosed. If ''the people'' can refer to the collective body politic, both readings remain logically possible.',
    'If forecloses, the kernel has a structural fault line — one reading''s victory is the other''s logical impossibility. If coexists, the contest is political, not logical, and the engine''s coexistence edge is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Logical relationship between individual-right and collective-right readings of the Second Amendment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__individual_right_reading, theater_ratio, 2008, 0.12).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_scope__individual_right_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(seco_tr_t2016, second_amendment_scope__individual_right_reading, theater_ratio, 2016, 0.19).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_scope__individual_right_reading, theater_ratio, 2022, 0.25).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_scope__individual_right_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__individual_right_reading, base_extractiveness, 2008, 0.35).
narrative_ontology:measurement(seco_be_t2010, second_amendment_scope__individual_right_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(seco_be_t2016, second_amendment_scope__individual_right_reading, base_extractiveness, 2016, 0.51).
narrative_ontology:measurement(seco_be_t2022, second_amendment_scope__individual_right_reading, base_extractiveness, 2022, 0.63).
narrative_ontology:measurement(seco_be_t2024, second_amendment_scope__individual_right_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t2008, second_amendment_scope__individual_right_reading, suppression_requirement, 2008, 0.28).
narrative_ontology:measurement(seco_su_t2010, second_amendment_scope__individual_right_reading, suppression_requirement, 2010, 0.31).
narrative_ontology:measurement(seco_su_t2016, second_amendment_scope__individual_right_reading, suppression_requirement, 2016, 0.35).
narrative_ontology:measurement(seco_su_t2022, second_amendment_scope__individual_right_reading, suppression_requirement, 2022, 0.4).
narrative_ontology:measurement(seco_su_t2024, second_amendment_scope__individual_right_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__individual_right_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__civic_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, state_preemption_doctrines).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, dickey_amendment_research_restrictions).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, bruen_history_tradition_test).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the second_amendment_scope kernel. The collective_right_reading and civic_right_reading are sibling constraints with different beneficiary/victim structures and ε values. All three are linked via affects_constraints. The individual_right_reading has the highest extractiveness because it forecloses the most regulatory space and externalizes the most concentrated harm.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_scope__individual_right_reading, institutional, 0.1).
constraint_indexing:directionality_override(second_amendment_scope__individual_right_reading, powerful, 0.05).
constraint_indexing:directionality_override(second_amendment_scope__individual_right_reading, powerless, 0.95).
constraint_indexing:directionality_override(second_amendment_scope__individual_right_reading, moderate, 0.8).
constraint_indexing:directionality_override(second_amendment_scope__individual_right_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
