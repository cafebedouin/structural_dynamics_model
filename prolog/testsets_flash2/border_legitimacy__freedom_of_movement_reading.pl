% ============================================================================
% CONSTRAINT STORY: border_legitimacy__freedom_of_movement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__freedom_of_movement_reading, []).

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
 *   constraint_id: border_legitimacy__freedom_of_movement_reading
 *   human_readable: Border Restrictions (Freedom of Movement Reading)
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This constraint story analyzes border restrictions from the 'freedom of
 *   movement' reading, which posits that freedom of movement is a fundamental
 *   human right and that state borders are presumptively illegitimate
 *   restrictions on this right. From this perspective, border enforcement is
 *   a highly extractive and suppressive mechanism that primarily benefits
 *   state security apparatuses and certain domestic labor market segments,
 *   while imposing severe costs on migrants and indirectly on existing
 *   citizens through distorted labor markets and welfare systems. The
 *   constraint is claimed as a Snare due to its high extraction, active
 *   enforcement, and identifiable victims.
 *
 * KEY AGENTS:
 *   - migrants_seeking_entry: Primary target (powerless/trapped) — bears extraction, denied fundamental rights.
 *   - state_security_apparatus: Primary beneficiary/agenda_setter (institutional/constrained) — benefits from expanded authority and resources, enforces the constraint.
 *   - domestic_labor_market_segments: Secondary beneficiary (organized/mobile) — benefits from reduced competition.
 *   - displaced_workers_in_destination_countries: Victim (moderate/constrained) — bears indirect costs through labor market distortions.
 *   - welfare_recipients_in_destination_countries: Victim (powerless/constrained) — bears indirect costs through perceived strain on social systems.
 *   - international_human_rights_advocates: Observer (organized/analytical) — challenges the constraint's legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, 0.85).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_reading, 0.92).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, snare).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Border Restrictions (Freedom of Movement Reading)").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, '24779224-9fd1-4f20-a364-c761b90c73ca').
narrative_ontology:cs_kernel_codification('24779224-9fd1-4f20-a364-c761b90c73ca', formalized).
narrative_ontology:cs_authority_grounding('24779224-9fd1-4f20-a364-c761b90c73ca', extraction).
narrative_ontology:cs_interpretation_layer_present('24779224-9fd1-4f20-a364-c761b90c73ca').
narrative_ontology:cs_reading_relation('24779224-9fd1-4f20-a364-c761b90c73ca', border_legitimacy__sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('24779224-9fd1-4f20-a364-c761b90c73ca', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('24779224-9fd1-4f20-a364-c761b90c73ca', foundational, freedom_of_movement_is_human_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_human_right, holdable).
narrative_ontology:cs_axiom_grounding('24779224-9fd1-4f20-a364-c761b90c73ca', freedom_of_movement_is_human_right, deontological).
narrative_ontology:cs_axiom('24779224-9fd1-4f20-a364-c761b90c73ca', foundational, borders_are_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(borders_are_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('24779224-9fd1-4f20-a364-c761b90c73ca', borders_are_presumptively_illegitimate, deontological).
narrative_ontology:cs_reference_frame('24779224-9fd1-4f20-a364-c761b90c73ca', universal_human_rights_framework).
narrative_ontology:cs_drift_state('24779224-9fd1-4f20-a364-c761b90c73ca', contemporary_global_politics, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('24779224-9fd1-4f20-a364-c761b90c73ca', '').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, state_security_apparatus).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, domestic_labor_market_segments).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, migrants_seeking_entry).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_workers_in_destination_countries).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, welfare_recipients_in_destination_countries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals attempting to cross borders for economic opportunity, safety, or family reunification. They face legal barriers, physical dangers, and often exploitation, bearing the direct costs of border enforcement through denied entry, detention, or forced return. Their 'exit' from the constraint is often impossible or extremely costly.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, migrants_seeking_entry, payer,
    powerless, immediate, trapped, global).

% The governmental agencies (border patrol, immigration services) responsible for enforcing border restrictions. They benefit from expanded budgets, personnel, and authority justified by the need to control borders. Their role is to actively suppress unauthorized movement.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, state_security_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Certain sectors of the domestic labor market (e.g., those competing with low-wage migrant labor) benefit from reduced competition and potentially higher wages due to restricted immigration. This benefit is often diffuse and indirect.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, domestic_labor_market_segments, beneficiary,
    organized, biographical, mobile, national).

% Existing citizens or legal residents in destination countries who may experience downward pressure on wages or job displacement in specific sectors due to increased competition from new arrivals. From the freedom of movement reading, their 'displacement' is a consequence of the border's existence, not its absence.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, displaced_workers_in_destination_countries, payer,
    moderate, biographical, constrained, national).

% Existing citizens or legal residents who rely on social welfare programs. Increased immigration, without corresponding increases in social support, can lead to perceived or actual strain on these systems, making them 'pay' through reduced benefits or increased competition for resources. This is a consequence of the border's existence, not its absence, as the border creates the distinction.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, welfare_recipients_in_destination_countries, payer,
    powerless, biographical, constrained, national).

% Organizations and individuals who argue for the universal right to freedom of movement and challenge the legitimacy of state borders as arbitrary restrictions on human liberty. They analyze the constraint's impact and advocate for policy changes.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, international_human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the constraint primarily coordinates the exclusion of certain populations to maintain existing national distributions of resources and social order, rather than solving a genuine collective action problem for all humanity.
% TRANSFER_FUNCTION: Transfers the right to reside and work in a desired territory from migrants seeking entry to existing citizens, and transfers resources (e.g., tax revenue, social services) from the general public to the state security apparatus for enforcement.
% ABSENT_VOICES: The voices of potential migrants who are denied entry, those who die attempting to cross borders, and future generations who might benefit from more open societies are largely absent from the policy-making discourse, which is dominated by national interests.
% DISAPPEARANCE_RATIONALE: If border restrictions vanished overnight, there would be massive global migration flows, significant demographic shifts, and a fundamental reordering of labor markets, social welfare systems, and national identities. The world would be profoundly different.
% FOUNDING_PROBLEM: The problem of managing national populations, resources, and cultural identities within defined territorial boundaries, and the perceived need to protect existing citizens from external competition or perceived threats.
% FOUNDING_PROBLEM_CORROBORATION: While states and their security apparatuses universally attest to the problem's live status, human rights advocates and some economists argue that the 'problem' is often a constructed justification for maintaining extractive systems, with corroboration from studies on the economic benefits of migration and the arbitrary nature of citizenship.
narrative_ontology:disappearance_verdict(border_legitimacy__freedom_of_movement_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__freedom_of_movement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__freedom_of_movement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_legitimacy__freedom_of_movement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__freedom_of_movement_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__freedom_of_movement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__freedom_of_movement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the severe costs imposed on migrants (loss of opportunity, risk of life, detention) and the indirect costs on domestic populations through economic distortions. Suppression (0.92) is extremely high due to the active, often militarized, enforcement of borders, which includes physical barriers, surveillance, and legal penalties. Theater ratio is low (0.1) because border enforcement is largely functional in its goal of exclusion, with little performative activity masking a lack of actual function. The increasing trend in extractiveness and suppression over time reflects the global hardening of borders and the intensification of enforcement mechanisms since the mid-20th century.
 *
 * PERSPECTIVAL GAP:
 *   The state security apparatus and domestic labor market segments would experience this as a necessary, legitimate constraint for national security and economic stability (potentially a Rope or even a Mountain of 'national interest'). Migrants and human rights advocates, however, experience it as a Snare, an illegitimate imposition that extracts fundamental rights and resources. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Migrants seeking entry are full targets (high d) as they bear the direct costs and are denied fundamental rights. The state security apparatus is a clear beneficiary (low d) due to its expanded power and resources. Domestic labor market segments are also beneficiaries, albeit diffuse ones. Displaced workers and welfare recipients in destination countries are victims (high d) as they bear indirect costs from the border's existence, which creates the distinction that leads to their 'displacement' or 'strain'.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling border restrictions as a 'Rope' of national security or a 'Mountain' of sovereignty, which would obscure the significant extraction and suppression inherent in the 'freedom of movement' reading. By classifying it as a Snare, the framework highlights the coercive and victimizing aspects that are central to this specific interpretation of border legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_right_vs_social_construct,
    'Is freedom of movement an inherent natural right, or a socially constructed right contingent on state recognition?',
    'Philosophical consensus on the grounding of human rights, or a global legal framework that universally codifies freedom of movement as an unalienable right independent of state borders.',
    'If a natural right, the extractiveness and suppression of borders are fundamentally illegitimate. If a social construct, the legitimacy of borders becomes a matter of political negotiation and international agreement, potentially lowering the perceived extractiveness from this reading''s perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_right_vs_social_construct, conceptual, 'The fundamental nature of freedom of movement as a right.').

omega_variable(
    economic_impact_distribution,
    'How are the economic costs and benefits of open vs. restricted borders distributed across different segments of both sending and receiving populations?',
    'Comprehensive, long-term economic modeling and empirical studies that account for all externalities and dynamic effects of migration on wages, public services, innovation, and remittances.',
    'More precise data on economic distribution could refine the extractiveness metric and clarify the beneficiary/victim sets, potentially shifting some ''payer'' seats to ''beneficiary'' or vice versa depending on the specific economic model adopted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_distribution, empirical, 'Detailed economic analysis of migration''s distributive effects.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (physical barriers, legal penalties) or internalized (fear, hopelessness, lack of information)?',
    'Post-exit suppression trajectory: if suppression persists (e.g., migrants self-censor movement even after legal barriers are removed), reclassify as partially internalized. This would require a counterfactual scenario or a policy shift.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — migrants carry the suppression with them even if physical borders were to open, due to psychological barriers or lack of knowledge about rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for migrants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1948, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(bord_tr_t1960, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1960, 0.07).
narrative_ontology:measurement(bord_tr_t1980, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(bord_tr_t2000, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(bord_tr_t2010, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2010, 0.095).
narrative_ontology:measurement(bord_tr_t2024, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1948, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(bord_be_t1960, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1960, 0.75).
narrative_ontology:measurement(bord_be_t1980, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1980, 0.8).
narrative_ontology:measurement(bord_be_t2000, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2000, 0.83).
narrative_ontology:measurement(bord_be_t2010, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(bord_be_t2024, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1948, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(bord_su_t1960, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1960, 0.8).
narrative_ontology:measurement(bord_su_t1980, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1980, 0.85).
narrative_ontology:measurement(bord_su_t2000, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(bord_su_t2010, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2010, 0.9).
narrative_ontology:measurement(bord_su_t2024, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
