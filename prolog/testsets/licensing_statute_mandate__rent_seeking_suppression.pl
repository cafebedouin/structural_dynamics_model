% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__rent_seeking_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__rent_seeking_suppression, []).

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
 *   constraint_id: licensing_statute_mandate__rent_seeking_suppression
 *   human_readable: Statutory Licensing Mandate as Incumbent Rent Extraction
 *   domain: economic/labor/regulatory
 *
 * SUMMARY:
 *   Statutory licensing requirements exist in jurisdictions as regulatory
 *   mandates for entry into professions (law, medicine, accounting, real
 *   estate, plumbing, dentistry, and many others). This story instantiates
 *   the RENT-SEEKING SUPPRESSION reading of the licensing-statute-mandate
 *   kernel: the constraint exists to restrict labor supply and extract rents
 *   for incumbent practitioners. This is ONE of three structurally distinct
 *   readings of the same statutory framework. The public-safety-coordination
 *   reading interprets the statute as solving genuine consumer information
 *   problems through minimum competence standards. The
 *   graduated-access-filter reading interprets the statute as creating tiered
 *   market access where credential barriers correlate with class and prior
 *   resource access. This story treats only the rent-seeking reading:
 *   beneficiaries are incumbents; victims are entrants (bearing credential
 *   costs and foregone earnings) and consumers (bearing inflated prices). The
 *   constraint is CLAIMED as snare and the metrics describe snare-typical
 *   operation: high extraction sustained by active enforcement, substantial
 *   theater (safety rhetoric covers extraction machinery), collapsed
 *   alternatives. The three readings are linked as a constraint family; each
 *   is authored as a separate constraint story with its own ε, stakeholders,
 *   metrics, and claim-metric relationship. This decomposition follows DP-001
 *   (ε-invariance principle): one observable (the statute) instantiates three
 *   different constraints depending on which causal mechanism you measure.
 *
 * KEY AGENTS:
 *   - incumbent_practitioners (organized beneficiary): Set and maintain licensing standards through professional associations and board control. Benefit directly from labor scarcity rents (wage and fee premiums). Geographic arbitrage available but identity-locked.
 *   - labor_market_entrants (powerless victim, identity-locked): Bear credential costs and uncertainty; locked into profession by identity fusion with the intended career.
 *   - consumers_paying_inflated_prices (powerless victim, constrained exit): Carry scarcity rents via higher service prices; constrained exit (cannot access unlicensed alternatives).
 *   - professional_regulatory_board (institutional agenda-setter): Executes credential inflation through examination control, fee structure, reciprocity barriers. Board members typically incumbents.
 *   - legislature (institutional observer, mobile): Delegates credentialing; receives lobbying from incumbents; faces weak pressure from diffuse consumer interests.
 *   - alternative_service_providers (excluded, trapped): Would-be competitors entirely barred; excluded from negotiation frame.
 *   - enforcement_agencies (institutional agenda-setter): Sustains monopoly through prosecution of unlicensed practice and license revocation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, 0.78).
domain_priors:suppression_score(licensing_statute_mandate__rent_seeking_suppression, 0.81).
domain_priors:theater_ratio(licensing_statute_mandate__rent_seeking_suppression, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, extractiveness, 0.78).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__rent_seeking_suppression, snare).
narrative_ontology:human_readable(licensing_statute_mandate__rent_seeking_suppression, "Statutory Licensing Mandate as Incumbent Rent Extraction").
narrative_ontology:topic_domain(licensing_statute_mandate__rent_seeking_suppression, "economic/labor/regulatory").

domain_priors:requires_active_enforcement(licensing_statute_mandate__rent_seeking_suppression).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__rent_seeking_suppression, '10419efa-162b-4849-91d6-65c06c9105ba').
narrative_ontology:cs_kernel_codification('10419efa-162b-4849-91d6-65c06c9105ba', formalized).
narrative_ontology:cs_authority_grounding('10419efa-162b-4849-91d6-65c06c9105ba', extraction).
narrative_ontology:cs_interpretation_layer_present('10419efa-162b-4849-91d6-65c06c9105ba').
narrative_ontology:cs_reading_relation('10419efa-162b-4849-91d6-65c06c9105ba', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('10419efa-162b-4849-91d6-65c06c9105ba', licensing_statute_mandate__graduated_access_filter, coexists_with).
narrative_ontology:cs_axiom('10419efa-162b-4849-91d6-65c06c9105ba', foundational, labor_supply_restriction_primary_function).
narrative_ontology:cs_axiom_status(labor_supply_restriction_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('10419efa-162b-4849-91d6-65c06c9105ba', labor_supply_restriction_primary_function, empirically_contingent).
narrative_ontology:cs_axiom('10419efa-162b-4849-91d6-65c06c9105ba', foundational, incumbent_capture_of_credentialing_authority).
narrative_ontology:cs_axiom_status(incumbent_capture_of_credentialing_authority, holdable).
narrative_ontology:cs_axiom_grounding('10419efa-162b-4849-91d6-65c06c9105ba', incumbent_capture_of_credentialing_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('10419efa-162b-4849-91d6-65c06c9105ba', competitive_labor_market_equilibrium).
narrative_ontology:cs_drift_state('10419efa-162b-4849-91d6-65c06c9105ba', contemporary_credential_inflation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('10419efa-162b-4849-91d6-65c06c9105ba', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, labor_market_entrants).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, consumers_paying_inflated_prices).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__rent_seeking_suppression, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(licensing_statute_mandate__rent_seeking_suppression, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because credential restrictions reduce labor supply, creating scarcity rents captured by incumbents; the constraint's primary function in this reading is extraction, not safety assurance. Suppression is very high (0.81) because persistence depends on active enforcement against unlicensed practice and on institutional control of examination standards—without enforcement, competitive entry would erode rents within months. Theater ratio rises from 0.48 to 0.62 over the interval, indicating that safety rhetoric increasingly functions as a cover story as credential requirements diverge from what is needed for safe practice; the ratio stabilizes at 0.62 once the enforcement infrastructure is mature and further inflation becomes slower (political costs rise). Accessibility collapse is high (0.73) because entrants face binary choice (meet rising standards or exit the profession entirely); consumers cannot access lower-cost unlicensed alternatives (statutory enforcement prevents this). Resistance is moderate (0.54) because entrants and consumer advocates push back through legislative testimony and market pressure, but incumbent beneficiaries and captured institutional actors (board, enforcement agencies) control the actual enforcement machinery. The measurement series tracks the ratchet mechanism: extractiveness rises as credential standards inflate and incumbent control solidifies; theater rises as the enforcement apparatus expands and requires narrative justification; suppression rises as enforcement infrastructure is built and applied more systematically. By year 30, metrics stabilize—the regime reaches maturity; further inflation slows because political resistance increases and incumbent gains are already captured. This trajectory is characteristic of a snare entering maturity.
 *
 * PERSPECTIVAL GAP:
 *   Incumbent practitioners perceive the constraint as legitimate professional regulation protecting public safety—this framing aligns with the public-safety-coordination reading and is operationally coherent within their worldview (they believe higher standards mean safer practice and that they are stewards of the profession). Entrants and consumer advocates perceive it as artificial labor restriction extracting rents—the rent-seeking reading captures their experience. The legislature perceives it as delegated regulation, nominally for safety but functionally captured by incumbents. The engine computes per-seat types from structural data: incumbents (high benefit, low constraint cost, organized power, control over rule-setting) experience a beneficiary type; entrants (high cost, low benefit, powerless, identity-locked) experience a victim/snare type; consumers (diffuse costs, no direct benefits, powerless, constrained exit) experience extraction. The perspectival gap IS the snare: different seats have fundamentally different structural relationships to the same constraint, and the snare's persistence depends on the agenda-setter's (incumbent) ability to control the narrative (safety rhetoric) while extracting from the payer seats (entrants and consumers).
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent practitioners: d ≈ 0.12 (beneficiary end, override applied). They set the rules, dominate the professional board, benefit directly from scarcity rents, and have geographic arbitrage options. Directionality derivation from beneficiary status + organized power + arbitrage exit would produce d ≈ 0.08; override applied to 0.12 reflects that incumbents also bear some constraint costs (must maintain professional standards, face litigation risk, market pressure from internationalization) and some exits are unavailable (career capital sunk in this jurisdiction). Entrants: d ≈ 0.85 (target end). They bear credential costs (tuition, exam fees, opportunity costs of years in school), face uncertainty about admission, and exit is identity-locked (the field is core to their intended career and self-concept; leaving feels like abandonment). Consumers: d ≈ 0.80 (target end). They bear inflated prices passively; they gain no direct benefit and cannot easily access unlicensed alternatives; their exit is constrained (mobility limited, switching costs high). Professional board: d ≈ 0.15 (slight beneficiary). The board collects examination and licensing fees that depend on the credentialing monopoly; its institutional survival depends on credential inflation; it faces no direct cost from the constraint. Legislature: d ≈ 0.40 (near symmetric). Legislators face pressure from both incumbents (organized, well-resourced lobbying) and consumers (diffuse, weak); they are not direct payers or beneficiaries. Enforcement agencies: d ≈ 0.20 (slight beneficiary). They benefit from having a clear enforcement mandate; credential inflation creates more boundary-crossing cases to prosecute and justifies larger enforcement budgets; they are not direct beneficiaries of the scarcity rents but their institutional role depends on the credentialing boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (consumer safety via credible signals of practitioner competence) is legitimately solved by well-designed credential standards that maintain professional competence standards proportionate to actual risk. The rent-seeking reading captures how the constraint decouples from this founding problem over time: as credential standards inflate beyond what is needed for safe practice (measured by comparative outcomes, task-level competence analysis, or international benchmarking), the constraint's actual function becomes labor supply restriction, not safety assurance. Mandatrophy is not fully resolved here because the constraint retains some genuine coordination function (consumer information) alongside its primary extractive function, suggesting the constraint is tangled_rope rather than pure snare. However, this reading explicitly emphasizes extraction as primary and coordination as secondary/rhetorical, which justifies the snare classification. Theater ratio (0.62) is the diagnostic evidence of mandatrophy drift: the constraint requires sustained narrative work (repeated public claims about rising safety risks, professional complexity, consumer vulnerability) to justify credential inflation that economically functions to restrict supply. Without this narrative maintenance, entrant resistance and consumer pressure would force recognition that the founding problem has been solved and the constraint has become pure extraction. The rising theater trajectory (0.48 to 0.62) indicates that mandatrophy drift is ongoing—the constraint's legitimacy increasingly depends on narrative rather than on functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_sufficiency_empirical,
    'What is the minimum credential level needed for safe practice in this profession, and how does it compare to the current statutory requirement?',
    'Comparative analysis of service outcomes (malpractice rates, consumer complaints, safety incidents) across jurisdictions with different credential levels. Deposition testimony from incumbents about credential necessity. Controlled study of task-level competence (what skills are actually needed for which services).',
    'If current requirements substantially exceed what is needed for safety, the rent-seeking reading is supported and the constraint is snare. If requirements track actual competence needs closely, the public-safety-coordination reading is supported and the constraint is more rope-like or tangled-rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_sufficiency_empirical, empirical, 'Whether statutory credential inflation exceeds what is needed for safe practice.').

omega_variable(
    incumbent_control_of_standard_setting,
    'To what extent do incumbent practitioners and their associations control the professional board that sets credential standards, versus independent or consumer-representative control?',
    'Analysis of board composition, voting rules, funding sources, and appointment procedures. Testimony from board members about agenda-setting processes. Comparison of credential changes to incumbent professional association positions over time.',
    'High incumbent control supports the rent-seeking reading and snare classification. Distributed control or consumer representation would shift the interpretation toward public-safety or graduated-access readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incumbent_control_of_standard_setting, empirical, 'Whether the credentialing authority is captured by incumbent interests.').

omega_variable(
    consumer_awareness_of_credential_inflation,
    'To what extent do consumers understand that rising credential requirements translate into higher service prices, and to what extent do they believe the inflation is justified by safety improvements?',
    'Survey data on consumer perception of profession-specific credential requirements. Comparison of consumer stated reasons for accepting higher prices (safety concern vs. inevitable cost) to actual credential changes and safety outcome data.',
    'If consumers are unaware that credential inflation drives price increases, suppression is higher (they are not resisting because they do not understand the mechanism). If consumers are aware but accept the inflation as justified, the public-safety reading gains support. If consumers are aware and skeptical, resistance should rise and suppression should decline over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_awareness_of_credential_inflation, empirical, 'Whether suppression is achieved through consumer awareness or consumer ignorance of the extraction mechanism.').

omega_variable(
    identity_lock_vs_rational_choice,
    'For labor market entrants, is the decision to pursue the credential despite rising barriers a rational calculation about net lifetime benefits, or an identity-driven commitment that persists despite negative expected value?',
    'Exit interviews with entrants who abandon the credential pursuit: what calculation changed? Longitudinal tracking of cohorts: do entrants'' earnings after credentialing justify the investment, and has the justification deteriorated over time? Psychological studies on professional identity fusion in credential-intensive fields.',
    'If entrants are making rational calculations that the credential investment still pays off, exit is more accurately described as ''constrained'' than ''identity_locked''. If entrants persist in credential pursuit despite negative expected value, identity_lock is the operative mechanism and suppression is internalized (the entrants are enforcing the barrier against themselves). The distinction affects whether the suppression can be reduced by information provision or whether it requires identity-frame disruption.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_vs_rational_choice, empirical, 'Whether entrants'' barrier compliance is rational or identity-driven.').

omega_variable(
    reading_foreclosure_public_safety,
    'Does the rent-seeking-suppression reading FORECLOSE the public-safety-coordination reading, or do they coexist as competing interpretations held by different parties?',
    'Logical analysis: can a credentialing statute be both a legitimate safety mechanism AND a rent-extraction mechanism simultaneously, or are these interpretations mutually exclusive? Historical evidence: was the statute originally enacted for public safety, and did incumbent rent-seeking capture it later, or was rent-seeking the operative motive from the outset, with safety rhetoric as cover?',
    'If the readings foreclose each other (mutually exclusive), only one can be true and the other is ideology/cover. If they coexist (both true, different aspects), the constraint is tangled_rope (coordinating safety AND extracting rents), not pure snare. The boundary between snare and tangled_rope depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_public_safety, conceptual, 'Whether the rent-seeking and public-safety interpretations are logically compatible or mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__rent_seeking_suppression, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0, 0.48).
narrative_ontology:measurement(lice_tr_t5, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 5, 0.52).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 10, 0.55).
narrative_ontology:measurement(lice_tr_t15, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 15, 0.58).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 20, 0.61).
narrative_ontology:measurement(lice_tr_t25, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 25, 0.62).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 30, 0.62).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(lice_be_t5, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 10, 0.69).
narrative_ontology:measurement(lice_be_t15, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(lice_be_t25, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 25, 0.77).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0, 0.71).
narrative_ontology:measurement(lice_su_t5, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 5, 0.74).
narrative_ontology:measurement(lice_su_t10, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 10, 0.76).
narrative_ontology:measurement(lice_su_t15, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 15, 0.78).
narrative_ontology:measurement(lice_su_t20, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(lice_su_t25, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 25, 0.81).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 30, 0.81).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__rent_seeking_suppression, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(licensing_statute_mandate__rent_seeking_suppression, 0.12).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__graduated_access_filter).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (licensing_statute_mandate). Two sibling readings exist: public_safety_coordination (statute as legitimate safety mechanism) and graduated_access_filter (statute as class-sorting mechanism). The three readings share the same statutory text but differ in attributed primary function, beneficiary identification, and victim set. Each reading is a separate constraint story with distinct ε, metrics, and stakeholder structure. This story (rent_seeking_suppression) claims the statute's actual function is incumbent labor supply restriction; the public_safety reading claims it solves genuine consumer information problems; the graduated_access reading claims it sorts entrants by prior resource access. The readings are linked as a constraint family via network.affects_constraints. Decomposition follows DP-001 (ε-invariance): one observable (the statute) instantiates three different constraints depending on which causal mechanism you measure. Each story carries a separate omega documenting the reading contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(licensing_statute_mandate__rent_seeking_suppression, organized, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
