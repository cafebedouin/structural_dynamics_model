% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__m4_m5_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__m4_m5_collapse_reading, []).

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
 *   constraint_id: electronic_money_emergence__m4_m5_collapse_reading
 *   human_readable: M4/M5 Statistical Distinction as Retroactive Electronic Money Category
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   The emergence of electronic money is conventionally dated to the
 *   codification of the M4/M5 distinction by central banks and monetary
 *   statisticians in the mid-to-late 20th century. This reading asserts that
 *   the distinction did not describe a pre-existing natural phenomenon but
 *   rather created the category retroactively — monetary statisticians, faced
 *   with financial innovation that blurred the boundary between money and
 *   near-money, retroactively defined 'electronic money' as the difference
 *   between two statistical aggregates. This is a pure classificatory piton:
 *   it stabilizes a measurement convention without corresponding to
 *   underlying monetary physics or pre-existing institutional practice. The
 *   constraint persists by institutional inertia (policy institutions depend
 *   on it, economists have built careers around it) despite substantial
 *   theater (the appearance that statisticians 'discovered' electronic money,
 *   when they actually created the category). This reading stands against two
 *   siblings: the 'became_thinkable_reading' (emergence occurred when digital
 *   money became conceptually possible) and the 'first_held_reading'
 *   (emergence occurred when institutions first held dematerialized currency
 *   in a distinguishable form). This reading claims the sibling readings
 *   mistake measurement artifacts for discoveries. The claim/metric gap is
 *   intentional: the constraint is claimed as piton (performance masking
 *   atrophied function) while extractiveness is moderate-high because
 *   institutional lock-in and gatekeeping maintain the distinction despite
 *   its arbitrary character.
 *
 * KEY AGENTS:
 *   - central_banking_establishment: Sets and maintains the M4/M5 distinction through authoritative measurement; collects institutional authority over monetary definition; could revise the categories but has incentive to preserve them
 *   - monetary_statisticians: Maintain professional authority by being the seats that 'discovered' electronic money through statistical innovation; careers depend on the stability of M4/M5 as a canonical framework
 *   - mainstream_monetary_economists: Use M4/M5 as reference point for models and empirical work; bear the cost of theory-building around a potentially arbitrary distinction; constrained exit due to sunk investment in the framework
 *   - heterodox_monetary_theorists: Argue for alternative emergence readings; systematically excluded from mainstream publication and policy venues; pay suppression cost for working outside canonical framework
 *   - alternative_payment_system_researchers: Study digital currencies and non-bank systems; bear cost of being told their research subjects are 'not really money' — a charge grounded entirely in M4/M5 definitional apparatus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, 0.62).
domain_priors:suppression_score(electronic_money_emergence__m4_m5_collapse_reading, 0.71).
domain_priors:theater_ratio(electronic_money_emergence__m4_m5_collapse_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse, 0.44).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__m4_m5_collapse_reading, piton).
narrative_ontology:human_readable(electronic_money_emergence__m4_m5_collapse_reading, "M4/M5 Statistical Distinction as Retroactive Electronic Money Category").
narrative_ontology:topic_domain(electronic_money_emergence__m4_m5_collapse_reading, "economic_history/monetary_theory/technology_studies").

domain_priors:requires_active_enforcement(electronic_money_emergence__m4_m5_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__m4_m5_collapse_reading, '007932c6-ccc8-4bbb-b9bb-e33c6df55cac').
narrative_ontology:cs_kernel_codification('007932c6-ccc8-4bbb-b9bb-e33c6df55cac', fixed_text).
narrative_ontology:cs_authority_grounding('007932c6-ccc8-4bbb-b9bb-e33c6df55cac', extraction).
narrative_ontology:cs_interpretation_layer_present('007932c6-ccc8-4bbb-b9bb-e33c6df55cac').
narrative_ontology:cs_reading_relation('007932c6-ccc8-4bbb-b9bb-e33c6df55cac', electronic_money_emergence__became_thinkable_reading, forecloses).
narrative_ontology:cs_reading_relation('007932c6-ccc8-4bbb-b9bb-e33c6df55cac', electronic_money_emergence__first_held_reading, forecloses).
narrative_ontology:cs_axiom('007932c6-ccc8-4bbb-b9bb-e33c6df55cac', foundational, measurement_creates_monetary_category).
narrative_ontology:cs_axiom_status(measurement_creates_monetary_category, holdable).
narrative_ontology:cs_axiom_grounding('007932c6-ccc8-4bbb-b9bb-e33c6df55cac', measurement_creates_monetary_category, conventional).
narrative_ontology:cs_axiom('007932c6-ccc8-4bbb-b9bb-e33c6df55cac', foundational, emergence_is_statistical_artifact).
narrative_ontology:cs_axiom_status(emergence_is_statistical_artifact, holdable).
narrative_ontology:cs_axiom_grounding('007932c6-ccc8-4bbb-b9bb-e33c6df55cac', emergence_is_statistical_artifact, deontological).
narrative_ontology:cs_reference_frame('007932c6-ccc8-4bbb-b9bb-e33c6df55cac', statistical_money_discovery).
narrative_ontology:cs_drift_state('007932c6-ccc8-4bbb-b9bb-e33c6df55cac', contemporary_digital_payments_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('007932c6-ccc8-4bbb-b9bb-e33c6df55cac', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, central_banking_establishment).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, monetary_statisticians).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, heterodox_monetary_theorists).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, alternative_payment_system_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, mainstream_monetary_economists).
narrative_ontology:constraint_vindicates(electronic_money_emergence__m4_m5_collapse_reading, money_is_what_the_central_bank_measures_it_to_be).
narrative_ontology:constraint_vindicates(electronic_money_emergence__m4_m5_collapse_reading, statistical_categories_have_no_independent_referent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the M4/M5 distinction through official monetary statistics and policy guidance. Retroactively defined electronic money as the difference between M4 (broad money including bank deposits and electronic instruments) and M3 (narrower measure excluding certain electronic holdings). Collects authority over what counts as 'money' through measurement authority, justifying the distinction as capturing underlying monetary aggregates. Can redefine the categories at will but has incentive to preserve them once policy and theory are built around them.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, central_banking_establishment, agenda_setter,
    institutional, generational, arbitrage, national).

% Maintain professional authority over monetary aggregation and classification. The M4/M5 distinction, once stabilized, becomes the canonical framework within which monetary analysis occurs. Their institutional position depends on the stability and apparent naturalness of these categories. They benefit from being the seat that can claim to have 'discovered' electronic money through measurement.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_statisticians, beneficiary,
    powerful, generational, arbitrage, national).

% Must use the M4/M5 framework as the reference point for monetary theory and empirical work, even when they privately suspect the distinction is somewhat arbitrary. Careers are built on models and papers that treat M4/M5 as real; adopting alternative classifications would require rewriting foundational work and accepting reputational costs.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, mainstream_monetary_economists, payer,
    powerful, biographical, constrained, national).

% Argue that electronic money emergence should be understood through different lenses — institutional possibility, social practice, technological capability — rather than retroactive statistical classification. Their alternative readings are systematically excluded from mainstream publication venues and policy influence. They bear the cost of working outside the canonical framework and face suppression of their work from review gatekeepers.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, heterodox_monetary_theorists, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__m4_m5_collapse_reading, heterodox_monetary_theorists, excluded).

% Study digital currencies, blockchain, and non-bank payment systems. The M4/M5 framework treats their research subjects (cryptocurrencies, private digital money) as outside 'real' money by definition, even though these systems may have equivalent functions. They pay the cost of defending research against the charge that they study 'not really money' — a charge grounded entirely in the M4/M5 definitional apparatus.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, alternative_payment_system_researchers, payer,
    moderate, biographical, constrained, global).

% Rely on the M4/M5 distinction for policy implementation and inflation targeting. They treat the categories as stable empirical facts, not as constructed classifications. This dependency gives the distinction institutional inertia even when economists privately doubt its foundations.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, policy_institutions, observer,
    institutional, biographical, analytical, national).

% Trace when electronic money 'actually' emerged in practice. They find evidence of electronic transfer systems, dematerialized holdings, and digital payment networks at dates that predate the M4/M5 codification, creating a puzzling gap between historical emergence and statistical emergence.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_historians, observer,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__m4_m5_collapse_reading, central_banking_establishment).
narrative_ontology:fixing_cost_class(electronic_money_emergence__m4_m5_collapse_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The M4/M5 distinction coordinates monetary policy analysis by establishing a standard categorization of liquid assets and payment instruments. It enables central banks to target policy to a measurable aggregate and allows economists to discuss 'the money supply' with a unified reference point. It solves the technical problem of defining which instruments count toward monetary aggregates when the boundary between money and near-money blurs with financial innovation.
% TRANSFER_FUNCTION: Moves definitional authority over what counts as 'money' from historians, practitioners, and heterodox theorists to the central banking establishment and monetary statisticians. Transfers policy-relevant knowledge production to those seats — whoever controls the M4/M5 definition controls what phenomena count as 'monetary' and therefore what policies are appropriate.
% ABSENT_VOICES: Practitioners who used dematerialized currency prior to its retroactive statistical definition are structurally absent from the definitional process. Alternative monetary frameworks (heterodox economics, post-Keynesian theory, cryptocurrency researchers) are excluded from the venues where M4/M5 legitimacy is established and defended. These seats would attest that electronic money 'emerged' long before the statistical distinction, or that the distinction is purely a measurement artifact with no ontological weight.
% DISAPPEARANCE_RATIONALE: If the M4/M5 distinction and its enforcement disappeared overnight, monetary policy would continue but the canonical reference frame would collapse. Some economists would pivot to alternative aggregation schemes; policy institutions would face a coordination problem in targeting. The alternative payment systems that were excluded by the definition would suddenly have no clear status relative to 'real money.' The world would not rearrange massively, but the professional and institutional arrangements built around the distinction would require restructuring.
% FOUNDING_PROBLEM: In the mid-20th century, financial innovation created assets (NOW accounts, money market funds, electronic transfer systems) that fell between traditional definitions of 'money' (currency and demand deposits) and 'near-money' (savings instruments). Central banks needed a wider aggregate to capture the monetary impact of these instruments. The distinction was built to resolve this classification problem: M4 (broad) includes electronic instruments; M3 (narrow) omits them.
% FOUNDING_PROBLEM_CORROBORATION: The central banking establishment attests the problem was live and the M4/M5 solution necessary. However, monetary historians (outside the benefiting parties) document that electronic payment infrastructure matured and stabilized by the 1990s-2000s, making the boundary problem less acute. Practitioners no longer struggle with the question 'is this electronic money?' because electronic instruments are now normal and their monetary role is obvious. The founding problem has been solved by ordinary institutional maturation, not maintained by the M4/M5 distinction per se. The distinction now persists by institutional inertia rather than by solving the problem it was built to solve.
narrative_ontology:disappearance_verdict(electronic_money_emergence__m4_m5_collapse_reading, contested).
narrative_ontology:founding_problem_status(electronic_money_emergence__m4_m5_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__m4_m5_collapse_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__m4_m5_collapse_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end) because the distinction transfers definitional authority to the central banking establishment and its statistician allies. The measurement series shows extractiveness rising sharply from 1970 to 2000 (from 0.15 to 0.58) as the M4/M5 framework became institutionalized in policy and academic practice, then plateaus (2000-2025) as the distinction settles into institutional inertia — the payoff is already captured, further enforcement merely sustains it. Theater ratio rises even faster (0.22 to 0.68 by 2000) and plateaus higher (0.68 by 2020), indicating that by the early 2000s the function of the distinction became predominantly performative — continued measurement and citation of M4/M5 appears to serve discovery or tracking but in fact stabilizes a classification scheme whose original coordination rationale has atrophied. Suppression is steady and substantial (0.28 to 0.71) because the constraint maintains itself through gatekeeping (what counts as 'legitimate' monetary analysis) and the exclusion of alternative frameworks from mainstream venues. Accessibility collapse is modest (0.44) because alternatives persist (heterodox economists, cryptocurrency researchers) — they are suppressed, not made inaccessible, showing the constraint's persistence depends on active enforcement rather than on the alternatives being logically impossible. Resistance is moderate-high (0.58) because heterodox economists and alternative researchers actively contest the M4/M5 distinction, though they lack the institutional power to dislodge it. The shared time grid enables lifecycle analysis: the measurement trajectory shows a constraint that rose to extractive dominance through 2000, then shifted into maintenance mode — the characteristic piton pattern of atrophied function sustained by institutional momentum.
 *
 * PERSPECTIVAL GAP:
 *   The central banking establishment and monetary statisticians experience the constraint as a genuine coordination solution they maintain; from their vantage point, M4/M5 represents a rational response to real technical problems in monetary aggregation. Mainstream economists experience it as a constraint they must work within, sometimes feeling its arbitrariness but lacking strong incentive to exit. Heterodox economists and alternative payment researchers experience it as a snare: a definitional gatekeeping mechanism that excludes their work and makes their research subjects appear illegitimate by fiat. The engine will compute per-seat classifications from this structural data: beneficiary seats (central bankers, statisticians) should experience high coordination value and low extraction; constrained payer seats (mainstream economists) should experience moderate-to-high extraction; excluded seats (heterodox theorists) should experience near-snare dynamics (asymmetric extraction with active enforcement excluding alternatives). The measurement series (rising theater, moderate suppression, modest accessibility collapse, persistent resistance) models a constraint that is NOT a natural law, NOT a pure rope, and NOT a full snare — but rather a piton: an atrophied classification scheme maintained by institutional inertia and gatekeeping, not by necessity or consensus benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   The central banking establishment (institutional power, near-perfect exit through re-definition, global scope) is the agenda-setter and primary beneficiary — directionality toward zero (full beneficiary). Monetary statisticians (institutional power, arbitrage-grade exit through professional mobility) are secondary beneficiaries — directionality near 0.2 (beneficiary with some exit options). Mainstream economists (powerful but constrained exit due to sunk theory investment) are moderate payers — directionality near 0.6 (mixed extraction and coordination value). Heterodox economists and alternative researchers (moderate power, identity-locked exit due to research identity fusion with their chosen framework) are victims — directionality near 0.75 (targets, though not trapped because they can in principle exit to heterodox institutions). Policy institutions (institutional observer status, analytical exit) are near-neutral at 0.5. No directionality overrides are needed; the derived values from beneficiary/victim + exit reflect the structural relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a classical mandatrophy case: the founding problem (distinguishing electronic from non-electronic money during financial innovation) was real in 1970 and live through 1990. By 2000-2010, when electronic payment systems matured and became normal practice, the problem the distinction was meant to solve had largely evaporated — practitioners and institutions no longer struggled with the boundary because electronic instruments were now ubiquitous and their monetary role was intuitive. Yet the M4/M5 distinction persists and is actively maintained, despite the founding problem's death. This is mandatrophy: the arrangement outlived its mandate. The constraint survives not because the coordination problem remains live but because institutional inertia, sunk investment in theories built around M4/M5, and the gatekeeping power of statisticians make revision costly. The 'discovering' narrative (statisticians uncovered electronic money through measurement) covers the actual dynamic (the distinction is an arbitrary classification maintained by institutional convenience). The classification persists as theater: continued measurement appears to track a monetary phenomenon, but the phenomenon is the statistical artifact itself, not something real the statistics describe. The mandatrophy verdict is certain: founding_problem_status='dead' while disappearance_verdict='contested' — if M4/M5 disappeared, policy institutions would scramble to find an alternative, showing that something real depends on it (albeit not the founding problem it was built to solve). This mismatch (dead founding problem, contested disappearance) triggers mandatrophy detection, classifying the constraint as a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergence_vs_retroactive_definition,
    'Did electronic money ''emerge'' as a real monetary phenomenon prior to its statistical codification in the M4/M5 framework, or was it created by the act of measurement itself?',
    'Historical documentation of dematerialized holdings, electronic transfer systems, and institutional practices predating the M4/M5 distinction. Compare dates: when practitioners first used electronic instruments vs. when statisticians first measured them as a separate category.',
    'If emergence precedes measurement, the constraint is a descriptive (albeit imperfect) classification of pre-existing reality, reducing extractiveness. If measurement creates the category retroactively, the constraint is a pure definitional piton with no independent referent — validating the m4_m5_collapse reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergence_vs_retroactive_definition, empirical, 'Whether electronic money existed prior to its statistical definition or was created by measurement.').

omega_variable(
    measurement_ontology_ambiguity,
    'In what sense can a statistical category be said to ''create'' a monetary phenomenon? Is this a real ontological claim (the thing did not exist until measured) or a semantic claim (we did not have a name/concept for it until we measured it)?',
    'Philosophical/linguistic analysis of what counts as a thing coming into existence. Examine whether practitioners experienced electronic instruments as a unified monetary category before statistics did; examine whether the statistical codification changed how practitioners understood their own activity.',
    'A purely semantic reading (we just named something that always existed) would reduce the constraint''s extractiveness and shift classification toward rope. An ontological reading (measurement creates the thing) validates the piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_ontology_ambiguity, conceptual, 'What kind of claim is made when a statistical category is said to retroactively create a monetary phenomenon.').

omega_variable(
    alternative_aggregation_schemes,
    'Could the coordination problem the M4/M5 distinction solves have been solved by alternative measurement schemes, and if so, why was this particular scheme adopted and maintained?',
    'Examine the history of alternative monetary aggregates proposed by central banks or economists; document why M4/M5 was chosen over alternatives; trace whether the choice was based on empirical superiority or institutional convenience.',
    'If alternative schemes were equally viable, the M4/M5 choice is revealed as arbitrary and the constraint appears more extractive (institutional lock-in rather than necessary coordination). If M4/M5 is empirically optimal, it appears more ropey.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_aggregation_schemes, empirical, 'Whether the M4/M5 distinction was the only viable solution or one choice among viable alternatives.').

omega_variable(
    suppression_mechanism_structural_or_internalized,
    'Is the suppression of heterodox monetary theories and alternative payment research structural (gatekeeping by journals/institutions that use M4/M5 as canonical) or internalized (researchers have absorbed the M4/M5 framework as commonsense and no longer question it)?',
    'Post-exit trajectory analysis: if researchers who leave mainstream institutions and adopt alternative frameworks stop suppressing their own critique of M4/M5, the suppression was partly structural. Survey heterodox economists on whether they avoid M4/M5 critique due to external pressure or because they doubt the critique themselves.',
    'If structural, the constraint''s effective suppression is higher than measured (the target carries the suppression into alternative spaces). If internalized, the constraint''s persistence is more stable but the escape routes are fewer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_or_internalized, empirical, 'Whether suppression of M4/M5 critique is structural gatekeeping or internalized methodological doubt.').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the ''electronic money emergence'' kernel. Is there a single underlying fact of when electronic money emerged, or are the multiple readings (became_thinkable_reading, first_held_reading, m4_m5_collapse_reading) measuring fundamentally different things that can coexist as valid alternative readings of the same kernel commitment?',
    'Examine whether the three readings answer the same question (when did electronic money emerge?) with different answers, or whether they are answering three different questions. If different questions, the kernel is under-specified; if same question, determine which reading''s core premise logically rules out others.',
    'If the readings are answering the same question, at most one can be correct (unless they measure different aspects of a multivalent phenomenon). If they answer different questions, they can coexist, and the kernel contest is one of framing rather than truth. This affects whether m4_m5_collapse reading forecloses or merely influences the others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the kernel contest is a dispute over one fact or a disagreement about which facts are relevant to ''emergence''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__m4_m5_collapse_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1970, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1970, 0.22).
narrative_ontology:measurement_basis(elec_tr_t1970, observed).
narrative_ontology:measurement(elec_tr_t1980, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement_basis(elec_tr_t1980, observed).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1990, 0.48).
narrative_ontology:measurement_basis(elec_tr_t1990, observed).
narrative_ontology:measurement(elec_tr_t2000, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2000, 0.62).
narrative_ontology:measurement_basis(elec_tr_t2000, observed).
narrative_ontology:measurement(elec_tr_t2010, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2010, 0.67).
narrative_ontology:measurement_basis(elec_tr_t2010, observed).
narrative_ontology:measurement(elec_tr_t2020, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2020, 0.68).
narrative_ontology:measurement_basis(elec_tr_t2020, observed).
narrative_ontology:measurement(elec_tr_t2025, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2025, 0.68).
narrative_ontology:measurement_basis(elec_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(elec_be_t1970, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement_basis(elec_be_t1970, observed).
narrative_ontology:measurement(elec_be_t1980, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement_basis(elec_be_t1980, observed).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement_basis(elec_be_t1990, observed).
narrative_ontology:measurement(elec_be_t2000, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement_basis(elec_be_t2000, observed).
narrative_ontology:measurement(elec_be_t2010, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement_basis(elec_be_t2010, observed).
narrative_ontology:measurement(elec_be_t2020, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement_basis(elec_be_t2020, observed).
narrative_ontology:measurement(elec_be_t2025, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(elec_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t1970, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 1970, 0.28).
narrative_ontology:measurement_basis(elec_su_t1970, observed).
narrative_ontology:measurement(elec_su_t1980, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 1980, 0.42).
narrative_ontology:measurement_basis(elec_su_t1980, observed).
narrative_ontology:measurement(elec_su_t1990, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement_basis(elec_su_t1990, observed).
narrative_ontology:measurement(elec_su_t2000, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2000, 0.64).
narrative_ontology:measurement_basis(elec_su_t2000, observed).
narrative_ontology:measurement(elec_su_t2010, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement_basis(elec_su_t2010, observed).
narrative_ontology:measurement(elec_su_t2020, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement_basis(elec_su_t2020, observed).
narrative_ontology:measurement(elec_su_t2025, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2025, 0.71).
narrative_ontology:measurement_basis(elec_su_t2025, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1970, tn=2025
narrative_ontology:measurement(elec_grid_01, electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse(class), 1970, 0.42).
narrative_ontology:measurement(elec_grid_02, electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse(class), 2025, 0.48).
narrative_ontology:measurement(elec_grid_03, electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse(individual), 1970, 0.38).
narrative_ontology:measurement(elec_grid_04, electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse(individual), 2025, 0.44).
narrative_ontology:measurement(elec_grid_05, electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse(organizational), 1970, 0.35).
narrative_ontology:measurement(elec_grid_06, electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse(organizational), 2025, 0.52).
narrative_ontology:measurement(elec_grid_07, electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse(structural), 1970, 0.28).
narrative_ontology:measurement(elec_grid_08, electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse(structural), 2025, 0.38).
narrative_ontology:measurement(elec_grid_09, electronic_money_emergence__m4_m5_collapse_reading, resistance(class), 1970, 0.48).
narrative_ontology:measurement(elec_grid_10, electronic_money_emergence__m4_m5_collapse_reading, resistance(class), 2025, 0.58).
narrative_ontology:measurement(elec_grid_11, electronic_money_emergence__m4_m5_collapse_reading, resistance(individual), 1970, 0.52).
narrative_ontology:measurement(elec_grid_12, electronic_money_emergence__m4_m5_collapse_reading, resistance(individual), 2025, 0.62).
narrative_ontology:measurement(elec_grid_13, electronic_money_emergence__m4_m5_collapse_reading, resistance(organizational), 1970, 0.35).
narrative_ontology:measurement(elec_grid_14, electronic_money_emergence__m4_m5_collapse_reading, resistance(organizational), 2025, 0.42).
narrative_ontology:measurement(elec_grid_15, electronic_money_emergence__m4_m5_collapse_reading, resistance(structural), 1970, 0.42).
narrative_ontology:measurement(elec_grid_16, electronic_money_emergence__m4_m5_collapse_reading, resistance(structural), 2025, 0.38).
narrative_ontology:measurement(elec_grid_17, electronic_money_emergence__m4_m5_collapse_reading, stakes_inflation(class), 1970, 0.15).
narrative_ontology:measurement(elec_grid_18, electronic_money_emergence__m4_m5_collapse_reading, stakes_inflation(class), 2025, 0.38).
narrative_ontology:measurement(elec_grid_19, electronic_money_emergence__m4_m5_collapse_reading, stakes_inflation(individual), 1970, 0.12).
narrative_ontology:measurement(elec_grid_20, electronic_money_emergence__m4_m5_collapse_reading, stakes_inflation(individual), 2025, 0.28).
narrative_ontology:measurement(elec_grid_21, electronic_money_emergence__m4_m5_collapse_reading, stakes_inflation(organizational), 1970, 0.22).
narrative_ontology:measurement(elec_grid_22, electronic_money_emergence__m4_m5_collapse_reading, stakes_inflation(organizational), 2025, 0.58).
narrative_ontology:measurement(elec_grid_23, electronic_money_emergence__m4_m5_collapse_reading, stakes_inflation(structural), 1970, 0.18).
narrative_ontology:measurement(elec_grid_24, electronic_money_emergence__m4_m5_collapse_reading, stakes_inflation(structural), 2025, 0.42).
narrative_ontology:measurement(elec_grid_25, electronic_money_emergence__m4_m5_collapse_reading, suppression(class), 1970, 0.35).
narrative_ontology:measurement(elec_grid_26, electronic_money_emergence__m4_m5_collapse_reading, suppression(class), 2025, 0.72).
narrative_ontology:measurement(elec_grid_27, electronic_money_emergence__m4_m5_collapse_reading, suppression(individual), 1970, 0.28).
narrative_ontology:measurement(elec_grid_28, electronic_money_emergence__m4_m5_collapse_reading, suppression(individual), 2025, 0.71).
narrative_ontology:measurement(elec_grid_29, electronic_money_emergence__m4_m5_collapse_reading, suppression(organizational), 1970, 0.22).
narrative_ontology:measurement(elec_grid_30, electronic_money_emergence__m4_m5_collapse_reading, suppression(organizational), 2025, 0.75).
narrative_ontology:measurement(elec_grid_31, electronic_money_emergence__m4_m5_collapse_reading, suppression(structural), 1970, 0.15).
narrative_ontology:measurement(elec_grid_32, electronic_money_emergence__m4_m5_collapse_reading, suppression(structural), 2025, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__m4_m5_collapse_reading, information_standard).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__m4_m5_collapse_reading, 0.06).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__first_held_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'electronic_money_emergence' kernel. The three readings are structurally distinct constraints with different ε values and different victim/beneficiary structures. The m4_m5_collapse_reading (this file) claims the emergence was retroactively constructed by measurement, making it a piton with moderate extractiveness. The became_thinkable_reading would claim emergence occurred when the concept was possible, making it a rope or even a mountain if the concept's possibility is treated as natural. The first_held_reading would claim emergence occurred at first institutional practice, making it a rope. These readings cannot be merged — they answer the same question (when did electronic money emerge?) with incompatible answers, and each would be confounded by averaging. The constraint family exists to preserve the contest: each reading is authored as a clean constraint, and the network links show which readings affect which others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
