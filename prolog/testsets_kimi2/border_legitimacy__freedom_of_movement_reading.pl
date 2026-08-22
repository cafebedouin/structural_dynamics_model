% ============================================================================
% CONSTRAINT STORY: border_legitimacy__freedom_of_movement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: border_legitimacy__freedom_of_movement_reading
 *   human_readable: Freedom of Movement Reading of Border Legitimacy
 *   domain: political/migration/law
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom_of_movement_reading of the
 *   contested border_legitimacy kernel. The standing arrangement under
 *   contest is the system of territorial border enforcement. From this
 *   reading, freedom of movement is a human right and borders are
 *   presumptively illegitimate restrictions. The enforcement apparatus
 *   extracts from migrants and refugees through deportation, detention, and
 *   criminalization, and also from domestic displaced workers and welfare
 *   recipients who suffer labor-market segmentation and fiscal closure
 *   legitimated by the border regime. The state apparatus and labor-market
 *   insiders are the structural beneficiaries. The claim is snare; the
 *   metrics are authored independently to describe high extraction, high
 *   suppression, and substantial performative theater.
 *
 * KEY AGENTS:
 *   - Sovereign state apparatus (agenda_setter/institutional/arbitrage) â administers enforcement and captures sovereignty rents
 *   - Labor market insiders (beneficiary/organized/constrained) â receive protected wages and conditions from restricted labor supply
 *   - Economic migrants (payer/powerless/trapped) â criminalized and exploited, bear direct extraction costs
 *   - Refugees (payer/powerless/trapped) â denied entry and confined, bear direct extraction costs
 *   - Displaced workers (payer/powerless/constrained) â domestic victims of segmented labor markets sustained by borders
 *   - Welfare recipients (payer/powerless/constrained) â domestic victims of fiscal closure and austerity legitimated by borders
 *   - Human rights observers (observer/analytical/analytical) â document violence and advocate for mobility rights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, 0.85).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_reading, 0.82).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, snare).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Freedom of Movement Reading of Border Legitimacy").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political/migration/law").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, 'e0f96573-f4c8-408a-83c4-a6225ec94b17').
narrative_ontology:cs_kernel_codification('e0f96573-f4c8-408a-83c4-a6225ec94b17', formalized).
narrative_ontology:cs_authority_grounding('e0f96573-f4c8-408a-83c4-a6225ec94b17', lineage).
narrative_ontology:cs_interpretation_layer_present('e0f96573-f4c8-408a-83c4-a6225ec94b17').
narrative_ontology:cs_reading_relation('e0f96573-f4c8-408a-83c4-a6225ec94b17', border_legitimacy__sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('e0f96573-f4c8-408a-83c4-a6225ec94b17', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('e0f96573-f4c8-408a-83c4-a6225ec94b17', foundational, freedom_of_movement_as_universal_human_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_as_universal_human_right, holdable).
narrative_ontology:cs_axiom_grounding('e0f96573-f4c8-408a-83c4-a6225ec94b17', freedom_of_movement_as_universal_human_right, deontological).
narrative_ontology:cs_axiom('e0f96573-f4c8-408a-83c4-a6225ec94b17', foundational, presumptive_illegitimacy_of_territorial_exclusion).
narrative_ontology:cs_axiom_status(presumptive_illegitimacy_of_territorial_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('e0f96573-f4c8-408a-83c4-a6225ec94b17', presumptive_illegitimacy_of_territorial_exclusion, deontological).
narrative_ontology:cs_reference_frame('e0f96573-f4c8-408a-83c4-a6225ec94b17', unrestricted_territorial_mobility).
narrative_ontology:cs_drift_state('e0f96573-f4c8-408a-83c4-a6225ec94b17', contemporary_westphalian_regime, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('e0f96573-f4c8-408a-83c4-a6225ec94b17', '').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, sovereign_state_apparatus).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, labor_market_insiders).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, refugees).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_workers).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, welfare_recipients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers border enforcement through immigration agencies, customs, and security forces. Collects sovereignty rentsâpolitical legitimacy, fiscal extraction via visa fees and detention infrastructure, and territorial control. Could liberalize borders but chooses sustained enforcement.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, sovereign_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Citizen workers in sectors protected from global labor competition. Benefit from restricted labor supply that sustains wages and working conditions above global market levels. Do not administer the constraint but receive the labor-market premium it creates.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, labor_market_insiders, beneficiary,
    organized, biographical, constrained, national).

% Seek to move across borders for economic opportunity. Face deportation, detention, debt to smugglers, and death in transit. Their movement is criminalized and their labor is exploited when undocumented. No viable exit from the enforcement regime except by abandoning migration.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, economic_migrants, payer,
    powerless, immediate, trapped, global).

% Flee persecution and seek asylum. Confined to camps, denied work rights, or pushed back at borders. Their need for safety is met with enforcement apparatuses designed to refuse entry.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, refugees, payer,
    powerless, immediate, trapped, global).

% Domestic low-wage workers displaced or deskilled by the segmented labor market that border enforcement sustains. Employers use the threat of migrant labor or offshoring to suppress wages and conditions. They are trapped in local labor markets with few alternatives.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, displaced_workers, payer,
    powerless, immediate, constrained, national).

% Receive means-tested public support within a fiscal closure enforced by borders. The same enforcement that excludes migrants is used to justify austerity and conditionality against them, making them targets of the extractive apparatus.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, welfare_recipients, payer,
    powerless, immediate, constrained, national).

% Monitor border violence and advocate for freedom of movement. They document deaths, detention conditions, and labor exploitation, analyzing the gap between human rights norms and enforcement practice.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, human_rights_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocating territorial jurisdiction and membership; regulating access to state-provided public goods and domestic labor markets.
% TRANSFER_FUNCTION: Moves freedom of residence and lawful employment from non-citizens to the sovereign state and labor-market insiders, extracting compliance costs, detention, deportation, and wage suppression from migrants and domestic vulnerable populations.
% ABSENT_VOICES: Economic migrants without legal status are structurally excluded from democratic deliberation; refugees in detention lack voice; future potential migrants are unrepresented; undocumented workers fear visibility and deportation if they speak.
% DISAPPEARANCE_RATIONALE: If territorial border enforcement disappeared, global labor markets would rapidly re-equilibrate, state fiscal and social insurance systems would face fundamental restructuring, and the distribution of population across territories would shift dramatically. The current architecture of citizenship, wage segmentation, and state sovereignty depends on the constraint.
% FOUNDING_PROBLEM: The need to organize collective self-governance and public goods provision within defined territories and to manage interstate conflict over jurisdiction following the Westphalian settlement.
% FOUNDING_PROBLEM_CORROBORATION: International historians and international relations scholars attest the Westphalian founding problem from outside the beneficiary set. Human rights organizations and open-borders philosophers attest that the problem is superseded by global interdependence and does not justify current extraction. No neutral arbiter attests unambiguously; the corroboration is split across normative frameworks.
narrative_ontology:disappearance_verdict(border_legitimacy__freedom_of_movement_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__freedom_of_movement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__freedom_of_movement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_legitimacy__freedom_of_movement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__freedom_of_movement_reading, 0.85, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.85) because the constraint denies a claimed human right and imposes severe material and bodily costs. Suppression is high (0.82) because persistence depends on active, often violent enforcement: deportation, militarized borders, and detention. Theater ratio is substantial (0.58) because border security has become heavily performative (walls, spectacles) relative to its actual function. Accessibility collapse is high (0.78): open borders are treated as politically impossible and are largely absent from mainstream policy discourse. Resistance is moderate (0.55): migrant movements, human rights organizations, and some scholars contest the regime, but enforcement capacity continues to expand.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus experiences border enforcement as necessary governance and sovereignty maintenance; labor insiders experience it as legitimate protection. Migrants and refugees experience it as violent extraction and bodily endangerment. Domestic displaced workers and welfare recipients experience it as labor-market rigidity and fiscal conditionality that harms them despite their formal citizenship. The engine computes these divergent seat types from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The sovereign state apparatus and labor-market insiders are declared beneficiaries, giving them directionality near the subsidy end. Economic migrants, refugees, displaced workers, and welfare recipients are declared victims (role: payer), giving them directionality near the full-target end. Migrants and refugees are trapped (exit: trapped), amplifying their effective extraction. Domestic victims are constrained (exit: constrained) within the national economy. The state apparatus has arbitrage-grade exit (it could liberalize) but chooses enforcement, maintaining a low derived d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâorganizing territorial governance and public goods after Westphaliaâis contested by this reading. The constraint persists well beyond any original coordination justification because the enforcement apparatus has become self-sustaining: states extract political legitimacy, fiscal revenue, and labor-market control from the arrangement. The reading prevents mislabeling by distinguishing the historical origin of territorial jurisdiction from the current extractive enforcement regime, which now functions as a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint instantiates the freedom_of_movement_reading of the border_legitimacy kernel. How would classification change under the sovereignty_reading or humanitarian_obligation_reading?',
    'Compare compiled stories for sibling readings in the same kernel family.',
    'The sovereignty_reading would reverse beneficiary/victim roles and likely classify as rope or mountain; the humanitarian_obligation_reading would narrow the victim set to refugees and classify as tangled_rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Position of this reading within the border legitimacy kernel.').

omega_variable(
    domestic_victim_extraction,
    'Does border enforcement genuinely extract from domestic displaced workers and welfare recipients, or are their harms caused by other structural factors that enforcement merely correlates with?',
    'Empirical analysis of wage and welfare effects in jurisdictions with varying enforcement intensity, controlling for automation and domestic policy.',
    'If domestic harms are independently caused, the victim set contracts to migrants only and base extractiveness may lower; if border enforcement is a direct cause, the high-epsilon reading is supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domestic_victim_extraction, empirical, 'Whether current citizens are structural victims of border enforcement.').

omega_variable(
    coordination_or_cover,
    'Is there any genuine coordination function (public goods, democratic closure) separable from the extractive mechanism, or is the coordination story entirely cover for extraction?',
    'Comparative analysis of jurisdictions with open versus controlled borders, measuring public goods provision and democratic participation independently.',
    'If separable, the constraint may be a tangled_rope rather than a snare; if inseparable, the snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_or_cover, conceptual, 'Whether border enforcement has a genuine coordination component separable from extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 0, 74).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(border_fom_tr_t0, border_legitimacy__freedom_of_movement_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(border_fom_tr_t15, border_legitimacy__freedom_of_movement_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(border_fom_tr_t30, border_legitimacy__freedom_of_movement_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(border_fom_tr_t45, border_legitimacy__freedom_of_movement_reading, theater_ratio, 45, 0.45).
narrative_ontology:measurement(border_fom_tr_t60, border_legitimacy__freedom_of_movement_reading, theater_ratio, 60, 0.52).
narrative_ontology:measurement(border_fom_tr_t74, border_legitimacy__freedom_of_movement_reading, theater_ratio, 74, 0.58).

% Extraction over time
narrative_ontology:measurement(border_fom_be_t0, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(border_fom_be_t15, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(border_fom_be_t30, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(border_fom_be_t45, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 45, 0.74).
narrative_ontology:measurement(border_fom_be_t60, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 60, 0.8).
narrative_ontology:measurement(border_fom_be_t74, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 74, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(border_fom_su_t0, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(border_fom_su_t15, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(border_fom_su_t30, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(border_fom_su_t45, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 45, 0.72).
narrative_ontology:measurement(border_fom_su_t60, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(border_fom_su_t74, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 74, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__freedom_of_movement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% The border_legitimacy kernel decomposes into three structurally distinct constraints because the natural-language concept conflates territorial sovereignty, humanitarian exception, and universal freedom of movement. Each reading has a distinct epsilon, beneficiary/victim structure, and classification. This reading (freedom_of_movement) is the most extractive; the sovereignty reading is the least.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
