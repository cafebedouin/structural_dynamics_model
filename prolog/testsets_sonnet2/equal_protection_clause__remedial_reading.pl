% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__remedial_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: equal_protection_clause__remedial_reading
 *   human_readable: Equal Protection Clause — Remedial (Substantive Equality) Reading
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   This story instantiates the remedial reading of the Equal Protection
 *   Clause: that the clause not only permits but requires race-conscious
 *   action to remediate historical group subordination, so long as documented
 *   disparities traceable to that subordination persist. This is the
 *   highest-ε reading of the equal-protection kernel because it treats
 *   race-consciousness as constitutionally mandatory rather than merely
 *   permitted (diversity_reading) or forbidden (colorblind_reading) — its
 *   coordination story (closing a specific historical debt) is real but its
 *   transfer mechanism (positional goods moved from individual non-preferred
 *   applicants to preferred group members) is direct and identifiable, which
 *   is why the claimed type is scaffold with active enforcement rather than a
 *   pure rope: the constraint is meant to sunset once the specific remediable
 *   gap closes, but until then it operates with real coercive weight on
 *   displaced individuals.
 *
 * KEY AGENTS:
 *   - historically_subordinated_racial_minorities: beneficiary group whose group-level history licenses individual-level preference
 *   - non_preferred_group_individual_applicants: bear individualized cost for a group-level historical harm
 *   - remediating_institutions: administer and defend the remedial criteria, gaining legitimacy and diversity outcomes
 *   - courts_and_constitutional_interpreters: analytical seat adjudicating narrow tailoring and compelling interest
 *   - future_generations_post_remediation: excluded from judging whether the sunset condition has been met
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.68).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.52).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, scaffold).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Equal Protection Clause — Remedial (Substantive Equality) Reading").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).
narrative_ontology:has_sunset_clause(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, '958758f8-1756-4e1b-af40-20ea03046c0a').
narrative_ontology:cs_kernel_codification('958758f8-1756-4e1b-af40-20ea03046c0a', fixed_text).
narrative_ontology:cs_authority_grounding('958758f8-1756-4e1b-af40-20ea03046c0a', lineage).
narrative_ontology:cs_interpretation_layer_present('958758f8-1756-4e1b-af40-20ea03046c0a').
narrative_ontology:cs_reading_relation('958758f8-1756-4e1b-af40-20ea03046c0a', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('958758f8-1756-4e1b-af40-20ea03046c0a', equal_protection_clause__diversity_reading, influences).
narrative_ontology:cs_axiom('958758f8-1756-4e1b-af40-20ea03046c0a', foundational, equal_protection_has_antisubordination_purpose).
narrative_ontology:cs_axiom_status(equal_protection_has_antisubordination_purpose, holdable).
narrative_ontology:cs_axiom_grounding('958758f8-1756-4e1b-af40-20ea03046c0a', equal_protection_has_antisubordination_purpose, conventional).
narrative_ontology:cs_axiom('958758f8-1756-4e1b-af40-20ea03046c0a', foundational, group_level_historical_debt_licenses_group_conscious_remedy).
narrative_ontology:cs_axiom_status(group_level_historical_debt_licenses_group_conscious_remedy, holdable).
narrative_ontology:cs_axiom_grounding('958758f8-1756-4e1b-af40-20ea03046c0a', group_level_historical_debt_licenses_group_conscious_remedy, empirically_contingent).
narrative_ontology:cs_reference_frame('958758f8-1756-4e1b-af40-20ea03046c0a', reconstruction_era_antisubordination_purpose).
narrative_ontology:cs_drift_state('958758f8-1756-4e1b-af40-20ea03046c0a', post_civil_rights_era_strict_scrutiny_regime, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('958758f8-1756-4e1b-af40-20ea03046c0a', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, historically_subordinated_racial_minorities).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, black_applicant_pool).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, indigenous_and_latino_applicant_pool).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, non_preferred_group_individual_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, marginal_non_preferred_applicants_displaced).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, substantive_equality_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, group_subordination_remediation_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enter selective institutions and public programs under admissions or allocation rules that weight group history of subordination as a factor favoring access. The remediation is framed as compensatory for documented past exclusion (segregation, redlining, disenfranchisement) whose effects the group argues persist structurally today. They cannot individually exit the group category the remedy is keyed to.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, historically_subordinated_racial_minorities, beneficiary,
    organized, generational, constrained, national).

% A specific instantiation of the beneficiary class in university admissions and public contracting: receives a preference calibrated to documented historical subordination (slavery, Jim Crow, redlining) rather than to individual disadvantage. Benefits accrue by group membership, not by demonstrated individual hardship.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, black_applicant_pool, beneficiary,
    organized, generational, constrained, national).

% A second beneficiary instantiation with a distinct subordination history (dispossession, conquest, exclusion). Grouped with other beneficiary classes under the same remedial rationale despite different historical particulars, which some members experience as flattening distinct claims into one racial remedy category.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, indigenous_and_latino_applicant_pool, beneficiary,
    organized, generational, constrained, national).

% Compete for the same finite slots (admissions seats, contracts, positions) without the group-history weighting, regardless of their own individual circumstances — including applicants who are themselves poor, first-generation, or otherwise disadvantaged but fall outside the remedial group definition. They bear a concrete individual cost (a specific denied seat) for a harm (historical group subordination) they did not personally inflict.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, non_preferred_group_individual_applicants, payer,
    moderate, biographical, constrained, national).

% The specific individuals at the admissions or allocation margin who would have received the slot or contract absent the remedial weighting. Their exit option is essentially nonexistent for that specific decision cycle — the loss is immediate, individualized, and non-repeatable even though the policy rationale is diffuse and forward-looking.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, marginal_non_preferred_applicants_displaced, payer,
    powerless, immediate, trapped, regional).

% Universities, public agencies, and contracting bodies that design and administer race-conscious remedial criteria, define which groups qualify as historically subordinated, and set the weighting given to that history. They bear reputational and legal risk but also gain legitimacy, diversity metrics, and insulation from claims of complicity in historical exclusion by adopting the remedial framework.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, remediating_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Adjudicate whether a given remedial program satisfies strict scrutiny, evaluating whether the remediation is narrowly tailored to a documented, specific instance of prior discrimination rather than general societal subordination. Their doctrine determines whether this reading of equal protection is legally sustainable at all.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, courts_and_constitutional_interpreters, observer,
    institutional, civilizational, analytical, national).

% Would inherit whatever equilibrium results once (or if) the remedial program sunsets — either genuine substantive parity or an entrenched preference regime with no natural end point. They have no voice in whether the sunset condition is ever actually judged satisfied, since that judgment is made by the remediating institutions and courts, not by them.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, future_generations_post_remediation, excluded,
    powerless, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__remedial_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_clause__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a societal commitment to closing measurable, documented gaps in group outcomes (wealth, representation, institutional access) traceable to specific historical state-sanctioned subordination, by using race as a proxy for group-specific accumulated disadvantage that individual-blind criteria fail to capture.
% TRANSFER_FUNCTION: Moves scarce positional goods — admissions seats, contracts, hiring slots — from individual members of non-preferred groups at the competitive margin to individual members of groups defined by historical subordination, on the premise that the transfer offsets a debt owed at the group level.
% ABSENT_VOICES: Individual non-preferred applicants displaced at the margin rarely have a forum to make their specific case (their loss is diffused into aggregate litigation); intra-group dissenters among beneficiary populations who reject being defined primarily by historical subordination are also largely absent from the doctrinal debate, which proceeds at the level of group categories.
% DISAPPEARANCE_RATIONALE: If the remedial reading were repudiated overnight, selective institutions would revert to facially race-neutral criteria (test scores, need-based proxies, geographic diversity), beneficiary group representation in elite admissions would likely decline sharply in the near term, and remediating institutions would lose the doctrinal cover that currently lets them frame these programs as constitutionally compelled rather than merely permitted.
% FOUNDING_PROBLEM: State-sanctioned segregation, disenfranchisement, and exclusion produced durable, measurable group disadvantage (wealth gaps, educational access gaps, representation gaps) that facially neutral post-Reconstruction and post-Civil-Rights-Act rules did not close, and arguably could not close given how the disadvantage was structurally embedded.
% FOUNDING_PROBLEM_CORROBORATION: Economists and sociologists outside the remediating institutions (e.g., wealth-gap and intergenerational-mobility researchers) corroborate that measurable group disparities traceable to historical subordination persist. However, courts applying strict scrutiny and colorblind-reading advocates dispute that the persistence of a disparity implies the state may still constitutionally treat it as a live, remediable harm rather than a diffuse societal condition — the corroboration on the empirical gap does not settle the constitutional question of whether that gap remains a 'live' problem this specific remedy may address.
narrative_ontology:disappearance_verdict(equal_protection_clause__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_clause__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__remedial_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the remedy operates through a direct, zero-sum reallocation of scarce positional goods rather than expanding the total pool — every unit of remedial benefit to the beneficiary class corresponds to an identifiable individual cost to a displaced non-preferred applicant. Suppression is moderate (0.52) because the constraint's persistence depends on active judicial and institutional defense against strict-scrutiny challenges, but it does not rely on suppressing exit or alternatives to the same degree a snare would — displaced applicants can litigate, apply elsewhere, or contest the criteria in the political process. Theater ratio is low-moderate (0.22) and rising, reflecting a growing gap between programs justified as remediation for a specific historical harm and administration that increasingly tracks general diversity or holistic-review goals once litigation risk rose after strict scrutiny intensified. Resistance is high (0.72) because this reading is the most doctrinally contested of the three siblings and meets sustained legal and political challenge. Accessibility collapse is moderate (0.4): non-preferred applicants have real, if costly, alternative pathways (other institutions, other criteria), so alternatives have not collapsed the way they would under a mountain-type constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary group's seat, the constraint functions as overdue coordination correcting a specific, documented historical wrong — a scaffold with a real, if contested, sunset condition (closing the measurable gap). From the marginal displaced applicant's seat, the same structure functions as a concrete, individualized, uncompensated extraction imposed for a historical harm they did not personally commit and cannot personally offset. The engine should compute divergent seat classifications from these structural facts without either seat's framing overriding the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary groups are declared via base_properties.beneficiaries and carry low derived directionality (they receive net positive allocation under the remedial criterion). Non-preferred individual applicants and especially the marginal displaced applicants are declared as victims and carry high derived directionality — the marginal group in particular is trapped for that specific decision cycle (a denied admission or contract cannot be recovered later), which should push their directionality toward the full-target end even relative to the broader non-preferred applicant pool, which retains some constrained mobility (reapplication, alternative institutions).
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification (with declared has_sunset_clause) is essential here to avoid two symmetric mislabeling errors: treating the remedy as permanent pure coordination (a rope) would obscure that it is meant to end once the specific historical gap is closed, licensing indefinite extension without re-justification; treating it as pure extraction (a snare) would ignore the genuine, empirically corroborated coordination function of closing a documented, state-caused disparity. The open mandatrophy question is whether remediating institutions and courts will actually enforce the sunset condition once the founding problem's status shifts from 'live' to 'contested' or 'dead,' or whether the scaffold quietly becomes permanent — this is exactly the drift the theater_ratio trend line is beginning to register.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remediation_sunset_determinacy,
    'Is there any judicially or legislatively administrable standard for determining when historical-subordination remediation has been ''achieved,'' such that the scaffold''s sunset clause is more than aspirational?',
    'Track whether any remedial program under this reading has ever been formally sunset by its administering institution based on a specific, pre-declared metric being satisfied, versus programs that persist indefinitely with shifting justifications.',
    'If no administrable sunset standard exists in practice, the scaffold classification is at risk of being a piton in waiting — a constraint that never actually sunsets despite carrying the formal sunset clause, which would be a distinct classification from the one authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remediation_sunset_determinacy, empirical, 'Whether the remedial reading''s sunset clause is administrable or merely aspirational.').

omega_variable(
    group_versus_individual_harm_attribution,
    'Is it coherent under equal protection doctrine to remedy a group-level historical harm by imposing a cost on specific individuals who did not commit and may not have benefited from that harm, versus individuals in the beneficiary group who may not have suffered its effects?',
    'Doctrinal analysis of whether strict scrutiny''s narrow-tailoring requirement can ever be satisfied by group-proxy remedies, versus requiring individualized showings of harm and benefit — an unresolved and contested question within equal protection jurisprudence itself.',
    'If group-proxy remedies are held categorically unable to satisfy narrow tailoring, this reading''s remedial mechanism becomes constitutionally foreclosed regardless of the empirical merits of the historical-subordination claim, converging structurally toward the colorblind_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(group_versus_individual_harm_attribution, conceptual, 'Whether group-based remedy for individual-level cost/benefit is doctrinally coherent.').

omega_variable(
    empirical_gap_causal_attribution,
    'To what extent are present-day group disparities causally attributable to the specific historical subordination invoked (segregation, redlining, disenfranchisement) versus other intervening factors, and does that attribution matter for whether the remedy remains narrowly tailored?',
    'Longitudinal social-science research isolating the causal contribution of specific historical policies to present disparities, weighed against courts'' willingness to accept diffuse causal claims as sufficient for a ''compelling interest.''',
    'Weak causal attribution would undermine the founding_problem''s ''live'' status and support reclassification toward snare (pure extraction dressed as remediation) or toward the diversity_reading''s forward-looking, less remediation-specific justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_gap_causal_attribution, empirical, 'Strength of causal link between named historical subordination and present-day group disparities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_clause__remedial_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(equa_tr_t8, equal_protection_clause__remedial_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(equa_tr_t16, equal_protection_clause__remedial_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(equa_tr_t24, equal_protection_clause__remedial_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(equa_tr_t32, equal_protection_clause__remedial_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement(equa_tr_t40, equal_protection_clause__remedial_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_clause__remedial_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(equa_be_t8, equal_protection_clause__remedial_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(equa_be_t16, equal_protection_clause__remedial_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(equa_be_t24, equal_protection_clause__remedial_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(equa_be_t32, equal_protection_clause__remedial_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(equa_be_t40, equal_protection_clause__remedial_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_clause__remedial_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(equa_su_t8, equal_protection_clause__remedial_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(equa_su_t16, equal_protection_clause__remedial_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(equa_su_t24, equal_protection_clause__remedial_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(equa_su_t32, equal_protection_clause__remedial_reading, suppression_requirement, 32, 0.51).
narrative_ontology:measurement(equa_su_t40, equal_protection_clause__remedial_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__remedial_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__remedial_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint-family members instantiating the equal_protection_clause kernel. colorblind_reading forecloses race-conscious action of any kind (lowest ε, individual-rights framing); diversity_reading permits but does not require race-consciousness for forward-looking pedagogical benefit (moderate ε, benefits-all-students framing); remedial_reading (this file) requires race-conscious action to remediate documented historical group subordination (highest ε, backward-looking group-debt framing). Each carries its own stable ε and classification per the ε-invariance principle; they are linked here rather than merged because the natural-language label 'equal protection and race' would otherwise conflate three structurally distinct claims with three different beneficiary/victim structures and three different persistence conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
