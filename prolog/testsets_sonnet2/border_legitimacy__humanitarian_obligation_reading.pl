% ============================================================================
% CONSTRAINT STORY: border_legitimacy__humanitarian_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__humanitarian_obligation_reading, []).

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
 *   constraint_id: border_legitimacy__humanitarian_obligation_reading
 *   human_readable: Humanitarian Obligation Reading of Border Legitimacy (Refugee/Economic Migrant Distinction)
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This constraint instantiates the humanitarian-obligation reading of the
 *   contested border-legitimacy kernel: states owe admission to those fleeing
 *   individualized persecution (the Convention refugee) but not to those
 *   fleeing generalized economic hardship. The reading solves a genuine
 *   coordination problem — bounding an otherwise open-ended moral obligation
 *   so it is administrable — but the line it draws has hardened into a
 *   categorical apparatus that excludes an ever-larger population of climate-
 *   and violence-displaced people who do not fit the
 *   persecution-on-protected-grounds template. The ε referent here is the
 *   standing refugee/economic-migrant line as currently administered, not any
 *   endorsed alternative (open admission or pure sovereignty) — those are the
 *   sibling readings, evaluated in their own files.
 *
 * KEY AGENTS:
 *   - recognized_refugees: primary intended beneficiary of the humanitarian obligation
 *   - economic_migrants and survival_migrants_outside_convention_categories: excluded populations bearing the cost of the categorical line
 *   - asylum_seekers_misclassified_as_economic: bear administrative error cost even when they satisfy the reading's own criterion
 *   - host_state_publics: beneficiaries of a bounded, politically sustainable obligation
 *   - asylum_adjudication_apparatus: agenda-setter with institutional stake in maintaining the line
 *   - sending_states: excluded from classification debates despite material stakes
 *   - international_law_scholars_and_unhcr: analytical observers documenting the line's empirical strain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.52).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.61).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Humanitarian Obligation Reading of Border Legitimacy (Refugee/Economic Migrant Distinction)").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, '6563656b-7666-4b47-b17c-6ae1bfe97cce').
narrative_ontology:cs_kernel_codification('6563656b-7666-4b47-b17c-6ae1bfe97cce', formalized).
narrative_ontology:cs_authority_grounding('6563656b-7666-4b47-b17c-6ae1bfe97cce', lineage).
narrative_ontology:cs_interpretation_layer_present('6563656b-7666-4b47-b17c-6ae1bfe97cce').
narrative_ontology:cs_reading_relation('6563656b-7666-4b47-b17c-6ae1bfe97cce', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('6563656b-7666-4b47-b17c-6ae1bfe97cce', border_legitimacy__freedom_of_movement_reading, influences).
narrative_ontology:cs_axiom('6563656b-7666-4b47-b17c-6ae1bfe97cce', foundational, individualized_persecution_grounds_admission_duty).
narrative_ontology:cs_axiom_status(individualized_persecution_grounds_admission_duty, holdable).
narrative_ontology:cs_axiom_grounding('6563656b-7666-4b47-b17c-6ae1bfe97cce', individualized_persecution_grounds_admission_duty, deontological).
narrative_ontology:cs_axiom('6563656b-7666-4b47-b17c-6ae1bfe97cce', foundational, generalized_hardship_does_not_ground_admission_duty).
narrative_ontology:cs_axiom_status(generalized_hardship_does_not_ground_admission_duty, holdable).
narrative_ontology:cs_axiom_grounding('6563656b-7666-4b47-b17c-6ae1bfe97cce', generalized_hardship_does_not_ground_admission_duty, conventional).
narrative_ontology:cs_reference_frame('6563656b-7666-4b47-b17c-6ae1bfe97cce', id_1951_convention_persecution_standard).
narrative_ontology:cs_drift_state('6563656b-7666-4b47-b17c-6ae1bfe97cce', contemporary_climate_displacement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6563656b-7666-4b47-b17c-6ae1bfe97cce', '').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, recognized_refugees).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, host_state_publics).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, asylum_adjudication_apparatus).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, survival_migrants_outside_convention_categories).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, asylum_seekers_misclassified_as_economic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Flee persecution or war and are admitted, granted status, and protected from refoulement under the humanitarian obligation. Their admission is conditioned on proving they fit the 1951 Convention's persecution grounds — a narrow legal category they did not design and cannot always satisfy even when their lives are genuinely at risk.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, recognized_refugees, beneficiary,
    powerless, biographical, trapped, national).

% Flee destitution, climate collapse of livelihoods, or generalized state failure that falls short of individualized persecution. Under this reading they have no admission claim at all — the same border apparatus that admits a recognized refugee categorically excludes them, often through the identical crossing point, on the basis of a distinction they experience as arbitrary given comparably desperate circumstances.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, economic_migrants, payer,
    powerless, biographical, trapped, national).

% Flee famine, climate disaster, or generalized violence that does not meet the Convention's persecution-on-protected-grounds test. They are functionally in the same position as recognized refugees from their own point of view but sit on the wrong side of the doctrinal line this reading draws, and bear the full cost of exclusion despite having fled comparably existential threats.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, survival_migrants_outside_convention_categories, payer,
    powerless, biographical, trapped, national).

% Have genuine persecution claims but are screened out at the border or in adjudication due to evidentiary burden, adjudicator discretion, or credibility assessments skewed by resource constraints and political pressure to keep admission numbers low. They bear the cost of the categorical line's imperfect administration even though their situation matches the reading's own admission criterion.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, asylum_seekers_misclassified_as_economic, payer,
    powerless, immediate, trapped, national).

% Receive a bounded, legally defensible admission obligation instead of an open-ended one. The persecution/economic distinction lets the state claim moral seriousness (protecting the genuinely persecuted) while retaining broad discretion to exclude the much larger population of people fleeing poverty or slow-onset disaster, which is what makes the arrangement politically sustainable domestically.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, host_state_publics, beneficiary,
    organized, generational, mobile, national).

% Administers refugee status determination, border screening, and detention/removal for those who fail the persecution test. Its institutional existence, budget, and expertise are built around maintaining and adjudicating exactly this line; it has no incentive to see the line dissolve into either open admission or pure sovereign discretion.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, asylum_adjudication_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__humanitarian_obligation_reading, asylum_adjudication_apparatus, beneficiary).

% Bear the consequences of population outflow and the diplomatic framing that labels their emigrants either 'refugees' (an implicit indictment of the sending state) or 'economic migrants' (implicitly a judgment on their economic governance). They are not party to the receiving state's admission criteria despite having material stakes in how their nationals are classified.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, sending_states, excluded,
    moderate, generational, constrained, national).

% Study, critique, and attempt to reform the persecution/economic-migrant line, documenting its empirical breakdown under climate displacement and generalized violence that the 1951 Convention's drafters did not anticipate. They can propose new categories (e.g., complementary protection) but cannot bind states to adopt them.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, international_law_scholars_and_unhcr, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__humanitarian_obligation_reading, diffuse).
narrative_ontology:fixing_cost_class(border_legitimacy__humanitarian_obligation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides states a workable, bounded criterion for allocating a scarce, costly obligation (admission, housing, legal status, integration support) that avoids both the extremes of unlimited admission and blanket refusal — a line that lets a state discharge a moral obligation without treating every act of border-crossing as equally obligating.
% TRANSFER_FUNCTION: Moves protection, legal status, and integration resources to those who can demonstrate individualized persecution on a protected ground, while withholding the same from those fleeing comparably severe but differently-caused threats (poverty, climate collapse, generalized violence) — the transfer runs from the excluded survival-migrant population to the admitted refugee population and to the host state's political capacity to say no to the rest.
% ABSENT_VOICES: Sending states have no voice in how their emigrants are classified. Survival migrants fleeing climate disaster or generalized violence — the fastest-growing displacement category — have no forum to argue their exclusion from protection is itself a moral failure of the categorical line; they appear in the story only as the excluded remainder, not as parties to the admission debate.
% DISAPPEARANCE_RATIONALE: If the persecution/economic-migrant distinction disappeared overnight, states would either have to adopt a much broader admission obligation (collapsing toward the freedom-of-movement reading) or fall back on pure discretionary exclusion (collapsing toward the sovereignty reading) — the entire refugee status determination apparatus, detention/removal infrastructure for 'failed' claimants, and the diplomatic vocabulary of 'genuine refugee' vs. 'economic migrant' would need to be rebuilt or abandoned.
% FOUNDING_PROBLEM: After WWII, mass displacement of people fleeing state-sponsored persecution required an internationally agreed standard so states could not simply return people to face death or torture, while stopping short of obligating unlimited admission for any reason whatsoever.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR and refugee law scholars (outside the population of admitting states) attest the founding problem persists but has been outrun by new displacement drivers (climate, generalized violence, state collapse) that the persecution-ground test was never built to address; host states attest the original problem — protecting against refoulement of the individually persecuted — remains adequately served by the current line and resist widening it, which is itself evidence the current boundary now also serves an admission-limiting function beyond its founding purpose.
narrative_ontology:disappearance_verdict(border_legitimacy__humanitarian_obligation_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__humanitarian_obligation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__humanitarian_obligation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_legitimacy__humanitarian_obligation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__humanitarian_obligation_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__humanitarian_obligation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__humanitarian_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) rather than high: the reading does deliver real protection to a genuine beneficiary class (recognized refugees), which is the coordination function required for tangled_rope. But it also structurally manufactures a bifurcated victim set — survival migrants and misclassified asylum seekers — who bear categorical exclusion through the same enforcement apparatus that protects refugees. Suppression (0.61) reflects the active enforcement machinery (border screening, detention, removal) required to hold the line against the much larger economic-migrant population. Theater ratio (0.38) and its rising trajectory reflect growing gap between the doctrine's stated purpose (protecting the individually persecuted) and its administered function (limiting total admissions), as climate and generalized-violence displacement outpaces what the persecution-ground test was built to recognize.
 *
 * DIRECTIONALITY LOGIC:
 *   Recognized refugees sit near the beneficiary end: the constraint's whole justification is protecting them, though even they must survive an adversarial adjudication process. Economic migrants and survival migrants outside Convention categories sit at the full-target end: trapped, powerless, and categorically excluded by design — the constraint's entire discriminating function operates against them. Asylum seekers misclassified as economic migrants are a distinct, especially harsh victim class: they satisfy the reading's own criterion but are excluded by administrative failure, meaning the line harms even people it claims to protect. Host state publics and the adjudication apparatus are structural beneficiaries — the former gets a bounded obligation, the latter has an institutional stake in the line's continued existence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing refoulement of the individually persecuted) remains partially live, which is why this is tangled_rope rather than pure snare: there is a genuine, still-operative coordination function protecting a real population. But the line has not adapted to newer displacement drivers, and its administration increasingly serves an admission-limiting function for host states rather than purely a protection-extending one — the corroboration split (UNHCR says the line is outrun; host states say it remains adequate) is itself the diagnostic signature of a mandate drifting from its founding purpose without formal acknowledgment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persecution_economic_line_naturalness,
    'Is the persecution/economic-migrant distinction a principled moral line (individualized targeting is categorically worse than generalized hardship) or a historically contingent artifact of 1951 drafting politics that host states now find convenient to preserve?',
    'Compare philosophical defenses of the distinction (e.g., culpability/agency arguments for persecution) against the drafting history of the 1951 Convention and subsequent state practice in refusing to extend it to climate and generalized-violence displacement despite comparable severity of harm.',
    'If principled, the reading is closer to a genuine coordination mechanism with an acceptable exclusion boundary. If contingent-and-convenient, the persistence of the line despite its poor fit with contemporary displacement patterns is better explained by host-state interest in bounding admission than by the moral logic it claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persecution_economic_line_naturalness, conceptual, 'Whether the refugee/economic-migrant line is a principled moral distinction or a convenient historical artifact.').

omega_variable(
    climate_displacement_category_gap,
    'Should climate-driven and generalized-violence displacement be assimilated into the persecution category (widening this reading), treated as a wholly separate protection regime, or left to the current gap where they fall into the excluded economic-migrant category?',
    'Track adoption and enforcement of complementary/subsidiary protection regimes (EU Qualification Directive, Cartagena Declaration, Kampala Convention) as evidence of whether states are functionally expanding the beneficiary class without amending the formal doctrine.',
    'Expansion would shrink the victim set and move the reading''s metrics toward rope; continued exclusion under the current doctrine would confirm the bifurcated-victim-set structure as stable rather than transitional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_displacement_category_gap, empirical, 'Whether the doctrinal gap for climate/violence displacement is closing or hardening.').

omega_variable(
    adjudication_error_direction,
    'Is asylum adjudication error (misclassifying genuine persecution claims as economic) randomly distributed, or systematically biased toward under-recognition due to political pressure to limit admissions?',
    'Statistical analysis of asylum grant-rate variation correlated with political cycles, adjudicator caseload, and legal representation access, controlling for underlying claim strength.',
    'Systematic under-recognition bias would indicate the adjudication apparatus is not a neutral administrator of the humanitarian line but an active participant in converting genuine refugees into the excluded economic-migrant category — strengthening the tangled_rope reading over a purely coordination-focused rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adjudication_error_direction, empirical, 'Whether asylum adjudication errors systematically disfavor genuine claimants under political pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1951, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1951, 0.15).
narrative_ontology:measurement(bord_tr_t1970, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(bord_tr_t1990, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1990, 0.26).
narrative_ontology:measurement(bord_tr_t2005, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2005, 0.31).
narrative_ontology:measurement(bord_tr_t2015, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(bord_tr_t2024, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(bord_be_t1951, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1951, 0.28).
narrative_ontology:measurement(bord_be_t1970, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1970, 0.33).
narrative_ontology:measurement(bord_be_t1990, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(bord_be_t2005, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2005, 0.46).
narrative_ontology:measurement(bord_be_t2015, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2015, 0.49).
narrative_ontology:measurement(bord_be_t2024, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1951, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1951, 0.35).
narrative_ontology:measurement(bord_su_t1970, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(bord_su_t1990, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1990, 0.48).
narrative_ontology:measurement(bord_su_t2005, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(bord_su_t2015, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(bord_su_t2024, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2024, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__humanitarian_obligation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__freedom_of_movement_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the border_legitimacy kernel. The sovereignty_reading treats exclusion authority as flowing from territorial sovereignty with no categorical admission obligation (single undifferentiated victim set, lower coordination function, likely closer to snare or tangled_rope with a thinner coordination story). The freedom_of_movement_reading treats borders as presumptively illegitimate restrictions on a human right (near-universal beneficiary set among migrants, minimal legitimate victim class, likely closer to rope or snare depending on enforcement). This reading occupies the structural middle: it authors a genuine but partial coordination function (protecting the individually persecuted) alongside an actively enforced, categorically bifurcated victim set (excluded economic/survival migrants and misclassified asylum seekers) — hence tangled_rope rather than either sibling's likely classification. Each reading's ε is authored independently per the ε-invariance principle; they are not three measurements of one constraint but three distinct constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
