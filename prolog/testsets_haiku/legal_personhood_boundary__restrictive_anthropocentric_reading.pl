% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__restrictive_anthropocentric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__restrictive_anthropocentric_reading, []).

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
 *   constraint_id: legal_personhood_boundary__restrictive_anthropocentric_reading
 *   human_readable: Legal Personhood Boundary: Restrictive Anthropocentric Reading (Born Humans with Cognitive Capacity)
 *   domain: legal/constitutional/rights_theory
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'legal_personhood_boundary.' The restrictive-anthropocentric reading
 *   anchors personhood to birth and (for born humans) to the presumption of
 *   cognitive capacity sufficing for rights-bearing status. This reading
 *   excludes fetuses, ecosystems, artificial intelligences, and animals from
 *   the personhood category entirely—not on functional grounds (some humans
 *   lack demonstrable capacities) but because the reading's core axiom
 *   prioritizes categorical clarity (birth as the event) over capacity
 *   assessment. The constraint coordinates legitimate reproductive autonomy
 *   and clear legal classification (addressing the founding problem:
 *   indeterminacy about fetal status) while extracting from fetal-advocates,
 *   environmental constituencies, and future-generation interests by denying
 *   them legal standing. The reading is one of three sibling readings of the
 *   same kernel: developmental-potentiality (personhood from conception),
 *   functional-capacity (personhood follows demonstrable sentience regardless
 *   of species), and this restrictive-anthropocentric constraint. This JSON
 *   generates ONLY the restrictive reading; the other readings are separate
 *   constraint stories linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - pregnant_persons: holders of reproductive autonomy; role beneficiary; benefit from clear legal personhood for themselves and absence of state paternalism
 *   - state_regulatory_apparatus: agenda-setter; enforces the birth boundary; benefits from administrative clarity
 *   - fetal_advocates: structurally excluded; pay the cost of fetal non-personhood; unable to represent fetal interests in law
 *   - environmental_constituencies: payers; ecosystems and future generations lack standing under this reading
 *   - cognitive_capacity_borderline_humans: ambiguously positioned; retain birth-based personhood but vulnerable to capacity-based erosion
 *   - judicial_interpreters: observers; adjudicate edge cases (anencephalic infants, PVS patients, viability claims) that test the boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.68).
domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.72).
domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Legal Personhood Boundary: Restrictive Anthropocentric Reading (Born Humans with Cognitive Capacity)").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal/constitutional/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__restrictive_anthropocentric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, '5c5cab1f-f59a-4c95-998d-52fa9299e3e6').
narrative_ontology:cs_kernel_codification('5c5cab1f-f59a-4c95-998d-52fa9299e3e6', fixed_text).
narrative_ontology:cs_authority_grounding('5c5cab1f-f59a-4c95-998d-52fa9299e3e6', lineage).
narrative_ontology:cs_interpretation_layer_present('5c5cab1f-f59a-4c95-998d-52fa9299e3e6').
narrative_ontology:cs_reading_relation('5c5cab1f-f59a-4c95-998d-52fa9299e3e6', legal_personhood_boundary__developmental_potentiality_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c5cab1f-f59a-4c95-998d-52fa9299e3e6', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('5c5cab1f-f59a-4c95-998d-52fa9299e3e6', foundational, personhood_vests_at_birth).
narrative_ontology:cs_axiom_status(personhood_vests_at_birth, holdable).
narrative_ontology:cs_axiom_grounding('5c5cab1f-f59a-4c95-998d-52fa9299e3e6', personhood_vests_at_birth, conventional).
narrative_ontology:cs_axiom('5c5cab1f-f59a-4c95-998d-52fa9299e3e6', foundational, reproductive_autonomy_supremacy).
narrative_ontology:cs_axiom_status(reproductive_autonomy_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('5c5cab1f-f59a-4c95-998d-52fa9299e3e6', reproductive_autonomy_supremacy, deontological).
narrative_ontology:cs_reference_frame('5c5cab1f-f59a-4c95-998d-52fa9299e3e6', birth_based_personhood_doctrine).
narrative_ontology:cs_drift_state('5c5cab1f-f59a-4c95-998d-52fa9299e3e6', contemporary_post_potentiality_challenge_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5c5cab1f-f59a-4c95-998d-52fa9299e3e6', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, state_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_autonomy_advocates).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, fetal_rights_claimants).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_constituencies).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, future_generations).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, non_human_sentient_beings).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitive_capacity_borderline_humans).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, fetal_advocates_and_potential_persons).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_protection_movements).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitive_capacity_borderline_humans).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, constitutional_personhood_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, bodily_autonomy_supremacy_in_reproduction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, personhood status is vested in the pregnant person, not the fetus. This maximizes reproductive autonomy: the pregnant person retains exclusive decision-making authority over continuation or termination of pregnancy without requiring fetal consent or state paternalism. They face social pressure and access barriers but retain legal personhood standing to challenge restrictions on their choices.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons, beneficiary,
    moderate, biographical, constrained, national).

% Enforces the boundary that personhood begins at birth. The state prosecutes violations (protecting fetal life before birth is not a state duty under this reading), adjudicates personhood claims in courts, and legislates within the bounds this reading sets. The apparatus benefits from clarity in legal status (born humans are unambiguous persons) and from simplified administrative classification.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, state_regulatory_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% Under this reading, fetuses are excluded from the personhood category and thus from rights-bearing status. Advocates for fetal rights (often religious constituencies) are present but structurally unable to represent fetal interests in legal proceedings—no seat at the table. They bear the cost of exclusion: fetal interests cannot be weighed against pregnant person autonomy in law, and arguments for state protection of fetal life are categorically defeated by the personhood boundary itself.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, fetal_advocates_and_potential_persons, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(legal_personhood_boundary__restrictive_anthropocentric_reading, fetal_advocates_and_potential_persons, excluded).

% Environmental law operates outside the personhood framework this reading establishes. Ecosystems, species, and future generations are not rights-bearing persons; they must be protected through indirect mechanisms (government interest, public trust doctrine, intergenerational duties) rather than through direct legal standing. This constrains environmental litigation and places conservation law on weaker constitutional footing than rights claims.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_protection_movements, payer,
    organized, generational, constrained, global).

% AI systems and future sentient machines fall outside the anthropocentric boundary—personhood requires biological human birth. This reading forecloses the possibility of AI rights claims based on demonstrated cognitive capacity. The exclusion is structural and categorical, not contingent on empirical questions about machine sentience.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligence_and_potential_sentient_systems, excluded,
    powerless, immediate, trapped, global).

% Humans with profound cognitive disabilities, dementia, or brain injuries occupy an ambiguous position: they retain personhood status by virtue of birth, but the cognitive-capacity prong of the reading can be weaponized to question whether they hold the full bundle of rights (competency determinations, guardianship, medical decision-making authority). They benefit from the birth-based floor but are vulnerable to capacity-based erosion of autonomy.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitive_capacity_borderline_humans, beneficiary,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitive_capacity_borderline_humans, payer).

% Courts and legal scholars must apply and defend this boundary: interpreting statutes, adjudicating edge cases (anencephalic infants, persistent vegetative states, fetal viability claims), and articulating the doctrinal rationale. They witness the tension between the birth requirement and the cognitive-capacity requirement when cases present humans born but lacking demonstrable cognitive function.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, judicial_interpreters_and_legal_academia, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__restrictive_anthropocentric_reading, state_regulatory_apparatus).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__restrictive_anthropocentric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves foundational legal indeterminacy about when personhood vests by anchoring it to a clear, objective event (birth) rather to metaphysical or developmental questions. Enables pregnant persons to exercise reproductive autonomy without state paternalism by vesting personhood in the pregnant person rather than distributing it across potential future persons. Provides courts and legislatures with a bright-line rule for rights allocation.
% TRANSFER_FUNCTION: Transfers reproductive decision-making authority from the state (or any third party claiming to represent fetal interests) to the pregnant person. Transfers legal standing from fetuses, ecosystems, and animals to born humans. Transfers the cost of indeterminacy resolution onto those whose interests the boundary excludes (fetal advocates, environmental constituencies, future generations).
% ABSENT_VOICES: Fetuses cannot speak or be represented in legal proceedings under this reading. Environmental and animal-rights movements have institutional voice but lack legal standing to claim personhood on behalf of ecosystems or non-human sentient beings. Future generations have no institutional representation. Religious and potentiality-based traditions have organizational voice (churches, advocacy groups) but are structurally excluded from the core personhood narrative (they can argue for additional protections through state interest, but cannot assert fetal personhood directly).
% DISAPPEARANCE_RATIONALE: If this reading and its institutional enforcement disappeared, alternative readings would immediately restructure personhood law: potentiality-based fetal standing would reemerge, functional-capacity arguments would apply to animals and possibly AI, environmental law would gain direct standing for future generations and ecosystems. Reproductive autonomy law would face new litigation. The entire hierarchy of rights would reorganize around competing personhood framings.
% FOUNDING_PROBLEM: Medieval and early modern legal systems lacked a coherent rule for personhood: Is a fetus a person? At what stage? Natural law and theology produced competing answers that made law indeterminate and vulnerable to claims from all directions. The birth-based boundary solved this by tying personhood to an objective, observable event rather than to contested metaphysical or empirical claims. The cognitive-capacity component ensures the boundary does not trap humans who lack demonstrable capacities (who remain persons by birth).
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the beneficiary set (reproductive-autonomy advocates) attest that medieval and early modern law faced genuine indeterminacy about fetal status and that the shift to birth-based personhood in 19th and 20th century constitutional law was motivated by this problem. However, potentiality-based and functional-capacity traditions deny the founding problem was ever genuine: they argue the indeterminacy reflects a failure of legal imagination rather than an inherent difficulty. Contemporary legislative attempts to narrow personhood boundaries (fetal personhood amendments) and contemporary philosophical work on animal personhood attest that the founding problem is re-emerging as a live question. Academic scholars in philosophy and law outside the direct beneficiary set (e.g., animal rights theorists) note that the birth-based boundary is arbitrary and re-creates legal indeterminacy by requiring cognitive-capacity judgments that were supposed to be avoided.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__restrictive_anthropocentric_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__restrictive_anthropocentric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(legal_personhood_boundary__restrictive_anthropocentric_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) and suppression (0.72) are high because the constraint operates as enforced legal hierarchy: fetal interests are categorically excluded from rights consideration, not through empirical assessment but through definition. The temporal measurements show extractiveness rising from 0.52 to 0.68 over the interval and then plateauing—this reflects mounting challenges to the boundary (rising potentiality and functional-capacity arguments in courts and legislatures) that the constraint must actively suppress to maintain. Theater ratio (0.41, rising from 0.22 to 0.41) reflects growing performative defense: the personhood boundary is increasingly a doctrinal stance maintained against competing readings rather than a natural fact. Accessibility alternatives (potentiality-based personhood, capacity-based standing for animals/AI) are intellectually and institutionally available but are actively foreclosed by this reading's definition. Resistance (0.59) is substantial because potentiality-advocates, environmental movements, and animal-rights constituencies mount continuous pressure against the boundary. The coercion grid shows structural-level suppression rising from 0.61 to 0.74—the state apparatus hardens enforcement as challenges mount—while individual-level suppression stays roughly constant (the pregnant person experiences consistent regulatory authority regardless of rising institutional investment). Class-level resistance (from ecological and religious constituencies) rises slightly but is effectively contained by the structural-level suppression mechanism. Accessibility collapse is highest at the structural level (0.80–0.82), indicating the boundary is constitutionally entrenched and difficult to alter through ordinary legislation, but lower at the individual level (0.68–0.70), indicating pregnant persons and cognitive-borderline humans retain some space to litigate exceptions.
 *
 * PERSPECTIVAL GAP:
 *   The state and reproductive-autonomy advocates (who benefit from clear personhood) compute this as rope: a genuine coordination mechanism addressing foundational legal indeterminacy. Fetal advocates and potentiality-based traditions compute it as snare: the personhood boundary is a decision favoring one reading and suppressing all others through legal force. Functional-capacity advocates (the third reading) compute it as snare-to-tangled-rope depending on which capacities are recognized: if animals or AI demonstrate the required capacities, this reading's exclusion becomes indefensible extraction. The engine computes per-seat directionality from the structural data: pregnant persons get low d (beneficiaries), fetal advocates get high d (structurally excluded, interests foreclosed), environmental constituencies get high d (systematic exclusion from standing). The state sits near d=0.5 (coordinating genuine chaos + extracting through definition). This divergence is the core measurement the corpus takes: between the claim (rope, coordinating a genuine chaos) and the structural reality (tangled-rope slipping toward snare under pressure from alternative readings).
 *
 * DIRECTIONALITY LOGIC:
 *   Pregnant persons benefit from this reading's allocation of reproductive decision-making authority to themselves rather than to the state or fetal representatives. Their directionality is low (near 0.2–0.3): the constraint subsidizes their autonomy. Fetal advocates and potentiality-based traditions are structurally excluded; their interests (recognizing fetal personhood) are foreclosed by definition, not merely disfavored. Their directionality is high (0.8–0.9): they pay the full cost of exclusion. Environmental constituencies face a similar structure: ecosystems and future generations are denied standing by the anthropocentric boundary, so their directionality is high (0.75–0.85). The state apparatus sits at moderate directionality (0.45–0.55): it benefits from the clarity the boundary provides but must expend enforcement energy as competing readings mount pressure. Cognitive-capacity borderline humans occupy an unstable position: they retain personhood through the birth prong (low d) but are vulnerable to capacity-based arguments (rising d) as the cognitive-capacity requirement becomes a pressure point in litigation. No override is needed if the beneficiary/victim declarations capture this structure; the engine derives d from the structural facts.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits early signs of mandate decay: the founding problem (legal indeterminacy about fetal status) was solved by anchoring personhood to birth, but the mounting pressure from potentiality and functional-capacity readings indicates the founding function is no longer universally accepted as legitimate. The theater ratio rises from 0.22 to 0.41 as the constraint's operation becomes increasingly performative (the boundary is maintained through definitional force rather than natural inevitability). However, mandatrophy is not resolved: the boundary still functions as a coordination mechanism for reproductive law and rights allocation. The reading has not degraded to piton status (where the beneficiaries no longer believe it and the payers cannot escape it). Instead, it sits in contested tangled-rope status: it coordinates reproductive autonomy (genuine coordination function) while extracting from fetal-advocates, environmental constituencies, and future generations through categorical exclusion (asymmetric extraction). The mismatch between the claimed type (rope) and the computed type (tangled_rope) reflects the kernel-contest structure: this reading claims to be a natural, inevitable boundary but is actually a contested doctrinal choice that must be defended against live alternative readings. The constraint persists not because it is the only defensible reading but because the institutional machinery of birth-based personhood is entrenched and the beneficiaries (state, reproductive autonomy advocates) have power to maintain the boundary through law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    birth_as_arbitrary_boundary,
    'Is the birth event a natural, meaningful boundary for personhood, or is it an arbitrary point that creates instability because sentience and cognitive capacity develop gradually?',
    'Neuroscientific evidence on fetal sentience and post-birth cognitive development; comparative analysis of legal systems using alternative boundaries (viability, functional capacity, conception); litigation patterns in jurisdictions testing the boundary (anencephalic infants, extreme prematurity cases).',
    'If birth is arbitrary, the reading becomes unstable under pressure from potentiality and functional-capacity arguments; if birth is shown to be the precise point at which the sufficient personhood criteria are met, the boundary becomes defensible as natural law rather than doctrinal choice. High impact on whether this constraint reads as mountain (natural boundary) vs. snare (contested exclusion).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(birth_as_arbitrary_boundary, empirical, 'Whether birth is a natural or constructed boundary for personhood').

omega_variable(
    cognitive_capacity_floor_ambiguity,
    'What level of cognitive capacity suffices for personhood under the born-human prong? Does profound cognitive disability exclude someone from full personhood, or is birth a sufficient marker even without demonstrable capacity?',
    'Jurisprudence on guardianship, competency determinations, and rights of humans with severe cognitive disabilities; empirical analysis of how courts apply capacity tests in practice; philosophical and legal argument about whether the reading''s ''cognitive capacity'' clause is aspirational or a gating requirement.',
    'If birth alone suffices, the reading is internally coherent but vulnerable to the charge that it smuggles in an unexplained exception to the cognitive-capacity requirement. If cognitive capacity is actually a gate, the reading collapses into functional-capacity personhood and loses the birth-based clarity it claims to provide.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_capacity_floor_ambiguity, conceptual, 'Whether cognitive capacity is a gate or aspirational for born humans').

omega_variable(
    suppression_vs_natural_law,
    'Is the measured suppression (0.72) a symptom that the reading requires active enforcement to maintain, indicating it is not a natural boundary but a contested doctrinal choice?',
    'Comparative institutional analysis: in jurisdictions where potentiality-based or functional-capacity readings are entrenched, do they require comparable suppression machinery? If so, suppression reflects the cost of any clear boundary, not proof this one is constructed. If potentiality or functional-capacity readings can persist with lower suppression, it indicates this reading is unstable under its own logic.',
    'High suppression + rising theater ratio suggests the boundary is increasingly performative (defended by institutional power rather than intuitive clarity). This routes toward falsification of the ''natural boundary'' claim and supports reclassification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_vs_natural_law, empirical, 'Whether suppression indicates this reading is constructed rather than natural').

omega_variable(
    kernel_reading_contest,
    'Are the three readings (potentiality, functional-capacity, restrictive-anthropocentric) genuinely exhaustive, or do alternative framings of the personhood boundary exist that would restructure the contest entirely?',
    'Genealogical legal history: recovery of personhood conceptualizations outside the modern Western canon (non-Western, pre-modern, indigenous legal traditions); novel philosophical argument that produces a fourth or fifth reading from the same kernel; empirical discovery of legal systems operating with different personhood boundaries that clarify what is and is not possible.',
    'If the three readings exhaust the possibility space, the kernel contest is a closed triplet. If others exist, the description of sibling readings is incomplete and the constraint''s position within the larger personhood landscape is mischaracterized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the three identified readings are exhaustive').

omega_variable(
    future_generations_standing,
    'Why are future generations'' interests (environmental consequences, climate, resource availability) excluded from personhood standing under this reading? Is the exclusion justified, or is it a cost-shifting mechanism that the reading does not name?',
    'Philosophical argument on intergenerational justice and whether future beings can be rights-bearers; comparative law analysis of jurisdictions that grant future-generation standing (some environmental courts do); empirical analysis of whether climate and environmental law function adequately without direct future-generation representation.',
    'If the exclusion of future generations is unjustified, the reading is not merely restrictive but extractive from an unrepresented constituency, supporting reclassification toward snare. If justified, the reading carries a defended theory of why present-generation duties suffice without granting future persons legal standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generations_standing, preference, 'Whether exclusion of future-generation interests is justified or extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(lega_tr_t0, observed).
narrative_ontology:measurement(lega_tr_t8, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement_basis(lega_tr_t8, observed).
narrative_ontology:measurement(lega_tr_t16, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement_basis(lega_tr_t16, observed).
narrative_ontology:measurement(lega_tr_t24, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement_basis(lega_tr_t24, observed).
narrative_ontology:measurement(lega_tr_t32, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(lega_tr_t32, observed).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(lega_tr_t40, observed).
narrative_ontology:measurement(lega_tr_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(lega_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(lega_be_t0, observed).
narrative_ontology:measurement(lega_be_t8, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(lega_be_t8, observed).
narrative_ontology:measurement(lega_be_t16, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement_basis(lega_be_t16, observed).
narrative_ontology:measurement(lega_be_t24, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(lega_be_t24, observed).
narrative_ontology:measurement(lega_be_t32, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(lega_be_t32, observed).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(lega_be_t40, observed).
narrative_ontology:measurement(lega_be_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(lega_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(lega_su_t0, observed).
narrative_ontology:measurement(lega_su_t8, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(lega_su_t8, observed).
narrative_ontology:measurement(lega_su_t16, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement_basis(lega_su_t16, observed).
narrative_ontology:measurement(lega_su_t24, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(lega_su_t24, observed).
narrative_ontology:measurement(lega_su_t32, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(lega_su_t32, observed).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(lega_su_t40, observed).
narrative_ontology:measurement(lega_su_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(lega_su_t50, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(lega_grid_01, legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse(class), 0, 0.78).
narrative_ontology:measurement(lega_grid_02, legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse(class), 50, 0.79).
narrative_ontology:measurement(lega_grid_03, legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse(individual), 0, 0.68).
narrative_ontology:measurement(lega_grid_04, legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse(individual), 50, 0.7).
narrative_ontology:measurement(lega_grid_05, legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse(organizational), 0, 0.75).
narrative_ontology:measurement(lega_grid_06, legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse(organizational), 50, 0.76).
narrative_ontology:measurement(lega_grid_07, legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse(structural), 0, 0.82).
narrative_ontology:measurement(lega_grid_08, legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse(structural), 50, 0.8).
narrative_ontology:measurement(lega_grid_09, legal_personhood_boundary__restrictive_anthropocentric_reading, resistance(class), 0, 0.64).
narrative_ontology:measurement(lega_grid_10, legal_personhood_boundary__restrictive_anthropocentric_reading, resistance(class), 50, 0.61).
narrative_ontology:measurement(lega_grid_11, legal_personhood_boundary__restrictive_anthropocentric_reading, resistance(individual), 0, 0.48).
narrative_ontology:measurement(lega_grid_12, legal_personhood_boundary__restrictive_anthropocentric_reading, resistance(individual), 50, 0.52).
narrative_ontology:measurement(lega_grid_13, legal_personhood_boundary__restrictive_anthropocentric_reading, resistance(organizational), 0, 0.58).
narrative_ontology:measurement(lega_grid_14, legal_personhood_boundary__restrictive_anthropocentric_reading, resistance(organizational), 50, 0.62).
narrative_ontology:measurement(lega_grid_15, legal_personhood_boundary__restrictive_anthropocentric_reading, resistance(structural), 0, 0.52).
narrative_ontology:measurement(lega_grid_16, legal_personhood_boundary__restrictive_anthropocentric_reading, resistance(structural), 50, 0.48).
narrative_ontology:measurement(lega_grid_17, legal_personhood_boundary__restrictive_anthropocentric_reading, stakes_inflation(class), 0, 0.62).
narrative_ontology:measurement(lega_grid_18, legal_personhood_boundary__restrictive_anthropocentric_reading, stakes_inflation(class), 50, 0.68).
narrative_ontology:measurement(lega_grid_19, legal_personhood_boundary__restrictive_anthropocentric_reading, stakes_inflation(individual), 0, 0.72).
narrative_ontology:measurement(lega_grid_20, legal_personhood_boundary__restrictive_anthropocentric_reading, stakes_inflation(individual), 50, 0.71).
narrative_ontology:measurement(lega_grid_21, legal_personhood_boundary__restrictive_anthropocentric_reading, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(lega_grid_22, legal_personhood_boundary__restrictive_anthropocentric_reading, stakes_inflation(organizational), 50, 0.62).
narrative_ontology:measurement(lega_grid_23, legal_personhood_boundary__restrictive_anthropocentric_reading, stakes_inflation(structural), 0, 0.64).
narrative_ontology:measurement(lega_grid_24, legal_personhood_boundary__restrictive_anthropocentric_reading, stakes_inflation(structural), 50, 0.65).
narrative_ontology:measurement(lega_grid_25, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression(class), 0, 0.58).
narrative_ontology:measurement(lega_grid_26, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression(class), 50, 0.71).
narrative_ontology:measurement(lega_grid_27, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression(individual), 0, 0.65).
narrative_ontology:measurement(lega_grid_28, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression(individual), 50, 0.71).
narrative_ontology:measurement(lega_grid_29, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression(organizational), 0, 0.54).
narrative_ontology:measurement(lega_grid_30, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression(organizational), 50, 0.68).
narrative_ontology:measurement(lega_grid_31, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression(structural), 0, 0.61).
narrative_ontology:measurement(lega_grid_32, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression(structural), 50, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.12).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary__developmental_potentiality_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary__functional_capacity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'legal_personhood_boundary,' which decomposes into three structurally distinct constraints: (1) restrictive-anthropocentric reading (this file): personhood vested at birth; high extraction from fetal advocates and environmental constituencies; (2) developmental-potentiality reading: personhood from conception; different victim set and enforcement structure; higher state paternalism, lower reproductive autonomy; (3) functional-capacity reading: personhood follows capacity regardless of species; opens standing for sentient animals and potential future AI. Each reading has its own ε, its own beneficiary/victim structure, and its own type classification. They are not variations of one constraint but three distinct constraints sharing a common kernel. They influence each other: this reading forecloses potentiality-based standing in any single legal framework; it coexists with functional-capacity reading as competing live positions in courts and legislatures; the potentiality reading influences this one (pressure from anti-abortion movements that attempt to establish fetal personhood forces this reading to defend its boundary more explicitly). Each sibling must link to the others via network.affects_constraints; the ε-invariance principle requires separate stories because measuring personhood via 'at birth' vs. 'at conception' vs. 'where capacity is demonstrated' yields structurally different constraints with different values of the core parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_personhood_boundary__restrictive_anthropocentric_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
